cmo_lopsided <- function(n = 100, big = 300, seed = 5) {

  set.seed(seed)

  ids <- paste0("P", seq_len(n))

  micro <- as.data.frame(matrix(rgamma(n * big, 2), ncol = big))
  micro <- micro / rowSums(micro)
  colnames(micro) <- paste0("OTU", seq_len(big))
  rownames(micro) <- ids

  scfa <- 3 * micro$OTU1 + rnorm(n, 5, 0.5)
  apoa1 <- 2 * scfa + rnorm(n, 120, 10)

  proteome <- as.data.frame(matrix(rnorm(n * 20), ncol = 20))
  colnames(proteome) <- paste0("PR", seq_len(20))
  proteome$APOA1 <- apoa1
  rownames(proteome) <- ids

  obj <- load_data(
    list(
      microbiome = micro,
      metabolome = data.frame(SCFA = scfa, M2 = rnorm(n), row.names = ids),
      proteome = proteome,
      clinical = data.frame(bmi = rnorm(n, 26, 3), sbp = rnorm(n, 130, 15),
                            row.names = ids)
    ),
    metadata = data.frame(sample_id = ids,
                          HDL = 0.05 * apoa1 + 0.4 * scfa + rnorm(n),
                          age = rnorm(n, 60, 8), stringsAsFactors = FALSE)
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

# =============================================================================
# Screening
# =============================================================================

test_that("a huge block no longer takes the whole screening budget", {

  # Regression: screening ranked every feature together, so 300 microbiome
  # columns took all 60 slots and the two-variable clinical block vanished
  # from the analysis, taking every cross-block mediation with it.

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 60))

  surviving <- table(res$data$feature_block[colnames(res$data$x)])

  expect_true(all(c("microbiome", "metabolome", "proteome", "clinical") %in%
                    names(surviving)))

  # The small blocks come through whole.
  expect_equal(unname(surviving[["clinical"]]), 2L)
  expect_equal(unname(surviving[["metabolome"]]), 2L)

  # And the big one no longer has everything.
  expect_lt(unname(surviving[["microbiome"]]), 60)

})

test_that("the allocation is recorded, not just applied", {

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 60))

  allocation <- res$data$screening$allocation

  expect_false(is.null(allocation))
  expect_equal(sum(allocation), res$performance$features_retained)

})

test_that("the quota respects the floor and never exceeds the block", {

  feature_block <- c(rep("big", 500), rep("small", 3))
  names(feature_block) <- c(paste0("B", 1:500), paste0("S", 1:3))

  quota <- .allocate_screening(feature_block, max_features = 50,
                               min_per_block = 10)

  expect_equal(sum(quota), 50)
  expect_equal(unname(quota[["small"]]), 3)   # smaller than the floor
  expect_equal(unname(quota[["big"]]), 47)

})

test_that("no block is dropped, even when the budget cannot stretch", {

  # With more blocks than slots the budget has to yield: losing a block
  # entirely is the failure this allocation exists to prevent, and a silent
  # gap is worse than exceeding a target the user set for speed.

  feature_block <- stats::setNames(paste0("b", 1:20), paste0("f", 1:20))

  quota <- .allocate_screening(feature_block, max_features = 5,
                               min_per_block = 10)

  expect_true(all(quota >= 1))
  expect_length(quota, 20)

})

test_that("the floor is raised evenly before anyone goes above it", {

  # A small block should be filled before a large one takes a second helping,
  # or the large one soaks up the budget again through the back door.

  feature_block <- c(rep("big", 200), rep("small", 8))
  names(feature_block) <- c(paste0("B", 1:200), paste0("S", 1:8))

  quota <- .allocate_screening(feature_block, max_features = 20,
                               min_per_block = 10)

  expect_equal(sum(quota), 20)
  expect_equal(unname(quota[["small"]]), 8)   # the whole small block
  expect_equal(unname(quota[["big"]]), 12)

})

test_that("a budget larger than the data changes nothing", {

  feature_block <- c(a = "x", b = "x", c = "y")

  expect_equal(sum(.allocate_screening(feature_block, 100)), 3)

})

# =============================================================================
# Block-level evidence
# =============================================================================

test_that("evidence is summarised between blocks", {

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 50))

  blocks <- res$network$blocks$evidence

  expect_s3_class(blocks, "data.frame")
  expect_gt(nrow(blocks), 0)

  expect_true(all(c("from", "to", "relationships", "mean_score",
                    "crosses_blocks", "reaches_outcome") %in% names(blocks)))

  # The counts have to add up to the edges in the graph.
  expect_equal(sum(blocks$relationships), nrow(res$graph$edges))

})

test_that("block shares add up to the whole", {

  # A relationship between two blocks belongs to both, so counting it whole
  # in each made the shares sum past 100.

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 50))

  importance <- res$network$blocks$importance

  expect_gt(nrow(importance), 0)
  expect_equal(sum(importance$share_percent), 100, tolerance = 0.5)

})

test_that("block importance reflects where the evidence actually is", {

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 50))

  importance <- res$network$blocks$importance

  expect_true(all(importance$features_analysed > 0))
  expect_true(all(importance$share_percent >= 0))

  # Sorted best first.
  expect_false(is.unsorted(rev(importance$share_percent)))

})

test_that("blocks are scored by kind of evidence", {

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 50))

  scores <- res$network$blocks$scores

  expect_true(all(c("predictive", "associational", "temporal",
                    "mechanistic") %in% names(scores)))

  numeric_columns <- scores[, -1, drop = FALSE]

  for (column in numeric_columns) {
    finite <- column[is.finite(column)]
    if (length(finite) > 0) {
      expect_true(all(finite >= 0 & finite <= 1))
    }
  }

})

test_that("communities are described by what they are made of", {

  skip_if_not_installed("igraph")

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 50))

  communities <- res$network$blocks$communities

  skip_if(nrow(communities) == 0)

  expect_true(all(c("community", "size", "composition", "kind") %in%
                    names(communities)))

  expect_true(all(communities$kind %in%
                    c("single layer", "dominated by one layer",
                      "mixed layers")))

  # "Community 3" says nothing; the composition has to name the layers.
  expect_true(all(grepl("%", communities$composition)))

})

test_that("the graph can be read with blocks as the nodes", {

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 50))

  block_graph <- res$network$blocks$graph

  expect_gt(nrow(block_graph$nodes), 0)

  # Only relationships that actually cross appear as edges between blocks.
  if (nrow(block_graph$edges) > 0) {
    expect_true(all(block_graph$edges$from != block_graph$edges$to))
  }

  expect_true("internal_relationships" %in% names(block_graph$nodes))

})

# =============================================================================
# cross_block stays out of the score
# =============================================================================

test_that("crossing blocks is reported beside the score, not inside it", {

  res <- analyze(cmo_lopsided(), "HDL", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(max_features = 50))

  edges <- res$graph$edges

  expect_true(all(c("source_block", "target_block", "cross_block") %in%
                    names(edges)))

  within <- edges[isFALSE(edges$cross_block) |
                    (!is.na(edges$cross_block) & !edges$cross_block), ]
  across <- edges[!is.na(edges$cross_block) & edges$cross_block, ]

  skip_if(nrow(within) == 0 || nrow(across) == 0)

  # The score is built from size, precision and agreement alone, so knowing
  # whether an edge crosses tells you nothing about its score. If crossing
  # were folded in, every crossing edge would outrank every internal one.
  expect_false(min(across$evidence_score) > max(within$evidence_score))

})

test_that("an edge touching the outcome is not called cross-block", {

  # Everything reaching the outcome crosses from a block to the outcome, so
  # marking those would make the field true almost everywhere and mean
  # nothing.

  expect_true(is.na(.is_cross_block("proteome", "outcome")))
  expect_true(is.na(.is_cross_block("outcome", "proteome")))

  expect_true(.is_cross_block("proteome", "metabolome"))
  expect_false(.is_cross_block("proteome", "proteome"))
  expect_true(is.na(.is_cross_block(NA_character_, "proteome")))

})
