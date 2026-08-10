cmo_layered <- function(n = 100, seed = 5) {

  set.seed(seed)

  ids <- paste0("P", seq_len(n))

  micro <- as.data.frame(matrix(rgamma(n * 60, 2), ncol = 60))
  micro <- micro / rowSums(micro)
  colnames(micro) <- paste0("OTU", seq_len(60))
  rownames(micro) <- ids

  scfa <- 3 * micro$OTU1 + rnorm(n, 5, 0.5)
  apoa1 <- 2 * scfa + rnorm(n, 120, 10)

  prot <- as.data.frame(matrix(rnorm(n * 15), ncol = 15))
  colnames(prot) <- paste0("PR", seq_len(15))
  prot$APOA1 <- apoa1
  rownames(prot) <- ids

  obj <- load_data(
    list(
      microbiome = micro,
      metabolome = data.frame(SCFA = scfa, TMAO = rnorm(n), row.names = ids),
      proteome = prot,
      clinical = data.frame(bmi = rnorm(n, 26, 3), row.names = ids)
    ),
    metadata = data.frame(sample_id = ids,
                          HDL = 0.05 * apoa1 + 0.4 * scfa + rnorm(n),
                          stringsAsFactors = FALSE)
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

test_that("analyze() produces both circos figures", {

  res <- analyze(cmo_layered(), "HDL", effort = "fast", plots = TRUE, quiet = TRUE, 
    control = analysis_control(max_features = 40))

  expect_true("circos" %in% names(res$plots))
  expect_s3_class(res$plots$circos, "recordedplot")

  expect_true("circos_blocks" %in% names(res$plots))
  expect_s3_class(res$plots$circos_blocks, "recordedplot")

  # A recorded plot with an empty display list replays as a blank canvas.
  expect_gt(length(res$plots$circos[[1]]), 0)
  expect_gt(length(res$plots$circos_blocks[[1]]), 0)

})

test_that("the circos survives being rasterised", {

  res <- analyze(cmo_layered(), "HDL", effort = "fast", plots = TRUE, quiet = TRUE, 
    control = analysis_control(max_features = 40))

  uri <- .html_plot_uri(res$plots$circos, width = 700, height = 700, res = 100)

  expect_false(is.null(uri))
  expect_match(uri, "^data:image/png;base64,")
  expect_gt(nchar(uri), 5000)

})

test_that("degenerate input gives nothing rather than an empty canvas", {

  expect_null(.plot_circos(EvidenceGraph(), "y"))
  expect_null(.plot_circos_blocks(data.frame(), "y"))

  # A graph with too few nodes to arrange around a circle.
  tiny <- EvidenceGraph()
  tiny$nodes <- data.frame(name = c("a", "y"), block = c("b", "outcome"),
                           evidence = c(1, 1), stringsAsFactors = FALSE)
  tiny$edges <- data.frame(source = "a", target = "y", evidence_score = 10,
                           stringsAsFactors = FALSE)

  expect_null(.plot_circos(tiny, "y"))

})

test_that("only relationships that cross layers appear in the block circos", {

  # An arc to itself would be a chord from a point to the same point.

  evidence <- data.frame(
    from = c("a", "a", "b"), to = c("a", "b", "outcome"),
    relationships = c(5L, 3L, 2L), total_evidence = c(100, 50, 20),
    stringsAsFactors = FALSE
  )

  expect_s3_class(.plot_circos_blocks(evidence, "outcome"), "recordedplot")

  # Nothing crossing at all: there is no figure to draw.
  only_internal <- evidence[evidence$from == evidence$to, , drop = FALSE]
  expect_null(.plot_circos_blocks(only_internal, "outcome"))

})

# =============================================================================
# The geometry underneath
# =============================================================================

test_that("a chord starts and ends exactly on the rim", {

  curve <- .chord_xy(0, pi, radius = 1)

  expect_equal(curve[1, ], c(1, 0), tolerance = 1e-9)
  expect_equal(curve[nrow(curve), ], c(-1, 0), tolerance = 1e-9)

  # Opposite ends bow through the middle; neighbours hug the rim.
  opposite <- .chord_xy(0, pi, 1)
  adjacent <- .chord_xy(0, 0.2, 1)

  depth <- function(m) min(sqrt(rowSums(m^2)))

  expect_lt(depth(opposite), 0.2)
  expect_gt(depth(adjacent), 0.8)

})

test_that("a chord bows the short way round the circle", {

  # Averaging two angles fails across the wrap-around, sending the control
  # point to the far side and drawing the curve right through the disc.

  curve <- .chord_xy(0.1, 2 * pi - 0.1, radius = 1)

  midpoint <- curve[round(nrow(curve) / 2), ]

  expect_gt(midpoint[1], 0)

})

test_that("sectors are sized by what each block contributed", {

  nodes <- data.frame(
    name = c(paste0("f", 1:10), "y"),
    block = c(rep("big", 8), rep("small", 2), "outcome"),
    evidence = c(runif(10), 5),
    stringsAsFactors = FALSE
  )

  layout <- .circos_layout(nodes, "y")

  expect_true(layout$span[["big"]] > layout$span[["small"]])

  # The outcome is placed last, so it reads as the destination.
  expect_equal(utils::tail(layout$sectors, 1), "outcome")

  # Every node landed somewhere.
  expect_false(any(is.na(layout$nodes$angle)))

  # And the sectors together fill the circle, minus the gaps.
  expect_lt(sum(layout$span), 2 * pi)

})
