cmo_consensus_data <- function(n = 200, seed = 21) {

  set.seed(seed)

  ids <- paste0("S", seq_len(n))

  # One unmistakable driver, one borderline, and noise that will wander in
  # and out of the graph from resample to resample.
  strong <- rnorm(n)
  weak <- rnorm(n)
  noise <- matrix(rnorm(n * 6), n, 6,
                  dimnames = list(ids, paste0("n", 1:6)))

  y <- 1.2 * strong + 0.25 * weak + rnorm(n)

  block <- cbind(data.frame(strong = strong, weak = weak, row.names = ids),
                 noise)

  obj <- load_data(list(main = block),
                   metadata = data.frame(sample_id = ids, y = y))

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

cmo_consensus <- function(...) {
  analyze(cmo_consensus_data(), "y", effort = "standard", plots = FALSE, 
    quiet = TRUE, ..., control = analysis_control(methods = "association"))$consensus
}

# =============================================================================
# When there is nothing to say
# =============================================================================

test_that("without resampling it says so instead of implying stability", {

  res <- analyze(cmo_consensus_data(), "y", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(methods = "association"))

  cg <- res$consensus

  expect_s3_class(cg, "ConsensusGraph")
  expect_equal(cg$replicates, 0L)
  expect_equal(nrow(cg$edges), 0)
  expect_match(cg$notes[1], "did not run", fixed = TRUE)

  expect_no_error(capture.output(print(cg)))

  expect_false(any(grepl("Consensus graph", res$logs)))

})

test_that("a failed resampling degrades to an empty object, not an error", {

  cg <- .build_consensus_graph(list(available = FALSE), list())

  expect_s3_class(cg, "ConsensusGraph")
  expect_equal(cg$replicates, 0L)

})

# =============================================================================
# The shape of the graph
# =============================================================================

test_that("every relationship ever seen is accounted for", {

  cg <- cmo_consensus()

  expect_gt(cg$replicates, 0)
  expect_true(all(c("source", "target", "frequency", "consistent_frequency",
                    "median_rank", "best_rank", "worst_rank",
                    "sign_agreement", "orientation_agreement",
                    "reported") %in% names(cg$edges)))

  expect_true(all(cg$edges$frequency > 0 & cg$edges$frequency <= 1))
  expect_true(all(cg$edges$best_rank <= cg$edges$worst_rank))
  expect_true(all(cg$edges$median_rank >= cg$edges$best_rank))
  expect_true(all(cg$edges$median_rank <= cg$edges$worst_rank))

})

test_that("the replicate sizes are recorded, not just their average", {

  # A graph that is 12 edges in one resample and 40 in the next is not a
  # graph, and a median would hide exactly that.

  cg <- cmo_consensus()

  expect_length(cg$sizes, cg$replicates)
  expect_true(all(cg$sizes >= 0))

})

test_that("the real driver is the most consistent relationship there is", {

  cg <- cmo_consensus()

  # Filtered on both ends: a feature is the source of its edge to the outcome
  # and of any feature-to-feature edge a single resample happened to throw up.
  strong <- cg$edges[cg$edges$source == "strong" & cg$edges$target == "y", ]

  expect_equal(nrow(strong), 1)
  expect_equal(strong$frequency, 1)
  expect_equal(strong$sign_agreement, 1)
  expect_equal(strong$best_rank, 1)
  expect_equal(strong$worst_rank, 1)

  # Sorted by consistency, so it is the top row.
  expect_equal(cg$edges$source[1], "strong")

})

# =============================================================================
# Recurrence is not recovery
# =============================================================================

test_that("an edge that flips sign every time does not join the consensus", {

  # The failure this guards against: a variable unrelated to the outcome
  # turns up in nine resamples out of ten pointing a different way each time.
  # Counting appearances alone would report it as highly stable.

  resampling <- list(
    available = TRUE, replicates = 100L, scheme = "bootstrap",
    sizes = rep(2L, 100),
    edges = list(
      "solid -> y" = list(found = 90L, evaluable = 100L, stability = 0.9,
                          sign_agreement = 1.0, ranks = rep(1L, 90)),
      "coinflip -> y" = list(found = 90L, evaluable = 100L, stability = 0.9,
                             sign_agreement = 0.5, ranks = rep(2L, 90))
    )
  )

  cg <- .build_consensus_graph(resampling, list())

  solid <- cg$edges[cg$edges$source == "solid", ]
  flip <- cg$edges[cg$edges$source == "coinflip", ]

  # Identical recurrence...
  expect_equal(solid$frequency, flip$frequency)

  # ...opposite verdicts.
  expect_equal(solid$consistent_frequency, 0.9)
  expect_equal(flip$consistent_frequency, 0.45)

  expect_true("solid" %in% cg$consensus$source)
  expect_false("coinflip" %in% cg$consensus$source)

})

test_that("an edge with no settled orientation is marked as such", {

  resampling <- list(
    available = TRUE, replicates = 100L, scheme = "bootstrap",
    sizes = rep(2L, 100),
    edges = list(
      "a -> b" = list(found = 50L, evaluable = 100L, stability = 0.5,
                      sign_agreement = 1, ranks = rep(1L, 50)),
      "b -> a" = list(found = 50L, evaluable = 100L, stability = 0.5,
                      sign_agreement = 1, ranks = rep(1L, 50))
    )
  )

  cg <- .build_consensus_graph(resampling, list())

  expect_equal(cg$edges$orientation_agreement, c(0.5, 0.5))

})

# =============================================================================
# Agreement with what was reported
# =============================================================================

test_that("both kinds of disagreement with the report are surfaced", {

  cg <- cmo_consensus()
  a <- cg$agreement

  expect_equal(a$both + length(a$reported_only), a$reported)
  expect_equal(a$both + length(a$consensus_only), a$consensus)

  expect_gte(a$jaccard, 0)
  expect_lte(a$jaccard, 1)

})

test_that("relationships that recur but were never reported are named", {

  # The direction a reader cannot discover any other way: the sample that was
  # collected happened not to show these.

  resampling <- list(
    available = TRUE, replicates = 10L, scheme = "bootstrap",
    sizes = rep(1L, 10),
    edges = list(
      "hidden -> y" = list(found = 10L, evaluable = 10L, stability = 1,
                           sign_agreement = 1, ranks = rep(1L, 10))
    )
  )

  reported <- list(.new_edge("shown", "y", 1, "association",
                             "linear regression", quantity = "beta"))

  cg <- .build_consensus_graph(resampling, reported)

  expect_equal(cg$agreement$consensus_only, "hidden -> y")
  expect_equal(cg$agreement$reported_only, "shown -> y")
  expect_equal(cg$agreement$both, 0L)
  expect_equal(cg$agreement$jaccard, 0)

})

test_that("the headline findings are tracked by rank, not just presence", {

  cg <- cmo_consensus()

  rs <- cg$rank_stability

  expect_s3_class(rs, "data.frame")
  expect_gt(nrow(rs), 0)
  expect_equal(rs$reported_rank, seq_len(nrow(rs)))
  expect_true(all(rs$kept_top >= 0 & rs$kept_top <= 1))

  # The unmistakable driver never moved.
  strong <- rs[rs$source == "strong", ]
  expect_equal(strong$kept_top, 1)
  expect_equal(strong$median_rank, 1)

})

# =============================================================================
# What it must not be read as
# =============================================================================

test_that("the caveat travels with the object", {

  # This is the number most likely to be quoted out of context, so it cannot
  # rely on the reader having read the documentation.

  cg <- cmo_consensus()

  expect_true(any(grepl("not evidence that the relationship is real",
                        cg$notes)))

  printed <- capture.output(print(cg))
  expect_true(any(grepl("does not tell you", printed)))

})

test_that("noise looks stable, and that is the point of the caveat", {

  # Documenting the real behaviour rather than pretending otherwise: a
  # bootstrap resamples one dataset, so a chance correlation in that dataset
  # recurs throughout. Anyone reading frequency as evidence of a real effect
  # is reading it wrong, which is why the object says so itself.

  cg <- cmo_consensus()

  noise <- cg$edges[grepl("^n[0-9]$", cg$edges$source) &
                      cg$edges$target == "y", ]

  skip_if(nrow(noise) == 0)

  expect_gt(max(noise$frequency), 0.5)

})

# =============================================================================
# Presentation
# =============================================================================

test_that("the consensus prints and plots without complaint", {

  cg <- cmo_consensus()

  expect_no_error(capture.output(print(cg)))
  expect_no_error(print(.plot_consensus(cg, "y")))

})

test_that("the plot survives a consensus with nothing rankable", {

  cg <- ConsensusGraph()
  expect_null(.plot_consensus(cg, "y"))

})

test_that("the result carries the consensus and the run log mentions it", {

  res <- analyze(cmo_consensus_data(), "y", effort = "standard", plots = FALSE, 
    quiet = TRUE, control = analysis_control(methods = "association"))

  expect_s3_class(res$consensus, "ConsensusGraph")
  expect_true(any(grepl("Consensus graph", res$logs)))

})
