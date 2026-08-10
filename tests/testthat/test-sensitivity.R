cmo_run <- function(seed = 1, effect = 0.8, outcome_type = "continuous",
                    missing = 0.1) {

  sim <- simulate_data(
    n = 250,
    blocks = list(a = c("x", "z"), b = 3),
    dag = data.frame(from = c("x", "z"), to = c("y", "y"),
                     effect = c(effect, 0.4), stringsAsFactors = FALSE),
    outcome = "y", outcome_type = outcome_type,
    missing = c(a = missing), seed = seed
  )

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association"))

}

# =============================================================================
# Comparing two analyses
# =============================================================================

test_that("the four questions get four answers", {

  cmp <- compare_results(cmo_run(1), cmo_run(2))

  expect_s3_class(cmp, "CMOComparison")

  expect_true(all(c("source", "target", "estimate_a", "estimate_b", "ratio",
                    "same_direction", "in_a", "in_b", "status") %in%
                    names(cmp$edges)))

  expect_true(cmp$agreement$both > 0)
  expect_gte(cmp$agreement$jaccard, 0)
  expect_lte(cmp$agreement$jaccard, 1)

  # Every relationship gets exactly one verdict.
  expect_false(any(is.na(cmp$edges$status)))

})

test_that("two runs of the same generator mostly agree", {

  cmp <- compare_results(cmo_run(1), cmo_run(2))

  driver <- cmp$edges[cmp$edges$source == "x", ]

  skip_if(nrow(driver) == 0)

  expect_true(driver$in_a && driver$in_b)
  expect_true(driver$same_direction)
  expect_equal(driver$ratio, 1, tolerance = 0.3)
  expect_match(driver$status, "in both")

})

test_that("a reversal is called a reversal", {

  # The clearest disagreement there is, and the one a comparison based on
  # presence alone would report as agreement.

  a <- cmo_run(1, effect = 0.8)
  b <- cmo_run(2, effect = -0.8)

  cmp <- compare_results(a, b)

  driver <- cmp$edges[cmp$edges$source == "x", ]

  skip_if(nrow(driver) == 0 || !driver$in_a || !driver$in_b)

  expect_false(driver$same_direction)
  expect_equal(driver$status, "reverses")
  expect_true(any(grepl("x -> y", cmp$agreement$reversed, fixed = TRUE)))

})

test_that("a direction that holds at a fraction of the size is flagged", {

  # The usual way a replication is oversold: both significant, both the same
  # way, and the effect is a fifth of what it was.

  cmp <- compare_results(cmo_run(1, effect = 0.9),
                         cmo_run(2, effect = 0.12))

  driver <- cmp$edges[cmp$edges$source == "x", ]

  skip_if(nrow(driver) == 0 || !driver$in_a || !driver$in_b)

  expect_true(driver$same_direction)
  expect_lt(driver$ratio, 0.5)
  expect_equal(driver$status, "in both, much smaller")

})

test_that("comparability is checked before anything is compared", {

  # Sixty per cent overlap between two different questions is a number about
  # nothing, and the reader has to be told that before the number.

  a <- cmo_run(1)

  sim <- simulate_data(n = 200, blocks = list(a = c("x")),
                       dag = data.frame(from = "x", to = "w", effect = 0.8),
                       outcome = "w")
  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)
  b <- analyze(prep, "w", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association"))

  cmp <- compare_results(a, b)

  expect_false(cmp$comparable$comparable)
  expect_true(any(grepl("Different outcomes", cmp$comparable$problems)))
  expect_true(any(grepl("did not ask the same question", cmp$notes)))

})

test_that("a different adjustment makes a different estimate", {

  sim <- simulate_data(
    n = 250, blocks = list(a = c("x")),
    dag = data.frame(from = c("age", "age", "x"), to = c("x", "y", "y"),
                     effect = c(0.5, 0.4, 0.8), stringsAsFactors = FALSE),
    outcome = "y")

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  plain <- analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association"))

  adjusted <- analyze(prep, "y", covariates = "age", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(methods = "association"))

  cmp <- compare_results(plain, adjusted)

  expect_false(cmp$comparable$comparable)
  expect_true(any(grepl("Different adjustment", cmp$comparable$problems)))

})

test_that("nothing is pooled, and the object says so", {

  cmp <- compare_results(cmo_run(1), cmo_run(2))

  printed <- gsub("[[:space:]]+", " ",
                  paste(capture.output(print(cmp)), collapse = " "))

  expect_match(printed, "Nothing here is pooled", fixed = TRUE)
  expect_match(printed, "has not been refuted", fixed = TRUE)

})

test_that("comparison refuses what it cannot compare", {

  expect_error(compare_results("a", cmo_run(1)), "must both be CMOResult")
  expect_error(compare_results(cmo_run(1), cmo_run(2), names = "one"),
               "two labels")

})

test_that("an empty result is handled rather than crashed on", {

  a <- cmo_run(1)

  empty <- a
  empty$evidence <- list()

  cmp <- compare_results(a, empty)

  expect_match(cmp$notes[1], "found no relationships")
  expect_no_error(capture.output(print(cmp)))

})

# =============================================================================
# How much of it is the analyst
# =============================================================================

test_that("every defensible path is run and reported", {

  s <- sensitivity(cmo_run(1), "x", quiet = TRUE)

  expect_s3_class(s, "CMOSensitivity")

  expect_equal(s$source, "x")
  expect_equal(s$target, "y")
  expect_gt(s$ran, 5)

  expect_true(all(c("imputation", "transformation", "estimate", "p_value",
                    "is_original") %in% names(s$paths)))

  # Exactly one path is the one the report came from.
  expect_equal(sum(s$paths$is_original), 1)

})

test_that("the estimates are on one scale, whatever the transformation did", {

  # Without this the spread is a statement about units. A rank transformation
  # puts the exposure on a 1..n scale, so its raw coefficient is a few
  # thousandths of the untransformed one and the range looks catastrophic for
  # a relationship that never moved.

  s <- sensitivity(cmo_run(1), "x", quiet = TRUE)

  expect_true("raw_estimate" %in% names(s$paths))

  raw_spread <- diff(range(s$paths$raw_estimate))
  standardised_spread <- diff(range(s$paths$estimate))

  expect_lt(standardised_spread, raw_spread)

  # And the standardised ones sit close together for a real relationship.
  expect_lt(standardised_spread / abs(stats::median(s$paths$estimate)), 0.5)

})

test_that("scaling is not varied, because it cannot change the answer", {

  # Any affine rescaling leaves a per-SD estimate untouched, so including
  # scaling would fill the table with exact duplicates and report a finding
  # as steadier than it is.

  s <- sensitivity(cmo_run(1), "x", quiet = TRUE)

  expect_equal(length(unique(s$paths$scaling)), 1)

  combinations <- paste(s$paths$imputation, s$paths$transformation)

  expect_equal(length(combinations), length(unique(combinations)))

})

test_that("a real relationship survives every path", {

  s <- sensitivity(cmo_run(1, effect = 0.9), "x", quiet = TRUE)

  expect_equal(s$summary$sign_agreement, 1)
  expect_gt(s$summary$significant, 0.9)
  expect_match(s$verdict, "holds down every reasonable path")

})

test_that("noise passes this check, and the object says why", {

  # Worth fixing in a test because it is counterintuitive and would
  # otherwise be read as a bug. A chance correlation in this sample is not
  # created or destroyed by how the data was transformed, only re-expressed,
  # so a pure-noise feature that happened to correlate with the outcome is
  # perfectly stable across every preprocessing path.
  #
  # That is not a failure of the check. It is what the check measures:
  # dependence on the analyst, not on reality.

  res <- cmo_run(1)

  noise <- Filter(function(e) grepl("^b_", e$source), res$evidence)

  skip_if(length(noise) == 0)

  s <- .safe_try(sensitivity(res, noise[[1]]$source, quiet = TRUE), NULL)

  skip_if(is.null(s))

  expect_true(any(grepl("not evidence that the relationship is real",
                        s$notes)))

  printed <- paste(capture.output(print(s)), collapse = " ")

  expect_match(gsub("[[:space:]]+", " ", printed),
               "noise passes this check", fixed = TRUE)

})

test_that("the verdict follows the numbers", {

  agreeing <- data.frame(estimate = c(0.5, 0.52, 0.48), p_value = rep(1e-5, 3),
                         is_original = c(TRUE, FALSE, FALSE))

  edge <- .new_edge("x", "y", 0.5, "association", "linear regression",
                    quantity = "beta")

  steady <- .sensitivity_result(agreeing, edge, "y", "a", 3)

  expect_match(steady$verdict, "holds down every reasonable path")

  wobbling <- data.frame(estimate = c(0.5, -0.4, 0.1, -0.2),
                         p_value = c(0.01, 0.02, 0.9, 0.4),
                         is_original = c(TRUE, FALSE, FALSE, FALSE))

  shaky <- .sensitivity_result(wobbling, edge, "y", "a", 4)

  expect_match(shaky$verdict, "preprocessing choice with a coefficient")

})

test_that("it works on a binary outcome", {

  s <- sensitivity(cmo_run(1, outcome_type = "binary"), "x", quiet = TRUE)

  expect_gt(s$ran, 3)
  expect_true(all(is.finite(s$paths$estimate)))

})

test_that("the variants keep the original first and cap the rest", {

  recipe <- PreprocessingRecipe()
  recipe$imputation <- "knn"
  recipe$transformation <- "log"
  recipe$scaling <- "z-score"

  variants <- .sensitivity_variants(recipe, max_paths = 6)

  expect_length(variants, 6)
  expect_true(variants[[1]]$original)
  expect_equal(variants[[1]]$label$imputation, "knn")
  expect_equal(variants[[1]]$label$transformation, "log")

  # The fitted models belong to the original choices; replaying them under
  # different ones would apply a median computed on untransformed data to
  # transformed data.
  expect_null(variants[[2]]$recipe$imputation_model)
  expect_null(variants[[2]]$recipe$transformation_model)

  expect_length(.sensitivity_variants("not a recipe"), 0)

})

test_that("sensitivity refuses what it cannot vary", {

  res <- cmo_run(1)

  expect_error(sensitivity("not a result"), "must be a CMOResult")
  expect_error(sensitivity(res, "no_such_variable"),
               "no relationship with y")

  stripped <- res
  stripped$input <- NULL

  expect_error(sensitivity(stripped), "does not carry the preprocessing")

})

test_that("a sensitivity prints without complaint", {

  s <- sensitivity(cmo_run(1), "x", quiet = TRUE)

  printed <- paste(capture.output(print(s)), collapse = " ")

  expect_match(printed, "per SD", fixed = TRUE)
  expect_match(printed, "Agree on direction", fixed = TRUE)
  expect_match(printed, "reported", fixed = TRUE)

})
