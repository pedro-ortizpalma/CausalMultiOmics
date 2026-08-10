cmo_collinear <- function(n = 300, seed = 2) {

  set.seed(seed)
  ids <- paste0("S", seq_len(n))

  age <- rnorm(n)

  # Determined by the adjustment set, so nothing is left to estimate an
  # effect from.
  trapped <- 0.99 * age + rnorm(n, sd = 0.05)

  # Genuinely related to the outcome and independent of age, so it reaches
  # the graph and gives the check something that should pass.
  free <- rnorm(n)

  obj <- load_data(
    list(a = data.frame(trapped = trapped, free = free, row.names = ids)),
    metadata = data.frame(sample_id = ids, age = age,
                          y = 0.6 * free + 0.3 * age + rnorm(n))
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

# =============================================================================
# Positivity: the second identification condition
# =============================================================================

test_that("an exposure with nothing left to vary is caught", {

  # The failure this exists for: the model does not complain, it
  # extrapolates, and returns a coefficient with an interval like any other.

  prep <- cmo_collinear()

  res <- analyze(prep, "y", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association"))

  trapped <- Filter(function(e) identical(e$source, "trapped"), res$evidence)
  free <- Filter(function(e) identical(e$source, "free"), res$evidence)

  skip_if(length(trapped) == 0 || length(free) == 0)

  expect_false(trapped[[1]]$positivity)
  expect_lt(trapped[[1]]$residual_variation, 0.05)
  expect_true(length(trapped[[1]]$positivity_problems) > 0)
  expect_match(trapped[[1]]$positivity_problems[1], "extrapolating")

  expect_true(free[[1]]$positivity)
  expect_gt(free[[1]]$residual_variation, 0.9)

})

test_that("the failure reaches the warnings and the assumptions", {

  res <- analyze(cmo_collinear(), "y", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association"))

  trapped <- Filter(function(e) identical(e$source, "trapped"), res$evidence)

  skip_if(length(trapped) == 0)

  expect_true(any(grepl("free to vary", trapped[[1]]$warnings)))
  expect_true(any(grepl("Positivity", trapped[[1]]$assumptions)))

  expect_true(any(grepl("Positivity", res$logs)))

})

test_that("adjusting for nothing cannot fail positivity", {

  # Nothing was adjusted for, so nothing can have been adjusted away. A check
  # that reported a problem here would be measuring its own arithmetic.

  out <- .residual_variation(rnorm(100), NULL)

  expect_equal(out$residual, 1)
  expect_equal(out$r_squared, 0)

})

test_that("strata with no contrast are counted for a binary exposure", {

  set.seed(5)
  n <- 400

  group <- rep(c("a", "b", "c", "d"), each = n / 4)

  # Everyone in group d is exposed, so that quarter contributes no comparison.
  exposed <- ifelse(group == "d", 1L, stats::rbinom(n, 1, 0.5))

  out <- .empty_strata(exposed, data.frame(group = group))

  expect_equal(out$without_contrast, 1)
  expect_equal(out$people_stranded, 100)
  expect_equal(out$share_stranded, 0.25)
  expect_equal(out$worst, "d")

})

test_that("a continuous exposure is judged on variation, not on strata", {

  # Every stratum of a continuous exposure holds a range rather than a
  # contrast, so counting empty ones would report every study as hopeless.

  set.seed(6)

  expect_null(.empty_strata(rnorm(200), data.frame(g = rep(c("a","b"), 100))))

})

test_that("positivity can be switched off", {

  res <- analyze(cmo_collinear(), "y", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 assume = analysis_assumptions(positivity = FALSE),
                 control = analysis_control(methods = "association"))

  expect_true(all(vapply(res$evidence,
                         function(e) is.na(e$positivity), logical(1))))

})

# =============================================================================
# Measurement error
# =============================================================================

test_that("attenuation is undone by exactly the reliability", {

  out <- .attenuation_correction(0.5, 0.1, 0.8)

  expect_equal(out$estimate, 0.625)
  expect_equal(out$se, 0.125)
  expect_equal(out$inflation, 1.25)

})

test_that("the correction widens the interval rather than narrowing it", {

  # An attenuation correction that left the interval alone would turn a noisy
  # measurement into a stronger claim, which is precisely backwards.

  out <- .attenuation_correction(0.5, 0.1, 0.5)

  expect_gt(out$se, 0.1)
  expect_equal(out$se / out$estimate, 0.1 / 0.5, tolerance = 1e-12)

})

test_that("a corrected estimate is reported beside the measured one", {

  prep <- cmo_collinear()

  res <- analyze(prep, "y", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 assume = analysis_assumptions(reliability = c(free = 0.7)),
                 control = analysis_control(methods = "association"))

  free <- Filter(function(e) identical(e$source, "free"), res$evidence)

  skip_if(length(free) == 0)

  e <- free[[1]]

  expect_equal(e$reliability, 0.7)
  expect_true(is.finite(e$corrected_estimate))

  # Beside, never in place of: the correction rests on a number the caller
  # supplied, and replacing the measured value would hide an assumption
  # inside a result.
  expect_gt(abs(e$corrected_estimate), abs(e$estimate))
  expect_true(any(grepl("came from you and not from the data", e$assumptions)))

})

test_that("a variable with no reliability stated is left alone", {

  res <- analyze(cmo_collinear(), "y", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 assume = analysis_assumptions(reliability = c(free = 0.7)),
                 control = analysis_control(methods = "association"))

  trapped <- Filter(function(e) identical(e$source, "trapped"), res$evidence)

  skip_if(length(trapped) == 0)

  expect_true(is.na(trapped[[1]]$corrected_estimate))

})

test_that("an impossible reliability is refused", {

  expect_error(analysis_assumptions(reliability = c(x = 1.4)),
               "between 0 and 1")
  expect_error(analysis_assumptions(reliability = c(x = 0)), "between 0 and 1")
  expect_error(analysis_assumptions(reliability = 0.8), "named numeric")

  expect_error(.attenuation_correction(1, 0.1, 2), "between 0 and 1")

})

# =============================================================================
# Competing risks
# =============================================================================

test_that("censoring a competing event is stated, not assumed", {

  # A Cox model treats everything that is not the event as censoring, and
  # censoring means "still at risk". For someone who died of another cause
  # that is false, and in a mortality study it is not a small falsehood.

  plain <- .competing_risk_assumptions(NULL)

  expect_true(any(grepl("assumes those people could still have had it", plain)))
  expect_true(any(grepl("cause-specific hazard", plain)))

  named <- .competing_risk_assumptions("other_cause", 60L, 300L)

  expect_true(any(grepl("60 of 300 people \\(20%\\)", named)))
  expect_true(any(grepl("not the probability of the event", named)))
  expect_true(any(grepl("rise while the risk falls", named)))

})

test_that("a declared competing event reaches the survival edges", {

  skip_if_not_installed("survival")

  sim <- simulate_data(n = 300, blocks = list(a = c("x", "z")),
                       dag = data.frame(from = "x", to = "ev", effect = 0.8),
                       outcome = "ev", outcome_type = "survival", seed = 4)

  set.seed(1)
  sim$metadata$other_cause <- stats::rbinom(300, 1, 0.2)

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  res <- analyze(prep, "ev", time = "ev_time", effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 assume = analysis_assumptions(competing = "other_cause"),
                 control = analysis_control(methods = "survival"))

  skip_if(length(res$evidence) == 0)

  assumptions <- res$evidence[[1]]$assumptions

  expect_true(any(grepl("had a competing event and were censored", assumptions)))
  expect_true(any(grepl("cause-specific hazard", assumptions)))

})

# =============================================================================
# Several outcomes
# =============================================================================

cmo_two_outcomes <- function(n = 250, seed = 7) {

  set.seed(seed)
  ids <- paste0("S", seq_len(n))

  d <- matrix(rnorm(n * 12), n, 12,
              dimnames = list(ids, paste0("f", 1:12)))

  obj <- load_data(list(a = d), metadata = data.frame(
    sample_id = ids,
    y1 = 0.6 * d[, 1] + rnorm(n),
    y2 = 0.5 * d[, 2] + rnorm(n)))

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

test_that("each outcome is analysed and the results are all there", {

  res <- analyze(cmo_two_outcomes(), c("y1", "y2"), effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association"))

  expect_s3_class(res, "CMOMultiResult")
  expect_equal(res$outcomes, c("y1", "y2"))

  for (nm in res$outcomes) {
    expect_s3_class(res$results[[nm]], "CMOResult")
    expect_equal(res$results[[nm]]$outcome$name, nm)
  }

})

test_that("multiplicity is corrected across the outcomes, not within each", {

  # Running analyze() twice by hand gives the same estimates and the wrong
  # error rate: a relationship that was one of fifty tests is not one of a
  # hundred, and correcting within each pretends the other was never run.

  res <- analyze(cmo_two_outcomes(), c("y1", "y2"), effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association"))

  tests <- res$tests

  expect_true(all(c("outcome", "p_value", "fdr_within", "fdr_across") %in%
                    names(tests)))

  # Correcting over more tests can only make the figure larger.
  expect_true(all(tests$fdr_across >= tests$fdr_within - 1e-12))

  expect_equal(nrow(tests),
               sum(vapply(res$results, function(r) length(r$evidence),
                          integer(1))))

})

test_that("both figures travel on the edge", {

  res <- analyze(cmo_two_outcomes(), c("y1", "y2"), effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association"))

  edges <- res$results[["y1"]]$evidence

  skip_if(length(edges) == 0)

  expect_true(all(vapply(edges,
                         function(e) is.finite(e$fdr_across_outcomes),
                         logical(1))))

})

test_that("one outcome behaves exactly as it always did", {

  single <- analyze(cmo_two_outcomes(), "y1", effort = "fast", plots = FALSE,
                    quiet = TRUE,
                    control = analysis_control(methods = "association"))

  expect_s3_class(single, "CMOResult")
  expect_false(inherits(single, "CMOMultiResult"))

})

test_that("a multi-outcome result prints its caveats", {

  res <- analyze(cmo_two_outcomes(), c("y1", "y2"), effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association"))

  printed <- gsub("[[:space:]]+", " ",
                  paste(capture.output(print(res)), collapse = " "))

  expect_match(printed, "not modelled jointly", fixed = TRUE)
  expect_match(printed, "describes what you actually did", fixed = TRUE)

})

# =============================================================================
# The two groups
# =============================================================================

test_that("the groups are built, validated and printable", {

  a <- analysis_assumptions(modifiable = "diet", reliability = c(bmi = 0.9))
  k <- analysis_control(methods = "association", max_features = 50)

  expect_s3_class(a, "analysis_assumptions")
  expect_s3_class(k, "analysis_control")

  expect_equal(a$modifiable, "diet")
  expect_equal(k$max_features, 50)

  # Defaults preserved from the old signature.
  expect_equal(analysis_control()$max_features, 150)
  expect_equal(analysis_control()$bootstrap, 200)
  expect_equal(analysis_control()$goal, "causal")

  expect_no_error(capture.output(print(a)))
  expect_no_error(capture.output(print(k)))

})

test_that("a plain list is accepted and anything else is named", {

  prep <- cmo_two_outcomes()

  expect_no_error(
    analyze(prep, "y1", effort = "fast", plots = FALSE, quiet = TRUE,
            control = list(methods = "association")))

  expect_error(
    analyze(prep, "y1", control = "association", quiet = TRUE),
    "must be built with analysis_control")

  expect_error(
    analyze(prep, "y1", assume = "a dag", quiet = TRUE),
    "must be built with analysis_assumptions")

})

test_that("min_residual has to be a share", {

  expect_error(analysis_assumptions(min_residual = 0), "between 0 and 1")
  expect_error(analysis_assumptions(min_residual = 1), "between 0 and 1")

})
