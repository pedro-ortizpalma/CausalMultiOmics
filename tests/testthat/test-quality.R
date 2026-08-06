# check_data() drops any column more than 30% empty rather than imputing it,
# so anything that reaches the analysis sits in the 0-30% band. That is
# exactly the band where filling happens quietly and nobody looks.
cmo_gappy <- function(n = 200, gap = 0.25, seed = 11) {

  set.seed(seed)

  ids <- paste0("S", seq_len(n))

  clean <- rnorm(n)
  gappy <- rnorm(n)
  y <- 0.8 * clean + 0.8 * gappy + rnorm(n)

  block <- data.frame(clean = clean, gappy = gappy, spare = rnorm(n),
                      row.names = ids)

  block$gappy[sample(n, round(gap * n))] <- NA

  obj <- load_data(list(main = block),
                   metadata = data.frame(sample_id = ids, y = y))

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

# =============================================================================
# What each imputation method costs
# =============================================================================

test_that("filling with a constant costs more than borrowing from neighbours", {

  # Mean imputation collapses every gap onto one number, so the variance of
  # that part of the column is zero and any association through it is pulled
  # toward the null. kNN keeps most of the structure.

  expect_lt(.imputation_fidelity("mean"), .imputation_fidelity("knn"))
  expect_equal(.imputation_fidelity("mean"), .imputation_fidelity("median"))

  expect_true(all(vapply(c("mean", "median", "mode", "knn", "pseudocount"),
                         function(m) {
                           f <- .imputation_fidelity(m)
                           f >= 0 && f <= 1
                         }, logical(1))))

  # An unrecognised method is not assumed to be good or bad.
  expect_equal(.imputation_fidelity("something_new"), 0.5)
  expect_equal(.imputation_fidelity(NA_character_), 0.5)

})

# =============================================================================
# Provenance
# =============================================================================

test_that("the provenance table says what happened to each feature", {

  prep <- cmo_gappy()
  ft <- .feature_quality(prep)

  expect_s3_class(ft, "data.frame")
  expect_true(all(c("feature", "block", "missing_percent", "imputed",
                    "quality") %in% names(ft)))

  clean <- ft[ft$feature == "clean", ]
  gappy <- ft[ft$feature == "gappy", ]

  skip_if(nrow(gappy) == 0)

  expect_false(clean$imputed)
  expect_equal(clean$quality, 1)
  expect_length(clean$flags[[1]], 0)

  expect_true(gappy$imputed)
  expect_lt(gappy$quality, 1)
  expect_match(gappy$flags[[1]][1], "was missing and was filled in")

})

test_that("missing values that were never imputed cost nothing", {

  # Rows dropped for being incomplete cost sample size, which `confidence`
  # already reflects through n. Charging for them here as well would count
  # the same problem twice.

  ft <- data.frame(
    feature = "a", block = "b", missing_percent = 20, imputed = FALSE,
    imputation_method = NA_character_, quality = 1,
    flags = I(list(character())), stringsAsFactors = FALSE
  )

  edge <- .new_edge("a", "y", 1, "association", "linear regression",
                    quantity = "beta")
  edge$evidence_score <- 50

  out <- .quality_edges(list(edge), ft)[[1]]

  expect_equal(out$data_quality, 1)
  expect_equal(out$evidence_score, 50)

})

test_that("a feature with no recoverable provenance is not called clean", {

  prep <- cmo_gappy()
  prep$input <- NULL

  expect_null(.feature_quality(prep))

})

# =============================================================================
# The discount
# =============================================================================

test_that("an edge is worth no more than its worst-measured ingredient", {

  ft <- data.frame(
    feature = c("good", "bad", "cov"),
    block = "b",
    missing_percent = c(0, 30, 0),
    imputed = c(FALSE, TRUE, FALSE),
    imputation_method = c(NA, "median", NA),
    quality = c(1, 0.6, 1),
    flags = I(list(character(), "30% filled in", character())),
    stringsAsFactors = FALSE
  )

  edge <- .new_edge("good", "bad", 1, "association", "linear regression",
                    quantity = "beta")
  edge$evidence_score <- 40

  out <- .quality_edges(list(edge), ft)[[1]]

  expect_equal(out$data_quality, 0.6)
  expect_equal(out$quality_limited_by, "bad")
  expect_equal(out$evidence_score, 24)

})

test_that("a fabricated covariate drags the edge down too", {

  # Adjusting for a variable that was largely invented corrupts the
  # adjustment, so it cannot leave the estimate untouched.

  ft <- data.frame(
    feature = c("x", "y", "dirty_cov"),
    block = "b",
    missing_percent = c(0, 0, 30),
    imputed = c(FALSE, FALSE, TRUE),
    imputation_method = c(NA, NA, "mean"),
    quality = c(1, 1, 0.5),
    flags = I(list(character(), character(), "30% filled in")),
    stringsAsFactors = FALSE
  )

  edge <- .new_edge("x", "y", 1, "association", "linear regression",
                    quantity = "beta")
  edge$evidence_score <- 40

  out <- .quality_edges(list(edge), ft, covariates = "dirty_cov")[[1]]

  expect_equal(out$data_quality, 0.5)
  expect_equal(out$quality_limited_by, "dirty_cov")

})

test_that("a clean dataset is left exactly as it was", {

  # The property that makes this safe to run unconditionally: with nothing
  # imputed every quality is 1 and no score moves.

  set.seed(4)
  n <- 150
  ids <- paste0("S", seq_len(n))
  a <- rnorm(n)
  y <- 0.7 * a + rnorm(n)

  obj <- load_data(
    list(main = data.frame(a = a, b = rnorm(n), row.names = ids)),
    metadata = data.frame(sample_id = ids, y = y)
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

  res <- analyze(prep, "y", methods = "association", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  for (e in res$evidence) {
    expect_equal(e$data_quality, 1)
    expect_length(e$quality_flags, 0)
    expect_true(is.na(e$complete_case_estimate))
  }

  expect_false(any(grepl("Data quality", res$logs)))

})

# =============================================================================
# Inside analyze()
# =============================================================================

test_that("imputed variables reach the result carrying their provenance", {

  res <- analyze(cmo_gappy(), "y", methods = "association",
                 effort = "standard", plots = FALSE, quiet = TRUE)

  gappy <- Filter(function(e) identical(e$source, "gappy"), res$evidence)

  skip_if(length(gappy) == 0)

  e <- gappy[[1]]

  expect_lt(e$data_quality, 1)
  expect_equal(e$quality_limited_by, "gappy")
  expect_true(length(e$quality_flags) > 0)
  expect_true(any(grepl("Data quality", res$logs)))

})

test_that("the ranking agrees with the scores printed beside it", {

  # The discount can push a relationship built on filled-in values below one
  # that was measured. Leaving the old order would show a ranking that
  # contradicts its own numbers.

  res <- analyze(cmo_gappy(), "y", methods = "association",
                 effort = "standard", plots = FALSE, quiet = TRUE)

  scores <- vapply(res$evidence, function(e) e$evidence_score, numeric(1))

  expect_false(is.unsorted(rev(scores)))

})

test_that("the graph table carries data quality beside the score", {

  res <- analyze(cmo_gappy(), "y", methods = "association",
                 effort = "standard", plots = FALSE, quiet = TRUE)

  expect_true("data_quality" %in% names(res$graph$edges))

})

test_that("the score decomposition names the discount", {

  res <- analyze(cmo_gappy(), "y", methods = "association",
                 effort = "standard", plots = FALSE, quiet = TRUE)

  parts <- .decompose_evidence(res$evidence[[1]])

  expect_true("Data quality" %in% parts$component)

})

# =============================================================================
# Complete-case sensitivity
# =============================================================================

test_that("the observed mask marks filled-in cells and nothing else", {

  prep <- cmo_gappy()

  x <- prep$data$assays[["main"]]
  mask <- .observed_mask(prep, rownames(x), colnames(x))

  expect_true(is.matrix(mask))
  expect_true(all(mask[, "clean"]))
  expect_true(any(!mask[, "gappy"]))

  # The proportion marked as filled matches what actually went missing.
  expect_equal(mean(!mask[, "gappy"]), 0.25, tolerance = 0.02)

})

test_that("a finding produced entirely by the filling is caught", {

  # Built so the answer is known: the exposure is pure noise on the rows
  # where it was measured, and the filled rows are what carry the outcome.
  # A check that cannot catch this cannot catch anything.

  set.seed(7)
  n <- 300

  y <- rnorm(n)
  x <- rnorm(n)

  observed <- rep(c(TRUE, FALSE), each = n / 2)

  # On measured rows x runs against y; on filled rows it runs with it, and
  # there are enough of the latter for the pooled estimate to come out
  # positive.
  x[observed] <- -0.4 * y[observed] + rnorm(n / 2, sd = 0.5)
  x[!observed] <- 2.5 * y[!observed] + rnorm(n / 2, sd = 0.5)

  xm <- matrix(x, ncol = 1, dimnames = list(paste0("S", seq_len(n)), "x"))
  mask <- matrix(observed, ncol = 1, dimnames = dimnames(xm))

  edge <- .new_edge("x", "y", stats::coef(stats::lm(y ~ x))[2],
                    "association", "linear regression", quantity = "beta")
  edge$evidence_score <- 60

  out <- .complete_case_sensitivity(
    list(edge), xm,
    outcome = list(name = "y", values = y, type = "continuous"),
    covariates = NULL, mask = mask
  )[[1]]

  expect_gt(out$estimate, 0)
  expect_lt(out$complete_case_estimate, 0)
  expect_false(out$complete_case_agrees)
  expect_equal(out$complete_case_n, n / 2)
  expect_true(any(grepl("reverses direction", out$warnings)))

})

test_that("nothing imputed means nothing to check", {

  xm <- matrix(rnorm(100), ncol = 1,
               dimnames = list(paste0("S", 1:100), "x"))

  edge <- .new_edge("x", "y", 1, "association", "linear regression",
                    quantity = "beta")

  out <- .complete_case_sensitivity(
    list(edge), xm,
    outcome = list(name = "y", values = rnorm(100), type = "continuous"),
    covariates = NULL,
    mask = matrix(TRUE, 100, 1, dimnames = dimnames(xm))
  )[[1]]

  expect_true(is.na(out$complete_case_estimate))

})

test_that("a real relationship survives the removal of the filled rows", {

  res <- analyze(cmo_gappy(), "y", methods = "association",
                 effort = "standard", plots = FALSE, quiet = TRUE)

  gappy <- Filter(function(e) identical(e$source, "gappy"), res$evidence)

  skip_if(length(gappy) == 0 || !is.finite(gappy[[1]]$complete_case_estimate))

  e <- gappy[[1]]

  expect_true(e$complete_case_agrees)
  expect_lt(e$complete_case_n, 200)

})

test_that("the cheap check is skipped when diagnostics are off", {

  res <- analyze(cmo_gappy(), "y", methods = "association", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  expect_true(all(vapply(res$evidence,
                         function(e) is.na(e$complete_case_estimate),
                         logical(1))))

  # The discount itself is free, so it still applies.
  expect_true(any(vapply(res$evidence,
                         function(e) isTRUE(e$data_quality < 1), logical(1))))

})
