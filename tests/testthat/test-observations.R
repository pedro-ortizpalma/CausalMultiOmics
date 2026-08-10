skip_if_not_installed("igraph")
skip_if_not_installed("glmnet")
skip_if_not_installed("ranger")
skip_if_not_installed("bnlearn")
skip_if_not_installed("lavaan")
skip_if_not_installed("survival")

cmo_two_blocks <- function(n = 90, seed = 11) {

  set.seed(seed)

  ids <- paste0("S", seq_len(n))
  exposure <- rnorm(n)
  mediator <- 0.8 * exposure + rnorm(n, sd = 0.6)
  y <- 0.9 * mediator + 0.2 * exposure + rnorm(n, sd = 0.7)

  obj <- load_data(
    list(a = data.frame(EXP = exposure, A2 = rnorm(n), row.names = ids),
         b = data.frame(MED = mediator, B2 = rnorm(n), row.names = ids)),
    metadata = data.frame(sample_id = ids, y = y,
                          status = as.integer(y > stats::median(y)),
                          fu = rexp(n, 0.05) + 1,
                          age = rnorm(n, 55, 8), stringsAsFactors = FALSE)
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

# =============================================================================
# The registry
# =============================================================================

test_that("every quantity declares what it is and how it behaves", {

  registry <- .available_quantities()

  expect_s3_class(registry, "data.frame")
  expect_gt(nrow(registry), 5)

  expect_true(all(c("quantity", "label", "family", "scale", "signed",
                    "poolable") %in% names(registry)))

  expect_false(any(duplicated(registry$quantity)))
  expect_true(all(nzchar(registry$label)))
  expect_true(all(nzchar(registry$scale)))

})

test_that("quantities on the same scale share a family, and others do not", {

  expect_equal(.quantity("log_or")$family, .quantity("log_hr")$family)
  expect_equal(.quantity("correlation")$family,
               .quantity("partial_correlation")$family)

  # A log ratio and a permutation importance are not the same measurement.
  expect_false(identical(.quantity("log_or")$family,
                         .quantity("importance")$family))
  expect_false(identical(.quantity("log_or")$family,
                         .quantity("arc_strength")$family))

})

test_that("unsigned and shrunk quantities are not pooled", {

  # Permutation importance has no direction of its own, and a penalised
  # coefficient is biased towards zero by an amount set by the penalty rather
  # than by the data. Neither belongs in an average with an unbiased estimate.

  expect_false(.quantity("importance")$signed)
  expect_false(.quantity("importance")$poolable)
  expect_false(.quantity("arc_strength")$poolable)
  expect_false(.quantity("penalised_beta")$poolable)

  expect_true(.quantity("log_or")$poolable)
  expect_true(.quantity("beta")$poolable)

})

test_that("an unregistered quantity degrades instead of failing", {

  unknown <- .quantity("something_new")

  expect_equal(unknown$family, "unknown")
  expect_false(unknown$poolable)

  expect_equal(.quantity(NA_character_)$label, "unspecified")

})

# =============================================================================
# What the generators declare
# =============================================================================

test_that("every generator declares the quantity it reports", {

  res <- analyze(cmo_two_blocks(), "y", effort = "fast", plots = FALSE,
                 quiet = TRUE)

  for (e in res$evidence) {

    expect_false(is.na(e$quantity), label = paste(e$source, e$target))
    expect_true(nzchar(e$quantity_family))

    for (row in seq_len(nrow(e$contributions))) {
      expect_true(nzchar(e$contributions$quantity[row]))
    }

  }

})

test_that("the quantity matches the model that produced it", {

  prep <- cmo_two_blocks()

  # A Cox model reports a log hazard ratio, whatever else is running.
  surv <- analyze(prep, "status", time = "fu", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(methods = c("survival")))

  skip_if(length(surv$evidence) == 0)

  expect_true(all(vapply(surv$evidence, function(e) e$quantity,
                         character(1)) == "log_hr"))

  # A logistic regression reports a log odds ratio; a linear one a coefficient.
  binary <- analyze(prep, "status", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association"))
  continuous <- analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association"))

  skip_if(length(binary$evidence) == 0 || length(continuous$evidence) == 0)

  expect_equal(unique(vapply(binary$evidence, function(e) e$quantity,
                             character(1))), "log_or")
  expect_equal(unique(vapply(continuous$evidence, function(e) e$quantity,
                             character(1))), "beta")

})

# =============================================================================
# Pooling
# =============================================================================

test_that("estimates are pooled only within a shared scale", {

  # Regression: the integrator took a median across every method, so a
  # log-odds of 0.29, a permutation importance of 0.72 and an arc strength of
  # 9715 were averaged and the middle one reported as if it estimated
  # something.

  res <- analyze(cmo_two_blocks(), "y", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  for (e in res$evidence) {

    contributions <- e$contributions

    pooled <- contributions[contributions$pooled, , drop = FALSE]

    if (nrow(pooled) == 0) next

    # The reported estimate has to lie within the range of the observations
    # it was pooled from, and cannot be dragged by the ones left out.
    same_family <- vapply(pooled$quantity, function(q) {
      any(vapply(.quantity_registry(), function(spec)
        identical(spec$label, q) && identical(spec$family, e$quantity_family),
        logical(1)))
    }, logical(1))

    if (!any(same_family)) next

    values <- pooled$estimate[same_family]

    expect_gte(e$estimate, min(values) - 1e-8)
    expect_lte(e$estimate, max(values) + 1e-8)

  }

})

test_that("an unpoolable observation cannot move the headline estimate", {

  registry <- .quantity_registry()

  real <- .new_edge("A", "B", estimate = 0.30, generator = "association",
                    method = "linear regression", quantity = "beta")

  huge <- .new_edge("A", "B", estimate = 9715, generator = "bayesnet",
                    method = "Bayesian network (hill climbing)",
                    quantity = "arc_strength")

  merged <- .integrate_evidence(list(real, huge),
                                list(min_evidence_score = 0))

  expect_length(merged, 1)

  edge <- merged[[1]]

  expect_equal(edge$estimate, 0.30)
  expect_equal(edge$quantity, "beta")
  expect_equal(edge$pooled_from, 1L)
  expect_true("network arc strength" %in% edge$not_pooled)

})

test_that("two observations on the same scale are pooled", {

  a <- .new_edge("A", "B", estimate = 0.20, generator = "association",
                 method = "logistic regression", quantity = "log_or")

  b <- .new_edge("A", "B", estimate = 0.40, generator = "survival",
                 method = "Cox proportional hazards", quantity = "log_hr")

  merged <- .integrate_evidence(list(a, b), list(min_evidence_score = 0))[[1]]

  expect_equal(merged$estimate, 0.30)
  expect_equal(merged$quantity_family, "log_ratio")
  expect_equal(merged$pooled_from, 2L)
  expect_length(merged$not_pooled, 0)

})

test_that("the strongest kind of evidence supplies the headline", {

  # A mediation estimate and a partial correlation are both poolable but live
  # on different scales. The one from the stronger kind of evidence is the one
  # a reader should be quoting.

  weak <- .new_edge("A", "B", estimate = 0.70, generator = "conditional",
                    method = "partial correlation",
                    quantity = "partial_correlation")

  strong <- .new_edge("A", "B", estimate = 0.25, generator = "mediation",
                      method = "bootstrap mediation",
                      quantity = "indirect_effect")

  merged <- .integrate_evidence(list(weak, strong),
                                list(min_evidence_score = 0))[[1]]

  expect_equal(merged$estimate, 0.25)
  expect_equal(merged$quantity, "indirect_effect")
  expect_true("partial correlation" %in% merged$not_pooled)

})

test_that("methods left out of pooling still count towards agreement", {

  # Two methods pointing the same way is informative even when their
  # magnitudes cannot be combined, so consistency must not ignore them.

  a <- .new_edge("A", "B", estimate = 0.30, generator = "association",
                 method = "linear regression", quantity = "beta")

  b <- .new_edge("A", "B", estimate = 500, generator = "bayesnet",
                 method = "Bayesian network (hill climbing)",
                 quantity = "arc_strength")

  agree <- .integrate_evidence(list(a, b), list(min_evidence_score = 0))[[1]]

  expect_length(agree$supporting_methods, 2)
  expect_equal(agree$consistency, 1)

})

test_that("nothing poolable falls back to the strongest single observation", {

  only_importance <- .new_edge("A", "B", estimate = 0.8,
                               generator = "randomforest",
                               method = "random forest importance",
                               quantity = "importance")

  merged <- .integrate_evidence(list(only_importance),
                                list(min_evidence_score = 0))[[1]]

  expect_equal(merged$estimate, 0.8)
  expect_equal(merged$quantity, "importance")
  expect_equal(merged$pooled_from, 1L)

})

# =============================================================================
# What the reader sees
# =============================================================================

test_that("the per-method table says what each number is", {

  res <- analyze(cmo_two_blocks(), "y", effort = "fast", plots = FALSE,
                 quiet = TRUE)

  contributions <- res$evidence[[1]]$contributions

  expect_true(all(c("quantity", "scale", "pooled") %in% names(contributions)))
  expect_type(contributions$pooled, "logical")

})

test_that("explain() reports what was pooled and what was not", {

  res <- analyze(cmo_two_blocks(), "y", effort = "fast", plots = FALSE,
                 quiet = TRUE)

  feature <- res$graph$edges$source[1]

  out <- paste(capture.output(print(explain(res, feature))), collapse = "\n")

  expect_match(out, "Headline estimate", fixed = TRUE)

})

test_that("the report names the measurement behind every row", {

  res <- analyze(cmo_two_blocks(), "y", effort = "fast", plots = TRUE,
                 quiet = TRUE)

  path <- file.path(tempdir(), "cmo-observations.html")
  on.exit(unlink(path), add = TRUE)

  report(res, file = path, open = FALSE, quiet = TRUE)

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(txt, "What it measured", fixed = TRUE)
  expect_match(txt, "cannot be averaged together", fixed = TRUE)

})
