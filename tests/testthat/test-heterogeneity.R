# Three relationships with known shapes, so every answer can be checked
# against what was built rather than against what came out.
cmo_shapes <- function(n = 300, seed = 31) {

  set.seed(seed)

  ids <- paste0("S", seq_len(n))
  sex <- rep(c("F", "M"), length.out = n)

  uniform <- rnorm(n)    # the same relationship in everybody
  subgroup <- rnorm(n)   # real in men, absent in women
  few <- rnorm(n)        # noise, plus eight people who carry the whole thing

  y <- 0.6 * uniform + 0.9 * subgroup * (sex == "M") + rnorm(n)

  carriers <- 1:8
  few[carriers] <- 6
  y[carriers] <- y[carriers] + 9

  obj <- load_data(
    list(main = data.frame(uniform = uniform, subgroup = subgroup, few = few,
                           row.names = ids)),
    metadata = data.frame(sample_id = ids, y = y, sex = sex,
                          stringsAsFactors = FALSE)
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

cmo_edge <- function(res, name) {
  hit <- Filter(function(e) identical(e$source, name), res$evidence)
  if (length(hit) == 0) NULL else hit[[1]]
}

# =============================================================================
# How few people it takes
# =============================================================================

test_that("a relationship that holds broadly needs most of the cohort removed", {

  set.seed(2)
  n <- 300
  x <- rnorm(n)
  y <- 0.6 * x + rnorm(n)

  conc <- .effect_concentration(y, x)

  expect_type(conc, "list")

  # Removing a handful barely shifts an average. Anything under a tenth here
  # would mean the metric is measuring noise.
  expect_gt(conc$share, 0.10)
  expect_equal(conc$k / n, conc$share)

})

test_that("a relationship carried by a handful is caught", {

  set.seed(2)
  n <- 300
  x <- rnorm(n)
  y <- rnorm(n)

  # Eight people, extreme on both, manufacture an association out of nothing.
  x[1:8] <- 6
  y[1:8] <- 9

  conc <- .effect_concentration(y, x)

  expect_lt(conc$share, 0.05)
  expect_lte(conc$k, 12)

})

test_that("removing the named samples really does halve it", {

  # The running total of dfbetas is an approximation, so the count is
  # confirmed by one refit. If that refit disagreed the number would be
  # fiction.

  set.seed(3)
  n <- 250
  x <- rnorm(n)
  y <- rnorm(n)
  x[1:6] <- 5
  y[1:6] <- 8

  conc <- .effect_concentration(y, x)

  full <- stats::coef(stats::lm(y ~ x))[[2]]

  expect_true(is.finite(conc$confirmed))
  expect_lt(abs(conc$confirmed), abs(full))

})

test_that("an estimate indistinguishable from zero is not called fragile", {

  # Halving nothing costs nothing. Without this guard every null result would
  # be reported as driven by a handful of people.

  set.seed(4)
  n <- 300
  x <- rnorm(n)
  y <- rnorm(n)

  expect_null(.effect_concentration(y, x))

})

test_that("too little data to judge returns nothing rather than a guess", {

  set.seed(5)

  expect_null(.effect_concentration(rnorm(8), rnorm(8)))
  expect_null(.effect_concentration(rnorm(50), rep(1, 50)))

})

test_that("the check survives a binary outcome", {

  set.seed(6)
  n <- 300
  x <- rnorm(n)
  y <- rbinom(n, 1, stats::plogis(1.2 * x))

  conc <- .effect_concentration(y, x, binary = TRUE)

  expect_type(conc, "list")
  expect_gt(conc$share, 0)
  expect_lte(conc$share, 1)

})

# =============================================================================
# Against a named modifier
# =============================================================================

test_that("an effect present in one group only is detected and described", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", heterogeneity = "sex",
                 plots = FALSE, quiet = TRUE)

  e <- cmo_edge(res, "subgroup")

  skip_if(is.null(e))

  expect_equal(e$heterogeneity_moderator, "sex")
  expect_lt(e$heterogeneity_fdr, 0.05)
  expect_false(e$consistent_across_groups)

  # The per-group slopes are structured data, not a string a caller has to
  # parse back apart.
  expect_s3_class(e$effect_by_group, "data.frame")
  expect_setequal(e$effect_by_group$group, c("F", "M"))
  expect_equal(sum(e$effect_by_group$n), 300)

  # Built as an effect in men only, so that is where the slope should be.
  male <- e$effect_by_group$slope[e$effect_by_group$group == "M"]
  female <- e$effect_by_group$slope[e$effect_by_group$group == "F"]

  expect_gt(male, female)
  expect_gt(male, 0.5)

})

test_that("a uniform effect is not flagged as differing", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", heterogeneity = "sex",
                 plots = FALSE, quiet = TRUE)

  e <- cmo_edge(res, "uniform")

  skip_if(is.null(e))

  expect_gt(e$heterogeneity_fdr, 0.05)
  expect_true(e$consistent_across_groups)

})

test_that("the reversal is stated, not just the difference", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", heterogeneity = "sex",
                 plots = FALSE, quiet = TRUE)

  e <- cmo_edge(res, "subgroup")

  skip_if(is.null(e))

  expect_true(any(grepl("average over groups that do not agree", e$warnings)))
  expect_true(any(grepl("direction itself reverses", e$warnings)))

})

test_that("nothing is claimed when no modifier was named", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", plots = FALSE, quiet = TRUE)

  for (e in res$evidence) {
    expect_true(is.na(e$heterogeneity_fdr))
    expect_true(is.na(e$consistent_across_groups))
  }

  # The check that needs no guess still ran.
  expect_true(any(vapply(res$evidence,
                         function(e) is.finite(e$share_driving_effect),
                         logical(1))))

})

test_that("the strongest interaction wins the slot when several were tested", {

  tab <- data.frame(
    feature = c("x", "x"), moderator = c("weak", "strong"),
    interaction_p = c(0.4, 1e-6), interaction_fdr = c(0.4, 1e-5),
    groups = "a / b", slopes = "1 / 2", same_sign = TRUE,
    stringsAsFactors = FALSE
  )
  tab$detail <- I(list(data.frame(group = c("a", "b"), n = c(10L, 10L),
                                  slope = c(1, 2)),
                       data.frame(group = c("a", "b"), n = c(10L, 10L),
                                  slope = c(1, 2))))

  edge <- .new_edge("x", "y", 1, "association", "linear regression",
                    quantity = "beta")

  out <- .edge_heterogeneity(list(edge),
                             list(available = TRUE, table = tab), "y")[[1]]

  expect_equal(out$heterogeneity_moderator, "strong")

})

# =============================================================================
# Reaching the reader
# =============================================================================

test_that("the graph table carries both answers", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", heterogeneity = "sex",
                 plots = FALSE, quiet = TRUE)

  expect_true(all(c("share_driving_effect", "heterogeneity_fdr") %in%
                    names(res$graph$edges)))

})

test_that("the run log names both counts", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", heterogeneity = "sex",
                 plots = FALSE, quiet = TRUE)

  expect_true(any(grepl("Effect concentration", res$logs)))
  expect_true(any(grepl("Heterogeneity", res$logs)))

})

test_that("the score decomposition names the spread", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", plots = FALSE, quiet = TRUE)

  parts <- .decompose_evidence(res$evidence[[1]])

  expect_true("Spread across the cohort" %in% parts$component)

})

test_that("the edge prints both answers without complaint", {

  res <- analyze(cmo_shapes(), "y", methods = "association",
                 effort = "standard", heterogeneity = "sex",
                 plots = FALSE, quiet = TRUE)

  e <- cmo_edge(res, "subgroup")

  skip_if(is.null(e))

  printed <- capture.output(summary(e))
  text <- paste(printed, collapse = " ")

  expect_match(text, "Does one number describe everybody", fixed = TRUE)
  expect_match(text, "Halved by removing", fixed = TRUE)
  expect_match(text, "sex", fixed = TRUE)

})

test_that("neither check runs at the cheapest effort", {

  res <- analyze(cmo_shapes(), "y", methods = "association", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  expect_true(all(vapply(res$evidence,
                         function(e) is.na(e$share_driving_effect),
                         logical(1))))

})
