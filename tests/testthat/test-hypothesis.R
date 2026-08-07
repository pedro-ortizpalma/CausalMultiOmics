cmo_cohort <- function(n = 300, seed = 5, effect = 0.9) {

  set.seed(seed)
  ids <- paste0("S", seq_len(n))

  age <- rnorm(n, 55, 10)
  protein <- 0.04 * age + rnorm(n)
  y <- effect * protein + 0.02 * age + rnorm(n)

  load_data(
    list(blood = data.frame(protein = protein, noise = rnorm(n),
                            row.names = ids)),
    metadata = data.frame(sample_id = ids, y = y, age = age)
  )

}

cmo_discovery <- function(...) {
  obj <- cmo_cohort(...)
  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)
}

cmo_result <- function(dag = NULL, prep = NULL) {
  analyze(.report_or(prep, cmo_discovery()), "y", covariates = "age",
          dag = dag, methods = "association", effort = "standard",
          plots = FALSE, quiet = TRUE)
}

cmo_dag <- function() {
  data.frame(from = c("age", "age"), to = c("protein", "y"),
             stringsAsFactors = FALSE)
}

# =============================================================================
# Stating a claim
# =============================================================================

test_that("a claim is extracted with everything needed to argue with it", {

  h <- hypothesis(cmo_result())

  expect_s3_class(h, "Hypothesis")

  expect_equal(h$source, "protein")
  expect_equal(h$target, "y")
  expect_true(is.finite(h$estimate))
  expect_true(all(is.finite(h$ci)))

  expect_true(nzchar(h$claim))
  expect_true(nzchar(h$grade))
  expect_true(length(h$grade_reasons) > 0)
  expect_true(length(h$settles) > 0)

})

test_that("the default claim is the strongest relationship", {

  res <- cmo_result()

  h <- hypothesis(res)

  to_outcome <- Filter(function(e) identical(e$target, "y"), res$evidence)
  best <- to_outcome[[which.max(vapply(to_outcome,
                                       function(e) e$evidence_score,
                                       numeric(1)))]]

  expect_equal(h$source, best$source)

})

test_that("a named feature can be asked for instead", {

  res <- cmo_result()

  # Whatever reached the graph, rather than a name assumed to be there: a
  # feature the engine dropped for scoring too low is not askable about, and
  # that is the correct behaviour rather than something to work around.
  available <- unique(vapply(
    Filter(function(e) identical(e$target, "y"), res$evidence),
    function(e) e$source, character(1)))

  for (f in available) {
    expect_equal(hypothesis(res, f)$source, f)
  }

})

test_that("asking about something absent says what is available", {

  expect_error(hypothesis(cmo_result(), "not_measured"),
               "no relationship with y")

  # The message names what could have been asked for, so a typo is one read
  # away from being fixed.
  expect_error(hypothesis(cmo_result(), "not_measured"), "Available")

  expect_error(hypothesis("not a result"), "must be a CMOResult")

})

# =============================================================================
# What the claim is allowed to say
# =============================================================================

test_that("an adjusted association says it goes with, not that it causes", {

  # The failure this guards against is the strongest sentence in the package
  # being written by the score rather than by identification.

  h <- hypothesis(cmo_result())

  expect_match(h$grade, "association")
  expect_match(h$claim, "goes with", fixed = TRUE)
  expect_false(grepl("Raising", h$claim, fixed = TRUE))

})

test_that("a confirming diagram earns the stronger sentence", {

  # Auditing the adjustment against a stated structure is what turns "we
  # controlled for age" into a checkable argument. If the grade ignored it
  # the audit would be decorative.

  h <- hypothesis(cmo_result(dag = cmo_dag()), "protein")

  expect_match(h$grade, "identified")
  expect_match(h$claim, "Raising", fixed = TRUE)

  # And it stays conditional on the diagram, which the data cannot check.
  expect_true(any(grepl("cannot confirm the diagram", h$grade_reasons)))

})

test_that("precision and stability cannot promote an association", {

  # A relationship measured beautifully is still an association. Anything
  # that let agreement or resampling raise the grade would turn a
  # well-estimated correlation into a cause by arithmetic.

  edge <- .new_edge("x", "y", 1, "association", "linear regression",
                    quantity = "beta", identification = "adjustment")

  edge$bootstrap_stability <- 1
  edge$e_value <- 20
  edge$consistency <- 1
  edge$supporting_methods <- c("a", "b", "c", "d")

  expect_match(.hypothesis_grade(edge)$grade, "association")

})

test_that("a weakness that changes the kind of claim is in the grade", {

  edge <- .new_edge("x", "y", 1, "association", "linear regression",
                    quantity = "beta", identification = "adjustment")

  edge$complete_case_agrees <- FALSE
  expect_match(.hypothesis_grade(edge)$grade, "artefact")

  edge$complete_case_agrees <- TRUE
  edge$share_driving_effect <- 0.01
  edge$samples_driving_effect <- 3L
  expect_match(.hypothesis_grade(edge)$grade, "small minority")

  edge$share_driving_effect <- 0.4
  edge$heterogeneity_fdr <- 0.001
  edge$heterogeneity_moderator <- "sex"
  expect_match(.hypothesis_grade(edge)$grade, "disagree")

})

# =============================================================================
# The case, kept in two halves
# =============================================================================

test_that("what supports and what threatens are not mixed together", {

  res <- cmo_result()
  h <- hypothesis(res)

  expect_true(length(h$supports) > 0)

  # Only one generator ran, so nothing corroborates it, and that belongs on
  # the other side of the ledger.
  expect_true(any(grepl("Only one method", h$threatens)))

  expect_length(intersect(h$supports, h$threatens), 0)

})

test_that("a weak sensitivity is counted against, not for", {

  edge <- .new_edge("x", "y", 0.1, "association", "linear regression",
                    quantity = "beta")
  edge$e_value <- 1.1
  edge$fdr <- 0.3
  edge$bootstrap_stability <- 0.2

  case <- .hypothesis_case(edge, CMOResult())

  expect_length(case$supports, 0)
  expect_true(any(grepl("would erase it", case$threatens)))
  expect_true(any(grepl("does not survive correction", case$threatens)))

})

# =============================================================================
# What would settle it
# =============================================================================

test_that("each weakness produces the measurement that answers it", {

  res <- cmo_result()
  h <- hypothesis(res)

  text <- paste(h$settles, collapse = " ")

  # Cross-sectional, so ordering is unknown and that is actionable.
  expect_match(text, "before y in the same people", fixed = TRUE)

  # Adjusted but not identified, with an E-value, so the confounder that
  # would erase it can be described rather than merely feared.
  expect_match(text, "risk ratio", fixed = TRUE)

})

test_that("the confounding question is reframed once a diagram answers it", {

  plain <- hypothesis(cmo_result())
  with_dag <- hypothesis(cmo_result(dag = cmo_dag()), "protein")

  expect_match(paste(plain$settles, collapse = " "),
               "Measure a confounder", fixed = TRUE)

  # With a diagram the useful next step is testing the diagram, not assuming
  # it is wrong.
  expect_match(paste(with_dag$settles, collapse = " "),
               "does not have", fixed = TRUE)

})

test_that("a missing adjustment the diagram names is spelled out", {

  edge <- .new_edge("x", "y", 1, "association", "linear regression",
                    quantity = "beta", identification = "adjustment")
  edge$required_adjustment <- c("age", "sex")
  edge$adjustment_set <- "age"

  settles <- .hypothesis_settles(edge, CMOResult())

  expect_true(any(grepl("Measure sex", settles, fixed = TRUE)))

})

test_that("a finding carried by a few people asks for replication", {

  edge <- .new_edge("x", "y", 1, "association", "linear regression",
                    quantity = "beta", identification = "temporal")
  edge$temporal <- TRUE
  edge$share_driving_effect <- 0.02
  edge$samples_driving_effect <- 6L

  settles <- .hypothesis_settles(edge, CMOResult())

  expect_true(any(grepl("Replicate in a cohort", settles, fixed = TRUE)))
  expect_true(any(grepl("removing 6 people", settles, fixed = TRUE)))

})

test_that("a feature inseparable from its module says so", {

  # The honest limit of the data: if five variables move as one, the effect
  # may belong to any of them and nothing here can tell them apart.

  result <- CMOResult()
  result$modules <- ModuleGraph()
  result$modules$membership <- stats::setNames(rep(1L, 5), paste0("f", 1:5))

  edge <- .new_edge("f1", "y", 1, "association", "linear regression",
                    quantity = "beta", identification = "temporal")
  edge$temporal <- TRUE

  settles <- .hypothesis_settles(edge, result)

  expect_true(any(grepl("Separate f1 from the 4 other", settles,
                        fixed = TRUE)))

})

# =============================================================================
# Taking it somewhere else
# =============================================================================

test_that("a real claim replicates in a second cohort", {

  prep <- cmo_discovery()
  h <- hypothesis(cmo_result(dag = cmo_dag(), prep = prep), "protein")

  validation <- apply_preprocessing(cmo_cohort(280, seed = 99), prep,
                                    quiet = TRUE)

  out <- test_hypothesis(h, validation)

  r <- out$replication

  expect_true(r$tested)
  expect_equal(r$verdict, "replicated")
  expect_true(r$same_direction)
  expect_equal(r$ratio, 1, tolerance = 0.25)
  expect_equal(r$n, 280)

})

test_that("a claim that is not there is not called replicated", {

  prep <- cmo_discovery()
  h <- hypothesis(cmo_result(prep = prep), "protein")

  absent <- apply_preprocessing(cmo_cohort(280, seed = 77, effect = 0), prep,
                                quiet = TRUE)

  r <- test_hypothesis(h, absent)$replication

  expect_false(r$verdict == "replicated")
  expect_lt(abs(r$ratio), 0.5)

})

test_that("the same direction at a fraction of the size is not a success", {

  # The most common way a replication is oversold: the direction held, the
  # effect is a fifth as large, and a significance test calls it a win.

  prep <- cmo_discovery()
  h <- hypothesis(cmo_result(prep = prep), "protein")

  weak <- apply_preprocessing(cmo_cohort(600, seed = 21, effect = 0.15), prep,
                              quiet = TRUE)

  r <- test_hypothesis(h, weak)$replication

  expect_true(r$same_direction)
  expect_lt(r$ratio, 0.5)
  expect_false(r$verdict == "replicated")

})

test_that("missing covariates are reported rather than quietly dropped", {

  prep <- cmo_discovery()
  h <- hypothesis(cmo_result(prep = prep), "protein")

  cohort <- cmo_cohort(200, seed = 33)
  processed <- apply_preprocessing(cohort, prep, quiet = TRUE)

  processed$metadata$age <- NULL

  r <- test_hypothesis(h, processed)$replication

  expect_equal(r$covariates_missing, "age")
  expect_length(r$covariates_used, 0)
  expect_true(any(grepl("not the same model", r$notes)))

})

test_that("a cohort that cannot answer the question says which part", {

  prep <- cmo_discovery()
  h <- hypothesis(cmo_result(prep = prep), "protein")

  cohort <- apply_preprocessing(cmo_cohort(200, seed = 44), prep, quiet = TRUE)

  no_outcome <- cohort
  no_outcome$metadata$y <- NULL
  expect_error(test_hypothesis(h, no_outcome), "no 'y' column")

  no_feature <- cohort
  no_feature$assays$blood <- no_feature$assays$blood[, "noise", drop = FALSE]
  expect_error(test_hypothesis(h, no_feature), "not measured in the new cohort")

  expect_error(test_hypothesis("not a hypothesis", cohort),
               "must be a Hypothesis")

  expect_error(test_hypothesis(h, "not a cohort"), "after preprocessing")

})

# =============================================================================
# Presentation
# =============================================================================

test_that("a hypothesis prints its claim, its case and its next step", {

  h <- hypothesis(cmo_result())

  text <- paste(capture.output(print(h)), collapse = " ")

  expect_match(text, "Hypothesis", fixed = TRUE)
  expect_match(text, "What supports it", fixed = TRUE)
  expect_match(text, "What threatens it", fixed = TRUE)
  expect_match(text, "What would settle it", fixed = TRUE)
  expect_match(text, "CausalMultiOmics", fixed = TRUE)

})

test_that("a tested hypothesis prints the verdict too", {

  prep <- cmo_discovery()
  h <- hypothesis(cmo_result(prep = prep), "protein")

  validation <- apply_preprocessing(cmo_cohort(280, seed = 99), prep,
                                    quiet = TRUE)

  text <- paste(capture.output(print(test_hypothesis(h, validation))),
                collapse = " ")

  expect_match(text, "Tested on another cohort", fixed = TRUE)
  expect_match(text, "REPLICATED", fixed = TRUE)

})

test_that("an empty hypothesis prints without complaint", {

  expect_no_error(capture.output(print(Hypothesis())))

})

test_that("the claim carries enough provenance to be traced", {

  h <- hypothesis(cmo_result())

  expect_equal(h$provenance$package, "CausalMultiOmics")
  expect_true(nzchar(h$provenance$version))
  expect_equal(h$protocol$outcome, "y")
  expect_equal(h$protocol$covariates, "age")
  expect_equal(h$protocol$model, "linear regression")

})
