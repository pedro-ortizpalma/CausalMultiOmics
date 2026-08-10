skip_if_not_installed("dagitty")

# age confounds protein and disease; inflammation mediates; biomarker is a
# consequence of the disease, so adjusting for it is actively harmful.
cmo_structure <- function() {
  data.frame(
    from = c("age", "age", "protein", "inflammation", "disease"),
    to   = c("protein", "disease", "inflammation", "disease", "biomarker"),
    stringsAsFactors = FALSE
  )
}

cmo_dag_data <- function(n = 200, seed = 3) {

  set.seed(seed)

  age <- rnorm(n, 55, 10)
  protein <- 0.05 * age + rnorm(n)
  inflammation <- 0.8 * protein + rnorm(n, sd = 0.5)
  risk <- 0.4 * inflammation + 0.03 * age + rnorm(n)
  disease <- rbinom(n, 1, stats::plogis(risk))

  ids <- paste0("S", seq_len(n))

  obj <- load_data(
    list(blood = data.frame(protein = protein, inflammation = inflammation,
                            biomarker = 1.5 * disease + rnorm(n),
                            row.names = ids)),
    metadata = data.frame(sample_id = ids, disease = disease, age = age,
                          inflammation = inflammation,
                          stringsAsFactors = FALSE)
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

# =============================================================================
# Accepting a structure
# =============================================================================

test_that("a DAG is accepted in each form a user might have it", {

  from_frame <- .parse_dag(cmo_structure())
  from_string <- .parse_dag("dag { age -> protein; age -> disease }")

  expect_s3_class(from_frame, "dagitty")
  expect_s3_class(from_string, "dagitty")

  # Already parsed, passed through unchanged.
  expect_identical(.parse_dag(from_frame), from_frame)

  expect_null(.parse_dag(NULL))

})

test_that("a malformed structure is refused with a reason", {

  expect_error(.parse_dag(data.frame(a = 1, b = 2)), "'from' and 'to'")
  expect_error(.parse_dag("not a dag at all"), "Could not parse")
  expect_error(.parse_dag(42), "must be a dagitty object")

})

# =============================================================================
# The verdict
# =============================================================================

test_that("adjusting for the confounder identifies the effect", {

  check <- check_dag(cmo_structure(), "protein", "disease", adjusted = "age")

  expect_s3_class(check, "CMODagCheck")
  expect_true(check$identifiable)
  expect_match(check$reason, "closes every backdoor path", fixed = TRUE)
  expect_length(check$problems, 0)

})

test_that("adjusting for nothing leaves the backdoor open, and says which", {

  check <- check_dag(cmo_structure(), "protein", "disease")

  expect_false(check$identifiable)
  expect_equal(check$required, "age")
  expect_match(check$reason, "age", fixed = TRUE)

})

test_that("adjusting for a mediator is caught and named as the cause", {

  # Conditioning on a mediator removes part of the very effect being
  # measured. Reporting only "a backdoor path is open" would send the reader
  # hunting for a missing confounder instead.

  check <- check_dag(cmo_structure(), "protein", "disease",
                     adjusted = c("age", "inflammation"))

  expect_false(check$identifiable)
  expect_length(check$problems, 1)
  expect_match(check$problems[1], "lies on the path", fixed = TRUE)
  expect_match(check$reason, "inflammation", fixed = TRUE)
  expect_match(check$reason, "should not be in the adjustment set",
               fixed = TRUE)

})

test_that("adjusting for a consequence of the outcome is caught", {

  check <- check_dag(cmo_structure(), "protein", "disease",
                     adjusted = c("age", "biomarker"))

  expect_false(check$identifiable)
  expect_match(check$problems[1], "create an association where there is none",
               fixed = TRUE)

})

test_that("an effect that cannot be identified at all says so", {

  # Nothing can rescue an arrow pointing the wrong way: the biomarker is a
  # consequence of the disease, so no adjustment identifies its effect on it.

  check <- check_dag(cmo_structure(), "biomarker", "disease",
                     adjusted = "age")

  expect_false(check$identifiable)
  expect_match(check$reason, "whatever is adjusted for", fixed = TRUE)

})

test_that("a variable outside the DAG is not guessed at", {

  check <- check_dag(cmo_structure(), "not_in_the_dag", "disease",
                     adjusted = "age")

  expect_true(is.na(check$identifiable))
  expect_match(check$reason, "not in the supplied DAG", fixed = TRUE)

})

test_that("check_dag() validates its input and prints", {

  expect_error(check_dag(NULL, "a", "b"), "No usable causal structure")

  expect_no_error(
    capture.output(print(check_dag(cmo_structure(), "protein", "disease",
                                   adjusted = "age")))
  )

})

# =============================================================================
# Inside analyze()
# =============================================================================

test_that("without a DAG nothing is claimed either way", {

  res <- analyze(cmo_dag_data(), "disease", covariates = "age", effort = "fast", 
    plots = FALSE, quiet = TRUE, control = analysis_control(methods = "association"))

  for (e in res$evidence) {
    expect_true(is.na(e$identifiable))
    expect_length(e$adjustment_problems, 0)
  }

})

test_that("a correct adjustment is marked identifiable", {

  res <- analyze(cmo_dag_data(), "disease", covariates = "age", effort = "fast", 
    plots = FALSE, quiet = TRUE, assume = analysis_assumptions(dag = cmo_structure()), 
    control = analysis_control(methods = "association"))

  protein <- Filter(function(e) identical(e$source, "protein"), res$evidence)

  skip_if(length(protein) == 0)

  expect_true(protein[[1]]$identifiable)
  expect_equal(protein[[1]]$identification, "adjustment")
  expect_match(protein[[1]]$identification_reason, "closes every backdoor",
               fixed = TRUE)

})

test_that("a harmful adjustment downgrades the edge rather than flattering it", {

  # An estimate adjusted for a mediator is worse than the unadjusted one.
  # Letting it keep the "adjustment" label would present the more misleading
  # number as the more careful one.

  res <- analyze(cmo_dag_data(), "disease", covariates = c("age", "inflammation"), 
    effort = "fast", plots = FALSE, quiet = TRUE, assume = analysis_assumptions(dag = cmo_structure()), 
    control = analysis_control(methods = "association"))

  protein <- Filter(function(e) identical(e$source, "protein"), res$evidence)

  skip_if(length(protein) == 0)

  e <- protein[[1]]

  expect_equal(e$identification, "none")
  expect_false(e$identifiable)
  expect_true(length(e$adjustment_problems) > 0)
  expect_true(any(grepl("should not be conditioned on", e$warnings)))
  expect_true(any(grepl("worse than the unadjusted one", e$assumptions)))

})

test_that("an adjustment that does not do the job loses the label", {

  # No harmful variable is conditioned on here, but the effect of a downstream
  # biomarker on the disease that produced it is not identifiable by any
  # adjustment. Leaving the "adjustment" badge on it would tell the reader the
  # opposite of what the audit found.

  res <- analyze(cmo_dag_data(), "disease", covariates = "age", effort = "fast", 
    plots = FALSE, quiet = TRUE, assume = analysis_assumptions(dag = cmo_structure()), 
    control = analysis_control(methods = "association"))

  bio <- Filter(function(e) identical(e$source, "biomarker"), res$evidence)

  skip_if(length(bio) == 0)

  expect_false(bio[[1]]$identifiable)
  expect_equal(bio[[1]]$identification, "none")
  expect_length(bio[[1]]$adjustment_problems, 0)
  expect_true(any(grepl("does not identify this effect",
                        bio[[1]]$assumptions)))

})

test_that("the audit is summarised in the run log", {

  res <- analyze(cmo_dag_data(), "disease", covariates = "age", effort = "fast", 
    plots = FALSE, quiet = TRUE, assume = analysis_assumptions(dag = cmo_structure()), 
    control = analysis_control(methods = "association"))

  expect_true(any(grepl("DAG audit", res$logs)))

})

test_that("the DAG changes no estimate, only what may be claimed", {

  # The whole point: supplying a structure is an interpretive act, not a
  # statistical one. If it moved the numbers it would be fitting the data to
  # the belief.

  prep <- cmo_dag_data()

  without <- analyze(prep, "disease", covariates = "age", effort = "fast", plots = FALSE, 
    quiet = TRUE, control = analysis_control(methods = "association"))

  with_dag <- analyze(prep, "disease", covariates = "age", effort = "fast", plots = FALSE, 
    quiet = TRUE, assume = analysis_assumptions(dag = cmo_structure()), 
    control = analysis_control(methods = "association"))

  estimates <- function(r) {
    key <- vapply(r$evidence, function(e) paste(e$source, e$target),
                  character(1))
    est <- vapply(r$evidence, function(e) e$estimate, numeric(1))
    stats::setNames(est, key)[sort(key)]
  }

  a <- estimates(without)
  b <- estimates(with_dag)

  expect_identical(names(a), names(b))
  expect_equal(unname(a), unname(b))

})

test_that("a DAG naming none of the analysed variables is harmless", {

  unrelated <- data.frame(from = "x", to = "y", stringsAsFactors = FALSE)

  res <- analyze(cmo_dag_data(), "disease", covariates = "age", effort = "fast", 
    plots = FALSE, quiet = TRUE, assume = analysis_assumptions(dag = unrelated), 
    control = analysis_control(methods = "association"))

  expect_true(all(vapply(res$evidence,
                         function(e) is.na(e$identifiable), logical(1))))

})
