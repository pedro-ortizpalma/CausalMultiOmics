# The analysis engine reaches for optional packages. On a machine that
# has only the hard dependencies the generators skip themselves, but the
# assertions below expect a full engine, so the whole file is skipped
# rather than left to fail for the wrong reason.

skip_if_not_installed("igraph")
skip_if_not_installed("glmnet")
skip_if_not_installed("ranger")
skip_if_not_installed("bnlearn")
skip_if_not_installed("lavaan")
skip_if_not_installed("survival")

# A planted mechanism: EXP -> MED -> outcome, buried in noise features. The
# engine should recover it, and the tests below check both that it does and
# that it does not overclaim while doing so.

cmo_mechanism <- function(n = 80, seed = 42) {

  set.seed(seed)

  exposure <- rnorm(n)
  mediator <- 0.8 * exposure + rnorm(n, sd = 0.6)
  y <- 0.9 * mediator + 0.2 * exposure + rnorm(n, sd = 0.7)

  ids <- paste0("S", seq_len(n))

  obj <- load_data(
    assays = list(
      blockA = data.frame(EXP = exposure, A2 = rnorm(n), A3 = rnorm(n),
                          row.names = ids),
      blockB = data.frame(MED = mediator, B2 = rnorm(n), row.names = ids)
    ),
    metadata = data.frame(
      sample_id = ids,
      y = y,
      status = as.integer(y > stats::median(y)),
      fu_time = rexp(n, 0.05) + 1,
      age = rnorm(n, 55, 8),
      subject = paste0("P", rep(seq_len(n / 2), each = 2)),
      stringsAsFactors = FALSE
    )
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

cmo_analysis <- function(...) {

  analyze(cmo_mechanism(), outcome = "y", plots = FALSE, quiet = TRUE,
          bootstrap = 50, ...)

}

test_that("analyze() returns a CMOResult built from evidence", {

  res <- cmo_analysis()

  expect_s3_class(res, "CMOResult")
  expect_setequal(names(res), names(CMOResult()))

  expect_true(length(res$evidence) > 0)
  expect_s3_class(res$evidence[[1]], "EvidenceEdge")
  expect_s3_class(res$graph, "EvidenceGraph")

  expect_true(res$performance$generators_run > 1)

})

test_that("analyze() starts from a PreprocessingResult, not raw data", {

  obj <- load_data(
    list(a = data.frame(x = rnorm(30), row.names = paste0("S", 1:30))),
    metadata = data.frame(sample_id = paste0("S", 1:30), y = rnorm(30),
                          stringsAsFactors = FALSE)
  )

  expect_error(analyze(obj, "y"), "must be a PreprocessingResult")

})

test_that("analyze() validates the outcome and the generator names", {

  prep <- cmo_mechanism()

  expect_error(analyze(prep, "not_a_column", quiet = TRUE), "not a column")
  expect_error(analyze(prep, "y", methods = "telepathy", quiet = TRUE),
               "Unknown generator")
  expect_error(analyze(prep, "y", blocks = "nope", quiet = TRUE),
               "Unknown block")

})

test_that("the planted mechanism is recovered", {

  res <- cmo_analysis()

  edges <- res$graph$edges

  expect_true(any(edges$source == "MED" & edges$target == "y"))
  expect_true(any(edges$source == "EXP" & edges$target == "y"))

  # The real drivers should outrank the noise features.
  drivers <- res$interpretation$drivers

  expect_true(all(c("MED", "EXP") %in% utils::head(drivers$source, 2)))

  noise <- edges$evidence_score[edges$source %in% c("A2", "A3", "B2") &
                                  edges$target == "y"]
  signal <- edges$evidence_score[edges$source %in% c("MED", "EXP") &
                                   edges$target == "y"]

  if (length(noise) > 0) expect_gt(min(signal), max(noise))

})

test_that("the mediation path is found and ranked", {

  res <- cmo_analysis()

  expect_gt(nrow(res$causal_paths), 0)
  expect_true(any(grepl("EXP -> MED -> y", res$causal_paths$path, fixed = TRUE)))

})

# =============================================================================
# Identification: the part that must not overclaim
# =============================================================================

test_that("every edge records an identification strategy", {

  res <- cmo_analysis()

  for (e in res$evidence) {

    expect_true(e$identification %in%
                  c("none", "adjustment", "temporal", "instrument"))
    expect_true(length(e$assumptions) > 0)

  }

})

test_that("declared covariates make the identification an adjusted one", {

  # Regression: which.max() returns a position among the contributing
  # methods, and indexing the lookup table with it reported every edge as
  # unidentified no matter what was adjusted for.

  res <- analyze(cmo_mechanism(), outcome = "y", covariates = "age",
                 plots = FALSE, quiet = TRUE, bootstrap = 50)

  strategies <- vapply(res$evidence, function(e) e$identification, character(1))

  expect_true(any(strategies == "adjustment"))

  adjusted <- Filter(function(e) identical(e$identification, "adjustment"),
                     res$evidence)

  expect_true(length(adjusted) > 0)
  expect_true(any(vapply(adjusted, function(e) "age" %in% e$adjustment_set,
                         logical(1))))

})

test_that("a cross-sectional analysis never claims temporal identification", {

  res <- cmo_analysis()

  strategies <- vapply(res$evidence, function(e) e$identification, character(1))

  expect_false(any(strategies == "temporal"))
  expect_equal(res$performance$temporal_edges, 0)

})

test_that("a survival design does claim temporal precedence", {

  res <- analyze(cmo_mechanism(), outcome = "status", time = "fu_time",
                 plots = FALSE, quiet = TRUE, bootstrap = 50)

  expect_equal(res$design$type, "survival")
  expect_gt(res$performance$temporal_edges, 0)

  strategies <- vapply(res$evidence, function(e) e$identification, character(1))
  expect_true(any(strategies == "temporal"))

})

test_that("the report always states the limitations", {

  res <- cmo_analysis()

  expect_true(length(res$report$limitations) >= 3)
  expect_true(any(grepl("stability, not validity", res$report$limitations)))
  expect_true(any(grepl("no relationship reported here is identified by design",
                        res$report$limitations)))

})

# =============================================================================
# Integration
# =============================================================================

test_that("evidence from several methods is merged, not duplicated", {

  res <- cmo_analysis()

  keys <- vapply(res$evidence, function(e) paste(e$source, e$target),
                 character(1))

  expect_equal(length(keys), length(unique(keys)))

  # At least one relationship should have been seen by more than one method.
  n_methods <- vapply(res$evidence, function(e) length(e$supporting_methods),
                      integer(1))

  expect_gt(max(n_methods), 1)

})

test_that("reciprocal edges are resolved into one direction", {

  # Regression: methods disagreeing on direction left both A -> B and B -> A
  # in the graph, which produced paths that read as mechanisms but were
  # artefacts of the 2-cycle.

  res <- cmo_analysis()

  edges <- res$graph$edges

  forward <- paste(edges$source, edges$target)
  reverse <- paste(edges$target, edges$source)

  expect_length(intersect(forward, reverse), 0)

})

test_that("the three scores are bounded and independent", {

  res <- cmo_analysis()

  for (e in res$evidence) {

    expect_gte(e$strength, 0);    expect_lte(e$strength, 1)
    expect_gte(e$confidence, 0);  expect_lte(e$confidence, 1)
    expect_gte(e$consistency, 0); expect_lte(e$consistency, 1)
    expect_gte(e$evidence_score, 0)
    expect_lte(e$evidence_score, 100)

  }

})

test_that("analyze() leaves the caller's RNG untouched", {

  # igraph's community detection shuffles node order, so the engine needs a
  # backstop beyond the per-generator guards.

  prep <- cmo_mechanism()

  set.seed(321)
  expected <- runif(3)

  set.seed(321)
  invisible(analyze(prep, "y", plots = FALSE, quiet = TRUE, bootstrap = 30))
  actual <- runif(3)

  expect_equal(actual, expected)

})

test_that("analyze() is deterministic", {

  prep <- cmo_mechanism()

  a <- analyze(prep, "y", plots = FALSE, quiet = TRUE, bootstrap = 50)
  b <- analyze(prep, "y", plots = FALSE, quiet = TRUE, bootstrap = 50)

  expect_equal(
    vapply(a$evidence, function(e) e$evidence_score, numeric(1)),
    vapply(b$evidence, function(e) e$evidence_score, numeric(1))
  )

})

# =============================================================================
# Design detection and generator selection
# =============================================================================

test_that("the design selects which generators can run", {

  prep <- cmo_mechanism()

  cross <- analyze(prep, "y", plots = FALSE, quiet = TRUE, bootstrap = 30)
  expect_false("survival" %in% names(cross$models))
  expect_false("longitudinal" %in% names(cross$models))

  long <- analyze(prep, "y", subject = "subject", plots = FALSE, quiet = TRUE,
                  bootstrap = 30)
  expect_equal(long$design$type, "longitudinal")

  surv <- analyze(prep, "status", time = "fu_time", plots = FALSE,
                  quiet = TRUE, bootstrap = 30)
  expect_equal(surv$design$type, "survival")
  expect_true("survival" %in% names(surv$models))

})

test_that("goal = 'predictive' runs a narrower set", {

  res <- analyze(cmo_mechanism(), "y", goal = "predictive",
                 plots = FALSE, quiet = TRUE)

  expect_true(all(names(res$models) %in%
                    c("association", "elasticnet", "randomforest")))

})

test_that("a missing optional package is reported, not fatal", {

  res <- analyze(cmo_mechanism(), "y", methods = "association",
                 plots = FALSE, quiet = TRUE)

  expect_length(res$models, 1)
  expect_true(length(res$evidence) > 0)

})

# =============================================================================
# Alignment and screening
# =============================================================================

test_that("blocks are joined on shared samples and the loss is recorded", {

  set.seed(3)

  ids <- paste0("S", 1:40)

  obj <- load_data(
    list(
      a = data.frame(x1 = rnorm(40), x2 = rnorm(40), row.names = ids),
      b = data.frame(z1 = rnorm(30), row.names = ids[1:30])
    ),
    metadata = data.frame(sample_id = ids, y = rnorm(40),
                          stringsAsFactors = FALSE)
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)
  res <- analyze(prep, "y", plots = FALSE, quiet = TRUE, bootstrap = 30)

  expect_equal(length(res$data$samples), 30)
  expect_length(res$data$dropped_samples, 10)
  expect_true(any(grepl("dropped by the intersection", res$report$limitations)))

})

test_that("too little overlap is refused rather than silently analysed", {

  set.seed(4)

  obj <- load_data(
    list(
      a = data.frame(x = rnorm(30), row.names = paste0("S", 1:30)),
      b = data.frame(z = rnorm(30), row.names = paste0("T", 1:30))
    ),
    metadata = data.frame(sample_id = c(paste0("S", 1:30), paste0("T", 1:30)),
                          y = rnorm(60), stringsAsFactors = FALSE)
  )

  validation <- check_data(obj)

  # Caught at the first opportunity, so the user does not preprocess two
  # blocks for nothing before being told they cannot be analysed together.
  expect_true(any(grepl("present in every block", validation$errors)))

  prep <- preprocess(obj, validation, plots = FALSE, quiet = TRUE,
                     force = TRUE)

  expect_error(analyze(prep, "y", quiet = TRUE), "shared by all")

  # And when forced past that, the message says the identifiers are the
  # problem rather than leaving the user to guess.
  expect_error(analyze(prep, "y", quiet = TRUE), "naming difference")

})

test_that("screening is applied and recorded when the space is large", {

  set.seed(9)

  n <- 40
  p <- 60

  m <- matrix(rnorm(n * p), nrow = n)
  colnames(m) <- paste0("F", seq_len(p))
  rownames(m) <- paste0("S", seq_len(n))

  obj <- load_data(
    list(big = as.data.frame(m)),
    metadata = data.frame(sample_id = rownames(m), y = rnorm(n),
                          stringsAsFactors = FALSE)
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

  res <- analyze(prep, "y", max_features = 20, plots = FALSE, quiet = TRUE,
                 bootstrap = 30, methods = "association")

  expect_true(res$data$screening$screened)
  expect_equal(res$performance$features_retained, 20)
  expect_true(any(grepl("screened out", res$report$limitations)))

})

# =============================================================================
# annotate_evidence()
# =============================================================================

test_that("annotate_evidence() attaches support and records its provenance", {

  res <- cmo_analysis()

  annotations <- data.frame(
    source = "EXP", target = "MED", support = 0.9,
    database = "local export", stringsAsFactors = FALSE
  )

  annotated <- annotate_evidence(res, annotations)

  touched <- Filter(function(e) is.finite(e$biological_support),
                    annotated$evidence)

  expect_true(length(touched) > 0)
  expect_true(any(grepl("local export", unlist(lapply(touched, function(e) e$notes)))))
  expect_true(any(grepl("retrieved", unlist(lapply(touched, function(e) e$notes)))))

})

test_that("annotate_evidence() validates its input", {

  res <- cmo_analysis()

  expect_error(annotate_evidence(1, data.frame(source = "a", target = "b")),
               "must be a CMOResult")
  expect_error(annotate_evidence(res, data.frame(a = 1)), "source")
  expect_error(annotate_evidence(res, data.frame(source = "a", target = "b"),
                                 weight = 2), "between 0 and 1")

})

# =============================================================================
# Methods
# =============================================================================

test_that("the new classes print and summarise", {

  res <- cmo_analysis()

  expect_no_error(capture.output(print(res)))
  expect_no_error(capture.output(summary(res)))
  expect_no_error(capture.output(print(res$graph)))
  expect_no_error(capture.output(summary(res$graph)))
  expect_no_error(capture.output(print(res$evidence[[1]])))
  expect_no_error(capture.output(summary(res$evidence[[1]])))

  expect_no_error(capture.output(print(EvidenceEdge())))
  expect_no_error(capture.output(print(EvidenceGraph())))
  expect_no_error(capture.output(summary(EvidenceGraph())))
  expect_no_error(capture.output(print(CMOResult())))

})

test_that("summary() of an edge states the assumptions", {

  res <- cmo_analysis()

  out <- paste(capture.output(summary(res$evidence[[1]])), collapse = "\n")

  expect_match(out, "Assumptions required for a causal reading")

})

test_that("an unidentified edge says so when printed", {

  res <- cmo_analysis()

  unidentified <- Filter(
    function(e) e$identification %in% c("none", "adjustment"), res$evidence
  )

  skip_if(length(unidentified) == 0)

  out <- paste(capture.output(print(unidentified[[1]])), collapse = "\n")

  expect_match(out, "association, not an identified causal effect")

})
