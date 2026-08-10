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

cmo_reportable <- function() {

  set.seed(11)

  n <- 70
  exposure <- rnorm(n)
  mediator <- 0.8 * exposure + rnorm(n, sd = 0.6)
  y <- 0.9 * mediator + rnorm(n, sd = 0.7)
  ids <- paste0("S", seq_len(n))

  obj <- load_data(
    list(
      proteins = data.frame(APOA1 = exposure, CRP = rnorm(n), row.names = ids),
      metabolites = data.frame(SCFA = mediator, row.names = ids)
    ),
    metadata = data.frame(
      sample_id = ids, HDL = y,
      status = as.integer(y > stats::median(y)),
      fu_time = rexp(n, 0.05) + 1,
      age = rnorm(n, 55, 8),
      stringsAsFactors = FALSE
    )
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

  analyze(prep, outcome = "HDL", covariates = "age", plots = TRUE, 
    quiet = TRUE, control = analysis_control(bootstrap = 50))

}

test_that("report() writes a self-contained analysis document", {

  res <- cmo_reportable()
  path <- file.path(tempdir(), "cmo-analysis.html")

  on.exit(unlink(path), add = TRUE)

  back <- report(res, file = path, open = FALSE, quiet = TRUE)

  expect_true(file.exists(path))
  expect_s3_class(back, "CMOResult")
  expect_equal(attr(back, "report_path"), path)

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(txt, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(txt, "</html>", fixed = TRUE)

  # Must open with no network at all.
  expect_false(grepl("(src|href)=[\"']https?://", txt))

})

test_that("the general report carries every reader-facing section", {

  res <- cmo_reportable()
  path <- file.path(tempdir(), "cmo-general.html")

  on.exit(unlink(path), add = TRUE)

  report(res, file = path, open = FALSE, quiet = TRUE)

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  for (section in c("overview", "howto", "findings", "pathways", "network",
                    "importance", "methods", "data", "limitations")) {

    expect_match(txt, sprintf("id='%s'", section), fixed = TRUE)

  }

})

test_that("audience controls whether the technical tables are included", {

  res <- cmo_reportable()

  general <- file.path(tempdir(), "cmo-aud-general.html")
  technical <- file.path(tempdir(), "cmo-aud-technical.html")

  on.exit(unlink(c(general, technical)), add = TRUE)

  report(res, file = general, open = FALSE, quiet = TRUE)
  report(res, file = technical, open = FALSE, quiet = TRUE,
         audience = "technical")

  general_txt <- paste(readLines(general, warn = FALSE), collapse = "\n")
  technical_txt <- paste(readLines(technical, warn = FALSE), collapse = "\n")

  expect_false(grepl("id='technical'", general_txt, fixed = TRUE))
  expect_true(grepl("id='technical'", technical_txt, fixed = TRUE))

  expect_gt(nchar(technical_txt), nchar(general_txt))

})

# =============================================================================
# The part that keeps a non-expert from over-reading the result
# =============================================================================

test_that("the report explains that correlation is not causation", {

  res <- cmo_reportable()
  path <- file.path(tempdir(), "cmo-caution.html")

  on.exit(unlink(path), add = TRUE)

  report(res, file = path, open = FALSE, quiet = TRUE)

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(txt, "is not the same as showing that one causes the other",
               fixed = TRUE)

  # And specifically that method agreement does not repair it, which is the
  # misreading this engine invites.
  expect_match(txt, "High agreement means the finding is stable, not that it is causal",
               fixed = TRUE)

})

test_that("every finding is labelled with how much can be claimed", {

  res <- cmo_reportable()
  path <- file.path(tempdir(), "cmo-labels.html")

  on.exit(unlink(path), add = TRUE)

  report(res, file = path, open = FALSE, quiet = TRUE)

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  strategies <- unique(vapply(res$evidence, function(e) e$identification,
                              character(1)))

  for (s in strategies) {

    expect_match(txt, .result_identification_label(s), fixed = TRUE)

  }

  expect_match(txt, "badge-id", fixed = TRUE)

})

test_that("headline findings avoid the technical vocabulary", {

  # The summary statements produced for the analysis object are phrased for a
  # statistician; the report has to translate them, not pass them through.

  res <- cmo_reportable()

  plain <- .result_plain_statements(res)

  expect_true(length(plain) > 0)

  for (jargon in c("temporal precedence", "adjusted association",
                   "confounding", "integrated relationships")) {

    expect_false(any(grepl(jargon, plain, fixed = TRUE)), label = jargon)

  }

  expect_true(any(grepl("clearest finding", plain, fixed = TRUE)))

})

test_that("plain-language direction reads as a sentence", {

  expect_equal(.result_plain_direction("positive", "A", "B"),
               "Higher A goes together with higher B")
  expect_equal(.result_plain_direction("negative", "A", "B"),
               "Higher A goes together with lower B")
  expect_match(.result_plain_direction(NA, "A", "B"), "related to")

})

test_that("identification labels are plain and distinct", {

  labels <- vapply(c("none", "adjustment", "temporal", "instrument"),
                   .result_identification_label, character(1))

  expect_length(unique(labels), 4)

  # No underscores, no statistical terms of art.
  expect_false(any(grepl("_", labels)))

})

test_that("a cross-sectional report says nothing was measured in order", {

  res <- cmo_reportable()

  expect_equal(res$performance$temporal_edges, 0)

  plain <- .result_plain_statements(res)

  expect_true(any(grepl("single point in time", plain, fixed = TRUE)))

})

test_that("a survival report reports the relationships measured in order", {

  set.seed(12)

  n <- 70
  x <- rnorm(n)
  y <- 0.9 * x + rnorm(n, sd = 0.7)
  ids <- paste0("S", seq_len(n))

  obj <- load_data(
    list(a = data.frame(V1 = x, V2 = rnorm(n), row.names = ids)),
    metadata = data.frame(sample_id = ids,
                          status = as.integer(y > stats::median(y)),
                          fu_time = rexp(n, 0.05) + 1,
                          stringsAsFactors = FALSE)
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)
  res <- analyze(prep, "status", time = "fu_time", plots = FALSE, quiet = TRUE, 
    control = analysis_control(bootstrap = 30))

  plain <- .result_plain_statements(res)

  expect_true(any(grepl("cannot run backwards", plain, fixed = TRUE)))

})

# =============================================================================
# Console format and robustness
# =============================================================================

test_that("the console format works and returns the object", {

  res <- cmo_reportable()

  out <- capture.output(back <- report(res, format = "console"))

  expect_identical(back, res)
  expect_true(any(grepl("Analysis Report", out)))
  expect_true(any(grepl("Limitations", out)))
  expect_true(any(grepl("goes together with", out)))

})

test_that("report() validates its arguments", {

  res <- cmo_reportable()

  expect_error(report(res, format = "pdf"))
  expect_error(report(res, audience = "experts"))

})

test_that("report() survives an empty result", {

  path <- file.path(tempdir(), "cmo-empty-analysis.html")

  on.exit(unlink(path), add = TRUE)

  expect_no_error(report(CMOResult(), file = path, open = FALSE, quiet = TRUE))
  expect_true(file.exists(path))

  expect_no_error(capture.output(report(CMOResult(), format = "console")))

})

test_that("report() is registered for CMOResult", {

  expect_false(is.null(utils::getS3method("report", "CMOResult",
                                          optional = TRUE)))

})
