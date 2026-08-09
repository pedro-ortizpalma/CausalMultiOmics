cmo_exportable <- function() {

  sim <- simulate_data(
    n = 200,
    blocks = list(a = c("x", "z"), b = 3),
    dag = data.frame(from = c("x", "z"), to = c("y", "y"),
                     effect = c(0.8, 0.5), stringsAsFactors = FALSE),
    outcome = "y"
  )

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  analyze(prep, "y", methods = "association", effort = "fast",
          plots = FALSE, quiet = TRUE)

}

# =============================================================================
# Getting the graph out
# =============================================================================

test_that("every offered format writes something a reader could open", {

  res <- cmo_exportable()

  for (f in c("graphml", "dot", "json")) {

    skip_if(f != "json" && !requireNamespace("igraph", quietly = TRUE))

    path <- file.path(tempdir(), paste0("cmo_test.", f))

    expect_no_warning(export_graph(res, path, format = f))
    expect_true(file.exists(path))
    expect_gt(file.size(path), 200)

    unlink(path)

  }

})

test_that("the evidence survives the export", {

  # A graph exported with only its arrows arrives stripped of everything that
  # made it worth exporting.

  skip_if_not_installed("igraph")

  res <- cmo_exportable()

  path <- file.path(tempdir(), "cmo_attrs.graphml")
  export_graph(res, path)

  text <- paste(readLines(path, warn = FALSE), collapse = "")

  for (a in c("evidence_score", "strength", "confidence", "consistency",
              "identification", "data_quality")) {
    expect_match(text, a, fixed = TRUE)
  }

  unlink(path)

})

test_that("the json is parseable and carries nodes and links", {

  skip_if_not_installed("jsonlite")

  res <- cmo_exportable()

  path <- file.path(tempdir(), "cmo_test.json")
  export_graph(res, path, format = "json")

  parsed <- jsonlite::fromJSON(path)

  expect_equal(parsed$outcome, "y")
  expect_true(nrow(parsed$nodes) > 0)
  expect_true(nrow(parsed$links) > 0)
  expect_true(all(c("source", "target", "evidence_score") %in%
                    names(parsed$links)))

  unlink(path)

})

test_that("json needs nothing installed", {

  # The point of hand-rolling it: a user with no optional packages can still
  # get the graph out.

  res <- cmo_exportable()

  path <- file.path(tempdir(), "cmo_plain.json")

  expect_no_error(export_graph(res, path, format = "json"))

  unlink(path)

})

test_that("a score threshold filters what is written", {

  skip_if_not_installed("jsonlite")

  res <- cmo_exportable()

  everything <- file.path(tempdir(), "cmo_all.json")
  strongest <- file.path(tempdir(), "cmo_top.json")

  export_graph(res, everything, format = "json")

  cut <- stats::median(res$graph$edges$evidence_score)

  export_graph(res, strongest, format = "json", min_score = cut)

  expect_lt(nrow(jsonlite::fromJSON(strongest)$links),
            nrow(jsonlite::fromJSON(everything)$links))

  unlink(c(everything, strongest))

})

test_that("it refuses what it cannot export", {

  res <- cmo_exportable()

  expect_error(export_graph("not a result", "x.json"), "must be a CMOResult")
  expect_error(export_graph(res), "single path")
  expect_error(export_graph(res, file.path(tempdir(), "x.json"),
                            format = "json", min_score = 1e6),
               "No relationship scores")

  empty <- res
  empty$graph$edges <- data.frame()

  expect_error(export_graph(empty, file.path(tempdir(), "x.json")),
               "no relationships to export")

})

# =============================================================================
# Constraining structure learning
# =============================================================================

test_that("a constraint is checked before it is used", {

  expect_null(.constraint_frame(NULL, "forbidden"))
  expect_null(.constraint_frame(data.frame(from = character(),
                                           to = character()), "forbidden"))

  expect_error(.constraint_frame(data.frame(a = 1, b = 2), "forbidden"),
               "'from' and 'to'")

  expect_error(.constraint_frame(data.frame(from = "x", to = "x"), "required"),
               "to itself")

  out <- .constraint_frame(data.frame(from = factor("a"), to = factor("b")),
                           "forbidden")

  expect_type(out$from, "character")

})

test_that("forbidden relationships cannot appear in the learned graph", {

  skip_if_not_installed("bnlearn")

  sim <- simulate_data(
    n = 300,
    blocks = list(a = c("x", "z", "w")),
    dag = data.frame(from = c("x", "z"), to = c("y", "y"),
                     effect = c(0.8, 0.6), stringsAsFactors = FALSE),
    outcome = "y"
  )

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  # Nothing may point away from the outcome: an arrow out of a variable
  # measured at the end of follow-up is not a hypothesis worth searching.
  banned <- data.frame(from = "y", to = c("x", "z", "w"),
                       stringsAsFactors = FALSE)

  res <- analyze(prep, "y", methods = "bayesnet", effort = "fast",
                 forbidden = banned, plots = FALSE, quiet = TRUE)

  learned <- res$graph$edges

  skip_if(nrow(learned) == 0)

  expect_false(any(learned$source == "y"))

})

test_that("a constraint is recorded as an assumption, not applied silently", {

  # It shaped the graph that came out. Applying it without saying so would
  # let the caller's own belief reappear to them as a finding.

  skip_if_not_installed("bnlearn")

  sim <- simulate_data(
    n = 300, blocks = list(a = c("x", "z", "w")),
    dag = data.frame(from = "x", to = "y", effect = 0.8),
    outcome = "y"
  )

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  res <- analyze(prep, "y", methods = "bayesnet", effort = "fast",
                 forbidden = data.frame(from = "y", to = c("x", "z", "w")),
                 plots = FALSE, quiet = TRUE)

  skip_if(length(res$evidence) == 0)

  assumptions <- unlist(lapply(res$evidence, function(e) e$assumptions))

  expect_true(any(grepl("excluded from the search", assumptions)))
  expect_true(any(grepl("could not have been found", assumptions)))

})

test_that("analyze() checks the constraint before running anything", {

  sim <- simulate_data(n = 100, blocks = list(a = 3))
  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  expect_error(
    analyze(prep, "y", forbidden = data.frame(a = 1), quiet = TRUE),
    "'from' and 'to'")

  expect_error(
    analyze(prep, "y", required = data.frame(from = "x", to = "x"),
            quiet = TRUE),
    "to itself")

})
