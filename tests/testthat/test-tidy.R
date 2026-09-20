test_that("as.data.frame() returns the reading columns of a result", {

  data <- simulate_data(n = 80, blocks = list(main = 8), seed = 3)

  ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)

  fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(methods = c("association",
                                                        "conditional")))

  table <- as.data.frame(fit)

  expect_s3_class(table, "data.frame")
  expect_true(nrow(table) > 0)
  expect_true(all(c("source", "target", "estimate", "evidence_score") %in%
                    names(table)))

  # Ordered by evidence, so head() is the part a reader wants.
  expect_true(all(diff(table$evidence_score) <= 0))

  # The subset is narrower than the engine's full table, and all = TRUE
  # recovers it.
  expect_lt(ncol(table), ncol(as.data.frame(fit, all = TRUE)))
  expect_equal(nrow(table), nrow(as.data.frame(fit, all = TRUE)))

  # Row names are reset, so the frame can be written out as-is.
  expect_identical(rownames(table), as.character(seq_len(nrow(table))))

})


test_that("outcome_name() reads the name and not the outcome list", {

  data <- simulate_data(n = 60, blocks = list(main = 5), seed = 4)

  ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)

  fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(methods = "association"))

  # The outcome slot is a list; comparing it to a character column succeeds
  # by coercion and returns the wrong rows, which is exactly what this
  # accessor exists to prevent.
  expect_true(is.list(fit$outcome))
  expect_identical(outcome_name(fit), "y")
  expect_length(outcome_name(fit), 1L)

  restricted <- as.data.frame(fit, outcome_only = TRUE)

  expect_true(all(restricted$source == "y" | restricted$target == "y"))

})


test_that("as.data.frame() filters by evidence score", {

  data <- simulate_data(n = 80, blocks = list(main = 8), seed = 5)

  ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)

  fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(methods = "association"))

  full <- as.data.frame(fit)
  cut <- as.data.frame(fit, min_score = max(full$evidence_score))

  expect_lte(nrow(cut), nrow(full))
  expect_true(all(cut$evidence_score >= max(full$evidence_score)))

})


test_that("as.data.frame() works on the audit and the preprocessing result", {

  data <- simulate_data(n = 60, blocks = list(main = 6), seed = 6)

  audit <- check_data(data)
  cleaned <- preprocess(data, audit, plots = FALSE, quiet = TRUE)

  blocks <- as.data.frame(audit)

  expect_s3_class(blocks, "data.frame")
  expect_true("block" %in% names(blocks))
  expect_equal(nrow(blocks), length(data$assays))

  steps <- as.data.frame(cleaned)

  expect_s3_class(steps, "data.frame")
  expect_true(nrow(steps) > 0)
  expect_true(all(c("block", "stage", "method") %in% names(steps)))

})


test_that("as.data.frame() on an empty result returns an empty frame", {

  empty <- structure(list(graph = list(edges = data.frame()),
                          outcome = list(name = "y")),
                     class = "CMOResult")

  expect_equal(nrow(as.data.frame(empty)), 0L)
  expect_s3_class(as.data.frame(empty), "data.frame")

})

