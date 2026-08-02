test_that("load_data() builds a well-formed MultiOmicsData object", {

  obj <- cmo_object()

  expect_s3_class(obj, "MultiOmicsData")
  expect_length(obj$assays, 3)
  expect_true(all(vapply(obj$assays, is.data.frame, logical(1))))
  expect_s3_class(obj$sample_info, "data.frame")
  expect_s3_class(obj$feature_info, "data.frame")
  expect_equal(nrow(obj$feature_info), 4 + 3 + 5)

})

test_that("load_data() rejects malformed input", {

  expect_error(load_data(), "must be supplied")
  expect_error(load_data(1), "must be a list")
  expect_error(load_data(list()), "is empty")
  expect_error(load_data(list(a = NULL)), "No valid data blocks")
  expect_error(load_data(list(a = 1)), "matrix or a data\\.frame")
  expect_error(load_data(list(cmo_block())), "named list")

})

test_that("load_data() reports emptiness before missing identifiers", {

  # Both problems are present at once; the more specific one has to win or
  # the message sends the user chasing row names on an empty block.

  expect_error(
    load_data(list(a = data.frame(x = integer(0)))),
    "no samples"
  )

  expect_error(
    load_data(list(a = data.frame(row.names = 1:5))),
    "no variables"
  )

})

test_that("load_data() refuses blocks without sample identifiers", {

  # Regression: a matrix with no dimnames used to be accepted and silently
  # labelled "1", "2", ..., so two unrelated blocks reported a full overlap.

  m <- matrix(rnorm(20), nrow = 5)

  expect_error(load_data(list(a = m)), "no sample identifiers")

  expect_error(
    load_data(list(a = as.data.frame(matrix(rnorm(20), nrow = 5)))),
    "no sample identifiers"
  )

  rownames(m) <- paste0("S", 1:5)
  expect_s3_class(load_data(list(a = m)), "MultiOmicsData")

})

test_that("load_data() rejects duplicated identifiers and feature names", {

  dup_features <- data.frame(A = rnorm(5), B = rnorm(5))
  colnames(dup_features) <- c("X", "X")
  rownames(dup_features) <- paste0("S", 1:5)

  expect_error(
    load_data(list(bad = dup_features)),
    "Duplicated feature names"
  )

  dup_samples <- data.frame(A = rnorm(4))
  attr(dup_samples, "row.names") <- c("S1", "S1", "S2", "S3")

  expect_error(
    load_data(list(bad = dup_samples)),
    "Duplicated sample identifiers"
  )

})

test_that("history stays the character log the class declares", {

  # Regression: history used to be assigned a nested list, which summary()
  # rendered as deparsed R code.

  obj <- cmo_object()

  expect_type(obj$history, "character")
  expect_type(MultiOmicsData()$history, "character")
  expect_match(obj$history, "^load_data\\(\\)")

  # The structured record is still available, just not in history.
  expect_equal(obj$misc$load_data$n_blocks, 3)

  out <- paste(capture.output(summary(obj)), collapse = "\n")
  expect_false(grepl("list(", out, fixed = TRUE))

})
