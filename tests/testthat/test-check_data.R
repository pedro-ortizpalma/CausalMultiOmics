test_that("check_data() audits a well-formed object", {

  v <- check_data(cmo_object())

  expect_s3_class(v, "CMOValidation")
  expect_true(v$valid)
  expect_true(is.numeric(v$score) && !is.na(v$score))

  expect_length(v$diagnostics, 3)
  expect_length(v$recipes, 3)
  expect_length(v$transformations, 3)

  expect_s3_class(v$diagnostics[[1]], "BlockDiagnostics")
  expect_s3_class(v$recipes[[1]], "PreprocessingRecipe")
  expect_s3_class(v$transformations[[1]], "TransformationRecommendation")

})

test_that("check_data() rejects anything that is not a MultiOmicsData", {

  expect_error(check_data(1), "must be a MultiOmicsData")
  expect_error(check_data(NULL), "must be a MultiOmicsData")
  expect_error(check_data(list(assays = list())), "must be a MultiOmicsData")

})

test_that("no class ever loses a slot", {

  v <- check_data(cmo_object())

  expect_setequal(names(v), names(CMOValidation()))
  expect_setequal(names(v$summary), names(CMOValidation()$summary))
  expect_setequal(names(v$details), names(CMOValidation()$details))
  expect_setequal(names(v$qc), names(CMOValidation()$qc))
  expect_setequal(names(v$plots), names(CMOValidation()$plots))
  expect_setequal(names(v$tables), names(CMOValidation()$tables))
  expect_setequal(names(v$execution), names(CMOValidation()$execution))

  for (nm in names(v$diagnostics)) {
    expect_setequal(names(v$diagnostics[[nm]]), names(BlockDiagnostics()))
    expect_setequal(names(v$recipes[[nm]]), names(PreprocessingRecipe()))
    expect_setequal(names(v$transformations[[nm]]),
                    names(TransformationRecommendation()))
  }

})

test_that("check_data() leaves the caller's RNG state untouched", {

  # Regression: set.seed(1) inside the fold splitter and sample() inside the
  # Shapiro subsampler moved the user's generator, so every simulation run
  # after a validation silently changed.

  obj <- cmo_object()

  set.seed(999)
  expected <- runif(3)

  set.seed(999)
  invisible(check_data(obj))
  actual <- runif(3)

  expect_equal(actual, expected)

})

test_that("check_data() is deterministic", {

  obj <- cmo_object()

  a <- check_data(obj)
  b <- check_data(obj)

  expect_equal(a$score, b$score)
  expect_equal(
    vapply(a$transformations, function(t) t$recommended, character(1)),
    vapply(b$transformations, function(t) t$recommended, character(1))
  )

})

test_that("structural problems are returned, not raised", {

  obj <- MultiOmicsData()
  obj$assays <- list()

  v <- check_data(obj)

  expect_s3_class(v, "CMOValidation")
  expect_false(v$valid)
  expect_true("No data blocks found." %in% v$errors)

})

test_that("a skipped block does not desynchronise the per-block plots", {

  # Regression: .build_plots() indexed labels by names(assays) but values by
  # diagnostics, so a block dropped by check_data() shifted the two apart.

  obj <- MultiOmicsData()
  obj$assays <- list(
    good = cmo_block(),
    broken = data.frame(row.names = paste0("S", 1:20))
  )

  v <- check_data(obj)

  expect_false(v$valid)
  expect_length(v$diagnostics, 1)
  expect_setequal(names(v$plots), names(CMOValidation()$plots))
  expect_false(is.null(v$plots$missing_by_block))

})

test_that("quality control tolerates an unpopulated validation", {

  # Regression: nrow(NULL) == 0 is logical(0), which blew up the if().

  expect_no_error(qc <- .compute_qc_checks(CMOValidation(), NA_real_, FALSE))

  expect_false(qc$passed)
  expect_true("missing_threshold" %in% qc$failed_checks)
  expect_true("outcome_availability" %in% qc$skipped_checks)

})

test_that("metadata problems are reported", {

  meta <- cmo_metadata()
  meta$sample_id <- NULL

  obj <- load_data(assays = list(a = cmo_block()), metadata = meta)
  v <- check_data(obj)

  expect_false(v$valid)
  expect_true(any(grepl("sample_id", v$errors)))

})

test_that("the overlap matrix reflects real shared samples", {

  v <- check_data(cmo_object())
  ov <- v$summary$overlap

  expect_true(isSymmetric(ov))
  expect_equal(diag(ov), vapply(cmo_object()$assays, nrow, numeric(1)))

  # transcriptomics and microbiome share all 20; proteomics only has 18.
  expect_equal(ov["transcriptomics", "microbiome"], 20)
  expect_equal(ov["transcriptomics", "proteomics"], 18)

})
