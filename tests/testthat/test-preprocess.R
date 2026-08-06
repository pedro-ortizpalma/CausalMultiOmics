cmo_prepared <- function() {

  obj <- cmo_object()

  list(object = obj, validation = check_data(obj))

}

test_that("preprocess() executes a plan and returns a PreprocessingResult", {

  p <- cmo_prepared()

  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  expect_s3_class(res, "PreprocessingResult")
  expect_setequal(names(res), names(PreprocessingResult()))

  expect_s3_class(res$data, "MultiOmicsData")
  expect_true(length(res$steps) > 0)
  expect_length(res$recipes, 3)

})

test_that("preprocess() refuses to derive a plan of its own", {

  p <- cmo_prepared()

  expect_error(preprocess(p$object), "must be supplied")
  expect_error(preprocess(1, p$validation), "must be a MultiOmicsData")
  expect_error(preprocess(p$object, "not a plan"), "must be a CMOValidation")

})

test_that("preprocess() accepts a single recipe or a list of recipes", {

  p <- cmo_prepared()

  one <- preprocess(p$object, p$validation$recipes$proteomics,
                    plots = FALSE, quiet = TRUE)

  expect_length(one$recipes, 1)
  expect_true(all(vapply(one$steps, function(s) s$block, character(1)) ==
                    "proteomics"))

  many <- preprocess(p$object, p$validation$recipes[c("proteomics", "microbiome")],
                     plots = FALSE, quiet = TRUE)

  expect_length(many$recipes, 2)

})

test_that("preprocess() refuses a plan that does not describe the object", {

  p <- cmo_prepared()

  other <- load_data(list(somethingelse = cmo_block()))

  expect_error(
    preprocess(other, p$validation, plots = FALSE, quiet = TRUE),
    "does not match"
  )

  # force records the mismatch rather than hiding it
  forced <- preprocess(other, p$validation, plots = FALSE, quiet = TRUE,
                       force = TRUE)

  expect_true(any(grepl("force = TRUE", forced$logs)))

})

test_that("preprocess() refuses to run an invalid validation", {

  obj <- MultiOmicsData()
  obj$assays <- list(a = cmo_block(), broken = data.frame(row.names = "S1"))

  v <- check_data(obj)

  expect_false(v$valid)
  expect_error(preprocess(obj, v, plots = FALSE, quiet = TRUE), "does not match")

})

test_that("the stage order honours the data type", {

  p <- cmo_prepared()

  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  stage_of <- function(block, stage) {
    hits <- Filter(function(s) identical(s$block, block) &&
                     identical(s$stage, stage), res$steps)
    if (length(hits) == 0) NA_integer_ else hits[[1]]$step
  }

  # proteomics is count data: library size has to be normalized before any
  # variance-stabilizing transformation.
  norm <- stage_of("proteomics", "normalization")
  trans <- stage_of("proteomics", "transformation")

  if (!is.na(norm) && !is.na(trans)) expect_lt(norm, trans)

  # Structural removal always comes first. Stages whose method resolves to
  # "none" are skipped entirely and leave no step behind, so this compares
  # against a stage that is always present.
  expect_lt(stage_of("proteomics", "remove_samples"),
            stage_of("proteomics", "transformation"))

  expect_lt(stage_of("proteomics", "remove_samples"),
            stage_of("proteomics", "remove_features"))

  # transcriptomics is continuous: it transforms before normalizing.
  tr_trans <- stage_of("transcriptomics", "transformation")
  tr_norm <- stage_of("transcriptomics", "normalization")

  if (!is.na(tr_trans) && !is.na(tr_norm)) expect_lt(tr_trans, tr_norm)

})

test_that("preprocessing actually cleans the data", {

  p <- cmo_prepared()

  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  for (nm in names(res$data$assays)) {

    processed <- res$data$assays[[nm]]

    expect_equal(sum(is.na(processed)), 0)
    expect_gt(ncol(processed), 0)

  }

  # G3 is constant and has to be gone.
  expect_false("G3" %in% names(res$data$assays$transcriptomics))
  expect_true("G3" %in% res$removed_features$transcriptomics)

})

test_that("every executed step is recorded with its model", {

  p <- cmo_prepared()

  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  for (s in res$steps) {

    expect_true(is.character(s$block) && nzchar(s$block))
    expect_true(is.character(s$stage) && nzchar(s$stage))
    expect_true(is.character(s$method) && nzchar(s$method))
    expect_false(is.null(s$model))
    expect_true(is.numeric(s$runtime))
    expect_true(is.logical(s$replay))

  }

  expect_s3_class(res$tables$steps, "data.frame")
  expect_equal(nrow(res$tables$steps), length(res$steps))

})

test_that("sample removal is marked as non-replayable and feature removal is not", {

  p <- cmo_prepared()

  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  for (s in res$steps) {

    if (s$stage %in% c("remove_samples", "outliers")) {
      expect_false(s$replay)
    }

    if (s$stage %in% c("remove_features", "filter")) {
      expect_true(s$replay)
    }

  }

})

# =============================================================================
# Replay
# =============================================================================

cmo_new_cohort <- function(n = 6) {

  set.seed(777)

  load_data(
    assays = list(
      transcriptomics = data.frame(
        G1 = rnorm(n), G2 = rnorm(n), G3 = rep(1, n),
        G4 = c(rnorm(n - 1), NA),
        row.names = paste0("N", seq_len(n))
      ),
      proteomics = data.frame(
        P1 = rpois(n, 5), P2 = rpois(n, 10), P3 = rpois(n, 3),
        row.names = paste0("N", seq_len(n))
      ),
      microbiome = {
        m <- matrix(rgamma(n * 5, 2), ncol = 5)
        m <- as.data.frame(m / rowSums(m))
        colnames(m) <- paste0("OTU", 1:5)
        rownames(m) <- paste0("N", seq_len(n))
        m
      }
    ),
    metadata = data.frame(
      sample_id = paste0("N", seq_len(n)),
      group = rep(c("A", "B"), length.out = n),
      batch = rep(c("X", "Y"), length.out = n),
      stringsAsFactors = FALSE
    )
  )

}

test_that("apply_preprocessing() pins the feature set of the training data", {

  p <- cmo_prepared()
  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  applied <- apply_preprocessing(cmo_new_cohort(), res, quiet = TRUE)

  expect_s3_class(applied, "MultiOmicsData")

  for (nm in names(res$data$assays)) {

    expect_equal(
      colnames(applied$assays[[nm]]),
      colnames(res$data$assays[[nm]])
    )

  }

})

test_that("apply_preprocessing() never drops samples from the new cohort", {

  # Silently discarding rows from a validation set is a standard way to
  # manufacture optimistic results, so the replay must not do it.

  p <- cmo_prepared()
  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  new_cohort <- cmo_new_cohort()
  applied <- apply_preprocessing(new_cohort, res, quiet = TRUE)

  for (nm in names(applied$assays)) {

    expect_equal(nrow(applied$assays[[nm]]), nrow(new_cohort$assays[[nm]]))
    expect_equal(rownames(applied$assays[[nm]]),
                 rownames(new_cohort$assays[[nm]]))

  }

})

test_that("the replay uses the stored models, not the new data", {

  # Running the identical cohort through preprocess() and through the replay
  # of a pipeline fitted on it must agree. If a step silently re-fitted on
  # the new data the two would drift apart.

  p <- cmo_prepared()
  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  # Feed the pipeline the very samples that survived its own filtering, so
  # the only difference left would be a re-fitted model.
  survivors <- p$object
  for (nm in names(res$data$assays)) {
    kept <- rownames(res$data$assays[[nm]])
    survivors$assays[[nm]] <- survivors$assays[[nm]][kept, , drop = FALSE]
  }

  replayed <- apply_preprocessing(survivors, res, quiet = TRUE)

  for (nm in names(res$data$assays)) {

    expect_equal(
      as.matrix(replayed$assays[[nm]]),
      as.matrix(res$data$assays[[nm]]),
      tolerance = 1e-8,
      info = nm
    )

  }

})

test_that("apply_preprocessing() validates its input", {

  p <- cmo_prepared()
  res <- preprocess(p$object, p$validation, plots = FALSE, quiet = TRUE)

  expect_error(apply_preprocessing(1, res), "must be a MultiOmicsData")
  expect_error(apply_preprocessing(cmo_new_cohort(), 1), "PreprocessingResult")

  expect_error(
    apply_preprocessing(load_data(list(other = cmo_block())), res),
    "has no block"
  )

  expect_error(
    apply_preprocessing(cmo_new_cohort(), PreprocessingResult()),
    "no steps to replay"
  )

})

# =============================================================================
# Registry
# =============================================================================

test_that("the registry reports what it can execute", {

  available <- .available_methods()

  expect_s3_class(available, "data.frame")
  expect_true(all(c("stage", "method", "label") %in% names(available)))

  for (stage in c("imputation", "transformation", "normalization",
                  "scaling", "batch", "feature_selection")) {

    expect_true("none" %in% available$method[available$stage == stage])

  }

})

test_that("an unknown method names the alternatives", {

  expect_error(.get_method("imputation", "telepathy"), "Available")
  expect_error(.get_method("nonsense", "median"), "Unknown preprocessing stage")

})

test_that("methods that cannot be replayed say why", {

  # mice and missForest complete a dataset instead of fitting a transferable
  # model, so the engine refuses them rather than pretending.

  entry <- .get_method("imputation", "mice")

  expect_error(entry$fit(cmo_block(), list(), list()), "cannot be replayed")
  expect_error(entry$fit(cmo_block(), list(), list()), "knn")

})

test_that("every check_data() recipe uses methods the engine implements", {

  # The orchestrator only executes the plan, so anything check_data() can
  # write has to be executable or the two have drifted apart.

  p <- cmo_prepared()

  for (r in p$validation$recipes) {

    for (stage in c("imputation", "transformation", "normalization",
                    "scaling", "feature_selection")) {

      config <- .stage_config(r, stage)

      expect_no_error(.get_method(stage, config$method))

    }

  }

})

# =============================================================================
# Individual methods round-trip
# =============================================================================

test_that("each transformation fits a model that reproduces itself", {

  set.seed(5)

  blk <- data.frame(A = rlnorm(30), B = rlnorm(30, 1, 2),
                    row.names = paste0("S", 1:30))

  for (m in c("identity", "log", "log2", "log10", "sqrt", "cuberoot", "vst",
              "arcsin", "robust", "rank", "quantile", "boxcox",
              "yeojohnson")) {

    entry <- .get_method("transformation", m)

    model <- entry$fit(blk, list(), list())
    once <- entry$apply(blk, model)
    twice <- entry$apply(blk, model)

    expect_equal(once, twice, info = m)
    expect_equal(nrow(once), 30, info = m)

  }

})

test_that("scaling and normalization models transfer to new samples", {

  set.seed(6)

  train <- data.frame(A = rnorm(40, 10), B = rnorm(40, 100),
                      row.names = paste0("S", 1:40))
  new <- train[1:5, , drop = FALSE]

  for (stage in c("scaling", "normalization")) {

    registry <- .method_registry()[[stage]]

    for (m in names(registry)) {

      entry <- .safe_try(.get_method(stage, m), NULL)

      if (is.null(entry)) next

      model <- .safe_try(entry$fit(train, list(), list()), NULL)

      if (is.null(model)) next

      full <- entry$apply(train, model)
      subset <- entry$apply(new, model)

      # The first five rows of the training output must equal the output of
      # the same five rows fed in on their own.
      expect_equal(as.matrix(subset), as.matrix(full[1:5, , drop = FALSE]),
                   tolerance = 1e-8, info = paste(stage, m))

    }

  }

})

test_that("knn imputation fills from the training cohort", {

  set.seed(7)

  train <- data.frame(A = rnorm(30), B = rnorm(30), C = rnorm(30),
                      row.names = paste0("S", 1:30))

  entry <- .get_method("imputation", "knn")
  model <- entry$fit(train, list(k = 5), list())

  new <- data.frame(A = c(1, NA), B = c(NA, 0.5), C = c(0.2, 0.3),
                    row.names = c("N1", "N2"))

  filled <- entry$apply(new, model)

  expect_equal(sum(is.na(filled)), 0)
  expect_equal(nrow(filled), 2)

})
