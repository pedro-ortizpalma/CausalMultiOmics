cmo_structure_df <- function() {
  data.frame(
    from   = c("age", "age", "protein", "inflammation", "disease"),
    to     = c("protein", "disease", "inflammation", "disease", "biomarker"),
    effect = c(0.5, 0.4, 0.8, 0.6, 0.9),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# The object it hands back
# =============================================================================

test_that("what comes out is ready to analyse", {

  sim <- simulate_data(n = 200, blocks = list(a = 4, b = 3))

  expect_s3_class(sim, "MultiOmicsData")
  expect_named(sim$assays, c("a", "b"))
  expect_equal(nrow(sim$assays$a), 200)
  expect_equal(ncol(sim$assays$a), 4)

  expect_true("sample_id" %in% names(sim$metadata))
  expect_equal(nrow(sim$metadata), 200)

  # Straight into the pipeline, with no unwrapping step.
  expect_no_error(check_data(sim))

})

test_that("the truth travels with the data", {

  # The whole reason for this function: a test can assert against what was
  # planted rather than against whatever came out on top.

  sim <- simulate_data(n = 200, blocks = list(blood = c("protein", "biomarker"),
                                              other = 5),
                       dag = cmo_structure_df(), outcome = "disease")

  truth <- sim$misc$simulation

  expect_equal(truth$n, 200)
  expect_equal(truth$outcome, "disease")
  expect_equal(truth$structure, cmo_structure_df())

  expect_setequal(truth$noise_features, paste0("other_0", 1:5))
  expect_setequal(truth$planted_features, c("protein", "biomarker"))

  # Named in the structure and claimed by no block, so it became a metadata
  # column. That is the mechanism by which covariates arrive, and it applies
  # to anything left unplaced: 'inflammation' is a mediator here, and leaving
  # it out of the blocks makes it an unmeasured one.
  expect_setequal(truth$metadata_variables, c("age", "inflammation"))
  expect_true(all(c("age", "inflammation") %in% names(sim$metadata)))

})

test_that("the run is recorded in the history", {

  sim <- simulate_data(n = 50, blocks = list(a = 3), seed = 9)

  expect_true(any(grepl("simulate_data", sim$history)))
  expect_true(any(grepl("seed = 9", sim$history, fixed = TRUE)))

})

# =============================================================================
# The effects are the effects
# =============================================================================

test_that("a regression recovers roughly what was planted", {

  # If the number in the argument is not the number a coefficient returns,
  # every test written against this function is testing the wrong thing.

  sim <- simulate_data(
    n = 2000,
    blocks = list(main = c("x", "m")),
    dag = data.frame(from = c("x", "m"), to = c("m", "y"),
                     effect = c(0.8, 0.5), stringsAsFactors = FALSE),
    outcome = "y", noise = 1
  )

  d <- cbind(as.data.frame(sim$assays$main), y = sim$metadata$y)

  expect_equal(unname(stats::coef(stats::lm(m ~ x, d))[2]), 0.8,
               tolerance = 0.1)
  expect_equal(unname(stats::coef(stats::lm(y ~ m, d))[2]), 0.5,
               tolerance = 0.1)

})

test_that("a variable with no parents is standard normal", {

  sim <- simulate_data(n = 3000, blocks = list(a = 2), seed = 4)

  expect_equal(mean(sim$assays$a[, 1]), 0, tolerance = 0.1)
  expect_equal(stats::sd(sim$assays$a[, 1]), 1, tolerance = 0.1)

})

test_that("features nothing generated are pure noise", {

  sim <- simulate_data(
    n = 1000,
    blocks = list(main = c("driver"), filler = 4),
    dag = data.frame(from = "driver", to = "y", effect = 0.9),
    outcome = "y"
  )

  y <- sim$metadata$y

  expect_gt(abs(stats::cor(sim$assays$main[, "driver"], y)), 0.5)

  for (f in colnames(sim$assays$filler)) {
    expect_lt(abs(stats::cor(sim$assays$filler[, f], y)), 0.15)
  }

})

# =============================================================================
# Kinds of outcome
# =============================================================================

test_that("a binary outcome is binary and not all one class", {

  sim <- simulate_data(n = 400, blocks = list(a = c("x")),
                       dag = data.frame(from = "x", to = "y", effect = 0.9),
                       outcome = "y", outcome_type = "binary")

  expect_setequal(unique(sim$metadata$y), c(0, 1))
  expect_gt(min(table(sim$metadata$y)), 40)

})

test_that("a survival outcome brings a time and a status", {

  sim <- simulate_data(n = 300, blocks = list(a = c("x")),
                       dag = data.frame(from = "x", to = "ev", effect = 0.8),
                       outcome = "ev", outcome_type = "survival")

  expect_true(all(c("ev", "ev_time") %in% names(sim$metadata)))
  expect_true(all(sim$metadata$ev_time > 0))
  expect_setequal(unique(sim$metadata$ev), c(0, 1))

  # Censoring, or it is not follow-up, it is a number called time.
  expect_gt(sum(sim$metadata$ev == 0), 10)

})

# =============================================================================
# Latent processes
# =============================================================================

test_that("a module makes its members move together across blocks", {

  sim <- simulate_data(
    n = 400,
    blocks = list(rna = paste0("g", 1:4), prot = paste0("p", 1:3)),
    modules = list(process = c(paste0("g", 1:4), paste0("p", 1:3)))
  )

  both <- cbind(sim$assays$rna, sim$assays$prot)

  correlation <- stats::cor(both)

  expect_gt(min(correlation[upper.tri(correlation)]), 0.5)

  expect_equal(sim$misc$simulation$modules$process,
               c(paste0("g", 1:4), paste0("p", 1:3)))

  # The latent variable itself is kept, so a test can check that a recovered
  # summary tracks the thing it is supposed to summarise.
  expect_length(sim$misc$simulation$module_latent$process, 400)

})

test_that("features outside a module stay independent of it", {

  sim <- simulate_data(
    n = 400,
    blocks = list(rna = paste0("g", 1:4), alone = 3),
    modules = list(process = paste0("g", 1:4))
  )

  cross <- stats::cor(sim$assays$rna, sim$assays$alone)

  expect_lt(max(abs(cross)), 0.2)

})

# =============================================================================
# The things that make real data hard
# =============================================================================

test_that("missingness is applied at the rate asked for", {

  sim <- simulate_data(n = 200, blocks = list(a = 5, b = 5),
                       missing = c(a = 0.2))

  expect_equal(mean(is.na(sim$assays$a)), 0.2, tolerance = 0.02)
  expect_equal(sum(is.na(sim$assays$b)), 0)

  expect_equal(sim$misc$simulation$cells_blanked$a, 200)

})

test_that("an unnamed block keeps the neutral value, not zero", {

  # Regression: one shared default of zero meant naming coverage for one
  # block silently cut every other block to ten people, because no
  # missingness is 0 and full coverage is 1.

  sim <- simulate_data(n = 300, blocks = list(a = 3, b = 3),
                       coverage = c(b = 0.5))

  expect_equal(nrow(sim$assays$a), 300)
  expect_equal(nrow(sim$assays$b), 150)

})

test_that("coverage below one stops the blocks sharing a population", {

  sim <- simulate_data(n = 400, blocks = list(a = 3, b = 3, c = 3),
                       coverage = c(a = 0.6, b = 0.6, c = 0.6))

  shared <- Reduce(intersect, lapply(sim$assays, rownames))

  expect_lt(length(shared), 400)
  expect_gt(length(shared), 0)

  expect_equal(length(sim$misc$simulation$covered$a), 240)

})

test_that("missingness can be made to depend on the outcome", {

  # The pattern imputation handles worst, and the one that manufactures
  # findings: gaps concentrated where the outcome is high.

  sim <- simulate_data(n = 400, blocks = list(a = 4),
                       dag = data.frame(from = "a_01", to = "y", effect = 0.7),
                       outcome = "y",
                       missing = 0.3, missing_pattern = "by_outcome",
                       seed = 11)

  blank_rate <- rowMeans(is.na(sim$assays$a))
  y <- sim$metadata$y[match(rownames(sim$assays$a), sim$metadata$sample_id)]

  expect_gt(stats::cor(blank_rate, y), 0.1)

  expect_equal(sim$misc$simulation$missing_pattern, "by_outcome")

})

test_that("batches shift the blocks and are recorded", {

  sim <- simulate_data(n = 300, blocks = list(a = 4), batch = 3,
                       batch_effect = 2)

  expect_true("batch" %in% names(sim$metadata))
  expect_length(unique(sim$metadata$batch), 3)

  means <- tapply(sim$assays$a[, 1], sim$metadata$batch, mean)

  expect_gt(diff(range(means)), 0.5)

  expect_length(sim$misc$simulation$batch, 300)

})

# =============================================================================
# Reproducibility
# =============================================================================

test_that("the same seed gives the same study", {

  a <- simulate_data(n = 100, blocks = list(x = 4), seed = 7)
  b <- simulate_data(n = 100, blocks = list(x = 4), seed = 7)

  expect_equal(a$assays$x, b$assays$x)

})

test_that("a different seed gives a different study", {

  a <- simulate_data(n = 100, blocks = list(x = 4), seed = 7)
  b <- simulate_data(n = 100, blocks = list(x = 4), seed = 8)

  expect_false(isTRUE(all.equal(a$assays$x, b$assays$x)))

})

test_that("the caller's random number generator is left alone", {

  # An example in a vignette that moved the reader's generator would change
  # every simulation they ran afterwards.

  set.seed(123)
  before <- .Random.seed

  invisible(simulate_data(n = 50, blocks = list(a = 3), seed = 999))

  expect_identical(.Random.seed, before)

})

# =============================================================================
# Refusals
# =============================================================================

test_that("it refuses what it cannot generate", {

  expect_error(simulate_data(n = 5), "at least 20")
  expect_error(simulate_data(blocks = list()), "named list")
  expect_error(simulate_data(blocks = list(3)), "named list")

  expect_error(simulate_data(dag = data.frame(a = 1, b = 2)),
               "'from' and 'to'")

  expect_error(
    simulate_data(dag = data.frame(from = "a", to = "b", effect = NA)),
    "finite number")

  expect_error(simulate_data(blocks = list(a = c("x", "y"), b = "x")),
               "unique across blocks")

  expect_error(simulate_data(coverage = c(nope = 0.5)),
               "blocks that do not exist")

  expect_error(simulate_data(modules = list(c("a", "b"))), "named list")

})

test_that("a cycle is refused rather than looped over", {

  # Generating a variable needs its parents to exist first. A cycle has no
  # such order, and without the guard the loop never terminates.

  cyclic <- data.frame(from = c("a", "b", "c"), to = c("b", "c", "a"),
                       stringsAsFactors = FALSE)

  expect_error(simulate_data(dag = cyclic), "contains a cycle")

})

# =============================================================================
# It has to survive the pipeline it exists to test
# =============================================================================

test_that("a planted driver is recovered by analyze()", {

  sim <- simulate_data(
    n = 300,
    blocks = list(main = c("driver", "passenger"), noise_block = 4),
    dag = data.frame(from = "driver", to = "y", effect = 0.9),
    outcome = "y"
  )

  prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)

  res <- analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association"))

  scores <- stats::setNames(
    vapply(res$evidence, function(e) e$evidence_score, numeric(1)),
    vapply(res$evidence, function(e) e$source, character(1)))

  expect_true("driver" %in% names(scores))
  expect_equal(names(which.max(scores)), "driver")

  # And the planted noise does not outrank it.
  planted_noise <- intersect(sim$misc$simulation$noise_features, names(scores))

  if (length(planted_noise) > 0) {
    expect_gt(scores[["driver"]], max(scores[planted_noise]))
  }

})

test_that("a study with gaps and batches still runs end to end", {

  sim <- simulate_data(
    n = 300,
    blocks = list(a = c("x", "z"), b = 4),
    dag = data.frame(from = "x", to = "y", effect = 0.8),
    outcome = "y", missing = 0.15, batch = 2, coverage = c(b = 0.8)
  )

  expect_no_error({
    prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)
    analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association"))
  })

})
