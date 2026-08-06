cmo_units <- function(n = 120, seed = 7) {

  set.seed(seed)

  # Deliberately realistic units, so a contrast has to survive the round trip
  # through preprocessing to come back as mg/dL rather than z-scores.
  apoa1 <- rnorm(n, 130, 20)
  scfa <- 0.05 * apoa1 + rnorm(n, 5, 1)
  genotype <- rbinom(n, 2, 0.3)

  risk <- -0.06 * (apoa1 - 130) - 0.5 * (scfa - 11) + rnorm(n, 0, 1)
  ids <- paste0("P", seq_len(n))

  obj <- load_data(
    list(
      blood = data.frame(APOA1 = apoa1, CRP = rnorm(n, 3, 1), row.names = ids),
      genetics = data.frame(APOE4 = genotype, row.names = ids)
    ),
    metadata = data.frame(
      sample_id = ids,
      event = rbinom(n, 1, stats::plogis(risk)),
      fu = rexp(n, 0.05) + 1,
      cont = risk,
      age = rnorm(n, 60, 8),
      stringsAsFactors = FALSE
    )
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

test_that("counterfactual() reports contrasts in the original units", {

  prep <- cmo_units()
  res <- analyze(prep, "event", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = "blood")

  expect_s3_class(cf, "CMOCounterfactual")
  expect_gt(nrow(cf$table), 0)

  apoa1 <- cf$table[cf$table$variable == "APOA1", ]

  skip_if(nrow(apoa1) == 0)

  # The contrast has to be anchored in mg/dL, near the observed median of 130,
  # not on the standardised scale the model actually worked in.
  expect_gt(apoa1$from, 100)
  expect_lt(apoa1$from, 160)
  expect_gt(abs(apoa1$change), 5)

})

test_that("the contrast grows with the size of the change asked for", {

  prep <- cmo_units()
  res <- analyze(prep, "cont", effort = "fast", plots = FALSE, quiet = TRUE)

  small <- counterfactual(res, modifiable = "blood", change = c(APOA1 = 5))
  large <- counterfactual(res, modifiable = "blood", change = c(APOA1 = 20))

  a <- unname(small$table$value[small$table$variable == "APOA1"])
  b <- unname(large$table$value[large$table$variable == "APOA1"])

  skip_if(length(a) == 0 || length(b) == 0)

  expect_equal(sign(a), sign(b))
  expect_gt(abs(b), abs(a))

  # Deliberately NOT four times: the block went through a non-linear
  # transformation during preprocessing, so a move of twenty units is not
  # four moves of five. That is why every contrast is anchored at a stated
  # reference value instead of being quoted per unit.
  expect_false(isTRUE(all.equal(b / a, 4, tolerance = 0.01)))

})

test_that("increasing and decreasing point opposite ways", {

  prep <- cmo_units()
  res <- analyze(prep, "cont", effort = "fast", plots = FALSE, quiet = TRUE)

  up <- counterfactual(res, modifiable = "blood", direction = "increase")
  down <- counterfactual(res, modifiable = "blood", direction = "decrease")

  a <- unname(up$table$value[up$table$variable == "APOA1"])
  b <- unname(down$table$value[down$table$variable == "APOA1"])

  skip_if(length(a) == 0 || length(b) == 0)

  # Opposite signs, but not equal magnitudes: the transformation is not
  # symmetric around the reference point.
  expect_equal(sign(a), -sign(b))

})

test_that("a linear pipeline does give a proportional contrast", {

  # The counterpart to the two tests above: when nothing non-linear happens
  # to the variable, the contrast is exactly proportional. This is what
  # isolates the non-proportionality to preprocessing rather than to a fault
  # in the arithmetic.

  set.seed(21)

  n <- 100
  x <- rnorm(n, 50, 10)
  ids <- paste0("S", seq_len(n))

  obj <- load_data(
    list(b = data.frame(V1 = x, V2 = rnorm(n), V3 = rnorm(n), row.names = ids)),
    metadata = data.frame(sample_id = ids, y = 0.3 * x + rnorm(n),
                          stringsAsFactors = FALSE)
  )

  recipes <- check_data(obj)

  # Strip every stage that could bend the scale.
  recipes$recipes$b$transformation <- "identity"
  recipes$recipes$b$normalization <- "none"
  recipes$recipes$b$scaling <- "none"

  prep <- preprocess(obj, recipes, plots = FALSE, quiet = TRUE)
  res <- analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE)

  small <- counterfactual(res, modifiable = "b", change = c(V1 = 2))
  large <- counterfactual(res, modifiable = "b", change = c(V1 = 8))

  a <- unname(small$table$value[small$table$variable == "V1"])
  b <- unname(large$table$value[large$table$variable == "V1"])

  skip_if(length(a) == 0 || length(b) == 0)

  # Proportional to within floating-point noise accumulated through the
  # pipeline. The non-linear case above lands near 4.7, so this tolerance is
  # nowhere near loose enough to confuse the two.
  expect_equal(b / a, 4, tolerance = 1e-3)

})

# =============================================================================
# The part that stops a contrast being read as an intervention
# =============================================================================

test_that("nothing is included unless declared modifiable", {

  prep <- cmo_units()
  res <- analyze(prep, "event", effort = "fast", plots = FALSE, quiet = TRUE)

  expect_error(counterfactual(res), "must name the variables")
  expect_error(counterfactual(res), "genotype")

})

test_that("a block left out of modifiable never appears", {

  prep <- cmo_units()
  res <- analyze(prep, "event", effort = "fast", plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = "blood")

  expect_false("APOE4" %in% cf$table$variable)
  expect_false("APOE4" %in% cf$considered)

  # And it does appear once it is declared, so the exclusion is the
  # declaration doing its job rather than the variable being unreachable.
  both <- counterfactual(res, modifiable = c("blood", "genetics"))
  expect_true("APOE4" %in% both$considered)

})

test_that("the wording never promises an intervention without support", {

  prep <- cmo_units()
  res <- analyze(prep, "event", covariates = "age", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = "blood")

  skip_if(length(cf$statements) == 0)

  for (s in cf$statements) {

    if (s$identification %in% c("none", "adjustment")) {

      expect_match(s$sentence, "is associated with", fixed = TRUE)
      expect_false(grepl("would reduce|would lower|will reduce", s$sentence))
      expect_match(s$caveat, "not a prediction of what would happen",
                   fixed = TRUE)

    }

  }

})

test_that("identification can be required", {

  prep <- cmo_units()
  res <- analyze(prep, "event", effort = "fast", plots = FALSE, quiet = TRUE)

  # Cross-sectional: nothing reaches temporal, so requiring it empties the
  # table rather than quietly relaxing the requirement.
  strict <- counterfactual(res, modifiable = "blood",
                           min_identification = "temporal")

  expect_equal(nrow(strict$table), 0)
  expect_true(length(strict$skipped) > 0)
  expect_true(any(grepl("identification", strict$skipped)))

})

# =============================================================================
# Outcome types
# =============================================================================

test_that("a binary outcome gives an odds ratio against a stated baseline", {

  prep <- cmo_units()
  res <- analyze(prep, "event", effort = "fast", plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = "blood")

  skip_if(nrow(cf$table) == 0)

  expect_true(all(cf$table$measure == "odds ratio"))
  expect_true(is.finite(cf$baseline_risk))
  expect_gt(cf$baseline_risk, 0)
  expect_lt(cf$baseline_risk, 1)

})

test_that("a survival design gives a hazard ratio and claims temporality", {

  prep <- cmo_units()
  res <- analyze(prep, "event", time = "fu", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = "blood")

  skip_if(nrow(cf$table) == 0)

  expect_true(all(cf$table$measure == "hazard ratio"))
  expect_true(any(cf$table$identification == "temporal"))

})

test_that("a continuous outcome gives a difference in its own units", {

  prep <- cmo_units()
  res <- analyze(prep, "cont", effort = "fast", plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = "blood")

  skip_if(nrow(cf$table) == 0)

  expect_true(all(cf$table$measure == "difference in the outcome"))
  expect_true(all(is.na(cf$table$percent_change)))

})

test_that("counterfactual() validates its input", {

  prep <- cmo_units()
  res <- analyze(prep, "event", effort = "fast", plots = FALSE, quiet = TRUE)

  expect_error(counterfactual(1, modifiable = "blood"), "must be a CMOResult")
  expect_error(counterfactual(res, modifiable = "nonsense"),
               "is a variable or block")

})

test_that("it prints without error, including when empty", {

  prep <- cmo_units()
  res <- analyze(prep, "event", effort = "fast", plots = FALSE, quiet = TRUE)

  expect_no_error(capture.output(print(counterfactual(res, modifiable = "blood"))))

  empty <- counterfactual(res, modifiable = "blood",
                          min_identification = "instrument")

  out <- capture.output(print(empty))
  expect_true(any(grepl("No variable qualified", out)))

})

# =============================================================================
# The bug this feature uncovered
# =============================================================================

test_that("a low-cardinality block is not mistaken for duplicated samples", {

  # Regression: a single genotype column taking three values made almost
  # every row an exact duplicate of another, so preprocessing removed 137 of
  # 140 samples and the analysis had nothing left to align.

  set.seed(3)

  n <- 100
  ids <- paste0("S", seq_len(n))

  obj <- load_data(
    list(genetics = data.frame(SNP = rbinom(n, 2, 0.3), row.names = ids)),
    metadata = data.frame(sample_id = ids, y = rnorm(n),
                          stringsAsFactors = FALSE)
  )

  v <- check_data(obj)

  expect_length(v$diagnostics$genetics$duplicated_samples, 0)

  prep <- preprocess(obj, v, plots = FALSE, quiet = TRUE)

  expect_equal(nrow(prep$data$assays$genetics), n)

})

test_that("genuine duplicated samples are still caught", {

  set.seed(4)

  n <- 40
  m <- matrix(rnorm(n * 6), nrow = n)
  m[2, ] <- m[1, ]                       # one real re-measurement

  block <- as.data.frame(m)
  rownames(block) <- paste0("S", seq_len(n))

  expect_equal(.duplicated_samples(block), "S2")

})
