cmo_categorical <- function(n = 160, seed = 11) {

  set.seed(seed)

  ids <- paste0("P", seq_len(n))

  sex <- sample(c("F", "M"), n, TRUE)
  diet <- sample(c("mediterranean", "western", "vegetarian"), n, TRUE,
                 prob = c(0.45, 0.35, 0.20))
  rare <- sample(c("a", "b", "c", "d"), n, TRUE,
                 prob = c(0.9, 0.05, 0.03, 0.02))

  apoa1 <- rnorm(n, 130, 20)

  y <- 0.04 * (apoa1 - 130) + 1.2 * (sex == "M") -
    0.9 * (diet == "western") + rnorm(n, 0, 1)

  obj <- load_data(
    list(
      blood = data.frame(APOA1 = apoa1, CRP = rnorm(n, 3, 1), row.names = ids),
      lifestyle = data.frame(sex = sex, diet = diet, rare = rare,
                             barcode = paste0("lab_", seq_len(n)),
                             row.names = ids, stringsAsFactors = FALSE)
    ),
    metadata = data.frame(
      sample_id = ids, HDL = y,
      event = rbinom(n, 1, stats::plogis(y)),
      colour = sample(c("red", "green", "blue", "yellow"), n, TRUE),
      age = rnorm(n, 60, 8), stringsAsFactors = FALSE
    )
  )

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

test_that("factors survive loading and preprocessing untouched", {

  prep <- cmo_categorical()

  kept <- names(prep$data$assays$lifestyle)

  expect_true(all(c("sex", "diet") %in% kept))
  expect_type(prep$data$assays$lifestyle$sex, "character")

})

test_that("analyze() encodes categorical columns instead of dropping them", {

  # Regression: .align_blocks() kept only numeric columns, so a whole block
  # of categories vanished from the analysis without anyone being told.

  res <- analyze(cmo_categorical(), "HDL", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  expect_true(length(res$data$encoding) > 0)
  expect_true("sex=M" %in% colnames(res$data$x))

  encoded <- res$data$encoding[["sex=M"]]

  expect_equal(encoded$variable, "sex")
  expect_equal(encoded$level, "M")
  expect_true(encoded$reference %in% c("F", "M"))
  expect_false(identical(encoded$level, encoded$reference))

})

test_that("a k-level factor becomes k-1 comparisons against one reference", {

  res <- analyze(cmo_categorical(), "HDL", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  diet <- Filter(function(e) identical(e$variable, "diet"), res$data$encoding)

  expect_length(diet, 2)

  references <- unique(vapply(diet, function(e) e$reference, character(1)))

  expect_length(references, 1)
  expect_equal(references, "mediterranean")

})

test_that("the commonest level is used as the reference", {

  # Comparing against a rare category estimates every contrast from a handful
  # of people, so the reference is the level with the most observations.

  res <- analyze(cmo_categorical(), "HDL", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  for (e in res$data$encoding) {
    expect_gte(e$n_reference, e$n_level)
  }

})

test_that("identifier-like and too-rare categories are left out, with a reason", {

  res <- analyze(cmo_categorical(), "HDL", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  encoded_from <- vapply(res$data$encoding, function(e) e$variable,
                         character(1))

  # 160 distinct values: a barcode, not a variable.
  expect_false("barcode" %in% encoded_from)
  expect_true(any(grepl("identifier", res$logs)))

  # Levels with fewer than five observations.
  expect_false("rare" %in% encoded_from)
  expect_true(any(grepl("fewer than", res$logs)))

})

test_that("encoded features take part in the analysis like any other", {

  res <- analyze(cmo_categorical(), "HDL", covariates = "age",
                 effort = "fast", plots = FALSE, quiet = TRUE)

  sources <- unique(c(res$graph$edges$source, res$graph$edges$target))

  expect_true(any(grepl("^sex=|^diet=", sources)))

  # And they are scored by the same machinery, not special-cased.
  encoded_edges <- Filter(function(e) grepl("^sex=|^diet=", e$source),
                          res$evidence)

  skip_if(length(encoded_edges) == 0)

  for (e in encoded_edges) {
    expect_true(is.finite(e$evidence_score))
    expect_true(e$identification %in%
                  c("none", "adjustment", "temporal", "instrument"))
  }

})

# =============================================================================
# Wording
# =============================================================================

test_that("a category comparison is not described as being higher or lower", {

  # "Higher sex=M goes together with higher HDL" is neither English nor true:
  # the column is a yes/no marker for membership of a category.

  encoding <- list(variable = "sex", level = "M", reference = "F")

  sentence <- .result_plain_direction("positive", "sex=M", "HDL", encoding)

  expect_match(sentence, "Being M rather than F", fixed = TRUE)
  expect_false(grepl("Higher sex", sentence, fixed = TRUE))

  negative <- .result_plain_direction("negative", "sex=M", "HDL", encoding)
  expect_match(negative, "lower HDL", fixed = TRUE)

  # A plain measurement still reads the old way.
  expect_match(.result_plain_direction("positive", "APOA1", "HDL"),
               "Higher APOA1", fixed = TRUE)

})

test_that("explain() knows a feature came from a category", {

  res <- analyze(cmo_categorical(), "HDL", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  present <- intersect(names(res$data$encoding),
                       c(res$graph$edges$source, res$graph$edges$target))

  skip_if(length(present) == 0)

  account <- explain(res, present[1])

  expect_false(is.null(account$encoding))
  expect_no_error(capture.output(print(account)))

})

# =============================================================================
# Counterfactual
# =============================================================================

test_that("a category contrast is expressed as membership, not as units", {

  res <- analyze(cmo_categorical(), "HDL", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = "diet")

  skip_if(nrow(cf$table) == 0)

  expect_true(all(cf$table$variable == "diet"))
  expect_true(all(cf$table$from == "mediterranean"))
  expect_true(any(grepl("rather than",
                        vapply(cf$statements, function(s) s$sentence,
                               character(1)))))

  # No amount of standard deviations makes sense for a category.
  expect_true(all(grepl("category", cf$table$change)))

})

test_that("naming the original variable reaches its encoded columns", {

  # The user should not have to know that "diet" became "diet=western".

  res <- analyze(cmo_categorical(), "HDL", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  by_name <- counterfactual(res, modifiable = "diet")
  by_block <- counterfactual(res, modifiable = "lifestyle")

  expect_true(all(c("diet=western", "diet=vegetarian") %in% by_name$considered))
  expect_true(all(by_name$considered %in% by_block$considered))

})

test_that("a binary outcome gives an odds ratio for a category too", {

  res <- analyze(cmo_categorical(), "event", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  cf <- counterfactual(res, modifiable = c("sex", "diet"))

  skip_if(nrow(cf$table) == 0)

  expect_true(all(cf$table$measure == "odds ratio"))
  expect_true(all(is.finite(cf$table$percent_change)))

})

# =============================================================================
# The outcome side
# =============================================================================

test_that("an unordered outcome with more than two categories is refused", {

  # Coding red/green/blue as 1/2/3 lets every model run and produce results
  # that look ordinary and assert that blue is three times red.

  prep <- cmo_categorical()

  expect_error(analyze(prep, "colour", quiet = TRUE), "unordered categories")
  expect_error(analyze(prep, "colour", quiet = TRUE), "Recode it")

})

test_that("a two-category outcome is still accepted as binary", {

  prep <- cmo_categorical()

  res <- analyze(prep, "event", effort = "fast", plots = FALSE, quiet = TRUE)

  expect_equal(res$outcome$type, "binary")
  expect_setequal(unique(res$outcome$values), c(0, 1))

})

# =============================================================================
# The encoder on its own
# =============================================================================

test_that(".encode_categorical() handles the awkward cases", {

  set.seed(2)

  block <- data.frame(
    constant = rep("only", 50),
    binary = rep(c("yes", "no"), 25),
    tiny = c(rep("common", 48), "x", "y"),
    numeric = rnorm(50),
    stringsAsFactors = FALSE
  )

  out <- .encode_categorical(block)

  expect_false(any(grepl("^constant", names(out$columns))))
  expect_true(any(grepl("^binary", names(out$columns))))
  expect_false(any(grepl("^tiny", names(out$columns))))
  expect_false(any(grepl("^numeric", names(out$columns))))

  expect_true(any(grepl("single value", out$notes)))
  expect_true(any(grepl("fewer than", out$notes)))

  # The indicator is 0/1 and keeps missing values missing.
  values <- out$columns[[grep("^binary", names(out$columns))[1]]]
  expect_setequal(unique(values), c(0, 1))

})

test_that("missing values in a category stay missing", {

  block <- data.frame(
    g = c(rep("a", 20), rep("b", 20), rep(NA, 10)),
    stringsAsFactors = FALSE
  )

  out <- .encode_categorical(block)

  column <- out$columns[[1]]

  expect_equal(sum(is.na(column)), 10)

})
