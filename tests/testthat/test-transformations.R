test_that("sqrt and vst propagate missing values instead of zeroing them", {

  # Regression: pmax(x, 0, na.rm = TRUE) does not skip NA, it replaces it
  # with 0, so every missing value was silently imputed as zero before the
  # transformation benchmark scored the block.

  expect_equal(.t_sqrt(c(1, NA, 4)), c(1, NA, 2))
  expect_true(is.na(.t_vst(c(1, NA, 4))[2]))

  blk <- data.frame(A = c(1, NA, 3, 4, 5), row.names = paste0("S", 1:5))

  expect_true(is.na(.apply_transformation(blk, "sqrt")$block$A[2]))
  expect_true(is.na(.apply_transformation(blk, "vst")$block$A[2]))

  # Negatives are still floored, which is what pmax() was there for.
  expect_equal(.t_sqrt(c(-4, 0, 9)), c(0, 0, 3))

})

test_that("order preservation is not reported across mismatched pools", {

  # Regression: alr/ilr drop a column, so the before/after pools have
  # different lengths and comparing them by position correlated unrelated
  # cells while reporting a confident number.

  comp <- cmo_compositional(n = 30, p = 4)

  before <- .pool_numeric(comp)
  after <- .pool_numeric(.apply_transformation(comp, "alr")$block)

  expect_false(length(before) == length(after))
  expect_true(is.na(.transformation_metrics(before, after)$order_preservation))

  # Shape-preserving transforms still get a real number.
  same <- .pool_numeric(.apply_transformation(comp, "clr")$block)
  expect_equal(length(before), length(same))
  expect_false(is.na(.transformation_metrics(before, same)$order_preservation))

})

test_that("a missing order preservation does not break scoring", {

  metrics <- list(skewness = 0, kurtosis = 0, variance = 1, outliers_pct = 0,
                  normality_p = 0.9, stability = 0.9,
                  order_preservation = NA_real_)

  score <- .composite_score(metrics)

  expect_true(is.finite(score))
  expect_gte(score, 0)
  expect_lte(score, 100)

})

test_that("constant columns do not decide the type of a block", {

  # Regression: a column of 1s reads as "proportion" and a column of 7s as
  # "count", which dragged an otherwise continuous block to "mixed" and cut
  # its transformation battery down to the generic candidates.

  set.seed(1)
  blk <- data.frame(x = rnorm(20), y = rnorm(20), row.names = paste0("S", 1:20))

  expect_equal(.detect_data_type(blk), "continuous")

  blk$k <- 1
  expect_equal(.detect_data_type(blk), "continuous")

  blk$k <- 7
  expect_equal(.detect_data_type(blk), "continuous")

  expect_true("boxcox" %in% .candidate_transformations(.detect_data_type(blk)))

})

test_that("genuinely heterogeneous blocks are still mixed", {

  # Unanimity among the informative columns is the point: resolving this by
  # majority would hand a negative-valued column to a log-like transform.

  set.seed(2)

  blk <- data.frame(
    a = rnorm(20),
    b = rpois(20, 5),
    row.names = paste0("S", 1:20)
  )

  expect_equal(.detect_data_type(blk), "mixed")

  chr <- data.frame(a = rnorm(10), b = letters[1:10],
                    row.names = paste0("S", 1:10),
                    stringsAsFactors = FALSE)

  expect_equal(.detect_data_type(chr), "mixed")

})

test_that("compositional data is detected and counts are not", {

  expect_equal(.detect_data_type(cmo_compositional()), "compositional")

  for (p in c(5, 50, 500)) {

    set.seed(p)
    cnt <- as.data.frame(matrix(rpois(40 * p, 12), nrow = 40))
    rownames(cnt) <- paste0("S", 1:40)

    expect_equal(.detect_data_type(cnt), "count")

  }

})

test_that("every candidate transformation returns a well-formed block", {

  set.seed(3)

  blk <- data.frame(A = rlnorm(30), B = rlnorm(30, 1, 2),
                    row.names = paste0("S", 1:30))

  for (m in c("identity", "log", "log2", "log10", "sqrt", "cuberoot", "vst",
              "rank", "quantile", "robust", "boxcox", "yeojohnson")) {

    res <- .apply_transformation(blk, m)

    expect_s3_class(res$block, "data.frame")
    expect_equal(nrow(res$block), 30)
    expect_equal(rownames(res$block), rownames(blk))

  }

})
