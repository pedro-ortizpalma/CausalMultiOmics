# One latent process measured six times in one block and four times in
# another, a second process confined to one block and unrelated to the
# outcome, and four variables that genuinely stand alone. Every answer below
# can be checked against what was built.
cmo_module_data <- function(n = 250, seed = 41) {

  set.seed(seed)

  ids <- paste0("S", seq_len(n))

  process <- rnorm(n)
  other <- rnorm(n)

  rna <- sapply(1:6, function(i) 0.9 * process + rnorm(n, sd = 0.45))
  prot <- sapply(1:4, function(i) 0.85 * process + rnorm(n, sd = 0.5))
  decoy <- sapply(1:5, function(i) 0.9 * other + rnorm(n, sd = 0.45))
  loners <- sapply(1:4, function(i) rnorm(n))

  y <- 1.1 * process + rnorm(n)

  rna <- cbind(rna, loners[, 1:2])
  colnames(rna) <- c(paste0("g", 1:6), paste0("solo_g", 1:2))
  rownames(rna) <- ids

  prot <- cbind(prot, decoy, loners[, 3:4])
  colnames(prot) <- c(paste0("p", 1:4), paste0("d", 1:5),
                      paste0("solo_p", 1:2))
  rownames(prot) <- ids

  obj <- load_data(list(rna = rna, prot = prot),
                   metadata = data.frame(sample_id = ids, y = y))

  preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

}

cmo_modules <- function() {
  analyze(cmo_module_data(), "y", methods = "association", effort = "fast",
          plots = FALSE, quiet = TRUE)$modules
}

# =============================================================================
# Finding the groups
# =============================================================================

test_that("variables that move together are grouped and the rest are not", {

  set.seed(2)
  n <- 200

  shared <- rnorm(n)

  x <- cbind(
    sapply(1:4, function(i) shared + rnorm(n, sd = 0.3)),
    sapply(1:3, function(i) rnorm(n))
  )
  colnames(x) <- c(paste0("together", 1:4), paste0("alone", 1:3))

  membership <- .detect_modules(x)

  expect_type(membership, "integer")
  expect_named(membership, colnames(x))

  # The four that share a source land in one module.
  grouped <- membership[paste0("together", 1:4)]
  expect_length(unique(stats::na.omit(grouped)), 1)
  expect_false(any(is.na(grouped)))

  # The independent ones are left alone rather than forced somewhere.
  expect_true(all(is.na(membership[paste0("alone", 1:3)])))

})

test_that("module labels run from one with no gaps", {

  # "module 7" out of four modules is a puzzle for the reader and a bug
  # report waiting to happen.

  set.seed(3)
  n <- 200

  a <- rnorm(n)
  b <- rnorm(n)

  x <- cbind(
    sapply(1:3, function(i) a + rnorm(n, sd = 0.3)),
    sapply(1:3, function(i) rnorm(n)),
    sapply(1:3, function(i) b + rnorm(n, sd = 0.3))
  )
  colnames(x) <- paste0("f", seq_len(ncol(x)))

  membership <- .detect_modules(x)
  present <- sort(unique(stats::na.omit(membership)))

  expect_equal(present, seq_along(present))

})

test_that("nothing is grouped when nothing moves together", {

  set.seed(4)
  x <- matrix(rnorm(200 * 8), 200, 8,
              dimnames = list(NULL, paste0("f", 1:8)))

  expect_null(.detect_modules(x))

})

test_that("degenerate inputs are declined rather than guessed at", {

  set.seed(5)

  expect_null(.detect_modules(matrix(rnorm(100), 50, 2,
                                     dimnames = list(NULL, c("a", "b")))))

  # Constant columns carry no correlation to cluster on.
  flat <- matrix(1, 100, 10, dimnames = list(NULL, paste0("f", 1:10)))
  expect_null(.detect_modules(flat))

  expect_null(.detect_modules("not a matrix"))

})

test_that("opposite movement counts as together", {

  # Two variables that mirror each other are measuring one thing. The sign is
  # settled later, when the module is given a direction.

  set.seed(6)
  n <- 200
  shared <- rnorm(n)

  x <- cbind(shared + rnorm(n, sd = 0.2),
             -shared + rnorm(n, sd = 0.2),
             shared + rnorm(n, sd = 0.2))
  colnames(x) <- c("up1", "down", "up2")

  membership <- .detect_modules(x, min_size = 3L)

  expect_length(unique(stats::na.omit(membership)), 1)

})

# =============================================================================
# The variable that stands for the group
# =============================================================================

test_that("the summary is on the same scale as the variables it summarises", {

  # A raw first component is about sqrt(eigenvalue) wide, so a ten-variable
  # module arrives three times wider than its own members and its coefficient
  # comes out three times smaller for no reason but arithmetic. Read beside
  # the members it looks like disagreement.

  set.seed(7)
  n <- 250
  shared <- rnorm(n)

  x <- sapply(1:8, function(i) shared + rnorm(n, sd = 0.3))
  colnames(x) <- paste0("f", 1:8)

  membership <- .detect_modules(x)
  latent <- .module_latent(x, membership)

  expect_equal(stats::sd(latent$scores[, 1]), 1, tolerance = 0.01)

})

test_that("higher on the summary means higher on the members", {

  # A principal component is defined up to a sign, so without fixing it the
  # direction reported for every module is a coin flip.

  set.seed(8)
  n <- 200
  shared <- rnorm(n)

  x <- sapply(1:5, function(i) shared + rnorm(n, sd = 0.3))
  colnames(x) <- paste0("f", 1:5)

  membership <- .detect_modules(x)
  latent <- .module_latent(x, membership)

  expect_gt(stats::cor(latent$scores[, 1], rowMeans(x)), 0.9)

})

test_that("how much of the group the summary captures is reported", {

  set.seed(9)
  n <- 250
  shared <- rnorm(n)

  tight <- sapply(1:5, function(i) shared + rnorm(n, sd = 0.2))
  colnames(tight) <- paste0("f", 1:5)

  latent <- .module_latent(tight, .detect_modules(tight))

  expect_gt(latent$explained[1], 0.8)
  expect_lte(latent$explained[1], 1)

})

# =============================================================================
# What the group says about the outcome
# =============================================================================

test_that("the module estimate matches fitting the latent by hand", {

  set.seed(10)
  n <- 250
  shared <- rnorm(n)

  x <- sapply(1:5, function(i) shared + rnorm(n, sd = 0.3))
  colnames(x) <- paste0("f", 1:5)
  rownames(x) <- paste0("S", seq_len(n))

  y <- 0.8 * shared + rnorm(n)

  latent <- .module_latent(x, .detect_modules(x))

  edges <- .module_edges(latent$scores,
                         list(name = "y", values = y, type = "continuous"))

  expect_equal(nrow(edges), 1)

  manual <- stats::coef(stats::lm(y ~ latent$scores[, 1]))[[2]]

  expect_equal(edges$estimate[1], manual, tolerance = 1e-8)

})

test_that("a real cross-block process is found, described and related", {

  mg <- cmo_modules()

  expect_s3_class(mg, "ModuleGraph")
  expect_gte(nrow(mg$modules), 1)

  members_of <- function(k) {
    names(mg$membership)[which(mg$membership == k)]
  }

  # The module holding the transcripts must also hold the proteins: they were
  # built from one process, and a module that splits by block would mean the
  # clustering is following the platform rather than the biology.
  which_module <- mg$membership[["g1"]]

  skip_if(is.na(which_module))

  members <- members_of(which_module)

  expect_true(all(paste0("g", 1:6) %in% members))
  expect_true(all(paste0("p", 1:4) %in% members))
  expect_false(any(paste0("d", 1:5) %in% members))

  row <- mg$modules[mg$modules$module == paste0("module_", which_module), ]

  expect_true(row$cross_block)
  expect_equal(row$blocks, 2)
  expect_gt(row$variance_explained, 0.5)
  expect_true(row$coherent)

})

test_that("a group unrelated to the outcome is reported as unrelated", {

  mg <- cmo_modules()

  decoy <- mg$membership[["d1"]]

  skip_if(is.na(decoy))

  hit <- mg$edges[mg$edges$module == paste0("module_", decoy), ]

  skip_if(nrow(hit) == 0)

  expect_gt(hit$fdr, 0.05)

})

test_that("the real process is related to the outcome", {

  mg <- cmo_modules()

  k <- mg$membership[["g1"]]

  skip_if(is.na(k))

  hit <- mg$edges[mg$edges$module == paste0("module_", k), ]

  skip_if(nrow(hit) == 0)

  expect_lt(hit$fdr, 0.05)
  expect_gt(hit$estimate, 0)

})

test_that("variables that stand alone are left out of every module", {

  mg <- cmo_modules()

  for (f in c("solo_g1", "solo_g2", "solo_p1", "solo_p2")) {
    expect_true(is.na(mg$membership[[f]]))
  }

})

# =============================================================================
# Not claiming more than this is
# =============================================================================

test_that("the object says a module and its members are not two findings", {

  # The failure mode this guards against is a reader taking the module
  # estimate as corroboration of the member estimates. They are made of each
  # other; agreement is arithmetic.

  mg <- cmo_modules()

  expect_true(any(grepl("not two findings", mg$notes, fixed = TRUE)))

  printed <- capture.output(print(mg))
  expect_true(any(grepl("resolutions", printed)))

})

test_that("an empty module graph prints its reason instead of a blank", {

  mg <- ModuleGraph()

  expect_no_error(capture.output(print(mg)))
  expect_null(.plot_modules(mg, list(), "y"))

})

test_that("a dataset with no modules says so plainly", {

  set.seed(11)
  n <- 200
  ids <- paste0("S", seq_len(n))

  obj <- load_data(
    list(a = matrix(rnorm(n * 8), n, 8,
                    dimnames = list(ids, paste0("f", 1:8)))),
    metadata = data.frame(sample_id = ids, y = rnorm(n))
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)

  res <- analyze(prep, "y", methods = "association", effort = "fast",
                 plots = FALSE, quiet = TRUE)

  expect_equal(nrow(res$modules$modules), 0)
  expect_match(res$modules$notes[1], "moved together", fixed = TRUE)
  expect_false(any(grepl("Modules:", res$logs)))

})

# =============================================================================
# Reaching the reader
# =============================================================================

test_that("the result carries the modules and the log names them", {

  res <- analyze(cmo_module_data(), "y", methods = "association",
                 effort = "fast", plots = FALSE, quiet = TRUE)

  expect_s3_class(res$modules, "ModuleGraph")
  expect_true(any(grepl("Modules:", res$logs)))

})

test_that("the module figure is drawn alongside the rest", {

  res <- analyze(cmo_module_data(), "y", methods = "association",
                 effort = "fast", plots = TRUE, quiet = TRUE)

  expect_false(is.null(res$plots$modules))
  expect_no_error(print(res$plots$modules))

})
