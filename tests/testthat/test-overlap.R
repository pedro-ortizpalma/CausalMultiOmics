cmo_ids <- function(n = 200) paste0("S", seq_len(n))

cmo_block <- function(ids, p = 4) {
  matrix(rnorm(length(ids) * p), length(ids), p,
         dimnames = list(ids, paste0("f", seq_len(p))))
}

# =============================================================================
# The number the pairwise matrix cannot show
# =============================================================================

test_that("every pair can be complete while the whole set is empty", {

  # Eight blocks, each missing a different 25 people. Every pair shares 150;
  # all eight together share none. This is the case the overlap matrix is
  # silent about and the one that stops an analysis dead.

  set.seed(3)
  ids <- cmo_ids()

  blocks <- lapply(seq_len(8), function(i) {
    cmo_block(setdiff(ids, ids[((i - 1) * 25 + 1):(i * 25)]))
  })
  names(blocks) <- paste0("b", seq_len(8))

  cum <- .cumulative_overlap(blocks)

  expect_equal(nrow(cum), 8)
  expect_equal(cum$shared_after[8], 0)

  # Monotone by construction: adding a block can only remove samples.
  expect_false(is.unsorted(rev(cum$shared_after)))

  expect_equal(cum$shared_after[1] - cum$shared_after[8], sum(cum$lost))

})

test_that("a person counts only where something was measured on them", {

  # A module administered to ninety people, assembled against the full sample
  # list, carries a row for everyone with the rest left empty. Counting row
  # names calls it complete, reports the whole cohort as shared, and the
  # collapse then surfaces only after preprocessing drops those rows - which
  # is far too late for a check whose job is to run first.

  set.seed(12)
  ids <- cmo_ids()

  full <- cmo_block(ids)

  padded <- cmo_block(ids)
  padded[51:200, ] <- NA          # measured on fifty of the two hundred

  cum <- .cumulative_overlap(list(full = full, padded = padded))

  expect_equal(cum$samples[cum$block == "padded"], 50)
  expect_equal(cum$shared_after[nrow(cum)], 50)

  obj <- load_data(list(full = full, padded = padded),
                   metadata = data.frame(sample_id = ids, y = rnorm(200)))

  v <- check_data(obj)

  expect_equal(v$summary$shared_by_all, 50)

  # The pairwise matrix has to agree, or the two numbers beside each other in
  # the report would contradict.
  expect_equal(v$summary$overlap["full", "padded"], 50)
  expect_equal(v$summary$overlap["padded", "padded"], 50)

})

test_that("the ladder is ordered largest block first", {

  set.seed(4)
  ids <- cmo_ids()

  blocks <- list(small = cmo_block(ids[1:50]),
                 big = cmo_block(ids),
                 middling = cmo_block(ids[1:120]))

  cum <- .cumulative_overlap(blocks)

  expect_equal(cum$block, c("big", "middling", "small"))
  expect_false(is.unsorted(rev(cum$samples)))

})

test_that("an empty input does not break the ladder", {

  expect_equal(nrow(.cumulative_overlap(list())), 0)

})

test_that("check_data() reports what every block has in common", {

  set.seed(5)
  ids <- cmo_ids()

  obj <- load_data(list(a = cmo_block(ids), b = cmo_block(ids)),
                   metadata = data.frame(sample_id = ids, y = rnorm(200)))

  v <- check_data(obj)

  expect_equal(v$summary$shared_by_all, 200)
  expect_s3_class(v$summary$cumulative_overlap, "data.frame")

  # A dataset with nothing wrong stays quiet about it.
  expect_false(any(grepl("in every block", c(v$errors, v$warnings))))

})

test_that("check_data() warns when pairs look complete and the set does not", {

  set.seed(6)
  ids <- cmo_ids()

  blocks <- lapply(seq_len(5), function(i) {
    cmo_block(setdiff(ids, ids[((i - 1) * 20 + 1):(i * 20)]))
  })
  names(blocks) <- paste0("b", seq_len(5))

  obj <- load_data(blocks,
                   metadata = data.frame(sample_id = ids, y = rnorm(200)))

  v <- check_data(obj)

  expect_lt(v$summary$shared_by_all, 200)
  expect_true(any(grepl("in every block at once", v$warnings)))

})

test_that("blame is only assigned when one block deserves it", {

  set.seed(7)
  ids <- cmo_ids()

  # One block holds six people; the rest hold everyone.
  obj <- load_data(
    list(a = cmo_block(ids), b = cmo_block(ids), odd = cmo_block(ids[1:6])),
    metadata = data.frame(sample_id = ids, y = rnorm(200))
  )

  v <- check_data(obj)

  expect_true(any(grepl("'odd'", v$warnings, fixed = TRUE)))

})

test_that("no culprit is invented when the loss is spread evenly", {

  # Every block costs exactly the same. Naming the first three would be
  # picking names out of a tie and sending the reader after the wrong thing.

  set.seed(8)
  ids <- cmo_ids()

  blocks <- lapply(seq_len(10), function(i) {
    cmo_block(setdiff(ids, ids[((i - 1) * 20 + 1):(i * 20)]))
  })
  names(blocks) <- paste0("b", seq_len(10))

  obj <- load_data(blocks,
                   metadata = data.frame(sample_id = ids, y = rnorm(200)))

  v <- check_data(obj)

  expect_true(any(grepl("spread across", v$warnings)))
  expect_false(any(grepl("Most of the loss", v$warnings)))

})

# =============================================================================
# What analyze() says when it cannot proceed
# =============================================================================

test_that("the error names the block to drop", {

  set.seed(9)
  ids <- cmo_ids()

  id_sets <- list(a = ids, b = ids, odd = ids[1:6])

  lines <- .explain_no_overlap(id_sets, min_samples = 10)
  text <- paste(lines, collapse = "\n")

  expect_match(text, "Samples in each block", fixed = TRUE)
  expect_match(text, "without odd", fixed = TRUE)

  # Dropping the odd one out restores the full cohort.
  expect_match(text, "300|200", perl = TRUE)

})

test_that("identifiers written differently are diagnosed as such", {

  # The failure people actually hit: two blocks describing the same cohort
  # under different naming conventions. Reporting "0 shared samples" sends
  # them looking for a data problem that does not exist.

  id_sets <- list(clinical = paste0("S", 1:100),
                  omics = paste0("SUBJ-", 1:100))

  text <- paste(.explain_no_overlap(id_sets, 10), collapse = "\n")

  expect_match(text, "share no identifier at all", fixed = TRUE)
  expect_match(text, "naming difference", fixed = TRUE)

  # Examples from both sides, so the difference is visible rather than
  # described.
  expect_match(text, "S1, S2, S3", fixed = TRUE)
  expect_match(text, "SUBJ-1, SUBJ-2, SUBJ-3", fixed = TRUE)

})

test_that("a spread-out shortfall is explained as a ladder", {

  ids <- cmo_ids()

  id_sets <- lapply(seq_len(8), function(i)
    setdiff(ids, ids[((i - 1) * 25 + 1):(i * 25)]))
  names(id_sets) <- paste0("b", seq_len(8))

  text <- paste(.explain_no_overlap(id_sets, 60), collapse = "\n")

  expect_match(text, "No single block is responsible", fixed = TRUE)
  expect_match(text, "largest first", fixed = TRUE)

})

test_that("a long list of options is capped rather than dumped", {

  ids <- cmo_ids()

  id_sets <- lapply(seq_len(12), function(i)
    setdiff(ids, ids[((i - 1) * 16 + 1):(i * 16)]))
  names(id_sets) <- paste0("b", seq_len(12))

  text <- paste(.explain_no_overlap(id_sets, 10), collapse = "\n")

  skip_if_not(grepl("Dropping any one", text, fixed = TRUE))

  expect_match(text, "other block\\(s\\) with the same effect")

})

test_that("preprocessing is named when it is what broke the intersection", {

  # The inconsistency this exists for: check_data() reports a healthy overlap
  # on the raw blocks, preprocessing drops samples, and analyze() then
  # complains about a cohort that looked fine an hour ago.

  ids <- cmo_ids()

  original <- list(a = cmo_block(ids), b = cmo_block(ids))

  # After preprocessing the two blocks kept disjoint halves.
  id_sets <- list(a = ids[1:100], b = ids[101:200])

  text <- paste(.explain_no_overlap(id_sets, 10, original = original),
                collapse = "\n")

  expect_match(text, "Before preprocessing these blocks shared 200",
               fixed = TRUE)
  expect_match(text, "Samples were dropped from", fixed = TRUE)

  # And it must not also accuse the identifiers, which are fine.
  expect_false(grepl("naming difference", text, fixed = TRUE))

})

test_that("preprocessing is not blamed when the raw data was already broken", {

  ids <- cmo_ids()

  original <- list(a = cmo_block(ids[1:100]), b = cmo_block(ids[101:200]))
  id_sets <- list(a = ids[1:100], b = ids[101:200])

  text <- paste(.explain_no_overlap(id_sets, 10, original = original),
                collapse = "\n")

  expect_false(grepl("Before preprocessing", text, fixed = TRUE))

})

test_that("analyze() raises the explained error end to end", {

  set.seed(10)
  ids <- cmo_ids()

  obj <- load_data(
    list(a = cmo_block(ids), odd = cmo_block(ids[1:6])),
    metadata = data.frame(sample_id = ids, y = rnorm(200))
  )

  prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE,
                     force = TRUE)

  expect_error(
    analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE, 
    control = analysis_control(methods = "association")),
    "Samples in each block"
  )

})
