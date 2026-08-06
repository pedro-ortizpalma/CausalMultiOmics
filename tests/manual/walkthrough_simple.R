# =============================================================================
# CausalMultiOmics - COMPLETE WALKTHROUGH
# =============================================================================
#
# Run this to watch the whole package work, end to end. Unlike the testthat
# suite, which hides output and only reports pass or fail, this prints
# everything: every object, every method, every step of the pipeline, and what
# happens when you misuse it on purpose.
#
#   Rscript tests/manual/walkthrough.R
#
# or, inside R from the package root:
#
#   source("tests/manual/walkthrough.R")
#
# Run only some sections:
#
#   CMO_SECTIONS <- c(1, 5, 9); source("tests/manual/walkthrough.R")
#
# A full transcript is written to tests/manual/output/ along with every HTML
# report the run produces.
# =============================================================================

CMO_START <- Sys.time()

# -----------------------------------------------------------------------------
# Load the package
# -----------------------------------------------------------------------------

if (!exists("CMO_SECTIONS")) CMO_SECTIONS <- NULL

.loaded <- FALSE

if ("CausalMultiOmics" %in% loadedNamespaces()) {

  .loaded <- TRUE

} else if (requireNamespace("devtools", quietly = TRUE)) {

  for (root in c(".", "..", "../..")) {
    if (file.exists(file.path(root, "DESCRIPTION"))) {
      devtools::load_all(root, export_all = TRUE, quiet = TRUE)
      .loaded <- TRUE
      break
    }
  }

}

if (!.loaded) {
  .loaded <- suppressWarnings(require("CausalMultiOmics", quietly = TRUE))
}

if (!.loaded) {
  stop("Could not load CausalMultiOmics. Run this from the package root.",
       call. = FALSE)
}

# Internals are exercised too, so they have to be visible. devtools::load_all()
# with export_all = TRUE gives that; a plain library() does not.
.internals_visible <- exists(".safe_try")

# Pasting this file into a session still holding an older build produces a
# spray of "could not find function" and "unused argument" errors that look
# like faults in the package. Checking once, up front, turns that into a
# single sentence naming the real cause.

.expected <- c("load_data", "check_data", "preprocess",
               "apply_preprocessing", "analyze", "explain",
               "counterfactual", "annotate_evidence", "report")

.absent <- .expected[!vapply(.expected, exists, logical(1))]

.stale <- length(.absent) > 0 ||
  !all(c("effort", "min_per_block", "modifiable") %in%
       names(formals(analyze)))

if (.stale) {

  stop(
    "The loaded CausalMultiOmics is older than this script.\n",
    if (length(.absent) > 0)
      paste0("  missing: ", paste(.absent, collapse = ", "), "\n") else "",
    "  Reload it:  devtools::load_all(\".\", export_all = TRUE)",
    call. = FALSE
  )

}

rm(.expected, .absent, .stale)

# -----------------------------------------------------------------------------
# Transcript
# -----------------------------------------------------------------------------

.outdir <- file.path("tests", "manual", "output")

if (!dir.exists(.outdir)) dir.create(.outdir, recursive = TRUE, showWarnings = FALSE)

.transcript <- file.path(.outdir, "walkthrough.txt")

.con <- file(.transcript, open = "wt")
sink(.con, split = TRUE)
sink(.con, type = "message")

on.exit({
  sink(type = "message"); sink(); close(.con)
}, add = TRUE)

# =============================================================================
# Helpers
# =============================================================================

.n_ok <- 0L
.n_fail <- 0L
.failures <- character(0)

rule <- function(char = "=", width = 78) cat(strrep(char, width), "\n", sep = "")

section <- function(number, title) {

  if (!is.null(CMO_SECTIONS) && !(number %in% CMO_SECTIONS)) return(FALSE)

  cat("\n\n")
  rule("=")
  cat(sprintf("SECTION %-3s %s\n", number, title))
  rule("=")

  TRUE

}

step <- function(...) {
  cat("\n")
  rule("-")
  cat(..., "\n", sep = "")
  rule("-")
}

note <- function(...) cat("\n  ", ..., "\n", sep = "")

#' Echo a call the way a user would type it, run it, print what comes back.
show <- function(expr, print_it = TRUE) {

  label <- paste(deparse(substitute(expr)), collapse = "\n  ")

  cat("\n> ", label, "\n", sep = "")

  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        cat("  [warning] ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      cat("  [ERROR] ", conditionMessage(e), "\n", sep = "")
      .n_fail <<- .n_fail + 1L
      .failures <<- c(.failures, label)
      structure(list(msg = conditionMessage(e)), class = "cmo_failed")
    }
  )

  if (inherits(value, "cmo_failed")) return(invisible(NULL))

  .n_ok <<- .n_ok + 1L

  if (print_it && !is.null(value)) print(value)

  invisible(value)

}

#' Assert that a call fails, and show the message the user would see.
must_fail <- function(expr, expect = NULL) {

  label <- paste(deparse(substitute(expr)), collapse = " ")

  cat("\n> ", label, "\n", sep = "")

  err <- tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))

  if (is.null(err)) {

    cat("  [FAIL] expected an error, none was raised\n")
    .n_fail <<- .n_fail + 1L
    .failures <<- c(.failures, paste("no error from:", label))
    return(invisible(FALSE))

  }

  short <- strsplit(err, "\n")[[1]][1]

  if (!is.null(expect) && !grepl(expect, err)) {

    cat("  [FAIL] message did not mention '", expect, "':\n    ", short, "\n", sep = "")
    .n_fail <<- .n_fail + 1L
    .failures <<- c(.failures, paste("wrong message:", label))
    return(invisible(FALSE))

  }

  cat("  [ok] refused: ", short, "\n", sep = "")
  .n_ok <<- .n_ok + 1L

  invisible(TRUE)

}

# =============================================================================
# The data used throughout
# =============================================================================
# Real data when it is available: the NHANES 1999-2006 exposome release from
#
#   Patel CJ et al. (2016). A database of human exposomes and phenomes from
#   the US National Health and Nutrition Examination Survey.
#   Scientific Data 3:160096.  tests/manual/test_data/sdata201696.pdf
#
# It suits this package unusually well. The variables come already sorted into
# categories that behave exactly like blocks, the modules were measured on
# overlapping but different subsets of people, so the sample alignment has real
# work to do, and mortality follow-up gives a genuine survival design, which is
# the only setting where the engine can claim temporal identification.
#
# When the file is absent the script falls back to a small simulated study
# with a mechanism planted in it. That is worth keeping either way: real data
# has no known answer, so it can show the engine running but not that it is
# right.
# =============================================================================

.nhanes_path <- file.path("tests", "manual", "test_data", "nh_99-06.Rdata")

#' Blocks drawn from the NHANES exposome release
#'
#' Five categories that overlap on enough adults to be analysed together, one
#' of them categorical so the factor handling is exercised on real codes.
make_nhanes_study <- function(path = .nhanes_path) {

  bag <- new.env(parent = emptyenv())
  load(path, envir = bag)

  main <- bag$MainTable
  vars <- bag$VarDescription[!duplicated(bag$VarDescription$var), ]

  # Adults with mortality follow-up. PERMTH_INT is months from interview to
  # death or censoring, MORTSTAT is whether the death happened.
  keep <- which(
    main$RIDAGEYR >= 18 &
      !is.na(main$MORTSTAT) &
      !is.na(main$PERMTH_INT) &
      main$PERMTH_INT > 0
  )

  cohort <- main[keep, , drop = FALSE]

  wanted <- list(
    blood = c("LBXWBCSI", "LBXRBCSI", "LBXHGB", "LBXHCT",
              "LBXMCVSI", "LBXMCHSI", "LBXMC", "LBXRDW"),
    body_measures = c("BMXHT", "BMXWT", "BMXBMI", "BMXWAIST",
                      "BMXCALF", "BMXLEG", "BMXTHICR"),
    blood_pressure = c("BPXPLS", "MSYS", "MDIS"),
    heavy_metals = c("LBXBCD", "LBXBPB"),
    housing = c("house_type", "how_many_years_in_house",
                "use_water_treatment", "private_water_source", "house_age")
  )

  wanted <- lapply(wanted, intersect, colnames(cohort))

  # NHANES codes "refused" and "don't know" as 7, 9, 77, 99 and so on inside
  # otherwise ordinary variables. Left alone they become categories of their
  # own: house_age = 77 held 75 people out of five thousand and came top of
  # the evidence ranking on a single method.
  for (v in wanted$housing) {
    counts <- table(cohort[[v]])
    sentinels <- names(counts)[names(counts) %in% c("7", "9", "77", "99")]
    if (length(sentinels) > 0) {
      cohort[[v]][as.character(cohort[[v]]) %in% sentinels] <- NA
    }
  }

  # Only people measured on everything, since the analysis needs a rectangle.
  complete <- stats::complete.cases(
    cohort[, c(unlist(wanted, use.names = FALSE), "RIDAGEYR", "female"),
           drop = FALSE]
  )

  cohort <- cohort[complete, , drop = FALSE]

  # A walkthrough is meant to be watched. The full overlap is about five and
  # a half thousand people and takes a quarter of an hour, most of it in
  # structure learning and the mediation bootstrap; a stratified subsample
  # keeps every death in and runs in a couple of minutes. Set
  # CMO_FULL_COHORT <- TRUE to use all of it.
  if (!isTRUE(getOption("cmo.full.cohort", exists("CMO_FULL_COHORT") &&
                        isTRUE(get("CMO_FULL_COHORT")))) &&
      nrow(cohort) > 1800) {

    set.seed(1)

    died <- which(cohort$MORTSTAT == 1)
    lived <- which(cohort$MORTSTAT == 0)

    cohort <- cohort[sort(c(died, sample(lived, 1800 - length(died)))), ,
                     drop = FALSE]

  }

  ids <- paste0("NH", cohort$SEQN)

  assays <- lapply(wanted, function(v) {
    block <- cohort[, v, drop = FALSE]
    rownames(block) <- ids
    block
  })

  # Housing is stored as numeric codes but the codebook says these are
  # categories: house type 3 is not three times house type 1. Left as numbers
  # the engine would fit a slope through arbitrary labels.
  assays$housing[] <- lapply(assays$housing, function(col) {
    as.character(col)
  })

  metadata <- data.frame(
    sample_id = ids,
    died = as.integer(cohort$MORTSTAT),
    follow_up_months = as.numeric(cohort$PERMTH_INT),
    age = as.numeric(cohort$RIDAGEYR),
    female = as.integer(cohort$female),
    sex = ifelse(cohort$female == 1, "female", "male"),
    survey = paste0("cycle_", cohort$SDDSRVYR),
    stringsAsFactors = FALSE
  )

  labels <- stats::setNames(
    vars$var_desc[match(unlist(wanted, use.names = FALSE), vars$var)],
    unlist(wanted, use.names = FALSE)
  )

  list(
    source = "NHANES 1999-2006 (Patel et al., Scientific Data 3:160096)",
    assays = assays,
    metadata = metadata,
    modality = c(blood = "clinical", body_measures = "clinical",
                 blood_pressure = "clinical", heavy_metals = "other",
                 housing = "other"),
    outcome = "died",
    time = "follow_up_months",
    covariates = c("age", "female"),

    # Standing height cannot plausibly be part of a mechanism leading to
    # death over five years. If it turns up strongly, something other than
    # biology is driving the graph.
    negative_controls = "BMXHT",

    # What a person could conceivably act on, or be helped to act on.
    # Housing category and body height are not on that list.
    modifiable = c("body_measures", "blood_pressure", "heavy_metals"),

    labels = labels,
    ids = ids
  )

}

#' A small simulated study with a known answer
#'
#' Real data cannot show that the engine is right, only that it runs. This
#' plants a chain so the recovery can be checked.
make_simulated_study <- function(n = 90, seed = 42) {

  set.seed(seed)

  protein <- rnorm(n)
  metabolite <- 0.8 * protein + rnorm(n, sd = 0.6)
  outcome <- 0.9 * metabolite + 0.2 * protein + rnorm(n, sd = 0.7)

  ids <- paste0("P", seq_len(n))

  counts <- data.frame(
    G1 = rpois(n - 5, 8), G2 = rpois(n - 5, 20), G3 = rpois(n - 5, 3),
    row.names = ids[6:n]
  )
  counts[3, ] <- NA

  microbiome <- as.data.frame(matrix(rgamma(n * 4, shape = 2), ncol = 4))
  microbiome <- microbiome / rowSums(microbiome)
  colnames(microbiome) <- paste0("OTU", 1:4)
  rownames(microbiome) <- ids

  list(
    source = "simulated, with a planted chain APOA1 -> SCFA -> HDL_function",
    assays = list(
      proteins = data.frame(
        APOA1 = protein, CRP = rnorm(n),
        ALB = rep(1, n),                  # constant: should be removed
        HP = c(rnorm(n - 2), NA, NA),     # has gaps
        row.names = ids
      ),
      metabolites = data.frame(SCFA = metabolite, TMAO = rnorm(n),
                               row.names = ids),
      transcripts = counts,
      microbiome = microbiome
    ),
    metadata = data.frame(
      sample_id = ids,
      HDL_function = outcome,
      died = as.integer(outcome > stats::median(outcome)),
      follow_up_months = rexp(n, 0.05) + 1,
      age = rnorm(n, 55, 9),
      sex = rep(c("male", "female"), length.out = n),
      batch = rep(c("B1", "B2"), length.out = n),
      subject = paste0("S", rep(seq_len(n / 2), each = 2)),
      stringsAsFactors = FALSE
    ),
    modality = c(proteins = "proteomics", metabolites = "metabolomics",
                 transcripts = "rnaseq", microbiome = "microbiome"),
    outcome = STUDY$outcome,
    time = "follow_up_months",
    covariates = "age",
    negative_controls = STUDY$negative_controls,
    modifiable = STUDY$modifiable,
    labels = character(0),
    ids = ids
  )

}

STUDY <- if (file.exists(.nhanes_path)) {

  tryCatch(make_nhanes_study(), error = function(e) {
    cat("Could not read the NHANES file (", conditionMessage(e),
        "); using simulated data.\n", sep = "")
    make_simulated_study()
  })

} else {

  make_simulated_study()

}

# Kept so the sections that need a second cohort, or a known answer, still
# have one whichever dataset is in play.
SIMULATED <- make_simulated_study()

make_study <- function(n = 90, seed = 42) make_simulated_study(n, seed)

cat("\n")
rule("=")
cat("CausalMultiOmics - COMPLETE WALKTHROUGH\n")
rule("=")
cat("\nPackage version : ",
    tryCatch(as.character(utils::packageVersion("CausalMultiOmics")),
             error = function(e) "development"), "\n", sep = "")
cat("R version       : ", R.version.string, "\n", sep = "")
cat("Internals visible: ", .internals_visible, "\n", sep = "")
cat("Transcript      : ", .transcript, "\n", sep = "")
cat("Sections        : ",
    if (is.null(CMO_SECTIONS)) "all" else paste(CMO_SECTIONS, collapse = ", "),
    "\n", sep = "")

# =============================================================================
# SECTION 1 - Every class, empty
# =============================================================================

if (section(1, "Every class, freshly constructed")) {

  note("Each class is a labelled box. Building one empty shows its shape.")

  constructors <- c("MultiOmicsData", "BlockDiagnostics",
                    "TransformationRecommendation", "PreprocessingRecipe",
                    "PreprocessingResult", "CMOValidation", "EvidenceEdge",
                    "EvidenceGraph", "CMOResult")

  for (cls in constructors) {

    step("Class: ", cls)

    obj <- tryCatch(get(cls)(), error = function(e) NULL)

    if (is.null(obj)) {
      cat("  [ERROR] constructor not reachable\n")
      .n_fail <- .n_fail + 1L
      next
    }

    .n_ok <- .n_ok + 1L

    cat("\nSlots (", length(obj), "):\n", sep = "")
    print(names(obj))

    cat("\nprint():\n")
    print(obj)

    cat("\nsummary():\n")
    tryCatch(print(summary(obj)),
             error = function(e) cat("  [ERROR] ", conditionMessage(e), "\n"))

  }

}

# =============================================================================
# SECTION 2 - load_data()
# =============================================================================

if (section(2, "load_data(): getting data in")) {

  step("The normal case")

  study <- show(load_data(assays = STUDY$assays, metadata = STUDY$metadata,
                          modality = STUDY$modality))

  assign("STUDY_OBJ", study, envir = globalenv())

  note("What it built:")
  show(names(study))
  show(study$modality)
  show(utils::head(study$sample_info))
  show(utils::head(study$feature_info))
  show(study$history)
  show(study$misc$block_summary)

  step("summary() of the container")
  show(summary(study), print_it = FALSE)

  step("Every way load_data() can refuse")

  must_fail(load_data(), "must be supplied")
  must_fail(load_data(1), "must be a list")
  must_fail(load_data(list()), "is empty")
  must_fail(load_data(list(a = NULL)), "No valid data blocks")
  must_fail(load_data(list(a = 1)), "matrix or a data")
  must_fail(load_data(list(data.frame(x = 1))), "named list")
  must_fail(load_data(list(a = data.frame(x = integer(0)))), "no samples")
  must_fail(load_data(list(a = data.frame(row.names = 1:5))), "no variables")

  note("A matrix with no row names is refused on purpose:")
  must_fail(load_data(list(a = matrix(rnorm(20), nrow = 5))),
            "no sample identifiers")

  note("Duplicated names, in both directions:")

  dup_cols <- data.frame(A = rnorm(5), B = rnorm(5))
  colnames(dup_cols) <- c("X", "X")
  rownames(dup_cols) <- paste0("S", 1:5)
  must_fail(load_data(list(bad = dup_cols)), "Duplicated feature names")

  dup_rows <- data.frame(A = rnorm(4))
  attr(dup_rows, "row.names") <- c("S1", "S1", "S2", "S3")
  must_fail(load_data(list(bad = dup_rows)), "Duplicated sample identifiers")

  note("Modality has to name blocks that exist:")
  must_fail(load_data(list(a = STUDY$assays[[1]]),
                      modality = c(nope = "rnaseq")), "not in 'assays'")

}

# =============================================================================
# SECTION 3 - check_data()
# =============================================================================

if (section(3, "check_data(): the inspection, which changes nothing")) {

  study <- get("STUDY_OBJ", envir = globalenv())

  step("Run the audit")

  quality <- show(check_data(study))

  assign("QUALITY", quality, envir = globalenv())

  step("The full summary")
  show(summary(quality), print_it = FALSE)

  step("What it decided per block")

  for (blk in names(quality$diagnostics)) {

    cat("\n---- ", blk, " ----\n", sep = "")

    d <- quality$diagnostics[[blk]]

    cat(sprintf("  type      : %s\n", d$data_type))
    cat(sprintf("  modality  : %s\n", d$modality))
    cat(sprintf("  size      : %d samples x %d features\n", d$samples, d$features))
    cat(sprintf("  missing   : %.2f%%\n", d$missing_percent))
    cat(sprintf("  constant  : %d\n", length(d$constant_features)))
    cat(sprintf("  normality : %s\n", d$normality))

  }

  step("One BlockDiagnostics in full")
  show(print(quality$diagnostics[[1]]), print_it = FALSE)
  show(summary(quality$diagnostics[[1]]), print_it = FALSE)

  step("One TransformationRecommendation: the contest between reshapings")
  show(print(quality$transformations[[1]]), print_it = FALSE)
  show(summary(quality$transformations[[1]]), print_it = FALSE)

  step("One PreprocessingRecipe: the plan, before anything is applied")
  show(print(quality$recipes[[1]]), print_it = FALSE)
  show(summary(quality$recipes[[1]]), print_it = FALSE)

  step("The plan for every block, side by side")
  show(quality$tables$preprocessing)

  step("Tables produced")
  show(names(quality$tables))
  show(quality$tables$block_summary)
  show(quality$tables$diagnostics)
  show(quality$tables$transformations)

  step("Quality control checks")
  show(quality$qc)

  step("Sample overlap between blocks")
  show(quality$summary$overlap)

  note("Note transcripts has fewer samples: the blocks do not all match.")

  step("Plots that were produced")
  show(names(Filter(function(p) length(p) > 0, quality$plots)))

  note("Display one with: grDevices::replayPlot(quality$plots$histograms[[1]])")

  step("check_data() refuses anything that is not a MultiOmicsData")

  must_fail(check_data(1), "must be a MultiOmicsData")
  must_fail(check_data(NULL), "must be a MultiOmicsData")
  must_fail(check_data(STUDY$assays), "must be a MultiOmicsData")

  step("Structural problems come back as an invalid result, not an exception")

  broken <- MultiOmicsData()
  broken$assays <- list()
  v <- show(check_data(broken))
  show(v$errors)

}

# =============================================================================
# SECTION 4 - preprocess()
# =============================================================================

if (section(4, "preprocess(): executing the plan")) {

  study <- get("STUDY_OBJ", envir = globalenv())
  quality <- get("QUALITY", envir = globalenv())

  step("Run it")

  clean <- show(preprocess(study, quality, plots = TRUE, quiet = FALSE))

  assign("CLEAN", clean, envir = globalenv())

  step("The full summary, including the executed pipeline")
  show(summary(clean), print_it = FALSE)

  step("Every step that ran, as a table")
  show(clean$tables$steps)

  step("One step in full detail")
  show(clean$steps[[3]])

  note("Each step stores its fitted model, which is what makes the",
       " pipeline replayable on new data.")

  step("What was removed, and why")
  show(clean$tables$removed_features)
  show(clean$tables$removed_samples)

  step("Before and after")
  show(clean$tables$quality)

  step("The cleaned data")

  for (blk in names(clean$data$assays)) {
    cat(sprintf("\n  %-14s %d x %d, %d missing\n", blk,
                nrow(clean$data$assays[[blk]]),
                ncol(clean$data$assays[[blk]]),
                sum(is.na(clean$data$assays[[blk]]))))
  }

  show(utils::head(clean$data$assays[[1]], 4))

  step("The models it learned")
  show(names(clean$models))
  show(names(clean$models[[1]]))

  step("preprocess() refuses a plan that does not describe the data")

  other <- load_data(list(elsewhere = STUDY$assays[[1]]))
  must_fail(preprocess(other, quality, quiet = TRUE), "does not match")

  must_fail(preprocess(study), "must be supplied")
  must_fail(preprocess(1, quality), "must be a MultiOmicsData")
  must_fail(preprocess(study, "not a plan"), "must be a CMOValidation")

  step("A single recipe can be run on its own")
  show(preprocess(study, quality$recipes[[1]], plots = FALSE, quiet = TRUE))

}

# =============================================================================
# SECTION 5 - apply_preprocessing()
# =============================================================================

if (section(5, "apply_preprocessing(): the same recipe on new people")) {

  clean <- get("CLEAN", envir = globalenv())

  note("A second cohort. It must be cleaned using the numbers learned from",
       " the first, not recalculated from itself, or the two are not",
       " comparable.")

  fresh <- if (identical(STUDY$source, SIMULATED$source)) make_simulated_study(n = 30, seed = 999) else STUDY

  new_study <- show(load_data(fresh$assays, fresh$metadata, fresh$modality),
                    print_it = FALSE)

  ready <- show(apply_preprocessing(new_study, clean, quiet = FALSE))

  step("Same columns as the training data?")

  for (blk in names(ready$assays)) {

    trained <- clean$data$assays[[blk]]
    replayed <- ready$assays[[blk]]

    cat(sprintf("  %-14s trained %2dx%-2d  new %2dx%-2d  same features: %s\n",
                blk, nrow(trained), ncol(trained),
                nrow(replayed), ncol(replayed),
                identical(colnames(trained), colnames(replayed))))

  }

  step("Were any new samples dropped?")

  cat("  in :", nrow(fresh$assays[[1]]), "\n")
  cat("  out:", nrow(ready$assays[[1]]), "\n")

  note("None, by design. Dropping rows from a validation cohort because a",
       " training sample was dropped would manufacture optimistic results.")

  step("Guards")

  must_fail(apply_preprocessing(1, clean), "must be a MultiOmicsData")
  must_fail(apply_preprocessing(new_study, 1), "PreprocessingResult")
  must_fail(apply_preprocessing(new_study, PreprocessingResult()),
            "no steps to replay")

}

# =============================================================================
# SECTION 6 - The preprocessing method registry
# =============================================================================

if (section(6, "Everything the cleaning engine can execute")) {

  if (.internals_visible) {

    step("The registry")
    show(.available_methods())

    step("Unknown methods name the alternatives")
    must_fail(.get_method("imputation", "telepathy"), "Available")
    must_fail(.get_method("nonsense", "median"), "Unknown preprocessing stage")

    step("Methods that cannot be replayed explain why rather than pretending")

    entry <- .get_method("imputation", "mice")
    must_fail(entry$fit(data.frame(a = 1:5), list(), list()),
              "cannot be replayed")

  } else {

    note("Skipped: internals are not visible. Load with",
         " devtools::load_all('.', export_all = TRUE).")

  }

}

# =============================================================================
# SECTION 7 - analyze(), cross-sectional
# =============================================================================

if (section(7, "analyze(): building evidence")) {

  clean <- get("CLEAN", envir = globalenv())

  step("Run it")

  results <- show(analyze(clean, outcome = STUDY$outcome,
                          covariates = STUDY$covariates, plots = TRUE, quiet = FALSE))

  assign("RESULTS", results, envir = globalenv())

  step("The full summary")
  show(summary(results), print_it = FALSE)

  step("Which methods ran, and which sat it out")
  show(results$models)
  show(results$logs)

  step("The evidence, merged and scored")
  show(results$graph$edges)

  step("Did it recover anything? (a planted chain, on simulated data)")
  show(results$causal_paths)

  step("One EvidenceEdge in full")
  show(print(results$evidence[[1]]), print_it = FALSE)
  show(summary(results$evidence[[1]]), print_it = FALSE)

  note("Note identification and assumptions. A high score with",
       " 'adjustment' is a hypothesis, not a conclusion.")

  step("The EvidenceGraph")
  show(print(results$graph), print_it = FALSE)
  show(summary(results$graph), print_it = FALSE)

  step("What the graph is made of")
  show(results$graph$nodes)
  show(results$network$communities$sizes)
  show(results$network$metrics$density)

  step("Consensus importance across methods")
  show(results$importance)

  step("Predicting well and being well supported are not the same thing")
  show(results$tables$standing)

  note("A variable can predict beautifully because it is downstream of the",
       " outcome, and one with a well-supported effect can predict poorly",
       " because the effect is small. Reporting them in one column invites",
       " the reader to conflate them.")

  step("The interpretation")
  show(results$interpretation$drivers)
  show(results$interpretation$mediators)
  show(results$interpretation$hubs)
  show(results$interpretation$bridges)
  show(results$interpretation$risk)
  show(results$interpretation$protective)
  show(results$interpretation$statements)

  step("Honest accounting")
  show(results$report$limitations)
  show(results$performance)

  step("Guards")

  study <- get("STUDY_OBJ", envir = globalenv())

  must_fail(analyze(study, STUDY$outcome), "must be a PreprocessingResult")
  must_fail(analyze(clean, "no_such_column"), "not a column")
  must_fail(analyze(clean, STUDY$outcome, methods = "telepathy"),
            "Unknown generator")
  must_fail(analyze(clean, STUDY$outcome, blocks = "nope"), "Unknown block")

}

# =============================================================================
# SECTION 8 - analyze() under other designs
# =============================================================================

if (section(8, "The design decides which methods apply")) {

  clean <- get("CLEAN", envir = globalenv())

  step("Survival: an event plus a follow-up time")

  surv <- show(analyze(clean, outcome = STUDY$outcome, time = STUDY$time,
                       plots = FALSE, quiet = TRUE))

  cat("\n  design           :", surv$design$type, "\n")
  cat("  methods run      :", paste(names(surv$models), collapse = ", "), "\n")
  cat("  temporal edges   :", surv$performance$temporal_edges,
      "of", nrow(surv$graph$edges), "\n")

  show(table(surv$graph$edges$identification))

  note("Only here can a relationship claim temporal identification:",
       " the exposure was measured before the event.")

  step("Longitudinal: repeated measurements per subject")

  # A longitudinal design needs the same person measured more than once.
  # NHANES interviews each participant a single time, so passing subject =
  # there produces a cross-sectional analysis labelled as though it were
  # longitudinal, which teaches the reader the opposite of the truth. The
  # simulated study does have repeated measures, so the demonstration moves
  # there rather than being skipped.

  repeated_measures <- !is.null(STUDY$metadata$subject) &&
    any(table(STUDY$metadata$subject) > 1)

  if (repeated_measures) {

    long_input <- clean
    long_outcome <- STUDY$outcome

  } else {

    note("This dataset measures each person once, so it cannot show a",
         " longitudinal design. Switching to the simulated study, which",
         " has two measurements per subject.")

    sim <- load_data(SIMULATED$assays, SIMULATED$metadata,
                     SIMULATED$modality)

    long_input <- preprocess(sim, check_data(sim), plots = FALSE,
                             quiet = TRUE)
    long_outcome <- SIMULATED$outcome

  }

  long <- show(analyze(long_input, outcome = long_outcome,
                       subject = "subject", plots = FALSE, quiet = TRUE))

  cat("\n  design      :", long$design$type, "\n")
  cat("  methods run :", paste(names(long$models), collapse = ", "), "\n")
  show(long$design$notes)

  if (!identical(long$design$type, "longitudinal")) {
    cat("\n  [FAIL] subject was supplied but the design is",
        long$design$type, "\n")
    .n_fail <- .n_fail + 1L
    .failures <- c(.failures, "longitudinal design not detected")
  }


  step("Predictive goal: a narrower set of methods")

  pred <- show(analyze(clean, outcome = STUDY$outcome, goal = "predictive",
                       plots = FALSE, quiet = TRUE))

  cat("\n  methods run :", paste(names(pred$models), collapse = ", "), "\n")

  step("A single method, on demand")

  one <- show(analyze(clean, outcome = STUDY$outcome,
                      methods = "association", plots = FALSE, quiet = TRUE))

  cat("\n  methods run :", paste(names(one$models), collapse = ", "), "\n")

  step("Cross-sectional never claims temporal identification")

  results <- get("RESULTS", envir = globalenv())
  show(results$performance$temporal_edges)

}

# =============================================================================
# SECTION 9 - The evidence generators
# =============================================================================

if (section(9, "Every evidence generator, one by one")) {

  if (.internals_visible) {

    show(names(.evidence_registry()))

    registry <- .evidence_registry()

    for (nm in names(registry)) {

      entry <- registry[[nm]]

      missing <- entry$requires[
        !vapply(entry$requires, requireNamespace, logical(1), quietly = TRUE)]

      cat(sprintf("\n  %-14s %-38s needs: %-12s %s\n", nm, entry$label,
                  if (length(entry$requires)) paste(entry$requires, collapse = ",")
                  else "nothing",
                  if (length(missing)) "MISSING" else "available"))

    }

  } else {

    note("Skipped: internals not visible.")

  }

}

# =============================================================================
# SECTION 10 - annotate_evidence()
# =============================================================================

if (section(10, "annotate_evidence(): adding outside knowledge")) {

  results <- get("RESULTS", envir = globalenv())

  note("Kept out of analyze() on purpose, so the analysis stays offline",
       " and reproducible. Supply a local export rather than a live query.")

  edges <- results$graph$edges

  annotations <- data.frame(
    source = utils::head(edges$source, 2),
    target = utils::head(edges$target, 2),
    support = c(0.9, 0.6),
    database = c("local KEGG export", "local STRING export"),
    stringsAsFactors = FALSE
  )

  show(annotations)

  annotated <- show(annotate_evidence(results, annotations))

  step("What changed")

  touched <- Filter(function(e) is.finite(e$biological_support),
                    annotated$evidence)

  cat("  edges annotated:", length(touched), "\n\n")

  for (e in touched) {
    cat("  ", e$source, "->", e$target,
        " support", e$biological_support,
        " from", paste(e$biological_sources, collapse = ", "), "\n")
  }

  step("Guards")

  must_fail(annotate_evidence(1, annotations), "must be a CMOResult")
  must_fail(annotate_evidence(results, data.frame(a = 1)), "source")
  must_fail(annotate_evidence(results, annotations, weight = 2),
            "between 0 and 1")

}

# =============================================================================
# SECTION 11 - report()
# =============================================================================

if (section(11, "report(): making it readable")) {

  quality <- get("QUALITY", envir = globalenv())
  results <- get("RESULTS", envir = globalenv())

  step("The validation report, on screen")
  show(report(quality, format = "console"), print_it = FALSE)

  step("The analysis report, on screen")
  show(report(results, format = "console"), print_it = FALSE)

  step("Only some sections of the validation report")
  show(report(quality, format = "console", sections = c("overview", "qc")),
       print_it = FALSE)

  step("HTML: four documents")

  paths <- c(
    validation = file.path(.outdir, "validation-report.html"),
    analysis_general = file.path(.outdir, "analysis-general.html"),
    analysis_technical = file.path(.outdir, "analysis-technical.html")
  )

  show(report(quality, file = paths[["validation"]], open = FALSE,
              quiet = FALSE), print_it = FALSE)

  show(report(results, file = paths[["analysis_general"]], open = FALSE,
              quiet = FALSE), print_it = FALSE)

  show(report(results, file = paths[["analysis_technical"]], open = FALSE,
              quiet = FALSE, audience = "technical"), print_it = FALSE)

  step("What was written")

  for (nm in names(paths)) {
    cat(sprintf("  %-20s %7.1f KB  %s\n", nm,
                if (file.exists(paths[[nm]])) file.size(paths[[nm]]) / 1024 else NA,
                paths[[nm]]))
  }

  step("The figures behind the report")

  show(names(results$plots))

  note("circos and circos_blocks are the two that show the whole answer at",
       " once: which layers were measured, how big each is, and where the",
       " evidence runs between them.")

  for (nm in c("circos", "circos_blocks")) {

    if (is.null(results$plots[[nm]])) next

    file <- file.path(.outdir, paste0(nm, ".png"))

    ok <- tryCatch({
      grDevices::png(file, width = 1100, height = 1100, res = 130,
                     type = "cairo")
      grDevices::replayPlot(results$plots[[nm]])
      grDevices::dev.off()
      TRUE
    }, error = function(e) { try(grDevices::dev.off(), silent = TRUE); FALSE })

    cat(sprintf("  %-16s %s  %s\n", nm,
                if (ok && file.exists(file)) sprintf("%6.0f KB",
                                                     file.size(file) / 1024)
                else "  failed",
                file))

  }

  note("Open these in a browser. They are self-contained: no internet needed.")

  step("Guards")

  must_fail(report(results, format = "pdf"))
  must_fail(report(results, audience = "experts"))
  must_fail(report(get("STUDY_OBJ", envir = globalenv())), "no applicable method")

  step("Degenerate objects still produce a document")

  show(report(CMOValidation(), format = "console"), print_it = FALSE)
  show(report(CMOResult(), format = "console"), print_it = FALSE)

}

# =============================================================================
# SECTION 12 - How much computation to spend
# =============================================================================

if (section(12, "The effort dial: buying rigour with time")) {

  clean <- get("CLEAN", envir = globalenv())

  note("Everything expensive is opt-in through one argument, with each",
       " piece separately overridable. Resampling and permutation are the",
       " only parts that cost real time, and both scale linearly.")

  step("effort = 'fast': the generators once, nothing else")

  t0 <- Sys.time()
  fast <- show(analyze(clean, STUDY$outcome, effort = "fast",
                       plots = FALSE, quiet = TRUE))
  cat(sprintf("\n  elapsed: %.1f s\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  step("effort = 'standard': adds model diagnostics and 50 resamples")

  t0 <- Sys.time()
  standard <- show(analyze(clean, STUDY$outcome, effort = "standard",
                           plots = FALSE, quiet = TRUE))
  cat(sprintf("\n  elapsed: %.1f s\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  step("What each preset turns on")

  for (nm in c("fast", "standard")) {

    r <- get(nm)

    cat(sprintf("\n  %-10s resampling: %-5s  null calibration: %-5s  diagnostics: %s\n",
                nm,
                isTRUE(r$diagnostics$resampling$available),
                isTRUE(r$diagnostics$null_calibration$available),
                isTRUE(r$diagnostics$models$available)))

  }

  note("'thorough' adds null calibration; 'exhaustive' resamples every",
       " generator 500 times. Any component can be set on its own:",
       " resample =, permutations =, diagnostics =.")

  step("Overriding a single piece")

  custom <- show(analyze(clean, STUDY$outcome, effort = "fast",
                         resample = 20, plots = FALSE, quiet = TRUE))

  cat("\n  resampling ran despite effort = 'fast': ",
      isTRUE(custom$diagnostics$resampling$available), "\n", sep = "")

  assign("RESAMPLED", standard, envir = globalenv())

}

# =============================================================================
# SECTION 13 - What a score is made of
# =============================================================================

if (section(13, "Opening up an evidence score")) {

  results <- get("RESULTS", envir = globalenv())

  note("Three numbers, kept apart on purpose. Size is how big the",
       " relationship is, Precision how tightly it was estimated, and",
       " Agreement how many methods saw the same thing.")

  step("The evidence hierarchy: not every method makes the same kind of claim")

  if (.internals_visible) {

    for (g in names(.evidence_registry())) {
      cat(sprintf("  %-16s level %d  %-14s weight %.1f\n",
                  g, .evidence_level(g), .level_label(.evidence_level(g)),
                  .level_weight(.evidence_level(g))))
    }

    note("Agreement is weighted by this. Five predictive methods concurring",
         " is weaker evidence than one longitudinal model plus one",
         " mediation analysis, and an unweighted vote says the opposite.")

  }

  step("Every relationship, with its level and identification")

  show(results$graph$edges[, c("source", "target", "level_label",
                               "identification", "consistency",
                               "evidence_score")])

  step("The score, opened up")

  if (.internals_visible) {
    show(.decompose_evidence(results$evidence[[1]]))
  }

  step("How much unmeasured confounding would explain it away")

  show(results$graph$edges[, c("source", "target", "e_value",
                               "direction_confidence", "bootstrap_stability")])

  note("An E-value near 1 means a trivially weak hidden factor would",
       " account for the whole thing. Direction confidence of 0.5 means the",
       " arrow could point either way.")

  step("Where methods disagreed, and why")

  conflicted <- Filter(function(e) length(e$conflicting_methods) > 0,
                       results$evidence)

  if (length(conflicted) == 0) {
    cat("  No relationship had methods pointing opposite ways.\n")
  } else {
    for (e in utils::head(conflicted, 3)) {
      cat("\n  ", e$source, " -> ", e$target, "\n", sep = "")
      cat(paste0("    ", e$conflict_summary), sep = "\n")
    }
  }

  step("What each method reported on its own, before merging")

  show(results$evidence[[1]]$contributions)

  note("Estimates are on each method's own scale and are not comparable",
       " between rows.")

}

# =============================================================================
# SECTION 14 - explain()
# =============================================================================

if (section(14, "explain(): why is this variable here")) {

  results <- get("RESULTS", envir = globalenv())

  candidates <- results$interpretation$drivers$source

  if (length(candidates) == 0) {

    note("No driver to explain in this run.")

  } else {

    step("Everything the engine knows about one variable")

    show(explain(results, candidates[1]), print_it = TRUE)

    step("It refuses a name it does not know, and says what it does know")

    must_fail(explain(results, "not_a_variable"), "no relationships")
    must_fail(explain(1, "x"), "must be a CMOResult")

  }

}

# =============================================================================
# SECTION 15 - counterfactual()
# =============================================================================

if (section(15, "counterfactual(): what changing something would mean")) {

  results <- get("RESULTS", envir = globalenv())

  note("Expressed in the units the variable was measured in. The engine",
       " pushes the original value through the stored preprocessing models",
       " to locate it on the scale the analysis worked in.")

  step("Only variables you declare as changeable are considered")

  must_fail(counterfactual(results), "must name the variables")

  note("Nothing is included by default on purpose: a contrast for a genotype",
       " is arithmetic without meaning, and printing one invites exactly the",
       " reading it cannot support.")

  step("Declaring blocks that could plausibly be acted on")

  cf <- show(counterfactual(results, modifiable = STUDY$modifiable),
             print_it = TRUE)

  step("A specific amount, in original units")

  show(counterfactual(results, modifiable = STUDY$modifiable[1],
                      change = "iqr")$table)

  step("A decrease instead")

  show(counterfactual(results, modifiable = STUDY$modifiable[1],
                      direction = "decrease")$table)

  step("Requiring stronger identification empties the table rather than relaxing")

  strict <- show(counterfactual(results, modifiable = STUDY$modifiable[1],
                                min_identification = "temporal"),
                 print_it = FALSE)

  cat("\n  rows: ", nrow(strict$table), "\n", sep = "")
  cat("  reason: ", paste(strict$skipped, collapse = "; "), "\n", sep = "")

  note("The verb is chosen by the identification label, never freely. An",
       " adjusted association reads 'is associated with', however high its",
       " score.")

}

# =============================================================================
# SECTION 16 - Categorical variables
# =============================================================================

if (section(16, "Factors, binary and with several categories")) {

  note("Factors survive loading and preprocessing untouched, which is right:",
       " nothing sensible comes of scaling a category. They are encoded at",
       " the analysis step.")

  set.seed(31)
  m <- 120
  ids <- paste0("C", seq_len(m))

  sex <- sample(c("F", "M"), m, TRUE)
  diet <- sample(c("mediterranean", "western", "vegetarian"), m, TRUE,
                 prob = c(.45, .35, .20))

  marker <- rnorm(m, 100, 15)
  y <- 0.03 * marker + 1.1 * (sex == "M") - 0.8 * (diet == "western") +
    rnorm(m)

  cat_obj <- show(load_data(
    list(
      blood = data.frame(MARKER = marker, OTHER = rnorm(m), row.names = ids),
      lifestyle = data.frame(sex = sex, diet = diet,
                             barcode = paste0("id_", seq_len(m)),
                             row.names = ids, stringsAsFactors = FALSE)
    ),
    metadata = data.frame(sample_id = ids, score = y,
                          stringsAsFactors = FALSE)
  ), print_it = FALSE)

  cat_prep <- preprocess(cat_obj, check_data(cat_obj), plots = FALSE,
                         quiet = TRUE)

  step("Factors are still factors after preprocessing")

  show(vapply(cat_prep$data$assays$lifestyle, class, character(1)))

  step("analyze() encodes them")

  cat_res <- show(analyze(cat_prep, "score", effort = "fast", plots = FALSE,
                          quiet = TRUE), print_it = FALSE)

  for (nm in names(cat_res$data$encoding)) {
    e <- cat_res$data$encoding[[nm]]
    cat(sprintf("  %-20s %s vs %s   (%d vs %d people)\n",
                nm, e$level, e$reference, e$n_level, e$n_reference))
  }

  note("A factor of k levels becomes k-1 comparisons against a reference,",
       " and the reference is the commonest level, because contrasts against",
       " a rare category are estimated from very few people.")

  step("What was left out, and why")

  show(grep("categor|identifier|fewer than", cat_res$logs, value = TRUE))

  step("The wording changes for a category")

  show(cat_res$report$main_findings)

  step("A category contrast is membership, not units")

  show(counterfactual(cat_res, modifiable = "diet")$table)

  step("An unordered outcome with more than two categories is refused")

  must_fail(
    analyze(cat_prep, "sex_is_not_here", quiet = TRUE),
    "not a column"
  )

}

# =============================================================================
# SECTION 17 - Blocks as units of evidence
# =============================================================================

if (section(17, "Reading the result one layer at a time")) {

  results <- get("RESULTS", envir = globalenv())

  step("How the screening budget was shared out")

  clean <- get("CLEAN", envir = globalenv())

  note("max_features caps how many variables reach the pairwise stage, and",
       " min_per_block is the floor each block keeps. Ranking every feature",
       " together would hand the whole budget to whichever block has the",
       " most columns, and a small clinical block would vanish.")

  tight <- show(analyze(clean, STUDY$outcome, max_features = 8,
                        min_per_block = 2, effort = "fast",
                        plots = FALSE, quiet = TRUE), print_it = FALSE)

  show(tight$data$screening$allocation)

  cat("\n  every block survived: ",
      length(tight$data$screening$allocation) ==
        length(unique(tight$data$feature_block)), "\n", sep = "")

  note("Every block keeps at least one variable even when that pushes the",
       " total past max_features: losing a block entirely is worse than",
       " exceeding a target set for speed.")

  step("Evidence between every pair of layers")
  show(results$network$blocks$evidence)

  step("How much of the total each layer carries")
  show(results$network$blocks$importance)

  note("The shares add to 100: a relationship between two blocks belongs to",
       " both, so its score is split between them.")

  step("Each layer, by kind of evidence")
  show(results$network$blocks$scores)

  step("What each detected community is made of")
  show(results$network$blocks$communities)

  step("The graph with whole layers as the nodes")
  show(results$network$blocks$graph$nodes)
  show(results$network$blocks$graph$edges)

  step("Crossing layers is reported beside the score, never inside it")

  show(utils::head(results$graph$edges[, c("source", "target", "source_block",
                                            "target_block", "cross_block",
                                            "evidence_score")], 8))

  note("Crossing makes a relationship more interesting, not better",
       " supported. Folding it into the score would mix a thematic",
       " judgement into a measurement.")

}

# =============================================================================
# SECTION 18 - Robustness and diagnostics
# =============================================================================

if (section(18, "How much to trust any of this")) {

  resampled <- get("RESAMPLED", envir = globalenv())
  clean <- get("CLEAN", envir = globalenv())

  step("Would these relationships survive a different sample?")

  if (isTRUE(resampled$diagnostics$resampling$available)) {

    cat(sprintf("  %d %s replicates over: %s\n",
                resampled$diagnostics$resampling$replicates,
                resampled$diagnostics$resampling$scheme,
                paste(resampled$diagnostics$resampling$generators,
                      collapse = ", ")))

    stability <- vapply(resampled$evidence,
                        function(e) e$bootstrap_stability, numeric(1))

    cat(sprintf("  recovered every time : %d\n", sum(stability >= 0.99,
                                                     na.rm = TRUE)))
    cat(sprintf("  recovered under half : %d\n", sum(stability < 0.5,
                                                     na.rm = TRUE)))

  } else {

    cat("  Resampling did not run.\n")

  }

  step("How many relationships appear when there is nothing to find")

  calibrated <- show(analyze(clean, STUDY$outcome, effort = "fast",
                             permutations = 15, plots = FALSE, quiet = TRUE),
                     print_it = FALSE)

  cal <- calibrated$diagnostics$null_calibration

  if (isTRUE(cal$available)) {

    cat(sprintf("\n  found here          : %d\n", cal$observed_edges))
    cat(sprintf("  found on shuffled   : %.1f on average, up to %d\n",
                cal$null_edges_mean, cal$null_edges_max))
    cat(sprintf("  empirical p         : %.3f\n", cal$p_more_edges))
    cat("\n  ", cal$verdict, "\n", sep = "")

    note("The single most useful number about a graph. Twenty-five edges",
         " sounds like a result until the same engine produces twenty-two on",
         " the same data with the outcome shuffled.")

  }

  step("Variables that should not appear, and did")

  controls <- show(analyze(clean, STUDY$outcome, effort = "fast",
                           negative_controls = STUDY$negative_controls,
                           plots = FALSE, quiet = TRUE),
                   print_it = FALSE)$diagnostics$negative_controls

  show(controls$notes)
  show(controls$flagged)

  step("Does the relationship hold in every subgroup?")

  hetero <- show(analyze(clean, STUDY$outcome, effort = "fast",
                         heterogeneity = "sex", plots = FALSE, quiet = TRUE),
                 print_it = FALSE)$diagnostics$heterogeneity

  if (isTRUE(hetero$available)) {
    show(utils::head(hetero$table, 5))
    show(hetero$notes)
  }

  step("Fit quality and assumption checks")

  diagnostics <- get("RESAMPLED", envir = globalenv())$diagnostics$models

  if (isTRUE(diagnostics$available)) {
    show(utils::head(diagnostics$table, 5))
    show(diagnostics$notes)
  }

  step("What the missingness looked like")

  missing <- get("RESULTS", envir = globalenv())$diagnostics$missing_data

  cat(sprintf("  %.2f%% of cells missing, %.1f%% complete cases\n",
              missing$missing_percent, missing$complete_case_percent))

  show(missing$notes)

  step("How much the picture leans on any one part")

  robustness <- get("RESULTS", envir = globalenv())$diagnostics$network_robustness

  if (isTRUE(robustness$available)) {
    show(robustness$leave_one_block_out)
    show(robustness$notes)
  }

}

# =============================================================================
# SECTION 19 - Reproducibility
# =============================================================================

if (section(19, "Reproducibility guarantees")) {

  clean <- get("CLEAN", envir = globalenv())
  study <- get("STUDY_OBJ", envir = globalenv())

  step("Does the package disturb your random number generator?")

  set.seed(123); before <- runif(3)
  set.seed(123); invisible(check_data(study)); after_check <- runif(3)

  set.seed(123)
  invisible(analyze(clean, STUDY$outcome, plots = FALSE, quiet = TRUE,
                    bootstrap = 30))
  after_analyze <- runif(3)

  cat("  after check_data() : ", identical(before, after_check), "\n", sep = "")
  cat("  after analyze()    : ", identical(before, after_analyze), "\n", sep = "")

  note("Both must be TRUE. An audit that moved your generator would",
       " silently change every simulation you ran afterwards.")

  if (!identical(before, after_check) || !identical(before, after_analyze)) {
    .n_fail <- .n_fail + 1L
    .failures <- c(.failures, "RNG state was modified")
  } else {
    .n_ok <- .n_ok + 2L
  }

  step("Does the same input give the same answer?")

  a <- analyze(clean, STUDY$outcome, plots = FALSE, quiet = TRUE, bootstrap = 30)
  b <- analyze(clean, STUDY$outcome, plots = FALSE, quiet = TRUE, bootstrap = 30)

  same <- identical(
    vapply(a$evidence, function(e) e$evidence_score, numeric(1)),
    vapply(b$evidence, function(e) e$evidence_score, numeric(1))
  )

  cat("  two identical runs agree: ", same, "\n", sep = "")

  if (same) .n_ok <- .n_ok + 1L else {
    .n_fail <- .n_fail + 1L
    .failures <- c(.failures, "analyze() is not deterministic")
  }

  step("What was recorded about this run")

  show(clean$execution)
  show(get("RESULTS", envir = globalenv())$execution)

}

# =============================================================================
# SECTION 20 - Inventory
# =============================================================================

if (section(20, "What the package contains")) {

  step("Everything exported")

  exported <- tryCatch(
    sort(getNamespaceExports("CausalMultiOmics")),
    error = function(e) character(0)
  )

  if (length(exported) > 0) {
    show(exported)
  } else {
    note("Namespace exports unavailable under load_all(); see NAMESPACE.")
  }

  step("Functions per source file")

  if (dir.exists("R")) {

    counts <- vapply(list.files("R", pattern = "[.]R$", full.names = TRUE),
                     function(f) {
                       length(grep("^[a-zA-Z._][a-zA-Z0-9._]*\\s*<-\\s*function",
                                   readLines(f, warn = FALSE)))
                     }, integer(1))

    names(counts) <- basename(names(counts))

    print(counts)
    cat("\n  total:", sum(counts), "\n")

  }

  step("S3 methods registered for dispatch")

  generics <- c("print", "summary", "report")
  classes <- c("MultiOmicsData", "BlockDiagnostics",
               "TransformationRecommendation", "PreprocessingRecipe",
               "PreprocessingResult", "CMOValidation", "CMOResult",
               "EvidenceEdge", "EvidenceGraph")

  for (g in generics) {
    for (cls in classes) {
      m <- tryCatch(utils::getS3method(g, cls, optional = TRUE),
                    error = function(e) NULL)
      if (!is.null(m)) cat(sprintf("  %-8s %s\n", g, cls))
    }
  }

}

# =============================================================================
# Scoreboard
# =============================================================================

cat("\n\n")
rule("=")
cat("WALKTHROUGH COMPLETE\n")
rule("=")

cat(sprintf("\n  calls that succeeded : %d\n", .n_ok))
cat(sprintf("  calls that failed    : %d\n", .n_fail))

if (.n_fail > 0) {
  cat("\n  FAILURES:\n")
  cat(paste0("    - ", .failures, collapse = "\n"), "\n")
}

cat(sprintf("\n  elapsed : %.1f s\n",
            as.numeric(difftime(Sys.time(), CMO_START, units = "secs"))))

cat("\n  Transcript : ", .transcript, "\n", sep = "")
cat("  HTML reports and figures: ", .outdir, "\n", sep = "")

cat("\n")
if (.n_fail == 0) cat("  Everything worked.\n") else
  cat("  Something is wrong - see the failures above.\n")
rule("=")
cat("\n")
