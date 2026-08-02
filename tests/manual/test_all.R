# =============================================================================
# CausalMultiOmics
# COMPLETE DEVELOPMENT TEST
# =============================================================================

cat("\n=====================================================\n")
cat("CausalMultiOmics - Development Test\n")
cat("=====================================================\n\n")

# =============================================================================
# Environment setup
# =============================================================================
# Tries devtools::load_all() from the package root first (the normal
# development workflow). If no package skeleton is found (e.g. the four
# R/ scripts are being tested standalone), falls back to sourcing them
# directly so this file can always be run on its own.
# =============================================================================

.pkg_root_candidates <- c(".", "..")
.loaded_via_devtools <- FALSE

# Already attached (library(CausalMultiOmics) or devtools::load_all()
# run beforehand)? Then use what is loaded and do not source anything.
.already_loaded <- "CausalMultiOmics" %in% loadedNamespaces()

if (.already_loaded) {

  cat("Using the already-loaded CausalMultiOmics namespace.\n\n")
  .loaded_via_devtools <- TRUE

}

if (!.already_loaded && requireNamespace("devtools", quietly = TRUE)) {

  for (root in .pkg_root_candidates) {

    if (file.exists(file.path(root, "DESCRIPTION"))) {

      devtools::load_all(root, export_all = TRUE, quiet = TRUE)
      .loaded_via_devtools <- TRUE
      break

    }

  }

}

if (!.loaded_via_devtools) {

  cat("No package skeleton found next to this script; sourcing R files directly.\n\n")

  .r_files <- c("classes.R", "data_loading.R", "methods.R", "validation.R")

  for (.f in .r_files) {

    .candidate <- if (file.exists(.f)) .f else file.path("R", .f)

    if (!file.exists(.candidate)) {

      stop(sprintf(
        "Could not find '%s'. Run this script from the package root (or next to the R files).",
        .f
      ))

    }

    source(.candidate)

  }

  rm(.f, .candidate, .r_files)

}

rm(.pkg_root_candidates, .loaded_via_devtools, .already_loaded)

# =============================================================================
# Internal visibility check
# =============================================================================
# This suite exercises unexported helpers, so they have to be reachable. A
# plain library() call only attaches exported objects, which would make the
# run die later with a confusing "could not find function" error.
# =============================================================================

if (!exists("MultiOmicsData") || !exists(".detect_data_type")) {

  stop(
    "The package internals are not visible.\n",
    "  This suite tests unexported helpers, so load the package with one of:\n",
    "    devtools::load_all(\".\", export_all = TRUE)\n",
    "    or run the script next to classes.R / data_loading.R / methods.R / validation.R\n",
    "  A plain library(CausalMultiOmics) only attaches exported objects.",
    call. = FALSE
  )

}


# =============================================================================
# S3 registration guard
# =============================================================================
# A method only dispatches if NAMESPACE contains the matching S3method()
# entry. Exporting it with export() is NOT enough: dispatch still fails with
# "no applicable method". If NAMESPACE is stale (for example it was generated
# before report() was added), every method below would fail for a reason that
# has nothing to do with the code under test, so the situation is detected
# here, reported clearly, and repaired for the duration of this session.
# =============================================================================

.s3_expected <- list(
  c("print", "MultiOmicsData"), c("print", "BlockDiagnostics"),
  c("print", "TransformationRecommendation"), c("print", "PreprocessingRecipe"),
  c("print", "PreprocessingResult"), c("print", "CMOValidation"),
  c("summary", "MultiOmicsData"), c("summary", "BlockDiagnostics"),
  c("summary", "TransformationRecommendation"), c("summary", "PreprocessingRecipe"),
  c("summary", "PreprocessingResult"), c("summary", "CMOValidation"),
  c("report", "CMOValidation")
)

# Registration cannot be tested by calling the method (that would execute it
# and could write files) nor with getS3method(), which finds the function by
# name even when NAMESPACE never registered it. The S3 methods table of the
# generic's own environment is the authoritative source.

.s3_registered <- function(generic, cls) {

  gen <- tryCatch(get(generic), error = function(e) NULL)

  if (is.null(gen)) return(TRUE)

  method <- paste0(generic, ".", cls)

  # Sourced workflow: the method sits next to the generic in globalenv.
  if (exists(method, envir = globalenv(), inherits = FALSE)) return(TRUE)

  env <- environment(gen)

  if (is.null(env)) return(FALSE)

  tbl <- tryCatch(get(".__S3MethodsTable__.", envir = env),
                  error = function(e) NULL)

  !is.null(tbl) && exists(method, envir = tbl, inherits = FALSE)

}

.s3_unregistered <- character(0)

for (.m in .s3_expected) {

  .generic <- .m[1]
  .cls <- .m[2]
  .method <- paste0(.generic, ".", .cls)

  if (!exists(.generic)) next

  if (.s3_registered(.generic, .cls)) next

  .fn <- NULL

  if (exists(.method)) {
    .fn <- get(.method)
  } else {
    .fn <- tryCatch(utils::getS3method(.generic, .cls, optional = TRUE),
                    error = function(e) NULL)
  }

  if (is.null(.fn) || !is.function(.fn)) next

  .s3_unregistered <- c(.s3_unregistered, .method)

  registerS3method(.generic, .cls, .fn)

}

if (length(.s3_unregistered) > 0) {

  cat("\n!! S3 METHODS NOT REGISTERED FOR DISPATCH:\n")
  cat(paste0("     - ", .s3_unregistered, collapse = "\n"), "\n")
  cat("   They were registered for this session so the tests can run, but\n")
  cat("   your NAMESPACE is missing:\n")
  cat(paste0("     S3method(", sub("\\.", ", ", .s3_unregistered), ")",
             collapse = "\n"), "\n")
  cat("   Fix it with devtools::document() and reinstall, or add the lines\n")
  cat("   above to NAMESPACE by hand.\n\n")

} else {

  cat("S3 dispatch: all expected methods are registered.\n\n")

}

rm(.s3_expected, .s3_unregistered, .s3_registered)

# .fn is only created when a method turns out to be unregistered, so the loop
# above may leave any subset of these behind.
rm(list = intersect(c(".m", ".generic", ".cls", ".method", ".fn"),
                    ls(all.names = TRUE)))

# =============================================================================
# Coverage instrumentation
# =============================================================================
# Every function the package defines is replaced by a thin wrapper that
# records the call and then delegates to the original. Lazy evaluation,
# missing(), S3 dispatch and match.arg() all still behave correctly because
# the arguments are forwarded untouched through `...`. Section 18 reports
# which functions were never reached.
# =============================================================================

.cov_env <- new.env(parent = emptyenv())
.cov_orig <- new.env(parent = emptyenv())
.cov_enabled <- FALSE

.cov_all <- Filter(
  function(n) is.function(get(n, envir = globalenv())),
  ls(globalenv(), all.names = TRUE)
)

# Do not instrument the harness itself.
.cov_all <- setdiff(.cov_all, c("run_test", "expect_error", "check", "user_call"))

if (length(.cov_all) > 0) {

  for (.fn in .cov_all) {

    local({

      nm <- .fn
      orig <- get(nm, envir = globalenv())

      force(nm)
      force(orig)

      assign(nm, orig, envir = .cov_orig)

      assign(
        nm,
        function(...) {
          assign(nm, TRUE, envir = .cov_env)
          orig(...)
        },
        envir = globalenv()
      )

    })

  }

  .cov_enabled <- TRUE
  rm(.fn)

}

cat(sprintf("Coverage instrumentation: %d function(s) tracked.\n\n", length(.cov_all)))

# =============================================================================
# Helpers
# =============================================================================

.n_ok <- 0L
.n_error <- 0L
.n_checks_passed <- 0L
.n_checks_failed <- 0L

#' Run an expression, report OK/ERROR, keep going either way.
run_test <- function(name, expr) {

  cat("\n--------------------------------------------------\n")
  cat(name, "\n")
  cat("--------------------------------------------------\n")

  out <- tryCatch(

    {
      value <- force(expr)
      cat("OK\n")
      .n_ok <<- .n_ok + 1L
      value
    },

    error = function(e) {

      cat("ERROR:\n")
      message(conditionMessage(e))
      .n_error <<- .n_error + 1L

      NULL

    }

  )

  invisible(out)

}

#' Assert that `expr` raises an error, optionally matching `pattern`.
#' Prints the exact message the user would see either way.
expect_error <- function(expr, pattern = NULL) {

  res <- tryCatch(
    { eval.parent(substitute(expr)); NULL },
    error = function(e) e
  )

  if (is.null(res)) {

    cat("[FAILED] expected an error, none was raised\n")
    .n_checks_failed <<- .n_checks_failed + 1L
    return(invisible(FALSE))

  }

  if (!is.null(pattern) && !grepl(pattern, conditionMessage(res))) {

    cat(sprintf(
      "[FAILED] error raised but message did not match /%s/: %s\n",
      pattern, conditionMessage(res)
    ))
    .n_checks_failed <<- .n_checks_failed + 1L
    return(invisible(FALSE))

  }

  cat("[OK] error as expected -> ", conditionMessage(res), "\n", sep = "")
  .n_checks_passed <<- .n_checks_passed + 1L

  invisible(TRUE)

}

#' Assert a condition, report [OK]/[FAILED], never halts the suite.
check <- function(cond, msg) {

  ok <- isTRUE(cond)

  if (ok) {

    cat("[OK] ", msg, "\n", sep = "")
    .n_checks_passed <<- .n_checks_passed + 1L

  } else {

    cat("[FAILED] ", msg, "\n", sep = "")
    .n_checks_failed <<- .n_checks_failed + 1L

  }

  invisible(ok)

}

#' Evaluate `expr` exactly as a user would type it at the console and
#' reproduce, as closely as base R allows, what they would actually see:
#' the echoed call, the printed/auto-printed value (if visible), any
#' warning message(s) (shown after the value, like R's default deferred
#' warnings), and any error in R's own "Error in call : message" format.
user_call <- function(expr) {

  call_text <- deparse(substitute(expr), width.cutoff = 500L)
  cat("\n> ", paste(call_text, collapse = "\n  "), "\n", sep = "")

  collected_warnings <- list()

  result <- tryCatch(

    withCallingHandlers(

      withVisible(expr),

      warning = function(w) {
        collected_warnings[[length(collected_warnings) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }

    ),

    error = function(e) {

      call <- conditionCall(e)

      if (is.null(call)) {
        cat("Error: ", conditionMessage(e), "\n", sep = "")
      } else {
        cat("Error in ", deparse(call), " : ", conditionMessage(e), "\n", sep = "")
      }

      NULL

    }

  )

  if (!is.null(result) && isTRUE(result$visible)) print(result$value)

  if (length(collected_warnings) == 1L) {

    w <- collected_warnings[[1L]]
    wcall <- conditionCall(w)

    if (is.null(wcall)) {
      cat("Warning message:\n", conditionMessage(w), "\n", sep = "")
    } else {
      cat("Warning message:\nIn ", deparse(wcall), " : ", conditionMessage(w), "\n", sep = "")
    }

  } else if (length(collected_warnings) > 1L) {

    cat("Warning messages:\n")

    for (i in seq_along(collected_warnings)) {

      w <- collected_warnings[[i]]
      wcall <- conditionCall(w)

      if (is.null(wcall)) {
        cat(i, ": ", conditionMessage(w), "\n", sep = "")
      } else {
        cat(i, ": In ", deparse(wcall), " : ", conditionMessage(w), "\n", sep = "")
      }

    }

  }

  invisible(if (!is.null(result)) result$value else NULL)

}

# =============================================================================
# Test data
# =============================================================================
# Deliberately dirty: a constant feature, missing values, an empty sample,
# a mixed clinical block, and a compositional block, plus metadata carrying
# both a grouping/batch variable and a time/status pair so the survival-
# outcome path in the transformation benchmark gets exercised too.
# =============================================================================

set.seed(123)

n <- 20

transcriptomics <- data.frame(
  G1 = rnorm(n),
  G2 = rnorm(n),
  G3 = rep(1, n),        # constant feature
  G4 = c(rnorm(n - 1), NA),  # missing value
  G5 = rlnorm(n),
  G6 = rnorm(n, 5, 2),
  row.names = paste0("S", 1:n)
)

proteomics <- data.frame(
  P1 = rpois(n - 2, 5),   # count data
  P2 = rpois(n - 2, 10),
  P3 = rpois(n - 2, 3),
  row.names = paste0("S", 3:n)
)
proteomics[5, ] <- c(NA, NA, NA)   # empty sample

clinical <- data.frame(
  Age = rnorm(n, 55, 10),
  Sex = sample(c("M", "F"), n, replace = TRUE),
  BMI = rnorm(n, 26, 4),
  row.names = paste0("S", 1:n)
)

microbiome <- as.data.frame(matrix(rgamma(n * 5, shape = 2), ncol = 5))
microbiome <- microbiome / rowSums(microbiome)
colnames(microbiome) <- paste0("OTU", 1:5)
rownames(microbiome) <- paste0("S", 1:n)

metadata <- data.frame(
  sample_id = paste0("S", 1:n),
  group = sample(c("A", "B"), n, TRUE),
  batch = rep(c("X", "Y"), n / 2),
  time = rexp(n, rate = 0.02),
  status = sample(c(0, 1), n, replace = TRUE, prob = c(0.35, 0.65)),
  stringsAsFactors = FALSE
)

# =============================================================================
# SECTION 1 - Constructors
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 1 - Class constructors\n")
cat("=====================================================\n")

run_test("Create MultiOmicsData", MultiOmicsData())
run_test("Create BlockDiagnostics", BlockDiagnostics())
run_test("Create TransformationRecommendation", TransformationRecommendation())
run_test("Create PreprocessingRecipe", PreprocessingRecipe())
run_test("Create PreprocessingResult", PreprocessingResult())
run_test("Create CMOValidation", CMOValidation())

run_test("Create CMOResult", CMOResult())

check(setequal(
  names(CMOResult()),
  c("data", "validation", "integration", "latent", "causal", "network",
    "biomarkers", "prediction", "enrichment", "parameters", "performance",
    "plots", "tables", "reports", "execution", "history", "misc")
), "CMOResult() exposes every slot print()/summary() read")

# =============================================================================
# SECTION 2 - load_data()
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 2 - load_data()\n")
cat("=====================================================\n")

omics <- run_test(
  "Load valid multi-block data",
  load_data(
    assays = list(
      transcriptomics = transcriptomics,
      proteomics = proteomics,
      clinical = clinical,
      microbiome = microbiome
    ),
    metadata = metadata
  )
)

run_test(
  "Load single-block data (no metadata)",
  load_data(assays = list(only = clinical))
)

cat("\nload_data() misuse -> expected errors\n")
expect_error(load_data(), "must be supplied")
expect_error(load_data(1), "must be a list")
expect_error(load_data(list()), "is empty")
expect_error(load_data(list(a = NULL)), "No valid data blocks")
expect_error(load_data(list(a = 1)), "matrix or a data.frame")
expect_error(load_data(assays = list(a = data.frame(x = integer(0)))), "no samples")
expect_error(load_data(assays = list(a = data.frame(row.names = 1:5))), "no variables")
expect_error(load_data(assays = list(1, 2)), "named list")
expect_error(load_data(assays = list(a = data.frame(), b = data.frame(x = 1))))

dup_feature_block <- data.frame(A = rnorm(5), B = rnorm(5))
colnames(dup_feature_block) <- c("X", "X")
rownames(dup_feature_block) <- paste0("S", 1:5)

expect_error(
  load_data(assays = list(bad = dup_feature_block)),
  "Duplicated feature names"
)

dup_sample_block <- data.frame(A = rnorm(4))
attr(dup_sample_block, "row.names") <- c("S1", "S1", "S2", "S3")  # bypass R's own uniqueness guard

expect_error(
  load_data(assays = list(bad = dup_sample_block)),
  "Duplicated sample identifiers"
)

# =============================================================================
# SECTION 3 - check_data()
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 3 - check_data()\n")
cat("=====================================================\n")

validation <- run_test("check_data() on valid multi-block object", check_data(omics))

cat("\ncheck_data() misuse -> expected errors\n")
expect_error(check_data(1), "must be a MultiOmicsData")
expect_error(check_data(NULL), "must be a MultiOmicsData")
expect_error(check_data("abc"), "must be a MultiOmicsData")
expect_error(check_data(list(assays = list())), "must be a MultiOmicsData")

cat("\ncheck_data() on structurally broken objects -> not exceptions, but invalid results\n")

empty_obj <- MultiOmicsData()
empty_obj$assays <- list()
v_empty <- run_test("check_data() on empty assays list", check_data(empty_obj))
check(isFALSE(v_empty$valid), "empty-assays object is reported as invalid, not raised as an error")
check("No data blocks found." %in% v_empty$errors, "the exact reason is recorded in validation$errors")

dup_ids_obj <- MultiOmicsData()
bad_block <- data.frame(A = rnorm(5), B = rnorm(5), row.names = paste0("S", 1:5))
colnames(bad_block) <- c("Y", "Y")   # bypass load_data()'s own guard on purpose
dup_ids_obj$assays <- list(bad = bad_block)
v_dup <- run_test("check_data() defense-in-depth on a hand-built object with duplicated feature names", check_data(dup_ids_obj))
check(isFALSE(v_dup$valid), "duplicated feature names are caught even when load_data() was bypassed")

meta_no_id <- metadata
meta_no_id$sample_id <- NULL
obj_no_id <- load_data(assays = list(clinical = clinical), metadata = meta_no_id)
v_no_id <- run_test("check_data() with metadata missing 'sample_id'", check_data(obj_no_id))
check(isFALSE(v_no_id$valid), "missing metadata$sample_id is reported as invalid")
check(any(grepl("sample_id", v_no_id$errors)), "the error text names the missing column")

# =============================================================================
# SECTION 4 - S3 methods (print / summary) across every block and class
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 4 - print() / summary() methods\n")
cat("=====================================================\n")

run_test("print.MultiOmicsData", print(omics))
run_test("summary.MultiOmicsData", summary(omics))

run_test("print.CMOValidation", print(validation))
run_test("summary.CMOValidation", summary(validation))

for (block_name in names(validation$diagnostics)) {

  run_test(
    sprintf("print.BlockDiagnostics [%s]", block_name),
    print(validation$diagnostics[[block_name]])
  )

  run_test(
    sprintf("summary.BlockDiagnostics [%s]", block_name),
    summary(validation$diagnostics[[block_name]])
  )

  run_test(
    sprintf("print.TransformationRecommendation [%s]", block_name),
    print(validation$transformations[[block_name]])
  )

  run_test(
    sprintf("summary.TransformationRecommendation [%s]", block_name),
    summary(validation$transformations[[block_name]])
  )

  run_test(
    sprintf("print.PreprocessingRecipe [%s]", block_name),
    print(validation$recipes[[block_name]])
  )

  run_test(
    sprintf("summary.PreprocessingRecipe [%s]", block_name),
    summary(validation$recipes[[block_name]])
  )

}

dummy_prep_res <- PreprocessingResult()
run_test("print.PreprocessingResult", print(dummy_prep_res))
run_test("summary.PreprocessingResult", summary(dummy_prep_res))

# =============================================================================
# SECTION 5 - API integrity checks
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 5 - API integrity checks\n")
cat("=====================================================\n")

# -----------------------------------------------------------------------------
# load_data() structure
# -----------------------------------------------------------------------------
check(inherits(omics, "MultiOmicsData"), "load_data() returns MultiOmicsData")
check(is.list(omics$assays), "assays is a list")
check(length(omics$assays) == 4, "correct number of blocks")
check(is.data.frame(omics$sample_info), "sample_info is a data.frame")
check(is.data.frame(omics$feature_info), "feature_info is a data.frame")

# -----------------------------------------------------------------------------
# check_data() structure and output types
# -----------------------------------------------------------------------------
check(inherits(validation, "CMOValidation"), "check_data() returns CMOValidation")
check(is.logical(validation$valid), "valid is logical")
check(isTRUE(validation$valid), "the well-formed multi-block object validates cleanly")
check(is.numeric(validation$score), "score is numeric")
check(is.list(validation$diagnostics), "diagnostics is a list")
check(is.list(validation$recipes), "recipes is a list")
check(is.list(validation$transformations), "transformations is a list")
check(length(validation$diagnostics) == 4, "one BlockDiagnostics per block")
check(length(validation$recipes) == 4, "one PreprocessingRecipe per block")
check(length(validation$transformations) == 4, "one TransformationRecommendation per block")

# -----------------------------------------------------------------------------
# Slot fidelity: no class should ever lose a slot, whatever the input
# -----------------------------------------------------------------------------
check(setequal(names(validation), names(CMOValidation())),
      "CMOValidation keeps every top-level slot")
check(setequal(names(validation$summary), names(CMOValidation()$summary)),
      "validation$summary keeps every slot")
check(setequal(names(validation$details), names(CMOValidation()$details)),
      "validation$details keeps every slot")
check(setequal(names(validation$qc), names(CMOValidation()$qc)),
      "validation$qc keeps every slot")
check(setequal(names(validation$plots), names(CMOValidation()$plots)),
      "validation$plots keeps every slot (even when a given plot could not be built)")
check(setequal(names(validation$tables), names(CMOValidation()$tables)),
      "validation$tables keeps every slot")
check(setequal(names(validation$execution), names(CMOValidation()$execution)),
      "validation$execution keeps every slot")

for (block_name in names(validation$diagnostics)) {

  check(
    setequal(names(validation$diagnostics[[block_name]]), names(BlockDiagnostics())),
    sprintf("BlockDiagnostics[%s] keeps every slot", block_name)
  )

  check(
    setequal(names(validation$recipes[[block_name]]), names(PreprocessingRecipe())),
    sprintf("PreprocessingRecipe[%s] keeps every slot", block_name)
  )

  check(
    setequal(names(validation$transformations[[block_name]]), names(TransformationRecommendation())),
    sprintf("TransformationRecommendation[%s] keeps every slot", block_name)
  )

}

# -----------------------------------------------------------------------------
# Sub-classes generated by check_data()
# -----------------------------------------------------------------------------
block_name <- names(omics$assays)[1]

check(inherits(validation$diagnostics[[block_name]], "BlockDiagnostics"),
      "Diagnostics correctly generated")
check(inherits(validation$recipes[[block_name]], "PreprocessingRecipe"),
      "Recipes correctly generated")
check(inherits(validation$transformations[[block_name]], "TransformationRecommendation"),
      "Transformations correctly generated")

# -----------------------------------------------------------------------------
# Data-type detection sanity (given the test data constructed above)
# -----------------------------------------------------------------------------
check(validation$diagnostics$proteomics$data_type == "count",
      "proteomics is detected as count data")
check(validation$diagnostics$microbiome$data_type == "compositional",
      "microbiome is detected as compositional data")

# -----------------------------------------------------------------------------
# Populated outputs
# -----------------------------------------------------------------------------
check(length(Filter(Negate(is.null), validation$plots)) > 0,
      "plots were generated and stored")
check(is.data.frame(validation$tables$block_summary), "tables$block_summary is a data.frame")
check(is.data.frame(validation$tables$diagnostics), "tables$diagnostics is a data.frame")
check(is.data.frame(validation$tables$transformations), "tables$transformations is a data.frame")
check(is.data.frame(validation$tables$preprocessing), "tables$preprocessing is a data.frame")
check(is.character(validation$recommendations$global), "recommendations$global is populated")
check(is.character(validation$recommendations$blocks[[block_name]]),
      "recommendations$blocks[[block]] is populated")
check(is.list(validation$qc) && is.logical(validation$qc$passed), "qc$passed is logical")
check(is.numeric(validation$execution$runtime), "execution$runtime was recorded")

cat("\n")
cat(sprintf("Assertions passed: %d | failed: %d\n", .n_checks_passed, .n_checks_failed))

# =============================================================================
# SECTION 6 - report(): console format
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 6 - report(): console format\n")
cat("=====================================================\n")

run_test("report(format = 'console') full report",
         report(validation, format = "console"))

for (sec in c("overview", "blocks", "issues", "overlap", "preprocessing",
              "transformations", "qc", "actions", "outputs")) {

  run_test(sprintf("report(format = 'console', sections = '%s')", sec),
           report(validation, format = "console", sections = sec))

}

run_test("report() console with several sections",
         report(validation, format = "console", sections = c("overview", "qc")))

run_test("report() console max_items = 1",
         report(validation, format = "console", sections = "actions", max_items = 1))

run_test("report() console width = 60",
         report(validation, format = "console",
                sections = c("overview", "blocks"), width = 60))

run_test("report() console on a structurally invalid validation",
         report(v_empty, format = "console"))
run_test("report() console on a validation with metadata errors",
         report(v_no_id, format = "console"))
run_test("report() console on an empty CMOValidation",
         report(CMOValidation(), format = "console"))

check(inherits(report(validation, format = "console", sections = "qc"), "CMOValidation"),
      "report() returns the validation object")
check(identical(report(validation, format = "console", sections = "qc"), validation),
      "report() console returns the object unmodified")

expect_error(report(validation, format = "console", sections = "not_a_section"))
expect_error(report(validation, format = "pdf"))

cat("\nreport() on other classes -> expected errors (no method defined)\n")
expect_error(report(omics), "no applicable method")
expect_error(report(42), "no applicable method")

# =============================================================================
# SECTION 6b - report(): HTML format
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 6b - report(): HTML format\n")
cat("=====================================================\n")

.html_dir <- file.path(tempdir(), "cmo_reports")
dir.create(.html_dir, showWarnings = FALSE, recursive = TRUE)

.html_path <- file.path(.html_dir, "report.html")

.html_res <- run_test(
  "report() writes a self-contained HTML file",
  report(validation, file = .html_path, open = FALSE, quiet = TRUE)
)

check(file.exists(.html_path), "the HTML file was created")
check(file.size(.html_path) > 10000, "the HTML file has real content")
check(identical(attr(.html_res, "report_path"), .html_path),
      "report() returns the destination path as an attribute")

.html_txt <- paste(readLines(.html_path, warn = FALSE), collapse = "\n")

check(grepl("<!DOCTYPE html>", .html_txt, fixed = TRUE), "document declares a doctype")
check(grepl("</html>", .html_txt, fixed = TRUE), "document is closed")
check(grepl("<style>", .html_txt, fixed = TRUE), "styles are inlined")
check(grepl("<script>", .html_txt, fixed = TRUE), "scripts are inlined")

check(!grepl("(src|href)=[\"']https?://", .html_txt),
      "no external resources are referenced (works offline, no CDN)")

.n_imgs <- length(gregexpr("data:image/png;base64,", .html_txt, fixed = TRUE)[[1]])
check(.n_imgs > 1, sprintf("plots are embedded as inline images (%d found)", .n_imgs))

for (.sec in c("overview", "blocks", "overlap", "preprocessing",
               "transformations", "qc", "actions", "plots", "tables")) {

  check(grepl(sprintf("id='%s'", .sec), .html_txt, fixed = TRUE),
        sprintf("HTML contains the '%s' section", .sec))

}

check(grepl("cmoShow(", .html_txt, fixed = TRUE), "section navigation is wired up")
check(grepl("cmoSort(", .html_txt, fixed = TRUE), "tables are sortable")
check(grepl("cmoFilterPlots(", .html_txt, fixed = TRUE), "plots are filterable")
check(grepl("id='lightbox'", .html_txt, fixed = TRUE), "plots open in a lightbox")
check(grepl("<section id='overview' class='active'>", .html_txt, fixed = TRUE),
      "the first section renders even if JavaScript is unavailable")

cat("\nDestination handling\n")

check(grepl("\\.html$", .report_destination("noext", prompt = FALSE)),
      ".report_destination() appends a missing extension")
check(grepl("\\.htm$", .report_destination("keep.htm", prompt = FALSE)),
      ".report_destination() keeps an existing .htm extension")
check(basename(.report_destination(.html_dir, prompt = FALSE)) ==
        "CausalMultiOmics_validation_report.html",
      ".report_destination() expands a directory to the default file name")

.deep <- file.path(tempdir(), "cmo_deep", "nested")
invisible(.report_destination(file.path(.deep, "r.html"), prompt = FALSE))
check(dir.exists(.deep), ".report_destination() creates missing directories")

check(!is.null(.report_destination(NULL, prompt = FALSE)),
      ".report_destination() falls back to a default when not prompting")

cat("\nHTML for degenerate validations\n")

.p_empty <- file.path(.html_dir, "empty.html")
run_test("HTML report for a structurally invalid validation",
         report(v_empty, file = .p_empty, open = FALSE, quiet = TRUE))
check(file.exists(.p_empty), "HTML written even when validation failed structurally")
check(grepl("VALIDATION FAILED",
            paste(readLines(.p_empty, warn = FALSE), collapse = ""), fixed = TRUE),
      "the failed verdict is shown in the HTML")

.p_bare <- file.path(.html_dir, "bare.html")
run_test("HTML report for an empty CMOValidation",
         report(CMOValidation(), file = .p_bare, open = FALSE, quiet = TRUE))
check(file.exists(.p_bare), "HTML written for a bare CMOValidation")

run_test("report() honours quiet = FALSE",
         report(validation, file = file.path(.html_dir, "loud.html"),
                open = FALSE, quiet = FALSE))

run_test("report() with custom plot dimensions",
         report(validation, file = file.path(.html_dir, "small.html"),
                open = FALSE, quiet = TRUE,
                plot_width = 400, plot_height = 300, plot_res = 72))

check(file.size(file.path(.html_dir, "small.html")) <
        file.size(.html_path),
      "smaller plot dimensions produce a smaller document")

# =============================================================================
# SECTION 6c - HTML engine internals
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 6c - HTML engine internals\n")
cat("=====================================================\n")

cat("\nBase64 encoder (verified against the RFC 4648 test vectors)\n")

check(.html_base64(charToRaw("Man")) == "TWFu", ".html_base64() 3-byte group")
check(.html_base64(charToRaw("Ma")) == "TWE=", ".html_base64() one padding byte")
check(.html_base64(charToRaw("M")) == "TQ==", ".html_base64() two padding bytes")
check(.html_base64(raw(0)) == "", ".html_base64() empty input")
check(.html_base64(charToRaw("hello world")) == "aGVsbG8gd29ybGQ=",
      ".html_base64() longer string")
check(.html_base64(charToRaw("any carnal pleasure.")) == "YW55IGNhcm5hbCBwbGVhc3VyZS4=",
      ".html_base64() sentence with padding")

cat("\nEscaping\n")

check(.html_escape("<script>") == "&lt;script&gt;", ".html_escape() escapes angle brackets")
check(.html_escape("a & b") == "a &amp; b", ".html_escape() escapes ampersands")
check(.html_escape("say \"hi\"") == "say &quot;hi&quot;", ".html_escape() escapes quotes")
check(.html_escape(NA) == "", ".html_escape() turns NA into an empty string")
check(.html_escape("&<") == "&amp;&lt;", ".html_escape() does not double-escape")
check(.html_id("a b/c") == "a_b_c", ".html_id() sanitizes id attributes")

cat("\nPlot embedding\n")

.rp <- .safe_record(function() graphics::plot(1:10, main = "probe"))
check(inherits(.rp, "recordedplot"), ".safe_record() returns a recorded plot")
check(length(.rp[[1]]) > 0,
      ".safe_record() captures a non-empty display list (regression guard)")

.uri <- .html_plot_uri(.rp, width = 300, height = 200, res = 72)
check(!is.null(.uri) && grepl("^data:image/png;base64,", .uri),
      ".html_plot_uri() returns a PNG data URI")
check(nchar(.uri) > 500, ".html_plot_uri() encodes a real image")
check(is.null(.html_plot_uri(NULL)), ".html_plot_uri() returns NULL for NULL input")
check(is.null(.html_plot_uri("not a plot")), ".html_plot_uri() rejects non-plots")

check(length(.html_plot_items(NULL)) == 0, ".html_plot_items() handles NULL")
check(length(.html_plot_items(.rp)) == 1, ".html_plot_items() wraps a single plot")
check(length(.html_plot_items(list(a = .rp, b = .rp))) == 2,
      ".html_plot_items() keeps a list of plots")
check(length(.html_plot_items(list(a = .rp, b = "junk"))) == 1,
      ".html_plot_items() drops non-plot entries")

cat("\nFragment builders\n")

check(grepl("<table", .html_table(head(validation$tables$diagnostics))),
      ".html_table() renders a table")
check(grepl("No data available", .html_table(data.frame())),
      ".html_table() handles an empty data.frame")
check(grepl("onclick='cmoSort", .html_table(data.frame(a = 1))),
      ".html_table() wires up sorting")
check(grepl("cap", .html_table(data.frame(a = 1), caption = "hello")),
      ".html_table() renders a caption")

check(grepl("<li>one</li>", .html_list(c("one", "two")), fixed = TRUE),
      ".html_list() renders list items")
check(grepl("None", .html_list(character(0))), ".html_list() handles an empty vector")
check(grepl("card-value", .html_card("L", "V")), ".html_card() renders a stat card")
check(grepl("card-sub", .html_card("L", "V", "sub")), ".html_card() renders the subtitle")

cat("\nSection builders\n")

.sections <- list(
  overview = .html_section_overview(validation),
  blocks = .html_section_blocks(validation),
  overlap = .html_section_overlap(validation),
  preprocessing = .html_section_preprocessing(validation),
  transformations = .html_section_transformations(validation),
  qc = .html_section_qc(validation),
  actions = .html_section_actions(validation),
  tables = .html_section_tables(validation),
  plots = .html_section_plots(validation, 300, 200, 72)
)

for (.nm in names(.sections)) {

  check(is.character(.sections[[.nm]]) && nchar(.sections[[.nm]]) > 0,
        sprintf(".html_section_%s() produces markup", .nm))

}

check(grepl("VALIDATION PASSED", .sections$overview, fixed = TRUE),
      ".html_section_overview() shows the verdict")
check(grepl("gauge-fill", .sections$overview, fixed = TRUE),
      ".html_section_overview() renders the quality gauge")

.ov_empty <- CMOValidation()
check(grepl("Not available", .html_section_overlap(.ov_empty), fixed = TRUE),
      ".html_section_overlap() handles a missing overlap matrix")
check(grepl("No plots", .html_section_plots(.ov_empty, 300, 200, 72), fixed = TRUE),
      ".html_section_plots() handles a validation with no plots")

check(grepl("<style>", .html_style(), fixed = TRUE), ".html_style() returns CSS")
check(grepl("<script>", .html_script(), fixed = TRUE), ".html_script() returns JavaScript")

.doc <- .report_html(validation, plot_width = 300, plot_height = 200, plot_res = 72)
check(grepl("<!DOCTYPE html>", .doc, fixed = TRUE), ".report_html() builds a full document")
check(grepl("</html>", .doc, fixed = TRUE), ".report_html() closes the document")
check(nchar(.doc) > 50000, ".report_html() produces a substantial document")

.doc_empty <- .report_html(CMOValidation())
check(grepl("</html>", .doc_empty, fixed = TRUE),
      ".report_html() still builds a valid document with no blocks")

# =============================================================================
# SECTION 7 - Internal helpers: utilities
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 7 - Internal helpers: utilities\n")
cat("=====================================================\n")

check(identical(.safe_try(1 + 1), 2), ".safe_try() returns the value on success")
check(identical(.safe_try(stop("x"), default = "fb"), "fb"), ".safe_try() falls back on error")
check(identical(.safe_try({ warning("w"); 5 }, default = -1), 5),
      ".safe_try() does NOT swallow results that merely warn")
check(identical(.safe_try(NULL, default = "fb"), "fb"), ".safe_try() treats NULL as failure")

.slot_obj <- list(a = 1, b = 2, c = 3)
check(length(.set_slot(.slot_obj, "b", NULL)) == 3,
      ".set_slot() keeps the slot when assigning NULL")
check(is.null(.set_slot(.slot_obj, "b", NULL)$b),
      ".set_slot() actually stores NULL")
check(identical(.set_slot(.slot_obj, "b", 99)$b, 99),
      ".set_slot() stores ordinary values")

.mix <- data.frame(num = 1:5, chr = letters[1:5], dbl = rnorm(5),
                   stringsAsFactors = FALSE)
check(setequal(.numeric_columns(.mix), c("num", "dbl")),
      ".numeric_columns() selects only numeric columns")
check(ncol(.numeric_matrix(.mix)) == 2, ".numeric_matrix() drops non-numeric columns")
check(ncol(.numeric_matrix(data.frame(a = letters[1:3]))) == 0,
      ".numeric_matrix() handles a block with no numeric columns")
check(length(.pool_numeric(.mix)) == 10, ".pool_numeric() pools all finite numeric values")
check(!any(is.infinite(.pool_numeric(data.frame(x = c(1, Inf, -Inf, 2))))),
      ".pool_numeric() drops non-finite values")

.meta_probe <- data.frame(sample_id = "a", Batch = 1, TIME = 2)
check(identical(.find_metadata_column(.meta_probe, "^batch$"), "Batch"),
      ".find_metadata_column() is case-insensitive")
check(is.null(.find_metadata_column(.meta_probe, "^nothing$")),
      ".find_metadata_column() returns NULL when nothing matches")
check(is.null(.find_metadata_column(NULL, "^batch$")),
      ".find_metadata_column() tolerates NULL metadata")

.mb <- .map_blocks(list(a = 1, b = 2), function(nm, x) if (nm == "a") "kept" else NULL)
check(identical(names(.mb), "a"), ".map_blocks() drops NULL results and keeps names")

check(identical(fmt_char(NULL), "None"), "fmt_char() handles NULL")
check(identical(fmt_char(NA), "None"), "fmt_char() handles NA")
check(identical(fmt_char("x"), "x"), "fmt_char() passes values through")
check(identical(fmt_num(NULL), "NA"), "fmt_num() handles NULL")
check(identical(fmt_num(3.14159, 2), "3.14"), "fmt_num() rounds to the requested digits")

# =============================================================================
# SECTION 8 - Internal helpers: data-type detection
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 8 - Internal helpers: data-type detection\n")
cat("=====================================================\n")

set.seed(11)

check(.detect_variable_type(c(TRUE, FALSE, TRUE)) == "binary",
      ".detect_variable_type() logical -> binary")
check(.detect_variable_type(c(0, 1, 1, 0)) == "binary",
      ".detect_variable_type() 0/1 numeric -> binary")
check(.detect_variable_type(factor(c("a", "b", "c"))) == "ordinal",
      ".detect_variable_type() multi-level factor -> ordinal")
check(.detect_variable_type(c(1L, 5L, 9L, 20L)) == "count",
      ".detect_variable_type() non-negative integers -> count")
check(.detect_variable_type(c(0.2, 0.5, 0.9)) == "proportion",
      ".detect_variable_type() values in [0,1] -> proportion")
check(.detect_variable_type(rnorm(20)) == "continuous",
      ".detect_variable_type() real-valued -> continuous")

.dt <- function(x) .detect_data_type(x)

check(.dt(as.data.frame(matrix(rnorm(40 * 6), nrow = 40))) == "continuous",
      ".detect_data_type() continuous block")
check(.dt(as.data.frame(matrix(rbinom(40 * 6, 1, 0.5), nrow = 40))) == "binary",
      ".detect_data_type() binary block")
check(.dt(as.data.frame(matrix(runif(40 * 6), nrow = 40))) == "proportion",
      ".detect_data_type() proportion block")

.comp <- as.data.frame(matrix(rgamma(40 * 6, 2), nrow = 40))
.comp <- .comp / rowSums(.comp)
check(.dt(.comp) == "compositional", ".detect_data_type() compositional block")
check(.dt(round(.comp * 100, 2)) == "compositional",
      ".detect_data_type() percentages rounded to 2 dp still compositional")

check(.dt(data.frame(a = rnorm(10), b = letters[1:10], stringsAsFactors = FALSE)) == "mixed",
      ".detect_data_type() heterogeneous block -> mixed")

cat("\nRegression: count data must never be read as compositional\n")

for (.p in c(5, 30, 100, 500, 2000)) {

  .cnt <- as.data.frame(matrix(rpois(40 * .p, 12), nrow = 40))

  check(.dt(.cnt) == "count",
        sprintf("count block with %d features -> count (not compositional)", .p))

}

if (requireNamespace("survival", quietly = TRUE)) {

  .surv <- data.frame(
    s = I(survival::Surv(rexp(20, 0.02), rbinom(20, 1, 0.7))),
    row.names = paste0("S", 1:20)
  )

  check(.dt(.surv) == "censored", ".detect_data_type() Surv block -> censored")

} else {

  cat("[skipped] censored-block test: package 'survival' not available\n")

}

# =============================================================================
# SECTION 9 - Internal helpers: descriptive statistics
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 9 - Internal helpers: descriptive statistics\n")
cat("=====================================================\n")

.x <- c(1, 2, 3, 4, 5)
.bs <- .compute_basic_stats(.x)

check(.bs$mean == 3, ".compute_basic_stats() mean")
check(.bs$median == 3, ".compute_basic_stats() median")
check(.bs$minimum == 1 && .bs$maximum == 5, ".compute_basic_stats() min/max")
check(.bs$range == 4, ".compute_basic_stats() range")
check(length(.bs$quantiles) == 5, ".compute_basic_stats() returns 5 quantiles")
check(is.na(.compute_basic_stats(numeric(0))$mean),
      ".compute_basic_stats() returns NA for an empty vector")

check(abs(.skewness(c(1, 1, 1, 1, 10))) > 1, ".skewness() detects right skew")
check(abs(.skewness(rnorm(500))) < 0.5, ".skewness() near zero for normal data")
check(is.na(.skewness(c(1, 2))), ".skewness() NA when n < 3")
check(is.na(.skewness(rep(5, 10))), ".skewness() NA for zero variance")

check(!is.na(.kurtosis(rnorm(200))), ".kurtosis() computed for normal data")
check(is.na(.kurtosis(c(1, 2, 3))), ".kurtosis() NA when n < 4")

check(.shapiro_p(rnorm(60)) > 0.01, ".shapiro_p() does not reject normal data")
check(.shapiro_p(rexp(300)) < 0.05, ".shapiro_p() rejects exponential data")
check(is.na(.shapiro_p(c(1, 2))), ".shapiro_p() NA when n < 3")
check(is.na(.shapiro_p(rep(1, 10))), ".shapiro_p() NA for zero variance")
check(!is.na(.shapiro_p(rnorm(6000))), ".shapiro_p() subsamples above 5000 values")

check(length(.outliers_iqr(c(rnorm(40), 100))) == 1, ".outliers_iqr() finds an outlier")
check(length(.outliers_iqr(c(1, 2, 3))) == 0, ".outliers_iqr() no outliers when n < 4")
check(length(.outliers_iqr(c(rep(1, 20), 100))) == 0,
      ".outliers_iqr() declines to flag anything when the IQR is zero")

.sh <- .compute_shape_stats(rnorm(100))
check(setequal(names(.sh), c("skewness", "kurtosis", "shapiro", "normality", "outliers")),
      ".compute_shape_stats() returns the expected fields")
check(.sh$normality %in% c("normal", "non-normal", "unknown"),
      ".compute_shape_stats() reports a normality verdict")

.dp <- .distribution_profile(rnorm(100))
check(all(c("mean", "sd", "variance") %in% names(.dp)),
      ".distribution_profile() adds mean, sd and variance")
check(!is.na(.dp$variance), ".distribution_profile() variance is populated")
check(is.na(.distribution_profile(numeric(0))$variance),
      ".distribution_profile() variance is NA for an empty vector")

.cs <- .compute_composition_stats(matrix(c(0, 1, -1, Inf), nrow = 2))
check(.cs$zero_percent > 0, ".compute_composition_stats() counts zeros")
check(.cs$negative_percent > 0, ".compute_composition_stats() counts negatives")
check(.cs$infinite_percent > 0, ".compute_composition_stats() counts infinities")
check(abs(.cs$sparsity + .cs$density - 1) < 1e-9,
      ".compute_composition_stats() sparsity and density are complementary")
check(is.na(.compute_composition_stats(matrix(numeric(0), nrow = 0))$zero_percent),
      ".compute_composition_stats() handles an empty matrix")

.miss_blk <- data.frame(a = c(1, NA, 3), b = c(NA, NA, 6), row.names = paste0("S", 1:3))
.ms <- .compute_missing_stats(.miss_blk)
check(.ms$missing_values == 3, ".compute_missing_stats() counts missing values")
check(abs(.ms$missing_percent - 50) < 1e-9, ".compute_missing_stats() missing percentage")
check(identical(.ms$empty_samples, "S2"), ".compute_missing_stats() finds empty samples")
check(length(.ms$empty_features) == 0, ".compute_missing_stats() no empty features here")

# =============================================================================
# SECTION 10 - Internal helpers: feature quality and multivariate
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 10 - Internal helpers: feature quality and multivariate\n")
cat("=====================================================\n")

.fq <- data.frame(
  const = rep(1, 30),
  near = c(rep(1, 29), 2),   # 96.7% identical: above the strict >95% threshold
  ok = rnorm(30),
  row.names = paste0("S", 1:30)
)
.fq$dup <- .fq$ok

check(length(.near_constant_features(
  data.frame(borderline = c(rep(1, 19), 2))
)) == 0, ".near_constant_features() threshold is strict (exactly 95% does not qualify)")

check(identical(.constant_features(.fq), "const"), ".constant_features() finds constants")
check(identical(.near_constant_features(.fq), "near"),
      ".near_constant_features() finds near-constants and excludes true constants")
check(identical(.duplicated_features(.fq), "dup"), ".duplicated_features() finds duplicates")

.dsamp <- data.frame(a = c(1, 1, 2), b = c(5, 5, 7), row.names = c("S1", "S2", "S3"))
check(identical(.duplicated_samples(.dsamp), "S2"), ".duplicated_samples() finds duplicates")
check(length(.duplicated_samples(.dsamp[1, , drop = FALSE])) == 0,
      ".duplicated_samples() handles a single-row block")

.mv <- .compute_multivariate(as.data.frame(matrix(rnorm(30 * 5), nrow = 30)))
check(!is.null(.mv$correlation) && nrow(.mv$correlation) == 5,
      ".compute_multivariate() computes a correlation matrix")
check(!is.null(.mv$covariance), ".compute_multivariate() computes a covariance matrix")
check(!is.null(.mv$pca), ".compute_multivariate() computes a PCA")
check(abs(sum(.mv$explained_variance) - 1) < 1e-9,
      ".compute_multivariate() explained variance sums to 1")

.mv_small <- .compute_multivariate(data.frame(a = 1:2))
check(is.null(.mv_small$pca), ".compute_multivariate() returns NULL PCA when too small")

.mv_wide <- .compute_multivariate(
  as.data.frame(matrix(rnorm(20 * 300), nrow = 20)), max_features = 50
)
check(nrow(.mv_wide$correlation) == 50,
      ".compute_multivariate() caps the number of features by variance")

# =============================================================================
# SECTION 11 - Internal helpers: transformations
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 11 - Internal helpers: transformations\n")
cat("=====================================================\n")

check(min(.t_shift_positive(c(-5, 0, 5))) > 0, ".t_shift_positive() makes values positive")
check(identical(.t_shift_positive(c(1, 2, 3)), c(1, 2, 3)),
      ".t_shift_positive() leaves positive data untouched")

.pos <- c(1, 10, 100)
check(all(abs(.t_log(.pos) - log(.pos)) < 1e-9), ".t_log() natural log")
check(all(abs(.t_log(.pos, base = 2) - log2(.pos)) < 1e-9), ".t_log() base 2")
check(all(abs(.t_log(.pos, base = 10) - c(0, 1, 2)) < 1e-9), ".t_log() base 10")
check(all(is.finite(.t_log(c(-1, 0, 1)))), ".t_log() shifts non-positive input")

check(all(abs(.t_sqrt(c(0, 4, 9)) - c(0, 2, 3)) < 1e-9), ".t_sqrt() square root")
check(abs(.t_cuberoot(-8) + 2) < 1e-9, ".t_cuberoot() preserves sign")
check(all(is.finite(.t_vst(c(0, 1, 10)))), ".t_vst() variance-stabilizing transform")

check(identical(as.numeric(.t_rank(c(10, 30, 20))), c(1, 3, 2)), ".t_rank() ranks values")
check(is.na(.t_rank(c(1, NA, 3))[2]), ".t_rank() keeps NA in place")
check(abs(mean(.t_quantile(rlnorm(200)))) < 0.2,
      ".t_quantile() maps to an approximately standard normal")
check(abs(stats::median(.t_robust(c(1, 2, 3, 4, 100)))) < 1e-9,
      ".t_robust() centres on the median")
check(.t_robust(rep(3, 5))[1] == 0, ".t_robust() survives zero MAD")

.bc <- .t_boxcox(rlnorm(100))
check(is.finite(.bc$lambda), ".t_boxcox() estimates lambda")
check(abs(.skewness(.bc$x)) < abs(.skewness(rlnorm(100))) + 1,
      ".t_boxcox() returns a transformed vector")

.yj <- .t_yeojohnson(c(rnorm(50), -rlnorm(50)))
check(is.finite(.yj$lambda), ".t_yeojohnson() estimates lambda on data with negatives")
check(abs(.yj_transform(0, 1)) < 1e-9, ".yj_transform() maps 0 to 0 at lambda = 1")
check(is.na(.yj_transform(NA, 1)), ".yj_transform() propagates NA")
check(is.finite(.yj_transform(-3, 2)), ".yj_transform() handles the lambda = 2 branch")
check(is.finite(.yj_transform(3, 0)), ".yj_transform() handles the lambda = 0 branch")

.cm <- as.data.frame(matrix(rgamma(30 * 4, 2), nrow = 30))
.cm <- .cm / rowSums(.cm)

check(all(abs(rowSums(.t_clr(.cm))) < 1e-9), ".t_clr() rows sum to zero")
check(ncol(.t_alr(.cm)) == ncol(.cm) - 1, ".t_alr() drops the reference part")
check(ncol(.t_ilr(.cm)) == ncol(.cm) - 1, ".t_ilr() returns D-1 balances")

cat("\n.candidate_transformations() per data type\n")

for (.type in c("continuous", "count", "proportion", "compositional",
                "binary", "ordinal", "censored", "mixed", "unknown_type")) {

  .cands <- .candidate_transformations(.type)

  check("identity" %in% .cands,
        sprintf(".candidate_transformations('%s') always offers identity", .type))

}

check(all(c("clr", "alr", "ilr") %in% .candidate_transformations("compositional")),
      ".candidate_transformations() offers log-ratio methods for compositional data")
check(identical(.candidate_transformations("binary"), "identity"),
      ".candidate_transformations() leaves binary data alone")

cat("\n.apply_transformation() over every method\n")

.tblk <- data.frame(A = rlnorm(30), B = rlnorm(30, 1, 2),
                    row.names = paste0("S", 1:30))

for (.m in c("identity", "log", "log2", "log10", "sqrt", "cuberoot", "vst",
             "rank", "quantile", "robust", "boxcox", "yeojohnson")) {

  .res <- .apply_transformation(.tblk, .m)

  check(is.data.frame(.res$block) && nrow(.res$block) == 30,
        sprintf(".apply_transformation('%s') returns a well-formed block", .m))

}

check(length(.apply_transformation(.tblk, "boxcox")$info$parameters) == 2,
      ".apply_transformation('boxcox') stores one lambda per feature")
check(length(.apply_transformation(.tblk, "yeojohnson")$info$parameters) == 2,
      ".apply_transformation('yeojohnson') stores one lambda per feature")
check(identical(.apply_transformation(.tblk, "identity")$block, .tblk),
      ".apply_transformation('identity') is a no-op")

.comp_blk <- as.data.frame(matrix(rgamma(20 * 4, 2), nrow = 20))
.comp_blk <- .comp_blk / rowSums(.comp_blk)
rownames(.comp_blk) <- paste0("S", 1:20)

check(ncol(.apply_transformation(.comp_blk, "clr")$block) == 4,
      ".apply_transformation('clr') preserves dimensionality")
check(ncol(.apply_transformation(.comp_blk, "alr")$block) == 3,
      ".apply_transformation('alr') drops one column")
check(all(grepl("^ILR", names(.apply_transformation(.comp_blk, "ilr")$block))),
      ".apply_transformation('ilr') renames columns to balances")

check(identical(
  .apply_transformation(data.frame(x = letters[1:5]), "log")$block,
  data.frame(x = letters[1:5])
), ".apply_transformation() leaves a non-numeric block untouched")

# =============================================================================
# SECTION 12 - Internal helpers: benchmark scoring
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 12 - Internal helpers: benchmark scoring\n")
cat("=====================================================\n")

.tm <- .transformation_metrics(rlnorm(200), log(rlnorm(200)))
check(all(c("skewness", "kurtosis", "variance", "outliers_pct", "normality_p",
            "stability", "order_preservation") %in% names(.tm)),
      ".transformation_metrics() returns every metric")

.score_bad <- .composite_score(list(skewness = 8, kurtosis = 20, variance = 1,
                                    outliers_pct = 40, normality_p = 1e-9,
                                    stability = 0.1, order_preservation = 0.2))
.score_good <- .composite_score(list(skewness = 0, kurtosis = 0, variance = 1,
                                     outliers_pct = 0, normality_p = 0.9,
                                     stability = 0.9, order_preservation = 1))

check(.score_good > .score_bad, ".composite_score() rewards better distributions")
check(.score_good <= 100 && .score_bad >= 0, ".composite_score() stays within 0-100")

.score_sup <- .composite_score(
  list(skewness = 0, kurtosis = 0, variance = 1, outliers_pct = 0,
       normality_p = 0.9, stability = 0.9, order_preservation = 1),
  outcome_perf = 1
)
check(.score_sup != .score_good,
      ".composite_score() weights differ when an outcome is supplied")

set.seed(21)
.om <- matrix(rnorm(60 * 4), nrow = 60)
check(!is.na(.outcome_performance(.om, list(type = "continuous", y = rnorm(60)))),
      ".outcome_performance() scores a continuous outcome")
check(!is.na(.outcome_performance(.om, list(type = "binary", y = rbinom(60, 1, 0.5)))),
      ".outcome_performance() scores a binary outcome")
check(!is.na(.outcome_performance(.om, list(type = "survival",
                                            time = rexp(60, 0.02),
                                            status = rbinom(60, 1, 0.7)))),
      ".outcome_performance() scores a survival outcome")
check(is.na(.outcome_performance(.om, NULL)),
      ".outcome_performance() returns NA without an outcome")
check(is.na(.outcome_performance(.om[1:5, , drop = FALSE],
                                 list(type = "continuous", y = rnorm(5)))),
      ".outcome_performance() returns NA when n is too small for CV")

.disc <- .discover_outcome(omics, "transcriptomics")
check(!is.null(.disc) && .disc$type == "survival",
      ".discover_outcome() finds the time/status pair in metadata")
check(is.null(.discover_outcome(load_data(list(a = clinical)), "a")),
      ".discover_outcome() returns NULL without metadata")

.bench <- .benchmark_transformations("t", .tblk, "continuous")
check(inherits(.bench, "TransformationRecommendation"),
      ".benchmark_transformations() returns the right class")
check(nrow(.bench$ranking) == nrow(.bench$metrics),
      ".benchmark_transformations() ranking and metrics agree in size")
check(!is.unsorted(rev(.bench$ranking$score)),
      ".benchmark_transformations() ranking is sorted by descending score")
check(.bench$ranking$transformation[1] == .bench$recommended,
      ".benchmark_transformations() recommends the top-ranked method")
check(length(.bench$justification) > 0,
      ".benchmark_transformations() explains its choice")
check(all(c("mean", "sd", "variance") %in% names(.bench$after)),
      ".benchmark_transformations() stores a full after-profile")
check(.benchmark_transformations("b", data.frame(x = letters[1:5]), "mixed")$recommended == "identity",
      ".benchmark_transformations() falls back to identity when nothing can be evaluated")

# =============================================================================
# SECTION 13 - Internal helpers: quality score and recipe construction
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 13 - Internal helpers: quality score and recipes\n")
cat("=====================================================\n")

.clean <- .build_block_diagnostics("clean", as.data.frame(matrix(rnorm(40 * 6), nrow = 40)))
.dirty_blk <- as.data.frame(matrix(rnorm(40 * 6), nrow = 40))
.dirty_blk[1:20, 1] <- NA
.dirty_blk[[2]] <- rep(1, 40)
.dirty <- .build_block_diagnostics("dirty", .dirty_blk)

check(.compute_quality_score(.clean) > .compute_quality_score(.dirty),
      ".compute_quality_score() penalizes a dirty block")
check(.compute_quality_score(.clean) <= 100 && .compute_quality_score(.dirty) >= 0,
      ".compute_quality_score() stays within 0-100")

check(inherits(.clean, "BlockDiagnostics"),
      ".build_block_diagnostics() returns the right class")
check(.clean$observations == 40 * 6,
      ".build_block_diagnostics() counts observations")

cat("\nStage order\n")
check(identical(
  which(.stage_order("count") == "normalization") < which(.stage_order("count") == "transformation"),
  TRUE
), ".stage_order() normalizes before transforming for count data")
check(
  which(.stage_order("continuous") == "transformation") <
    which(.stage_order("continuous") == "normalization"),
  ".stage_order() transforms before normalizing for continuous data"
)

cat("\nNormalization coherence\n")
check(.resolve_normalization("count", 40, "rank") == "total_sum_scaling",
      ".resolve_normalization() keeps library-size normalization for counts")
check(.resolve_normalization("continuous", 40, "rank") == "none",
      ".resolve_normalization() drops quantile normalization after a rank transform")
check(.resolve_normalization("continuous", 40, "quantile") == "none",
      ".resolve_normalization() drops quantile normalization after a quantile transform")
check(.resolve_normalization("continuous", 40, "log") == "quantile",
      ".resolve_normalization() keeps quantile normalization for other transforms")
check(.resolve_normalization("continuous", 5, "log") == "none",
      ".resolve_normalization() skips normalization for low-dimensional blocks")
check(.resolve_normalization("compositional", 40, "clr") == "none",
      ".resolve_normalization() leaves compositional data closed")
check(.resolve_normalization("continuous", 40, NULL) == "quantile",
      ".resolve_normalization() tolerates a NULL transformation")

cat("\nScaling coherence\n")
check(.resolve_scaling("continuous", "quantile", 0) == "none",
      ".resolve_scaling() skips scaling after a quantile transform")
check(.resolve_scaling("continuous", "robust", 0) == "none",
      ".resolve_scaling() skips scaling after a robust transform")
check(.resolve_scaling("continuous", "log", 0.5) == "robust",
      ".resolve_scaling() uses robust scaling when outliers are frequent")
check(.resolve_scaling("continuous", "log", 0) == "z-score",
      ".resolve_scaling() uses z-score scaling otherwise")
check(.resolve_scaling("binary", "identity", 0) == "none",
      ".resolve_scaling() leaves binary data unscaled")

.rec <- .build_recipe("t", .clean, .bench, metadata = metadata)
check(inherits(.rec, "PreprocessingRecipe"), ".build_recipe() returns the right class")
check(!is.na(.rec$estimated_quality), ".build_recipe() fills estimated_quality")
check(!is.na(.rec$expected_variance), ".build_recipe() fills expected_variance")
check(any(grepl("stage order", .rec$comments)), ".build_recipe() records the stage order")
check(identical(.rec$batch_variable, "batch"),
      ".build_recipe() picks up the batch variable from metadata")
check(is.null(.build_recipe("t", .clean, .bench, metadata = NULL)$batch),
      ".build_recipe() leaves batch empty without metadata")

cat("\nImputation choice scales with missingness\n")

for (.frac in c(0, 0.03, 0.10, 0.40)) {

  .blk_m <- matrix(rnorm(40 * 6), nrow = 40)
  .n_na <- round(.frac * 240)
  if (.n_na > 0) .blk_m[sample(240, .n_na)] <- NA
  .blk <- as.data.frame(.blk_m)

  .d <- .build_block_diagnostics("m", .blk)
  .r <- .build_recipe("m", .d, .benchmark_transformations("m", .blk, .d$data_type))

  check(!is.null(.r$imputation),
        sprintf(".build_recipe() selects an imputation method at %.0f%% missing (%s)",
                100 * .frac, .r$imputation))

}

# =============================================================================
# SECTION 14 - Internal helpers: QC, tables and recommendations
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 14 - Internal helpers: QC, tables and recommendations\n")
cat("=====================================================\n")

.qc <- .compute_qc_checks(validation, 90, TRUE)
check(is.logical(.qc$passed), ".compute_qc_checks() reports a verdict")
check(length(.qc$passed_checks) > 0, ".compute_qc_checks() records passed checks")
check("outcome_availability" %in% .compute_qc_checks(validation, 90, FALSE)$skipped_checks,
      ".compute_qc_checks() skips the outcome check when no outcome exists")
check("quality_threshold" %in% .compute_qc_checks(validation, 10, TRUE)$failed_checks,
      ".compute_qc_checks() fails the quality check on a low score")

.dt_tab <- .build_diagnostics_table(validation$diagnostics)
check(is.data.frame(.dt_tab) && nrow(.dt_tab) == length(validation$diagnostics),
      ".build_diagnostics_table() one row per block")
check(nrow(.build_diagnostics_table(list())) == 0,
      ".build_diagnostics_table() handles an empty input")

check(nrow(.build_transformations_table(validation$transformations)) ==
        length(validation$transformations),
      ".build_transformations_table() one row per block")
check(nrow(.build_transformations_table(list())) == 0,
      ".build_transformations_table() handles an empty input")

check(nrow(.build_recipes_table(validation$recipes)) == length(validation$recipes),
      ".build_recipes_table() one row per block")
check(nrow(.build_recipes_table(list())) == 0,
      ".build_recipes_table() handles an empty input")

.qc_tab <- .build_qc_table(validation$qc)
check(is.data.frame(.qc_tab) && all(.qc_tab$status %in% c("passed", "failed", "skipped")),
      ".build_qc_table() labels every check")

.recs <- .generate_recommendations(validation$diagnostics, validation$recipes, 90)
check(is.character(.recs$global) && length(.recs$global) > 0,
      ".generate_recommendations() produces global advice")
check(setequal(names(.recs$blocks), names(validation$diagnostics)),
      ".generate_recommendations() covers every block")
check(any(grepl("low", .generate_recommendations(validation$diagnostics,
                                                 validation$recipes, 10)$global)),
      ".generate_recommendations() warns when overall quality is low")

# =============================================================================
# SECTION 15 - Internal helpers: structural validators and plots
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 15 - Internal helpers: structural validators and plots\n")
cat("=====================================================\n")

.vbs <- .validate_block_structure(omics)
check(is.data.frame(.vbs$block_summary), ".validate_block_structure() builds a block summary")
check(nrow(.vbs$block_summary) == length(omics$assays),
      ".validate_block_structure() one summary row per block")
check(length(.vbs$warnings) > 0,
      ".validate_block_structure() reports the seeded missing values")

.vm <- .validate_metadata(omics)
check(length(.vm$errors) == 0, ".validate_metadata() accepts well-formed metadata")
check(length(.validate_metadata(obj_no_id)$errors) > 0,
      ".validate_metadata() rejects metadata without sample_id")
check(length(.validate_metadata(load_data(list(a = clinical)))$errors) == 0,
      ".validate_metadata() tolerates a missing metadata table")

.ovl <- .compute_overlap_matrix(omics)
check(nrow(.ovl) == length(omics$assays) && isSymmetric(.ovl),
      ".compute_overlap_matrix() is square and symmetric")
check(all(diag(.ovl) == vapply(omics$assays, nrow, numeric(1))),
      ".compute_overlap_matrix() diagonal equals each block's sample count")

cat("\nPlot builders (rendered to a null device, never displayed)\n")

check(inherits(.safe_record(function() graphics::plot(1:10)), "recordedplot"),
      ".safe_record() captures a plot")
check(is.null(.safe_record(function() stop("cannot draw"))),
      ".safe_record() returns NULL when drawing fails")

check(inherits(.plot_bar(c(1, 2, 3), c("a", "b", "c"), "t"), "recordedplot"),
      ".plot_bar() renders")

.pblk <- omics$assays$transcriptomics
.pdiag <- validation$diagnostics$transcriptomics

.plot_calls <- list(
  missing_heatmap = function() .plot_missing_heatmap_block("t", .pblk),
  missing_by_sample = function() .plot_missing_by_sample_block("t", .pblk),
  missing_by_feature = function() .plot_missing_by_feature_block("t", .pblk),
  missing_pattern = function() .plot_missing_pattern_block("t", .pblk),
  overlap_heatmap = function() .plot_overlap_heatmap(.ovl),
  overlap_network = function() .plot_overlap_network(.ovl),
  correlation_heatmap = function() .plot_correlation_heatmap_block("t", .pdiag$correlation),
  correlation_distribution = function() .plot_correlation_distribution_block("t", .pdiag$correlation),
  histogram = function() .plot_histogram_block("t", .pool_numeric(.pblk)),
  density = function() .plot_density_block("t", .pool_numeric(.pblk)),
  qqplot = function() .plot_qqplot_block("t", .pool_numeric(.pblk)),
  boxplot = function() .plot_boxplot_block("t", .pblk),
  violin = function() .plot_violin_block("t", .pblk),
  pca = function() .plot_pca_block("t", .pdiag$pca),
  scree = function() .plot_scree_block("t", .pdiag$explained_variance),
  sample_clustering = function() .plot_sample_clustering_block("t", .pblk),
  feature_clustering = function() .plot_feature_clustering_block("t", .pblk),
  outliers = function() .plot_outliers_block("t", .pool_numeric(.pblk), .pdiag$outliers),
  transformation_scores = function() .plot_transformation_scores_block("t", .bench$ranking),
  transformation_comparison = function() .plot_transformation_comparison_block(
    "t", .pool_numeric(.pblk), .pool_numeric(.apply_transformation(.pblk, "log")$block)
  )
)

for (.nm in names(.plot_calls)) {

  .p <- .plot_calls[[.nm]]()

  check(is.null(.p) || inherits(.p, "recordedplot"),
        sprintf(".plot_%s() returns a recorded plot or NULL", .nm))

}

check(inherits(.plot_dendrogram(matrix(rnorm(30), nrow = 10), "d"), "recordedplot"),
      ".plot_dendrogram() renders")
check(is.null(.plot_dendrogram(matrix(1, nrow = 1), "d")),
      ".plot_dendrogram() returns NULL when there is too little data")
check(!is.null(.violin_polygon(rnorm(50), at = 1)), ".violin_polygon() builds a polygon")
check(is.null(.violin_polygon(rep(1, 10), at = 1)),
      ".violin_polygon() returns NULL for zero variance")

check(is.null(.plot_missing_by_sample_block("t", clinical)),
      ".plot_missing_by_sample_block() returns NULL when nothing is missing")
check(is.null(.plot_pca_block("t", NULL)),
      ".plot_pca_block() returns NULL without a PCA")

.plots_all <- .build_plots(omics$assays, validation$diagnostics,
                           validation$transformations, .ovl)
check(setequal(names(.plots_all), names(CMOValidation()$plots)),
      ".build_plots() fills exactly the declared plot slots")

# =============================================================================
# SECTION 16 - Report formatting helpers
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 16 - Report formatting helpers\n")
cat("=====================================================\n")

check(identical(.report_or(NULL, "fb"), "fb"), ".report_or() falls back on NULL")
check(identical(.report_or(character(0), "fb"), "fb"), ".report_or() falls back on empty")
check(identical(.report_or("v", "fb"), "v"), ".report_or() passes values through")

check(grepl("100.0", .report_bar(100)), ".report_bar() renders a full bar")
check(grepl("NA", .report_bar(NA)), ".report_bar() handles NA")
check(grepl("0.0", .report_bar(-50)), ".report_bar() clamps below zero")
check(grepl("100.0", .report_bar(150)), ".report_bar() clamps above 100")

check(.report_grade(95) == "excellent", ".report_grade() excellent")
check(.report_grade(75) == "good", ".report_grade() good")
check(.report_grade(55) == "acceptable", ".report_grade() acceptable")
check(.report_grade(35) == "poor", ".report_grade() poor")
check(.report_grade(10) == "critical", ".report_grade() critical")
check(.report_grade(NA) == "unknown", ".report_grade() unknown")

check(nchar(.report_pad("abc", 10)) == 10, ".report_pad() pads to width")
check(nchar(.report_pad(strrep("x", 30), 10)) == 10, ".report_pad() truncates to width")
check(grepl("~$", .report_pad(strrep("x", 30), 10)), ".report_pad() marks truncation")

run_test("`.report_rule()`", .report_rule(20))
run_test("`.report_title()`", .report_title("Title", 20))
run_test("`.report_section()`", .report_section("Section", 20))
run_test("`.report_bullets()` with items", .report_bullets(c("one", "two")))
run_test("`.report_bullets()` when empty", .report_bullets(character(0)))
run_test("`.report_bullets()` truncated", .report_bullets(letters, max_items = 3))

# =============================================================================
# SECTION 17 - CMOResult methods
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 17 - CMOResult methods\n")
cat("=====================================================\n")

cmo_result <- CMOResult()

cmo_result$data <- omics
cmo_result$validation <- validation
cmo_result$parameters <- list(seed = 1)
cmo_result$history <- "constructed by test_all.R"

run_test("print.CMOResult", print(cmo_result))
run_test("summary.CMOResult", summary(cmo_result))

run_test("print.CMOResult on an empty object", print(CMOResult()))
run_test("summary.CMOResult on an empty object", summary(CMOResult()))

check(inherits(cmo_result, "CMOResult"), "the object dispatches as CMOResult")

# =============================================================================
# SECTION 18 - Coverage audit
# =============================================================================
# Every function defined by the package was wrapped at start-up. This section
# reports which of them were actually reached by the tests above.
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 18 - Coverage audit\n")
cat("=====================================================\n")

if (.cov_enabled) {

  .cov_hit <- ls(.cov_env, all.names = TRUE)
  .cov_missed <- setdiff(.cov_all, .cov_hit)

  cat(sprintf(
    "\nFunctions defined : %d\nFunctions exercised: %d (%.1f%%)\n",
    length(.cov_all), length(.cov_hit),
    100 * length(.cov_hit) / max(length(.cov_all), 1)
  ))

  if (length(.cov_missed) > 0) {

    cat("\nNever called:\n")
    cat(paste0("  - ", sort(.cov_missed), collapse = "\n"), "\n")

  }

  check(length(.cov_missed) == 0, "every package function was exercised at least once")

} else {

  cat("\nCoverage instrumentation was not enabled; audit skipped.\n")

}

# The wrappers are removed here so that the user-facing section below reports
# the real call in every error message ("Error in check_data(...)" rather than
# "Error in orig(...)").

if (.cov_enabled) {

  for (.fn in ls(.cov_orig, all.names = TRUE)) {
    assign(.fn, get(.fn, envir = .cov_orig), envir = globalenv())
  }

  rm(.fn)

  cat("\nInstrumentation removed; the section below shows unwrapped behaviour.\n")

}

# =============================================================================
# SECTION 19 - As a user would see it
# =============================================================================
# No tryCatch-and-summarize here: every call below is echoed and evaluated
# the way a person typing at the R console would experience it, including
# the exact wording of any error or warning.
# =============================================================================

cat("\n=====================================================\n")
cat("SECTION 19 - As a user would see it\n")
cat("=====================================================\n")

cat("\n--- A normal session -------------------------------\n")

user_call(
  my_data <- load_data(
    assays = list(transcriptomics = transcriptomics, proteomics = proteomics,
                  clinical = clinical, microbiome = microbiome),
    metadata = metadata
  )
)

user_call(my_data)
user_call(summary(my_data))

user_call(my_validation <- check_data(my_data))
user_call(my_validation)
user_call(report(my_validation, format = "console"))
user_call(report(my_validation, file = file.path(tempdir(), "user_session_report.html"), open = FALSE))
user_call(summary(my_validation))

user_call(my_validation$diagnostics$transcriptomics)
user_call(my_validation$transformations$microbiome)
user_call(my_validation$recipes$proteomics)

user_call(my_validation$recommendations$blocks$transcriptomics)
user_call(head(my_validation$tables$diagnostics))
user_call(my_validation$qc)

cat("\n--- Common mistakes ---------------------------------\n")

user_call(load_data())
user_call(load_data(assays = "transcriptomics"))
user_call(load_data(assays = list()))
user_call(load_data(assays = list(transcriptomics)))
user_call(load_data(assays = list(clinical = 1:10)))

bad_meta <- metadata
bad_meta$sample_id <- NULL
user_call(broken <- load_data(assays = list(clinical = clinical), metadata = bad_meta))
user_call(check_data(broken))
user_call(check_data(broken)$errors)

user_call(check_data(my_data$assays))
user_call(check_data("my_data"))
user_call(check_data())

user_call(report(my_data))
user_call(report(my_validation, sections = "everything"))

cat("\n--- Does check_data() ever raise a real R warning? --\n")

no_condition_warnings <- withCallingHandlers(
  { check_data(my_data); TRUE },
  warning = function(w) { cat("(a real R warning was raised: ", conditionMessage(w), ")\n", sep = ""); invokeRestart("muffleWarning") }
)

cat(
  if (isTRUE(no_condition_warnings)) {
    "No base-R warning() conditions were raised; issues are returned as text in validation$warnings instead:\n"
  } else {
    "See above.\n"
  }
)

user_call(my_validation$warnings)

# =============================================================================
# FINAL WORKFLOW SUMMARY
# =============================================================================

cat("\n=====================================================\n")
cat("WORKFLOW SUMMARY\n")
cat("=====================================================\n")
cat(sprintf("Total Blocks Loaded: %d\n", length(omics$assays)))
cat(sprintf("Validation Passed: %s\n", validation$valid))
cat(sprintf("Quality Score: %.2f\n", validation$score))
cat(sprintf("Plots Generated: %d\n", length(Filter(Negate(is.null), validation$plots))))
cat(sprintf("run_test(): %d OK / %d ERROR\n", .n_ok, .n_error))
cat(sprintf("check()/expect_error(): %d passed / %d failed\n", .n_checks_passed, .n_checks_failed))

if (.n_error == 0L && .n_checks_failed == 0L) {
  cat("\nALL TESTS PASSED\n")
} else {
  cat("\nSOME TESTS FAILED - see log above\n")
}
