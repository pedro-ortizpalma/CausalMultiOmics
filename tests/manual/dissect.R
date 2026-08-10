# =============================================================================
# CausalMultiOmics - DISSECTION
# =============================================================================
#
# The walkthrough runs the package the way a user would. This runs it the way
# an author would: opening each function, printing the code that is about to
# execute, executing it, and printing everything that comes back.
#
#   Rscript tests/manual/dissect.R
#
# or, from the package root:
#
#   source("tests/manual/dissect.R")
#
# Only some parts:
#
#   CMO_PARTS <- c(1, 6); source("tests/manual/dissect.R")
#
# -----------------------------------------------------------------------------
# Why this calls the internals rather than pasting the function body
# -----------------------------------------------------------------------------
#
# analyze() is around seven hundred lines. Pasting them here would produce a
# copy that stops matching the real function the first time either changes,
# and a transcript showing code that is not the code that runs is worse than
# no transcript at all.
#
# So each step calls the real internal helper, in the order analyze() calls
# it, and prints that helper's own source immediately before running it. You
# see the code, you see it execute, and what you see is what is installed.
# The section headings below are analyze()'s own.
#
# A full transcript goes to tests/manual/output/dissect.txt, and every figure
# to the same directory.
# =============================================================================

CMO_START <- Sys.time()

if (!exists("CMO_PARTS")) CMO_PARTS <- NULL

# -----------------------------------------------------------------------------
# Load the package with its internals visible
# -----------------------------------------------------------------------------
# Almost everything below is a dotted internal, so export_all must be on.
# A plain library() call would make the whole script fail at the first step.

.here <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE)),
  error = function(e) NA_character_)

.root <- if (!is.na(.here) && nzchar(.here)) dirname(dirname(.here)) else "."

if (requireNamespace("devtools", quietly = TRUE)) {
  suppressMessages(devtools::load_all(.root, export_all = TRUE, quiet = TRUE))
} else {
  library(CausalMultiOmics)
}

# Pasting this file into a session holding an older build silently exercises
# the old code and reports it as the new. Better to stop.

.missing <- Filter(function(f) !exists(f), c(
  "load_data", "check_data", "preprocess", "analyze", "simulate_data",
  "sensitivity", "compare_results", "export_graph", "hypothesis",
  "analysis_assumptions", "analysis_control", ".align_blocks",
  ".integrate_evidence", ".positivity"))

if (length(.missing) > 0) {
  stop("The loaded CausalMultiOmics is older than this script.\n  missing: ",
       paste(.missing, collapse = ", "),
       "\n  Reload it:  devtools::load_all(\".\", export_all = TRUE)",
       call. = FALSE)
}

.outdir <- file.path(.root, "tests", "manual", "output")
dir.create(.outdir, showWarnings = FALSE, recursive = TRUE)

.transcript <- file.path(.outdir, "dissect.txt")

.con <- file(.transcript, open = "wt")
sink(.con, split = TRUE)
on.exit({ sink(); close(.con) }, add = TRUE)

# =============================================================================
# Helpers
# =============================================================================

.ok <- 0L
.bad <- 0L
.failures <- character(0)

rule <- function(ch = "=", w = 78) cat(strrep(ch, w), "\n", sep = "")

part <- function(n, title) {

  if (!is.null(CMO_PARTS) && !(n %in% CMO_PARTS)) return(FALSE)

  cat("\n\n"); rule("=")
  cat(sprintf("PART %-3s %s\n", n, title))
  rule("=")

  TRUE

}

step <- function(...) {
  cat("\n"); rule("-")
  cat("  ", ..., "\n", sep = "")
  rule("-")
}

#' A short note on what is happening and why.
why <- function(...) {
  cat("\n")
  cat(paste(strwrap(paste0(...), width = 76, prefix = "  # "),
            collapse = "\n"), "\n", sep = "")
}

#' Print a function's own source, so the code below is visibly the code that
#' runs rather than a transcription of it.
code <- function(f, lines = 40) {

  name <- deparse(substitute(f))

  cat("\n  ---- source of ", name, " ----\n", sep = "")

  src <- tryCatch(deparse(f), error = function(e) NULL)

  if (is.null(src)) { cat("    (not available)\n"); return(invisible(NULL)) }

  shown <- utils::head(src, lines)

  cat(paste0("  | ", shown), sep = "\n")

  if (length(src) > lines) {
    cat(sprintf("  | ... %d more line(s); the whole thing is in R/\n",
                length(src) - lines))
  }

  cat("\n")

  invisible(NULL)

}

#' Echo a call, run it, print what comes back.
run <- function(expr, print_it = TRUE) {

  label <- paste(deparse(substitute(expr)), collapse = "\n      ")

  cat("\n> ", label, "\n", sep = "")

  value <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      cat("  [warning] ", conditionMessage(w), "\n", sep = "")
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      cat("  [ERROR] ", conditionMessage(e), "\n", sep = "")
      .bad <<- .bad + 1L
      .failures <<- c(.failures, label)
      structure(list(), class = "cmo_failed")
    })

  if (inherits(value, "cmo_failed")) return(invisible(NULL))

  .ok <<- .ok + 1L

  if (print_it && !is.null(value)) print(value)

  invisible(value)

}

#' Assert a call fails, and show the message the user would see.
refuses <- function(expr, expect = NULL) {

  label <- paste(deparse(substitute(expr)), collapse = " ")

  cat("\n> ", label, "\n", sep = "")

  err <- tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))

  if (is.null(err)) {
    cat("  [FAIL] expected an error, none came\n")
    .bad <<- .bad + 1L
    .failures <<- c(.failures, paste("no error:", label))
    return(invisible(FALSE))
  }

  if (!is.null(expect) && !grepl(expect, err)) {
    cat("  [FAIL] message did not mention '", expect, "':\n    ",
        strsplit(err, "\n")[[1]][1], "\n", sep = "")
    .bad <<- .bad + 1L
    .failures <<- c(.failures, paste("wrong message:", label))
    return(invisible(FALSE))
  }

  cat("  [ok] ", strsplit(err, "\n")[[1]][1], "\n", sep = "")
  .ok <<- .ok + 1L

  invisible(TRUE)

}

#' Walk an object and print every slot, one level deep, so nothing hides.
inside <- function(x, name = deparse(substitute(x)), depth = 1L) {

  cat("\n  ==== every slot of ", name, " (", paste(class(x), collapse = "/"),
      ") ====\n", sep = "")

  if (!is.list(x)) { print(utils::head(x, 20)); return(invisible(NULL)) }

  for (nm in names(x)) {

    v <- x[[nm]]

    kind <- paste(class(v), collapse = "/")

    size <- if (is.data.frame(v)) sprintf("%d x %d", nrow(v), ncol(v))
    else if (is.list(v)) sprintf("%d element(s)", length(v))
    else sprintf("length %d", length(v))

    cat(sprintf("\n  $%-24s %-16s %s\n", nm, kind, size))

    if (is.null(v) || length(v) == 0) next

    if (is.data.frame(v)) {
      print(utils::head(v, 4))
    } else if (is.list(v)) {
      if (depth > 0L) {
        for (sub in utils::head(names(v), 6)) {
          cat(sprintf("      $%-20s %s\n", sub,
                      paste(class(v[[sub]]), collapse = "/")))
        }
      }
    } else if (is.function(v)) {
      cat("      (function)\n")
    } else {
      print(utils::head(v, 8))
    }

  }

  invisible(NULL)

}

#' Save a recorded plot and say where it went, so figures are not lost when
#' the script is run without a screen.
figure <- function(p, name) {

  if (is.null(p)) { cat("  (no figure)\n"); return(invisible(NULL)) }

  path <- file.path(.outdir, paste0(name, ".png"))

  grDevices::png(path, width = 1100, height = 750, res = 110)
  ok <- tryCatch({ print(p); TRUE }, error = function(e) FALSE)
  grDevices::dev.off()

  cat("  figure -> ", path, if (ok) "" else "  [failed]", "\n", sep = "")

  invisible(path)

}

cat("\n"); rule("=")
cat("CausalMultiOmics - DISSECTION\n")
rule("=")
cat("\n  package    : ",
    tryCatch(as.character(utils::packageVersion("CausalMultiOmics")),
             error = function(e) "development"), "\n", sep = "")
cat("  R          : ", R.version.string, "\n", sep = "")
cat("  transcript : ", .transcript, "\n", sep = "")
cat("  figures    : ", .outdir, "\n", sep = "")


# =============================================================================
# PART 1 - A study with every kind of relationship in it
# =============================================================================

if (part(1, "The data, and every relationship planted in it")) {

  why("Real data has no known answer, so nothing it produces can be checked. ",
      "Everything below is built with simulate_data(), which records what it ",
      "planted, so each output can be read against the truth rather than ",
      "admired.")

  code(simulate_data, 55)

  step("The causal structure")

  why("Six variables in a shape that contains one of everything worth ",
      "getting wrong. age is a CONFOUNDER of diet and disease. inflammation ",
      "is a MEDIATOR: diet acts through it. biomarker is downstream of the ",
      "disease, so adjusting for it is a COLLIDER and manufactures ",
      "association out of nothing. And exercise affects the disease ",
      "independently of all of it.")

  STRUCTURE <- data.frame(
    from   = c("age", "age", "diet", "inflammation", "disease", "exercise"),
    to     = c("diet", "disease", "inflammation", "disease", "biomarker",
               "disease"),
    effect = c(0.5, 0.45, 0.80, 0.70, 0.90, -0.55),
    stringsAsFactors = FALSE
  )

  run(STRUCTURE)

  step("Building it")

  why("Two blocks so alignment has work to do, a latent process spanning ",
      "both so modules have something to find, missingness so imputation ",
      "and the data-quality discount have something to bite on, two batches, ",
      "and a laboratory block measured on only 70% of people so the shared ",
      "sample count is a real number rather than everyone.")

  SIM <- run(simulate_data(
    n = 500,
    blocks = list(
      clinical = c("diet", "inflammation", "biomarker", "exercise"),
      lab = c(paste0("assay_", 1:6), paste0("noise_", 1:4))
    ),
    dag = STRUCTURE,
    outcome = "disease",
    outcome_type = "binary",
    modules = list(shared_process = c(paste0("assay_", 1:6))),
    missing = c(lab = 0.12),
    missing_pattern = "random",
    batch = 2,
    coverage = c(lab = 0.7),
    seed = 42
  ))

  step("What was planted")

  inside(SIM$misc$simulation, "SIM$misc$simulation")

  step("A second cohort, for replication, and a longitudinal one")

  why("test_hypothesis() needs a cohort the claim has never seen, and the ",
      "mixed-model generator needs repeated measures. Both are built here ",
      "so later parts have them.")

  VALID <- run(simulate_data(
    n = 400,
    blocks = list(clinical = c("diet", "inflammation", "biomarker",
                               "exercise"),
                  lab = c(paste0("assay_", 1:6), paste0("noise_", 1:4))),
    dag = STRUCTURE, outcome = "disease", outcome_type = "binary",
    seed = 77), print_it = FALSE)

  SURV <- run(simulate_data(
    n = 400,
    blocks = list(clinical = c("diet", "exercise"), lab = 6),
    dag = data.frame(from = c("diet", "exercise"), to = "died",
                     effect = c(0.7, -0.5)),
    outcome = "died", outcome_type = "survival", seed = 5),
    print_it = FALSE)

  step("Categorical and repeated-measures columns, added by hand")

  why("simulate_data() makes numbers. A factor with several levels and a ",
      "subject identifier have to be added so the categorical handling and ",
      "the longitudinal design can be exercised at all.")

  set.seed(1)

  n <- nrow(SIM$metadata)

  SIM$metadata$sex <- factor(sample(c("female", "male"), n, TRUE))
  SIM$metadata$smoking <- factor(sample(c("never", "former", "current"), n,
                                        TRUE, prob = c(.5, .3, .2)))
  SIM$metadata$centre <- factor(sample(paste0("C", 1:4), n, TRUE))
  SIM$metadata$subject <- paste0("P", rep(seq_len(n / 2), each = 2))
  SIM$metadata$visit <- rep(c(1, 2), times = n / 2)
  SIM$metadata$other_cause <- rbinom(n, 1, 0.15)

  run(utils::str(SIM$metadata), print_it = FALSE)

  assign("STRUCTURE", STRUCTURE, envir = globalenv())
  assign("SIM", SIM, envir = globalenv())
  assign("VALID", VALID, envir = globalenv())
  assign("SURV", SURV, envir = globalenv())

}


# =============================================================================
# PART 2 - Every class, empty, every slot named
# =============================================================================

if (part(2, "Every class and every slot it declares")) {

  why("A constructor with no arguments is the class definition made ",
      "executable: it names every slot the rest of the package may fill. ",
      "Anything missing here is a field that will be NULL forever.")

  for (cls in c("MultiOmicsData", "BlockDiagnostics",
                "TransformationRecommendation", "PreprocessingRecipe",
                "PreprocessingResult", "CMOValidation", "EvidenceEdge",
                "EvidenceGraph", "ConsensusGraph", "ModuleGraph",
                "Hypothesis", "CMOResult")) {

    step(cls)

    obj <- get(cls)()

    cat("  class:", paste(class(obj), collapse = "/"),
        " slots:", length(obj), "\n\n")

    cat(paste(strwrap(paste(names(obj), collapse = ", "), width = 74,
                      prefix = "    "), collapse = "\n"), "\n", sep = "")

  }

  step("The option groups")

  why("These two are not data containers but decisions: assume() is where ",
      "the caller is accountable, control() is where they are impatient.")

  run(analysis_assumptions())
  run(analysis_control())

  step("Printing an empty object must not crash")

  why("A half-finished analysis is represented by empty slots, so print ",
      "methods meet them constantly.")

  for (cls in c("EvidenceEdge", "EvidenceGraph", "ConsensusGraph",
                "ModuleGraph", "Hypothesis", "CMOResult", "CMOValidation")) {
    run(print(get(cls)()), print_it = FALSE)
  }

}


# =============================================================================
# PART 3 - load_data(), opened up
# =============================================================================

if (part(3, "load_data(): the container, step by step")) {

  code(load_data, 60)

  step("What it does with the blocks")

  why("Blocks arrive as a named list and need not share samples. What they ",
      "must have is identifiers in their row names: without them blocks were ",
      "labelled by position, which made unrelated blocks appear to share ",
      "every sample.")

  obj <- run(load_data(SIM$assays, metadata = SIM$metadata,
                       modality = c(clinical = "clinical", lab = "proteomics")))

  step("The row-name rule, in the code")

  code(.has_sample_ids)

  run(.has_sample_ids(SIM$assays$clinical))
  run(.has_sample_ids(matrix(1:4, 2)))

  step("Every slot of what came back")

  inside(obj, "obj")

  step("Modality, and why it cannot be inferred")

  why("RNA-seq counts and any other counts are identical as numbers, so the ",
      "statistical type cannot recover what a block is. Modality is what ",
      "lets check_data() recommend a normalisation that suits it.")

  run(obj$modality)

  step("Both display methods")

  run(print(obj), print_it = FALSE)
  run(summary(obj), print_it = FALSE)

  step("Every refusal")

  refuses(load_data(list()), "is empty")
  refuses(load_data("not a list"), "must be a list")
  refuses(load_data(list(SIM$assays$clinical)), "named")
  refuses(load_data(list(a = matrix(1:4, 2))), "no sample identifiers")
  refuses(load_data(list(a = SIM$assays$clinical), modality = c(nope = "x")),
          "not in 'assays'")

  assign("OBJ", obj, envir = globalenv())

}


# =============================================================================
# PART 4 - check_data(), opened up
# =============================================================================

if (part(4, "check_data(): the audit, step by step")) {

  obj <- get("OBJ", envir = globalenv())

  why("check_data() changes nothing. It profiles each block, benchmarks ",
      "candidate transformations, and derives a preprocessing recipe with ",
      "its reasoning attached. Everything comes back inside one object.")

  step("Step 1 - what kind of numbers is each block?")

  code(.detect_data_type)

  why("The type decides which transformations are even candidates. Constant ",
      "columns are excluded from the vote: a column of 1s reads as a ",
      "proportion and a column of 7s as a count, and either drags an ",
      "otherwise homogeneous block to 'mixed'.")

  for (b in names(obj$assays)) {
    cat(sprintf("  %-10s -> %s\n", b, .detect_data_type(obj$assays[[b]])))
  }

  step("Step 2 - profile one block")

  code(.distribution_profile, 25)

  run(utils::str(.safe_try(.distribution_profile(
    as.matrix(obj$assays$lab)), NULL), max.level = 1), print_it = FALSE)

  step("Step 3 - benchmark the transformations")

  why("Every candidate is applied and scored on normality, variance ",
      "stabilisation and order preservation. Order preservation is reported ",
      "as NA when a transformation changes the shape of the block, rather ",
      "than correlating unrelated cells by position.")

  code(.benchmark_transformations, 30)

  step("Step 4 - the whole audit")

  QUALITY <- run(check_data(obj), print_it = FALSE)

  run(print(QUALITY), print_it = FALSE)

  step("Every slot of the validation")

  inside(QUALITY, "QUALITY")

  step("Per block: diagnostics, transformations, recipe")

  inside(QUALITY$diagnostics$lab, "QUALITY$diagnostics$lab")

  run(QUALITY$transformations$lab$ranking)
  run(QUALITY$transformations$lab$recommended)
  run(QUALITY$transformations$lab$justification)

  inside(QUALITY$recipes$lab, "QUALITY$recipes$lab")

  step("Sample overlap: the two figures, and why the second is the one")

  why("The matrix is pairwise. An analysis needs the people present in every ",
      "block at once, and that can be far smaller than any pair suggests: ",
      "every pair of twenty blocks can share the whole cohort while the ",
      "twenty together share none of it.")

  run(QUALITY$summary$overlap)
  run(QUALITY$summary$shared_by_all)
  run(QUALITY$summary$cumulative_overlap)

  step("Quality control checks")

  run(QUALITY$qc)
  run(QUALITY$errors)
  run(QUALITY$warnings)
  run(QUALITY$recommendations)

  step("The report, in both formats")

  run(report(QUALITY, format = "console"), print_it = FALSE)

  run(report(QUALITY, file = file.path(.outdir, "dissect-validation.html"),
             prompt = FALSE, quiet = TRUE), print_it = FALSE)

  step("Diagnostic figures")

  for (nm in names(QUALITY$plots)) {
    figure(QUALITY$plots[[nm]], paste0("dissect-check-", nm))
  }

  step("Refusals")

  refuses(check_data("not an object"), "MultiOmicsData")

  assign("QUALITY", QUALITY, envir = globalenv())

}


# =============================================================================
# PART 5 - preprocess(), opened up
# =============================================================================

if (part(5, "preprocess(): executing the plan, step by step")) {

  obj <- get("OBJ", envir = globalenv())
  quality <- get("QUALITY", envir = globalenv())

  why("preprocess() derives nothing of its own. Every method, parameter and ",
      "threshold comes from the recipe, and the order of stages comes from ",
      "the recipe's stage_order. It is an executor, not a decision-maker.")

  step("The registry it executes from")

  code(.method_registry, 20)

  reg <- .method_registry()

  for (stage in names(reg)) {
    cat(sprintf("\n  %-20s %s\n", stage,
                paste(names(reg[[stage]]), collapse = ", ")))
  }

  step("One method, opened")

  why("Every method is a fit/apply pair: it returns a fitted model so the ",
      "same decision can be replayed on a second cohort. A method that ",
      "cannot express its decision as a transferable model is refused ",
      "rather than silently re-fitted.")

  code(.get_method)

  run(names(reg$imputation$knn))

  step("The order stages run in")

  code(.stage_order)

  run(.stage_order(quality$recipes$lab))

  step("Running it")

  CLEAN <- run(preprocess(obj, quality, plots = TRUE, quiet = FALSE),
               print_it = FALSE)

  run(print(CLEAN), print_it = FALSE)
  run(summary(CLEAN), print_it = FALSE)

  step("Every slot of the result")

  inside(CLEAN, "CLEAN")

  step("Every step that ran, as a record")

  for (s in CLEAN$steps) {
    cat(sprintf("  %-10s %-18s %-20s %4d x %-4d -> %4d x %-4d  %.3fs\n",
                s$block, s$stage, s$method,
                s$samples_before, s$features_before,
                s$samples_after, s$features_after, s$runtime))
  }

  step("A fitted model, which is what makes the pipeline replayable")

  run(utils::str(CLEAN$imputation$lab$model, max.level = 1),
      print_it = FALSE)

  step("Before and after, in numbers")

  run(CLEAN$quality$table)
  run(CLEAN$statistics$lab)

  step("apply_preprocessing(): the same pipeline on a cohort it never saw")

  why("Feature decisions are replayed so the columns match. Sample ",
      "decisions are not: dropping rows from a validation set because the ",
      "training set dropped them manufactures optimistic results.")

  code(apply_preprocessing, 35)

  VALID_CLEAN <- run(apply_preprocessing(get("VALID", envir = globalenv()),
                                         CLEAN, quiet = FALSE),
                     print_it = FALSE)

  cat("\n  columns match the training cohort:",
      identical(sort(colnames(CLEAN$data$assays$clinical)),
                sort(colnames(VALID_CLEAN$assays$clinical))), "\n")

  step("Figures")

  for (nm in names(CLEAN$plots)) {
    figure(CLEAN$plots[[nm]], paste0("dissect-prep-", nm))
  }

  step("Refusals")

  refuses(preprocess("not an object", quality), "MultiOmicsData")
  refuses(preprocess(obj, "not a plan"), "CMOValidation")
  refuses(preprocess(obj, quality, blocks = "nope"), "No recipe for block")

  assign("CLEAN", CLEAN, envir = globalenv())
  assign("VALID_CLEAN", VALID_CLEAN, envir = globalenv())

}


# =============================================================================
# PART 6 - analyze(), taken apart
# =============================================================================

if (part(6, "analyze(): every internal, in the order it runs")) {

  clean <- get("CLEAN", envir = globalenv())

  why("What follows is analyze() executed one internal at a time. Each ",
      "step prints the source of the helper it is about to call, so the ",
      "code on screen is the code that runs. The headings are analyze()'s ",
      "own section comments.")

  # ---------------------------------------------------------------------------
  step("6.0 - The two option groups, unpacked first")
  # ---------------------------------------------------------------------------

  why("analyze() takes eleven arguments, not twenty-seven, because two kinds ",
      "of decision were being mixed. assume() cannot be derived from the ",
      "data and changes what may be concluded; control() only changes how ",
      "long it takes.")

  code(analysis_assumptions, 25)

  ASSUME <- run(analysis_assumptions(
    dag = get("STRUCTURE", envir = globalenv()),
    modifiable = c("diet", "exercise"),
    negative_controls = "noise_1",
    heterogeneity = "sex",
    reliability = c(inflammation = 0.85),
    competing = "other_cause",
    positivity = TRUE
  ))

  CONTROL <- run(analysis_control(
    methods = c("association", "conditional", "mediation"),
    max_features = 40,
    resample = 30,
    seed = 7
  ))

  code(.as_group)

  # ---------------------------------------------------------------------------
  step("6.1 - Align the blocks onto one rectangle")
  # ---------------------------------------------------------------------------

  why("Blocks need not share samples and preprocessing may have removed ",
      "different ones from each. An analysis needs a rectangle, so the ",
      "intersection is taken and reported: silently analysing whatever ",
      "survived would hide how much of the cohort a result rests on.")

  code(.align_blocks, 45)

  aligned <- run(.align_blocks(clean$data$assays, names(clean$data$assays),
                               original = clean$input$assays),
                 print_it = FALSE)

  cat("\n  samples kept   :", length(aligned$samples), "\n")
  cat("  samples dropped:", length(aligned$dropped_samples), "\n")
  cat("  features       :", ncol(aligned$x), "\n")
  cat("  blocks         :",
      paste(unique(unlist(aligned$feature_block)), collapse = ", "), "\n")

  why("When there is no rectangle the error names which block to drop and ",
      "what dropping it would leave, rather than reporting a count of zero.")

  code(.explain_no_overlap, 30)

  # ---------------------------------------------------------------------------
  step("6.2 - Resolve the outcome")
  # ---------------------------------------------------------------------------

  why("The outcome's type decides which models are even applicable. An ",
      "unordered outcome with more than two categories is refused: coding ",
      "it 1, 2, 3 would let every model run and assert that the third ",
      "category is three times the first.")

  code(.resolve_outcome, 40)

  outcome_spec <- run(.resolve_outcome(clean$data$metadata, "disease",
                                       aligned$samples))

  # ---------------------------------------------------------------------------
  step("6.3 - Detect the design")
  # ---------------------------------------------------------------------------

  why("The design decides which generators apply. Repeated measures unlock ",
      "mixed models, a time and status pair unlocks survival, a single ",
      "cross-section unlocks neither.")

  code(.detect_design, 35)

  md <- clean$data$metadata[match(aligned$samples,
                                  clean$data$metadata$sample_id), ]

  design <- run(.detect_design(md, outcome_spec, NULL, NULL))

  run(.detect_design(md, outcome_spec, NULL, "subject"))

  # ---------------------------------------------------------------------------
  step("6.4 - Build the covariate frame")
  # ---------------------------------------------------------------------------

  why("A factor with k levels becomes k-1 comparisons against its commonest ",
      "level, and the reference travels with the column so the estimate can ",
      "be read at all.")

  code(.build_covariates, 30)

  covariate_frame <- run(.build_covariates(clean$data$metadata,
                                           c("age", "sex", "smoking"),
                                           aligned$samples))

  run(utils::head(covariate_frame, 4))

  # ---------------------------------------------------------------------------
  step("6.5 - Screen the features")
  # ---------------------------------------------------------------------------

  why("The pairwise stage is quadratic, so features are screened against ",
      "the outcome first. The budget is shared between blocks with a floor ",
      "rather than ranked globally: a block of twenty thousand transcripts ",
      "used to take every slot and a five-variable clinical block vanished, ",
      "taking every cross-block mediation with it.")

  code(.screen_features, 40)

  screen <- run(.screen_features(aligned$x, outcome_spec, max_features = 40,
                                 feature_block = aligned$feature_block,
                                 min_per_block = 10, quiet = FALSE),
                print_it = FALSE)

  cat("\n  kept    :", length(screen$keep), "of", ncol(aligned$x), "\n")
  cat("  per block:\n")
  print(table(unlist(aligned$feature_block[screen$keep])))

  x <- aligned$x[, screen$keep, drop = FALSE]

  # ---------------------------------------------------------------------------
  step("6.6 - Assemble the context every generator receives")
  # ---------------------------------------------------------------------------

  params <- list(
    goal = "causal", seed = 7, bootstrap = 200, trees = 500,
    min_correlation = 0.3, min_evidence_score = 1, max_network_nodes = 25,
    max_path_length = 4, max_paths = 50, top_n = 10,
    mediation_candidates = utils::head(screen$keep, 12),
    sem_candidates = utils::head(screen$keep, 8),
    forbidden = NULL, required = NULL
  )

  context <- list(
    x = x, outcome = outcome_spec, covariates = covariate_frame,
    time_values = NULL, subject_values = NULL, design = design,
    feature_block = aligned$feature_block, encoding = aligned$encoding,
    metadata = clean$data$metadata,
    competing = clean$data$metadata$other_cause[
      match(aligned$samples, clean$data$metadata$sample_id)],
    params = params)

  cat("\n  context carries:", paste(names(context), collapse = ", "), "\n")

  # ---------------------------------------------------------------------------
  step("6.7 - Run each generator, one at a time")
  # ---------------------------------------------------------------------------

  why("Nine generators, each emitting EvidenceEdge objects. They sit at ",
      "different epistemic levels, and agreement between them is weighted ",
      "by that: five predictive methods concurring is weaker evidence than ",
      "one longitudinal model plus one mediation analysis.")

  registry <- .evidence_registry()

  ALL_EDGES <- list()

  # The same three named in CONTROL above, so the count at the end of this
  # part can be compared against analyze()'s. Looping over the whole registry
  # here would run generators analyze() was told to skip, and the comparison
  # would show a difference that means nothing.

  wanted <- CONTROL$methods

  cat("\n  generators analyze() was told to run: ",
      paste(wanted, collapse = ", "), "\n", sep = "")

  cat("  the rest are shown as skipped, with the reason.\n")

  for (g in names(registry)) {

    entry <- registry[[g]]

    applies <- entry$applies(design, outcome_spec)
    have <- is.null(entry$requires) ||
      all(vapply(entry$requires, requireNamespace, logical(1),
                 quietly = TRUE))
    asked <- g %in% wanted

    cat(sprintf("\n  %-14s %-34s applies: %-5s available: %-5s asked for: %s\n",
                g, entry$label, applies, have, asked))

    if (!applies || !have || !asked) next

    edges <- .safe_try(suppressWarnings(entry$generate(context)), list())

    cat(sprintf("                 emitted %d edge(s)\n", length(edges)))

    if (length(edges) > 0) {

      e <- edges[[1]]

      cat(sprintf("                 e.g. %s -> %s  estimate %s  quantity %s\n",
                  e$source, e$target, format(round(e$estimate, 4)),
                  e$quantity))

      ALL_EDGES <- c(ALL_EDGES, edges)

    }

  }

  cat("\n  total raw edges:", length(ALL_EDGES), "\n")

  step("6.7b - One generator's source, in full")

  code(.evidence_association, 60)

  step("6.7c - One raw EvidenceEdge, every slot")

  inside(ALL_EDGES[[1]], "ALL_EDGES[[1]]")

  # ---------------------------------------------------------------------------
  step("6.8 - Integrate: merge, resolve, score")
  # ---------------------------------------------------------------------------

  why("The same relationship arrives many times with different estimates. ",
      "The integrator merges them, resolves conflicts and scores what is ",
      "left. Only quantities of the same family are pooled: averaging a log ",
      "hazard ratio with a correlation gives a number in no units at all.")

  code(.quantity_registry, 20)

  qr <- .quantity_registry()

  for (q in names(qr)) {
    cat(sprintf("  %-20s family %-12s poolable %s\n", q, qr[[q]]$family,
                qr[[q]]$poolable))
  }

  code(.integrate_evidence, 70)

  EVIDENCE <- run(.integrate_evidence(ALL_EDGES, params), print_it = FALSE)

  cat("\n  ", length(ALL_EDGES), "raw edges ->", length(EVIDENCE),
      "integrated\n")

  step("6.8b - One integrated edge, every slot")

  inside(EVIDENCE[[1]], "EVIDENCE[[1]]")

  step("6.8c - What each method said before merging")

  run(EVIDENCE[[1]]$contributions)

  step("6.8d - The three scores, and why they are separate")

  why("Strength is effect size, confidence is estimation precision, ",
      "consistency is how many methods agreed on direction. Consistency is ",
      "NOT validity: methods sharing an unmeasured confounder agree while ",
      "all being biased, so it never feeds a causal claim.")

  run(.decompose_evidence(EVIDENCE[[1]]))

  # ---------------------------------------------------------------------------
  step("6.9 - Charge for what was reconstructed rather than measured")
  # ---------------------------------------------------------------------------

  why("Imputation fills gaps with plausible numbers, and from that moment ",
      "nothing downstream can tell a measurement from an estimate. The ",
      "interval comes out as narrow as if every value had been real.")

  code(.feature_quality, 45)

  fq <- run(.feature_quality(clean), print_it = FALSE)

  run(utils::head(fq[order(-fq$missing_percent),
                     c("feature", "block", "missing_percent", "imputed",
                       "imputation_method", "quality")], 8))

  code(.quality_edges, 35)

  EVIDENCE <- run(.quality_edges(EVIDENCE, fq, c("age", "sex", "smoking")),
                  print_it = FALSE)

  discounted <- Filter(function(e) isTRUE(e$data_quality < 1), EVIDENCE)

  cat("\n  edges discounted:", length(discounted), "of", length(EVIDENCE), "\n")

  if (length(discounted) > 0) {
    cat("  e.g.", discounted[[1]]$source, "quality",
        round(discounted[[1]]$data_quality, 3), "limited by",
        discounted[[1]]$quality_limited_by, "\n")
    cat("      ", discounted[[1]]$quality_flags[1], "\n")
  }

  step("6.9b - Does it survive on the rows that were measured?")

  code(.complete_case_sensitivity, 40)

  mask <- run(.observed_mask(clean, aligned$samples, colnames(x)),
              print_it = FALSE)

  cat("\n  cells measured:", sprintf("%.1f%%", 100 * mean(mask)), "\n")

  EVIDENCE <- .complete_case_sensitivity(EVIDENCE, x, outcome_spec,
                                         covariate_frame, mask)

  checked <- Filter(function(e) is.finite(e$complete_case_estimate), EVIDENCE)

  for (e in utils::head(checked, 4)) {
    cat(sprintf("  %-14s pooled %8.4f   measured rows only %8.4f  %s\n",
                e$source, e$estimate, e$complete_case_estimate,
                if (isTRUE(e$complete_case_agrees)) "same direction"
                else "REVERSES"))
  }

  # ---------------------------------------------------------------------------
  step("6.10 - Is the effect estimable at all?")
  # ---------------------------------------------------------------------------

  why("Reading an adjusted estimate causally needs two conditions, not one. ",
      "No unmeasured confounding is the famous one. The other is positivity: ",
      "at every combination of the adjustment set the exposure has to vary. ",
      "Where it does not, the model does not fail, it extrapolates.")

  code(.positivity, 40)
  code(.residual_variation, 25)

  EVIDENCE <- run(.audit_estimability(EVIDENCE, x, covariate_frame,
                                      "disease", ASSUME), print_it = FALSE)

  for (e in utils::head(Filter(function(e) !is.na(e$positivity), EVIDENCE), 6)) {
    cat(sprintf("  %-14s positivity %-5s  %.0f%% of it free to vary\n",
                e$source, e$positivity, 100 * e$residual_variation))
  }

  step("6.10b - Measurement error, where a reliability was declared")

  code(.attenuation_correction, 30)

  corrected <- Filter(function(e) is.finite(e$corrected_estimate), EVIDENCE)

  for (e in corrected) {
    cat(sprintf("  %-14s measured %8.4f  corrected %8.4f  (reliability %.2f)\n",
                e$source, e$estimate, e$corrected_estimate, e$reliability))
  }

  # ---------------------------------------------------------------------------
  step("6.11 - Audit the adjustment against the stated structure")
  # ---------------------------------------------------------------------------

  why("Adjusting for a mediator removes part of the effect being measured. ",
      "Adjusting for a collider opens a closed path and manufactures ",
      "association. Both look like diligence, both make the estimate worse ",
      "than doing nothing, and only a declared structure tells them apart.")

  code(.dag_audit, 45)

  dag <- run(.parse_dag(get("STRUCTURE", envir = globalenv())),
             print_it = FALSE)

  run(.dag_audit(dag, "diet", "disease", adjusted = "age"))
  run(.dag_audit(dag, "diet", "disease", adjusted = c("age", "inflammation")))
  run(.dag_audit(dag, "diet", "disease", adjusted = c("age", "biomarker")))

  code(.audit_edges, 35)

  EVIDENCE <- run(.audit_edges(EVIDENCE, dag, c("age", "sex", "smoking")),
                  print_it = FALSE)

  for (e in utils::head(EVIDENCE, 5)) {
    cat(sprintf("  %-14s identification %-12s identifiable %s\n",
                e$source, e$identification, e$identifiable))
  }

  # ---------------------------------------------------------------------------
  step("6.12 - Does one number describe everybody?")
  # ---------------------------------------------------------------------------

  code(.effect_concentration, 40)

  EVIDENCE <- .edge_concentration(EVIDENCE, x, outcome_spec, covariate_frame)

  for (e in utils::head(Filter(function(e)
    is.finite(e$share_driving_effect), EVIDENCE), 6)) {
    cat(sprintf("  %-14s halved by removing %3d people (%.1f%%)\n",
                e$source, e$samples_driving_effect,
                100 * e$share_driving_effect))
  }

  step("6.12b - Against a named modifier")

  code(.heterogeneity, 35)

  het <- run(.heterogeneity(context, "sex", utils::head(screen$keep, 8),
                            quiet = TRUE), print_it = FALSE)

  if (isTRUE(het$available)) {
    run(utils::head(het$table[, c("feature", "moderator", "interaction_fdr",
                                  "groups", "slopes", "same_sign")], 6))
  }

  # ---------------------------------------------------------------------------
  step("6.13 - Modules: the resolution between a feature and a block")
  # ---------------------------------------------------------------------------

  code(.detect_modules, 35)
  code(.module_latent, 35)

  MODULES <- run(.build_module_graph(x, outcome_spec, covariate_frame,
                                     aligned$feature_block),
                 print_it = FALSE)

  run(print(MODULES), print_it = FALSE)

  # ---------------------------------------------------------------------------
  step("6.14 - Resampling, and the graph across resamples")
  # ---------------------------------------------------------------------------

  why("Per-edge stability answers 'would this relationship come back?'. It ",
      "cannot answer 'would this picture come back?', and a graph whose ",
      "edges are each recovered six times in ten is a stable structure only ",
      "if it is the same six every time.")

  code(.resample_evidence, 45)

  resampling <- run(.resample_evidence(context, .fast_generators(), 30,
                                       quiet = TRUE), print_it = FALSE)

  cat("\n  replicates:", resampling$replicates,
      " relationships seen:", length(resampling$edges), "\n")

  code(.build_consensus_graph, 40)

  CONSENSUS <- run(.build_consensus_graph(resampling, EVIDENCE),
                   print_it = FALSE)

  run(print(CONSENSUS), print_it = FALSE)

  # ---------------------------------------------------------------------------
  step("6.15 - Build the graph and interpret it")
  # ---------------------------------------------------------------------------

  code(.build_evidence_graph, 45)

  GRAPH <- run(.build_evidence_graph(EVIDENCE, aligned$feature_block,
                                     "disease", params), print_it = FALSE)

  run(print(GRAPH), print_it = FALSE)

  inside(GRAPH, "GRAPH")

  run(GRAPH$edges)
  run(GRAPH$nodes)
  run(GRAPH$paths)

  code(.interpret_graph, 40)

  importance <- .consensus_importance(ALL_EDGES, "disease",
                                      aligned$feature_block)

  INTERP <- run(.interpret_graph(GRAPH, importance, "disease", params),
                print_it = FALSE)

  inside(INTERP, "INTERP")

  # ---------------------------------------------------------------------------
  step("6.16 - Now the whole thing, in one call")
  # ---------------------------------------------------------------------------

  why("Everything above, done by analyze() itself. The numbers should match ",
      "the pieces: this is the check that the dissection above is the real ",
      "pipeline and not a parallel one.")

  RESULTS <- run(analyze(clean, outcome = "disease",
                         covariates = c("age", "sex", "smoking"),
                         effort = "standard", assume = ASSUME,
                         control = CONTROL, plots = TRUE, quiet = FALSE),
                 print_it = FALSE)

  run(print(RESULTS), print_it = FALSE)
  run(summary(RESULTS), print_it = FALSE)

  cat("\n  edges from the step-by-step run :", length(EVIDENCE), "\n")
  cat("  edges from analyze()            :", length(RESULTS$evidence), "\n")

  step("Every slot of the result")

  inside(RESULTS, "RESULTS")

  step("Every table it produced")

  for (nm in names(RESULTS$tables)) {
    cat("\n  --- tables$", nm, " ---\n", sep = "")
    v <- RESULTS$tables[[nm]]
    if (is.data.frame(v) && nrow(v) > 0) print(utils::head(v, 5)) else
      cat("    (empty)\n")
  }

  step("Every log line")

  run(RESULTS$logs)

  step("Every figure")

  for (nm in names(RESULTS$plots)) {
    figure(RESULTS$plots[[nm]], paste0("dissect-analyze-", nm))
  }

  step("Refusals")

  refuses(analyze(get("OBJ", envir = globalenv()), "disease"),
          "PreprocessingResult")
  refuses(analyze(clean, "no_such_column"), "not a column")
  refuses(analyze(clean, "disease",
                  control = analysis_control(methods = "telepathy")),
          "Unknown generator")
  refuses(analyze(clean, "disease", blocks = "nope"), "Unknown block")
  refuses(analyze(clean, "disease", control = "association"),
          "must be built with")

  assign("RESULTS", RESULTS, envir = globalenv())
  assign("ASSUME", ASSUME, envir = globalenv())

}


# =============================================================================
# PART 7 - Every argument of analyze(), exercised
# =============================================================================

if (part(7, "Every option, and what changes when you turn it")) {

  clean <- get("CLEAN", envir = globalenv())

  step("effort: the four presets")

  why("effort buys resampling and calibration, nothing else. It changes how ",
      "precisely the same question is answered, never the question.")

  for (e in c("fast", "standard", "thorough")) {

    t0 <- Sys.time()

    r <- analyze(clean, "disease", effort = e, plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association",
                                            max_features = 20))

    cat(sprintf("  %-10s %2d edge(s)  resamples %-4s permutations %-4s  %.1fs\n",
                e, length(r$evidence),
                .report_or(r$parameters$resample, 0),
                .report_or(r$parameters$permutations, 0),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  }

  step("goal: causal versus predictive")

  for (g in c("causal", "predictive")) {
    r <- analyze(clean, "disease", effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(goal = g, max_features = 20))
    cat(sprintf("  %-12s generators run: %s\n", g,
                paste(names(r$models), collapse = ", ")))
  }

  step("blocks: analysing a subset")

  for (b in list("all", "clinical", c("clinical", "lab"))) {
    r <- analyze(clean, "disease", blocks = b, effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(methods = "association"))
    cat(sprintf("  %-22s %d sample(s), %d edge(s)\n",
                paste(b, collapse = "+"), r$performance$samples,
                length(r$evidence)))
  }

  step("covariates: what adjustment does to the same relationship")

  for (cv in list(NULL, "age", c("age", "sex", "smoking"))) {
    r <- analyze(clean, "disease", covariates = cv, effort = "fast",
                 plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association"))
    e <- Filter(function(e) identical(e$source, "diet"), r$evidence)
    cat(sprintf("  adjusted for %-24s identification %-12s estimate %s\n",
                if (is.null(cv)) "nothing" else paste(cv, collapse = "+"),
                if (length(e)) e[[1]]$identification else "-",
                if (length(e)) format(round(e[[1]]$estimate, 4)) else "-"))
  }

  step("max_features and min_per_block: the screening budget")

  for (mf in c(10, 20, 40)) {
    r <- analyze(clean, "disease", effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(methods = "association",
                                            max_features = mf))
    cat(sprintf("  max_features %-4d analysed %d of %d\n", mf,
                r$performance$features_analysed, r$performance$features_total))
  }

  step("min_evidence_score: the reporting threshold")

  for (th in c(0, 1, 20)) {
    r <- analyze(clean, "disease", effort = "fast", plots = FALSE,
                 quiet = TRUE,
                 control = analysis_control(methods = "association",
                                            min_evidence_score = th))
    cat(sprintf("  threshold %-4d %d edge(s) reported\n", th,
                length(r$evidence)))
  }

  step("resample_scheme: bootstrap against cross-validation")

  for (sc in c("bootstrap", "cv")) {
    r <- analyze(clean, "disease", effort = "fast", plots = FALSE, quiet = TRUE,
                 control = analysis_control(methods = "association",
                                            resample = 20,
                                            resample_scheme = sc,
                                            max_features = 20))
    cat(sprintf("  %-10s median stability %s\n", sc,
                format(round(stats::median(vapply(r$evidence, function(e)
                  .report_or(e$bootstrap_stability, NA), numeric(1)),
                  na.rm = TRUE), 3))))
  }

  step("negative_controls: variables that should show nothing")

  r <- run(analyze(clean, "disease", effort = "fast", plots = FALSE,
                   quiet = TRUE,
                   assume = analysis_assumptions(negative_controls = "noise_1"),
                   control = analysis_control(methods = "association")),
           print_it = FALSE)

  run(r$diagnostics$negative_controls)

  step("permutations: how many relationships appear when there are none")

  why("The single most useful number about a graph. Twenty-five edges ",
      "sounds like a result until the same engine produces twenty-two on ",
      "the same data with the outcome shuffled.")

  r <- analyze(clean, "disease", effort = "fast", plots = FALSE, quiet = TRUE,
               control = analysis_control(methods = "association",
                                          permutations = 20,
                                          max_features = 20))

  run(r$diagnostics$null_calibration)

  step("forbidden and required: constraining structure learning")

  if (requireNamespace("bnlearn", quietly = TRUE)) {

    r <- run(analyze(clean, "disease", effort = "fast", plots = FALSE,
                     quiet = TRUE,
                     assume = analysis_assumptions(
                       forbidden = data.frame(from = "disease",
                                              to = c("diet", "exercise"))),
                     control = analysis_control(methods = "bayesnet",
                                                max_features = 15)),
             print_it = FALSE)

    if (length(r$evidence) > 0) {
      run(grep("excluded from the search", r$evidence[[1]]$assumptions,
               value = TRUE))
    }

  }

  step("Several outcomes at once")

  why("Running analyze() twice by hand gives the same estimates and the ",
      "wrong error rate: a relationship that was one of fifty tests is not ",
      "one of a hundred.")

  clean$data$metadata$second <- clean$data$metadata$biomarker

  MULTI <- run(analyze(clean, c("disease", "second"), effort = "fast",
                       plots = FALSE, quiet = TRUE,
                       control = analysis_control(methods = "association",
                                                  max_features = 20)),
               print_it = FALSE)

  run(print(MULTI), print_it = FALSE)
  run(utils::head(MULTI$tests, 8))

  step("Survival, and competing risks")

  surv_clean <- preprocess(get("SURV", envir = globalenv()),
                           check_data(get("SURV", envir = globalenv())),
                           plots = FALSE, quiet = TRUE)

  surv_clean$data$metadata$other_cause <-
    rbinom(nrow(surv_clean$data$metadata), 1, 0.2)

  rs <- run(analyze(surv_clean, "died", time = "died_time", effort = "fast",
                    plots = FALSE, quiet = TRUE,
                    assume = analysis_assumptions(competing = "other_cause"),
                    control = analysis_control(methods = "survival")),
            print_it = FALSE)

  cat("\n  design:", rs$design$type, "\n")

  if (length(rs$evidence) > 0) run(rs$evidence[[1]]$assumptions)

  step("Longitudinal")

  rl <- run(analyze(clean, "disease", subject = "subject", effort = "fast",
                    plots = FALSE, quiet = TRUE,
                    control = analysis_control(methods = "association",
                                               max_features = 15)),
            print_it = FALSE)

  cat("\n  design:", rl$design$type, "\n")

}


# =============================================================================
# PART 8 - Everything downstream of a result
# =============================================================================

if (part(8, "Every function that reads a result")) {

  results <- get("RESULTS", envir = globalenv())

  sources <- unique(vapply(
    Filter(function(e) identical(e$target, results$outcome$name),
           results$evidence),
    function(e) e$source, character(1)))

  step("explain(): everything known about one variable")

  code(explain, 40)

  ex <- run(explain(results, sources[1]), print_it = FALSE)

  run(print(ex), print_it = FALSE)

  inside(ex, "ex")

  step("counterfactual(): the findings in original units")

  why("Nothing is included unless you name it as something that could ",
      "plausibly be changed. A contrast for a genotype is arithmetic ",
      "without meaning.")

  code(counterfactual, 40)

  run(counterfactual(results, modifiable = c("diet", "exercise")),
      print_it = FALSE)

  for (dir in c("increase", "decrease")) {
    cf <- .safe_try(counterfactual(results, modifiable = "diet",
                                   direction = dir, change = 1), NULL)
    if (!is.null(cf)) {
      cat("\n  direction =", dir, "\n")
      print(utils::head(cf$table[, c("variable", "from", "to",
                                     "percent_change")], 3))
    }
  }

  refuses(counterfactual(results), "modifiable")

  step("hypothesis(): a claim that could be wrong")

  code(hypothesis, 45)

  h <- run(hypothesis(results))

  inside(h, "h")

  run(h$settles)
  run(h$supports)
  run(h$threatens)

  step("test_hypothesis(): the claim on a cohort it never saw")

  code(test_hypothesis, 40)

  tested <- run(test_hypothesis(h, get("VALID_CLEAN", envir = globalenv())),
                print_it = FALSE)

  run(tested$replication)

  step("sensitivity(): how much of it is the analyst")

  code(sensitivity, 40)

  s <- run(sensitivity(results, sources[1], quiet = TRUE), print_it = FALSE)

  run(print(s), print_it = FALSE)
  run(s$paths)

  step("compare_results(): two analyses side by side")

  code(compare_results, 40)

  other <- analyze(get("CLEAN", envir = globalenv()), "disease",
                   covariates = "age", effort = "fast", plots = FALSE,
                   quiet = TRUE,
                   control = analysis_control(methods = "association"))

  cmp <- run(compare_results(results, other,
                             names = c("full", "age only")), print_it = FALSE)

  run(print(cmp), print_it = FALSE)
  run(utils::head(cmp$edges, 6))

  step("check_dag(): what an adjustment is worth, before fitting")

  code(check_dag, 30)

  st <- get("STRUCTURE", envir = globalenv())

  run(check_dag(st, "diet", "disease", adjusted = "age"))
  run(check_dag(st, "diet", "disease"))
  run(check_dag(st, "diet", "disease", adjusted = c("age", "inflammation")))
  run(check_dag(st, "diet", "disease", adjusted = c("age", "biomarker")))
  run(check_dag(st, "biomarker", "disease", adjusted = "age"))

  step("annotate_evidence(): prior knowledge, beside the score never inside")

  code(annotate_evidence, 35)

  ann <- data.frame(
    source = utils::head(results$graph$edges$source, 2),
    target = utils::head(results$graph$edges$target, 2),
    support = c(0.9, 0.5),
    database = c("local KEGG export", "local STRING export"),
    stringsAsFactors = FALSE)

  annotated <- run(annotate_evidence(results, ann), print_it = FALSE)

  cat("\n  scores before:",
      paste(round(utils::head(results$graph$edges$evidence_score, 4), 2),
            collapse = ", "), "\n")
  cat("  scores after :",
      paste(round(utils::head(annotated$graph$edges$evidence_score, 4), 2),
            collapse = ", "), "\n")

  step("export_graph(): taking it elsewhere")

  code(export_graph, 35)

  for (f in c("graphml", "dot", "json")) {
    p <- file.path(.outdir, paste0("dissect-graph.", f))
    run(export_graph(results, p, format = f), print_it = FALSE)
    cat("    ", p, " (", file.size(p), " bytes)\n", sep = "")
  }

  run(readLines(file.path(.outdir, "dissect-graph.json"), n = 8))

  step("report(): both audiences")

  for (a in c("general", "technical")) {
    run(report(results, file = file.path(.outdir,
                                         paste0("dissect-analysis-", a, ".html")),
               audience = a, prompt = FALSE, quiet = TRUE), print_it = FALSE)
  }

  run(report(results, format = "console"), print_it = FALSE)

}


# =============================================================================
# PART 9 - Inventory
# =============================================================================

if (part(9, "What the package contains")) {

  step("Exported")

  run(sort(getNamespaceExports("CausalMultiOmics")))

  step("S3 methods registered")

  ns <- asNamespace("CausalMultiOmics")

  for (g in c("print", "summary", "report")) {
    m <- grep(paste0("^", g, "[.]"), ls(ns), value = TRUE)
    cat(sprintf("\n  %-8s %s\n", g, paste(sub(paste0(g, "."), "", m,
                                              fixed = TRUE),
                                          collapse = ", ")))
  }

  step("Functions per file")

  counts <- vapply(list.files(file.path(.root, "R"), "[.]R$",
                              full.names = TRUE),
                   function(f) length(grep("<- function",
                                           readLines(f, warn = FALSE))),
                   integer(1))

  names(counts) <- basename(names(counts))

  print(counts)
  cat("\n  total:", sum(counts), "\n")

}


# =============================================================================
# Scoreboard
# =============================================================================

cat("\n\n"); rule("=")
cat("DISSECTION COMPLETE\n")
rule("=")

cat(sprintf("\n  calls that ran     : %d\n", .ok))
cat(sprintf("  calls that failed  : %d\n", .bad))

if (.bad > 0) {
  cat("\n  FAILURES:\n")
  cat(paste0("    - ", .failures, collapse = "\n"), "\n")
}

cat(sprintf("\n  elapsed : %.1f s\n",
            as.numeric(difftime(Sys.time(), CMO_START, units = "secs"))))

cat("\n  transcript : ", .transcript, "\n", sep = "")
cat("  figures    : ", .outdir, "\n\n", sep = "")

if (.bad == 0) cat("  Everything ran.\n") else
  cat("  Something is wrong - see the failures above.\n")

rule("=")
cat("\n")
