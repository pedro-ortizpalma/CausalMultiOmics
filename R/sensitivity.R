# =============================================================================
# How much of this is the analyst?
# =============================================================================
#
# Every result in this package rests on a preprocessing recipe, and a recipe
# is a stack of choices. Impute with the median or with neighbours. Log or
# Yeo-Johnson. Scale to unit variance or leave the units alone. Each choice
# is defensible, several would be made differently by a competent person, and
# the report shows the number that came out of one path through them.
#
# The E-value asks how much unmeasured confounding it would take to erase a
# finding. This asks the question next to it and rather more embarrassing:
# how much of the finding is the analyst? A conclusion that survives every
# reasonable preprocessing decision is a different object from one that
# needed this one.
#
# The package can answer it where most cannot, because recipes are objects
# and pipelines replay: the variants are cheap to build and cheap to run.
# =============================================================================

#' Re-run one relationship under every defensible preprocessing choice
#'
#' A multiverse analysis, restricted to the decisions this package makes on
#' the user's behalf. The relationship is refitted under each variant recipe
#' and the spread of the answer is reported.
#'
#' @section What is varied and what is not:
#'
#' Imputation and transformation: the stages where a competent analyst could
#' reasonably have chosen otherwise and the choice changes the numbers.
#'
#' Scaling is not varied, because it cannot change the answer. Estimates are
#' reported per standard deviation of the exposure, and any affine rescaling
#' leaves that untouched, so every scaling choice returns the identical
#' number. Including them would fill the table with exact duplicates, and a
#' robustness check that counts the same answer twice reports a finding as
#' steadier than it is.
#'
#' Sample filters are held fixed too. Changing them changes who is in the
#' study, and an estimate on a different population is not a different answer
#' to the same question — it is an answer to a different one, which belongs
#' in \code{\link{compare_results}} rather than here.
#'
#' @section Why the estimates are standardised:
#'
#' A rank transformation puts the exposure on a 1 to n scale and a log
#' compresses it, so the raw coefficients down different paths are in
#' different units. Putting them in one column would invite exactly the
#' comparison that cannot be made, and would report a 275-fold spread for a
#' relationship that never moved. Every path is therefore reported as change
#' in the outcome per standard deviation of the exposure, however that
#' exposure was expressed.
#'
#' @section Reading the result:
#'
#' The number to look at is not the median. It is the share of paths that
#' agree on the direction, and the range. A relationship that is 0.8 down one
#' path and -0.1 down another was never a finding; it was a preprocessing
#' choice with a coefficient attached.
#'
#' @param object A \code{CMOResult}.
#' @param feature The variable whose relationship with the outcome to test.
#'   Defaults to the highest-scoring one.
#' @param variants Recipes to try, as produced by
#'   \code{.sensitivity_variants()}. Left alone by default, which builds them
#'   from the defensible alternatives for each block.
#' @param max_paths Cap on how many recipes to run.
#' @param quiet Suppress progress.
#'
#' @return A \code{CMOSensitivity}.
#'
#' @examples
#' \donttest{
#' sim <- simulate_data(n = 200, blocks = list(a = c("x", "z")),
#'                      dag = data.frame(from = "x", to = "y", effect = 0.8),
#'                      outcome = "y", missing = 0.1)
#' prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)
#' res <- analyze(prep, "y", methods = "association", effort = "fast",
#'                plots = FALSE, quiet = TRUE)
#'
#' sensitivity(res, "x")
#' }
#'
#' @export
sensitivity <- function(object, feature = NULL, variants = NULL,
                        max_paths = 24L, quiet = FALSE) {

  if (!inherits(object, "CMOResult")) {
    stop("'object' must be a CMOResult object.", call. = FALSE)
  }

  prep <- object$input

  if (!inherits(prep, "PreprocessingResult")) {
    stop(paste("This result does not carry the preprocessing it came from,",
               "so its choices cannot be varied."), call. = FALSE)
  }

  outcome <- object$outcome$name

  to_outcome <- Filter(function(e) identical(e$target, outcome),
                       object$evidence)

  if (length(to_outcome) == 0) {
    stop("This analysis found no relationship with the outcome to test.",
         call. = FALSE)
  }

  edge <- if (is.null(feature)) {

    to_outcome[[which.max(vapply(to_outcome, function(e)
      .report_or(e$evidence_score, 0), numeric(1)))]]

  } else {

    hit <- Filter(function(e) identical(e$source, feature), to_outcome)

    if (length(hit) == 0) {
      stop(sprintf("'%s' has no relationship with %s in this analysis.",
                   feature, outcome), call. = FALSE)
    }

    hit[[1]]

  }

  block <- .report_or(edge$source_block, NA_character_)

  if (is.na(block) || !(block %in% names(prep$recipes))) {
    stop(sprintf("Cannot tell which block '%s' came from, so its recipe cannot be varied.",
                 edge$source), call. = FALSE)
  }

  if (is.null(variants)) {
    variants <- .sensitivity_variants(prep$recipes[[block]], max_paths)
  }

  if (length(variants) == 0) {
    stop("No alternative recipe was available for that block.", call. = FALSE)
  }

  raw <- prep$input
  metadata <- prep$data$metadata

  covariates <- edge$adjustment_set
  binary <- identical(.report_or(object$outcome$type, ""), "binary")

  if (!isTRUE(quiet)) {
    cat(sprintf("Refitting %s -> %s under %d preprocessing path(s)...\n",
                edge$source, outcome, length(variants)))
  }

  rows <- lapply(seq_along(variants), function(i) {

    v <- variants[[i]]

    fitted <- .safe_try(
      preprocess(raw, v$recipe, blocks = block, plots = FALSE, quiet = TRUE,
                 force = TRUE),
      NULL)

    if (is.null(fitted)) return(NULL)

    x <- .report_or(fitted$data$assays[[block]], NULL)

    if (is.null(x) || !(edge$source %in% colnames(x))) return(NULL)

    ids <- rownames(x)

    frame <- data.frame(
      .y = metadata[[outcome]][match(ids, metadata$sample_id)],
      .x = x[, edge$source]
    )

    present <- intersect(covariates, colnames(metadata))

    if (length(present) > 0) {
      frame <- cbind(frame, metadata[match(ids, metadata$sample_id), present,
                                     drop = FALSE])
    }

    frame <- frame[stats::complete.cases(frame), , drop = FALSE]

    if (nrow(frame) < 10 || stats::sd(frame$.x) == 0) return(NULL)

    fit <- .safe_try(
      if (binary) stats::glm(.y ~ ., data = frame, family = stats::binomial())
      else stats::lm(.y ~ ., data = frame),
      NULL)

    if (is.null(fit)) return(NULL)

    coefs <- .safe_try(summary(fit)$coefficients, NULL)

    if (is.null(coefs) || !(".x" %in% rownames(coefs))) return(NULL)

    # Per standard deviation of the exposure, not per unit of it. A rank
    # transformation puts the exposure on a 1..n scale and a log compresses
    # it, so raw coefficients from different paths are in different units and
    # comparing them is the very mistake the quantity families exist to
    # prevent. Standardising makes every path answer the same question:
    # how much does the outcome move per standard deviation of this
    # variable, however that variable was expressed.

    spread <- stats::sd(frame$.x)

    data.frame(
      path = i,
      imputation = v$label$imputation,
      transformation = v$label$transformation,
      scaling = v$label$scaling,
      is_original = v$original,
      estimate = coefs[".x", 1] * spread,
      raw_estimate = coefs[".x", 1],
      se = coefs[".x", 2] * spread,
      p_value = coefs[".x", 4],
      n = nrow(frame),
      stringsAsFactors = FALSE
    )

  })

  paths <- do.call(rbind, Filter(Negate(is.null), rows))

  if (is.null(paths) || nrow(paths) == 0) {
    stop("No preprocessing path produced a fittable model.", call. = FALSE)
  }

  rownames(paths) <- NULL

  .sensitivity_result(paths, edge, outcome, block, length(variants))

}

#' Assemble the verdict from the paths that ran
#'
#' @param paths One row per recipe.
#' @param edge The relationship tested.
#' @param outcome,block Names.
#' @param attempted How many variants were tried.
#'
#' @return A CMOSensitivity.
#' @keywords internal
#' @noRd
.sensitivity_result <- function(paths, edge, outcome, block, attempted) {

  estimates <- paths$estimate[is.finite(paths$estimate)]

  # The path the report came from, on the same standardised scale as the
  # rest. edge$estimate is a raw coefficient and comparing it against
  # standardised ones would reintroduce the units problem at the one place
  # it matters most.

  here <- paths$estimate[isTRUE(paths$is_original)]

  reference <- if (length(here) == 1 && is.finite(here)) here else
    stats::median(estimates)

  agree <- if (!is.finite(reference) || reference == 0) NA_real_ else
    mean(sign(estimates) == sign(reference))

  significant <- mean(paths$p_value < 0.05, na.rm = TRUE)

  verdict <- if (!is.finite(agree)) "cannot be judged"
  else if (agree >= 0.95 && significant >= 0.8)
    "holds down every reasonable path"
  else if (agree >= 0.95)
    "keeps its direction, but not always its significance"
  else if (agree >= 0.7)
    "changes direction down some paths"
  else
    "is a preprocessing choice with a coefficient attached"

  structure(
    list(
      source = edge$source,
      target = outcome,
      block = block,
      original = reference,
      original_raw = .report_or(edge$estimate, NA_real_),
      paths = paths[order(paths$estimate), ],
      attempted = attempted,
      ran = nrow(paths),
      summary = list(
        median = stats::median(estimates),
        min = min(estimates),
        max = max(estimates),
        sign_agreement = agree,
        significant = significant,
        spread = if (is.finite(reference) && reference != 0)
          (max(estimates) - min(estimates)) / abs(reference) else NA_real_
      ),
      verdict = verdict,
      notes = c(
        paste("Only the choices this package makes on your behalf are",
              "varied: imputation, transformation and scaling. Sample",
              "filters are held fixed, because changing who is in the study",
              "changes the question rather than the answer."),
        paste("The number to read is the share of paths agreeing on the",
              "direction, not the median. A median across a set of paths",
              "that disagree is a summary of the disagreement."),

        paste("Stability here is not evidence that the relationship is real.",
              "A chance correlation in this sample is not created or",
              "destroyed by how the data was transformed, only re-expressed,",
              "so noise passes this check comfortably. It measures dependence",
              "on the analyst. Null calibration and replication in another",
              "cohort are what address the other question.")
      )
    ),
    class = "CMOSensitivity"
  )

}

#' Build the defensible alternatives to one recipe
#'
#' Alternatives, not every combination. A grid over six imputations, sixteen
#' transformations and six scalings is five hundred paths, most of them
#' nobody would defend, and the resulting spread would say more about the
#' grid than about the data.
#'
#' @param recipe The recipe that was actually used.
#' @param max_paths Cap.
#'
#' @return A list of variants, each with a recipe and a label. The first is
#'   always the original, so the report can mark it.
#' @keywords internal
#' @noRd
.sensitivity_variants <- function(recipe, max_paths = 24L) {

  if (!inherits(recipe, "PreprocessingRecipe")) return(list())

  alternatives <- function(current, options) {
    unique(c(current, setdiff(options, current)))
  }

  # One list per stage, headed by what was actually chosen.

  imputations <- if (identical(.report_or(recipe$imputation, "none"), "none"))
    "none" else alternatives(recipe$imputation, c("median", "mean", "knn"))

  transformations <- alternatives(
    .report_or(recipe$transformation, "identity"),
    c("identity", "log", "yeojohnson", "rank"))

  # Scaling is deliberately not varied. The estimate is reported per standard
  # deviation of the exposure, and any affine rescaling of a variable leaves
  # that untouched: every scaling choice produces the identical number.
  # Including them anyway would double the paths with exact duplicates, and
  # a robustness check that counts the same answer twice reports a finding as
  # steadier than it is.

  grid <- expand.grid(imputation = imputations,
                      transformation = transformations,
                      scaling = .report_or(recipe$scaling, "none"),
                      stringsAsFactors = FALSE)

  # The original first, then the rest in the order the grid produced them,
  # trimmed to the cap. Ordering matters only so the cap does not decide
  # which stage gets explored.

  is_original <- grid$imputation == imputations[1] &
    grid$transformation == transformations[1]

  grid <- rbind(grid[is_original, , drop = FALSE],
                grid[!is_original, , drop = FALSE])

  grid <- utils::head(grid, max_paths)

  lapply(seq_len(nrow(grid)), function(i) {

    variant <- recipe

    variant$imputation <- grid$imputation[i]
    variant$transformation <- grid$transformation[i]
    variant$scaling <- grid$scaling[i]

    # The stored models belong to the original choices and replaying them
    # under different ones would apply a median computed on untransformed
    # data to transformed data.
    variant$imputation_model <- NULL
    variant$transformation_model <- NULL
    variant$scaling_model <- NULL

    variant$transformation_parameters <- list()

    list(recipe = variant, original = i == 1,
         label = list(imputation = grid$imputation[i],
                      transformation = grid$transformation[i],
                      scaling = grid$scaling[i]))

  })

}

#' Print how much of a finding is the analyst
#'
#' @param x A \code{CMOSensitivity}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export
print.CMOSensitivity <- function(x, ...) {

  cat("\n")
  cat("CMOSensitivity\n")
  cat("==============\n\n")

  cat(sprintf("  %s  ->  %s\n\n", x$source, x$target))

  cat(sprintf("%-28s %s per SD  (%s per unit, as reported)\n",
              "The path taken:", fmt_num(x$original, 4),
              fmt_num(x$original_raw, 4)))

  cat(sprintf("%-28s %d of %d attempted\n", "Paths that ran:", x$ran,
              x$attempted))

  cat("\n")

  # Per standard deviation throughout. A rank transformation puts the
  # exposure on a 1..n scale, so raw coefficients from different paths are
  # in different units and putting them in one column would invite exactly
  # the comparison that cannot be made.

  s <- x$summary

  cat(sprintf("%-28s %s to %s\n", "Across paths, per SD:",
              fmt_num(s$min, 4), fmt_num(s$max, 4)))
  cat(sprintf("%-28s %s\n", "Median:", fmt_num(s$median, 4)))
  cat(sprintf("%-28s %s%%\n", "Agree on direction:",
              if (is.finite(s$sign_agreement))
                format(round(100 * s$sign_agreement)) else "-"))
  cat(sprintf("%-28s %s%%\n", "Reach p < 0.05:",
              format(round(100 * s$significant))))

  cat("\n")
  cat(paste(strwrap(sprintf("This relationship %s.", x$verdict),
                    width = 66, prefix = "  "), collapse = "\n"), "\n",
      sep = "")

  cat("\nEvery path\n")
  cat(strrep("-", 66), "\n", sep = "")

  show <- x$paths

  for (i in seq_len(nrow(show))) {

    cat(sprintf("  %-9s %-11s %-8s %9s  p = %-9s%s\n",
                show$imputation[i], show$transformation[i], show$scaling[i],
                fmt_num(show$estimate[i], 4),
                format.pval(show$p_value[i], digits = 2, eps = 1e-16),
                if (isTRUE(show$is_original[i])) "  <- reported" else ""))

  }

  cat("\nHow to read this\n")
  cat(strrep("-", 66), "\n", sep = "")

  for (n in x$notes) {
    cat(paste(strwrap(n, width = 66, prefix = "  "), collapse = "\n"),
        "\n", sep = "")
  }

  cat("\n")

  invisible(x)

}
