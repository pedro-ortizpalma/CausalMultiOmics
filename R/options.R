# =============================================================================
# Two kinds of decision, kept apart
# =============================================================================
#
# analyze() had twenty-seven arguments, which is not a function signature but
# a form. The problem is not the count on its own: it is that two completely
# different kinds of decision were mixed in one list.
#
# Some arguments are claims about the world. A causal diagram, a variable
# declared modifiable, a reliability figure from the assay: these change what
# may be concluded, they cannot be derived from the data, and getting one
# wrong makes the answer wrong.
#
# The rest are decisions about effort. How many resamples, how many features
# to screen, which generators to run: these change how long it takes and how
# precisely the same question is answered. Getting one wrong makes the answer
# slower or noisier, never wrong.
#
# Grouping them by that axis is the point. `assume` is where you are
# accountable; `control` is where you are impatient.
# =============================================================================

#' What you are claiming, as opposed to what you measured
#'
#' Everything here is a statement the data cannot check. A causal diagram is
#' a claim about biology. A variable declared modifiable is a claim that
#' someone could act on it. A reliability figure is a claim about the assay.
#' Each changes what the analysis is allowed to conclude, and each is your
#' responsibility rather than the engine's.
#'
#' @param dag Causal structure, as a \code{dagitty} object, a dagitty
#'   specification string, or a data.frame with \code{from} and \code{to}.
#'   Supplying one lets the engine check whether an adjustment identifies each
#'   effect rather than only recording that something was adjusted for. It
#'   never changes an estimate. See \code{\link{check_dag}}.
#' @param modifiable Variables or blocks someone could plausibly act on.
#'   Nothing is expressed as a contrast in original units unless it appears
#'   here: a contrast for a genotype is arithmetic without meaning.
#' @param negative_controls Variables that should show no relationship with
#'   the outcome. If they do, something other than biology is driving the
#'   graph.
#' @param heterogeneity Metadata columns to test as effect modifiers. Naming
#'   one asks whether each relationship differs across the groups it defines.
#' @param reliability Named vector giving the share of each variable's
#'   measured variance that is real, from duplicates or from the assay.
#'   Classical measurement error attenuates a coefficient by exactly this
#'   factor, and supplying it corrects for that. Nothing is guessed: a
#'   variable absent from this vector is left alone.
#' @param competing Metadata column marking competing events in a survival
#'   analysis. Without it, dying of another cause is treated as censoring,
#'   which assumes those people could still have had the outcome.
#' @param forbidden,required data.frames of \code{from}/\code{to} pairs that
#'   structure learning may not propose, or must include. Both are claims
#'   about the world in the same way a diagram is, and both are recorded as
#'   assumptions on every edge they shaped.
#' @param positivity Check whether each effect is estimable at all: whether
#'   the variables adjusted for leave the exposure any independent variation.
#'   An estimate without it is the model extrapolating.
#' @param min_residual Share of the exposure's variance that must survive
#'   adjustment before the estimate is treated as measurement rather than
#'   extrapolation.
#'
#' @return An \code{analysis_assumptions} object.
#'
#' @seealso \code{\link{analyze}}, \code{\link{analysis_control}}, \code{\link{check_dag}}
#'
#' @examples
#' analysis_assumptions(
#'   dag = data.frame(from = "age", to = c("bmi", "death")),
#'   modifiable = "diet",
#'   reliability = c(bmi = 0.95)
#' )
#'
#'
#' @export
analysis_assumptions <- function(dag = NULL,
                                 modifiable = NULL,
                                 negative_controls = NULL,
                                 heterogeneity = NULL,
                                 reliability = NULL,
                                 competing = NULL,
                                 forbidden = NULL,
                                 required = NULL,
                                 positivity = TRUE,
                                 min_residual = 0.1) {

  if (!is.null(reliability)) {

    if (!is.numeric(reliability) || is.null(names(reliability))) {
      stop(paste("'reliability' must be a named numeric vector: one share of",
                 "real variance per variable."), call. = FALSE)
    }

    if (any(!is.finite(reliability) | reliability <= 0 | reliability > 1)) {
      stop(paste("Every reliability must be between 0 and 1. It is the share",
                 "of the measured variance that is real, not a p-value."),
           call. = FALSE)
    }

  }

  if (!is.numeric(min_residual) || min_residual <= 0 || min_residual >= 1) {
    stop("'min_residual' must be between 0 and 1.", call. = FALSE)
  }

  structure(
    list(dag = dag, modifiable = modifiable,
         negative_controls = negative_controls,
         heterogeneity = heterogeneity, reliability = reliability,
         competing = competing, forbidden = forbidden, required = required,
         positivity = isTRUE(positivity), min_residual = min_residual),
    class = "analysis_assumptions"
  )

}

#' How much work to do, and on what
#'
#' Nothing here changes what the analysis is allowed to conclude. These are
#' decisions about cost and scope: getting one wrong makes the answer slower
#' or noisier, never wrong.
#'
#' @param methods Which evidence generators to run. All applicable ones by
#'   default.
#' @param goal \code{"causal"} or \code{"predictive"}, which changes how
#'   features are screened.
#' @param resample How many resamples for stability. Taken from
#'   \code{effort} when left alone.
#' @param resample_scheme \code{"bootstrap"} or \code{"cv"}.
#' @param resample_methods Which generators to re-run on each resample.
#' @param permutations How many outcome permutations for null calibration.
#' @param diagnostics Run model diagnostics, effect concentration and the
#'   complete-case check.
#' @param bootstrap Resamples inside the mediation generator.
#' @param max_features Cap on features carried into the pairwise stage.
#' @param min_per_block Features guaranteed to each block during screening,
#'   so a large block cannot take every slot.
#' @param min_evidence_score Relationships scoring below this are dropped.
#' @param seed Passed to \code{set.seed()} where the engine needs randomness.
#'   The caller's generator is restored afterwards.
#'
#' @return An \code{analysis_control} object.
#'
#' @seealso \code{\link{analyze}}, \code{\link{analysis_assumptions}}, \code{\link{cmo_setup}}
#'
#' @examples
#' analysis_control(methods = c("association", "survival"), max_features = 200)
#'
#'
#' @export
analysis_control <- function(methods = NULL,
                             goal = c("causal", "predictive"),
                             resample = NULL,
                             resample_scheme = c("bootstrap", "cv"),
                             resample_methods = NULL,
                             permutations = NULL,
                             diagnostics = NULL,
                             bootstrap = 200,
                             max_features = 150,
                             min_per_block = 10,
                             min_evidence_score = 1,
                             seed = 1L) {

  structure(
    list(methods = methods,
         goal = match.arg(goal),
         resample = resample,
         resample_scheme = match.arg(resample_scheme),
         resample_methods = resample_methods,
         permutations = permutations,
         diagnostics = diagnostics,
         bootstrap = bootstrap,
         max_features = max_features,
         min_per_block = min_per_block,
         min_evidence_score = min_evidence_score,
         seed = seed),
    class = "analysis_control"
  )

}

#' Accept a group, or build the default one
#'
#' @param value What the caller passed.
#' @param builder The constructor for that group.
#' @param label Its name, for the message.
#'
#' @return A validated group object.
#' @keywords internal
#' @noRd
.as_group <- function(value, builder, label) {

  if (is.null(value)) return(builder())

  if (inherits(value, label)) return(value)

  # A list is accepted so the two calls can be written inline, but anything
  # else is a mistake worth naming: passing dag = directly to analyze() is
  # the obvious one now that it lives in a group.

  if (is.list(value)) return(do.call(builder, value))

  stop(sprintf("'%s' must be built with %s().",
               sub("^analysis_", "", label), label), call. = FALSE)

}

#' Print what is being assumed
#'
#' @param x An \code{analysis_assumptions}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export
print.analysis_assumptions <- function(x, ...) {

  cat("\nanalysis_assumptions\n")
  cat("====================\n\n")

  say <- function(label, value) {

    if (is.null(value) || length(value) == 0) return(invisible(NULL))

    text <- if (is.data.frame(value)) sprintf("%d relationship(s)", nrow(value))
    else if (!is.null(names(value)))
      paste(sprintf("%s = %s", names(value), format(unname(value))),
            collapse = ", ")
    else paste(as.character(value), collapse = ", ")

    cat(sprintf("  %-18s %s\n", label, text))

  }

  say("causal diagram", x$dag)
  say("modifiable", x$modifiable)
  say("negative controls", x$negative_controls)
  say("effect modifiers", x$heterogeneity)
  say("reliability", x$reliability)
  say("competing events", x$competing)
  say("forbidden arcs", x$forbidden)
  say("required arcs", x$required)

  cat(sprintf("  %-18s %s\n", "positivity", if (x$positivity)
    sprintf("checked, at %.0f%% residual variation", 100 * x$min_residual)
    else "not checked"))

  cat("\n  Every line above is a claim the data cannot check.\n\n")

  invisible(x)

}

#' Print how much work will be done
#'
#' @param x An \code{analysis_control}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export
print.analysis_control <- function(x, ...) {

  cat("\nanalysis_control\n")
  cat("================\n\n")

  for (nm in names(x)) {

    value <- x[[nm]]

    cat(sprintf("  %-20s %s\n", nm,
                if (is.null(value)) "from effort" else
                  paste(as.character(value), collapse = ", ")))

  }

  cat("\n  Nothing here changes what may be concluded.\n\n")

  invisible(x)

}
