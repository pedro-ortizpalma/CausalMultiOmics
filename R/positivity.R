# =============================================================================
# The identification assumption nobody checks
# =============================================================================
#
# Reading an adjusted estimate causally takes two conditions, not one.
#
# The first is no unmeasured confounding, and this package works hard at it:
# the DAG audit, the E-value, negative controls, the assumptions carried on
# every edge.
#
# The second is positivity, and until now nothing here said a word about it.
# At every combination of the adjustment variables there has to be variation
# in the exposure. If everyone over seventy is hypertensive, the effect of
# hypertension at seventy is not estimable from the data: the model does not
# fail, it extrapolates, and hands back a coefficient with an interval like
# any other.
#
# Unlike unmeasured confounding, this one is checkable. It is a fact about
# the data in hand rather than about the world, which is exactly why leaving
# it unstated was the wrong silence to keep.
# =============================================================================

#' How much independent variation an exposure has left after adjustment
#'
#' Positivity fails on a continuum rather than at a threshold, so the useful
#' quantity is how much of the exposure survives the adjustment. Regress the
#' exposure on the variables being adjusted for: whatever variance is left is
#' what an effect can be estimated from. If they predict it almost perfectly
#' there is nothing left, and the coefficient that comes out is the model
#' extrapolating into a region the data never visited.
#'
#' @param x The exposure.
#' @param covariates A data.frame of what is being adjusted for, or NULL.
#'
#' @return A list with the share of the exposure's variance left after
#'   adjustment, or \code{NULL} when there is nothing to check.
#' @keywords internal
#' @noRd
.residual_variation <- function(x, covariates) {

  if (is.null(covariates) || ncol(covariates) == 0) {

    # Nothing was adjusted for, so nothing can have been adjusted away. That
    # is a complete answer, not a missing one.
    return(list(residual = 1, r_squared = 0, adjusted_for = 0L))

  }

  frame <- cbind(data.frame(.x = x), covariates)
  frame <- frame[stats::complete.cases(frame), , drop = FALSE]

  if (nrow(frame) < 10 || stats::sd(frame$.x) == 0) return(NULL)

  fit <- .safe_try(stats::lm(.x ~ ., data = frame), NULL)

  if (is.null(fit)) return(NULL)

  r2 <- .safe_try(summary(fit)$r.squared, NA_real_)

  if (!is.finite(r2)) return(NULL)

  list(residual = 1 - r2, r_squared = r2, adjusted_for = ncol(covariates))

}

#' Strata of the adjustment set in which the exposure never varies
#'
#' The textbook form of the problem, for an exposure with two levels. A
#' stratum containing only exposed people contributes nothing to a contrast
#' between exposed and unexposed, and a model that includes it is filling in
#' the missing half from the shape of its own equation.
#'
#' Continuous covariates are cut into quantile bins, because exact strata of
#' a continuous variable each contain one person and would report every study
#' ever done as hopeless.
#'
#' @param x The exposure.
#' @param covariates What is being adjusted for.
#' @param bins How many bins to cut a continuous covariate into.
#'
#' @return A list describing the strata with no contrast, or \code{NULL}.
#' @keywords internal
#' @noRd
.empty_strata <- function(x, covariates, bins = 4L) {

  if (is.null(covariates) || ncol(covariates) == 0) return(NULL)

  levels_present <- unique(stats::na.omit(x))

  # Only meaningful for an exposure with a small number of levels. For a
  # continuous one every stratum contains a range rather than a contrast,
  # and .residual_variation() is the question that can be asked instead.
  if (length(levels_present) > 5) return(NULL)

  discretise <- function(v) {

    if (is.factor(v) || is.character(v) || length(unique(stats::na.omit(v))) <= bins) {
      return(as.character(v))
    }

    cuts <- stats::quantile(v, probs = seq(0, 1, length.out = bins + 1),
                            na.rm = TRUE)

    as.character(cut(v, unique(cuts), include.lowest = TRUE))

  }

  strata <- do.call(paste, c(lapply(covariates, discretise), sep = " | "))

  keep <- !is.na(x) & !is.na(strata)

  if (sum(keep) < 10) return(NULL)

  x <- x[keep]
  strata <- strata[keep]

  contrast <- tapply(x, strata, function(v) length(unique(v)) > 1)

  sizes <- table(strata)

  without <- names(contrast)[!contrast]

  list(
    strata = length(contrast),
    without_contrast = length(without),
    people_stranded = sum(sizes[without]),
    share_stranded = sum(sizes[without]) / length(x),
    worst = if (length(without) == 0) NA_character_ else
      names(sort(sizes[without], decreasing = TRUE))[1]
  )

}

#' Can this effect be estimated from this data at all?
#'
#' @param x The exposure.
#' @param covariates What is being adjusted for, or NULL.
#' @param min_residual Below this share of surviving variance, the estimate
#'   is treated as extrapolation rather than measurement.
#'
#' @return A list with the verdict, the numbers behind it, and what to say.
#' @keywords internal
#' @noRd
.positivity <- function(x, covariates, min_residual = 0.1) {

  variation <- .residual_variation(x, covariates)

  if (is.null(variation)) return(NULL)

  strata <- .safe_try(.empty_strata(x, covariates), NULL)

  problems <- character()

  if (variation$residual < min_residual) {

    problems <- c(problems, sprintf(
      paste("The variables adjusted for account for %.0f%% of this one, so",
            "only %.0f%% of it is free to vary independently. An effect",
            "estimated from that is mostly the model extrapolating."),
      100 * variation$r_squared, 100 * variation$residual))

  }

  if (!is.null(strata) && isTRUE(strata$share_stranded > 0.05)) {

    problems <- c(problems, sprintf(
      paste("%.0f%% of people are in groups where the exposure never varies,",
            "so they contribute no comparison. The model fills that in from",
            "its own shape rather than from anything observed."),
      100 * strata$share_stranded))

  }

  list(
    residual = variation$residual,
    r_squared = variation$r_squared,
    strata = strata,
    holds = length(problems) == 0,
    problems = problems
  )

}

# =============================================================================
# What the instrument did to the estimate
# =============================================================================
#
# `data_quality` records how much of a variable was invented by imputation.
# It says nothing about the values that were measured, and those are not
# exact either. Every assay has error, and classical measurement error in an
# exposure does something specific and one-directional: it attenuates. The
# coefficient comes out systematically closer to zero than the truth, by a
# factor equal to the reliability of the measurement.
#
# The correction is old and simple. What makes it rare is that it needs a
# number almost nobody has: the reliability, from duplicate measurements or
# from the manufacturer. So this is offered rather than applied, and refuses
# to guess.
# =============================================================================

#' Correct an estimate for the attenuation a noisy exposure causes
#'
#' Classical measurement error in an exposure multiplies the coefficient by
#' the reliability of that exposure. Dividing by it undoes the attenuation.
#'
#' @section What this does not fix:
#'
#' Only error in the exposure, and only error that is independent of
#' everything else. Error in a covariate causes residual confounding rather
#' than attenuation, and error correlated with the outcome can bias in either
#' direction. Neither is corrected here, and neither is detectable from the
#' data.
#'
#' The corrected estimate is also less certain than the original, not more:
#' the interval widens by the same factor. An attenuation correction that
#' left the interval alone would turn a noisy measurement into a stronger
#' claim, which is precisely backwards.
#'
#' @param estimate,se The reported coefficient and its standard error.
#' @param reliability The share of the measured variance that is real, from
#'   duplicates or from the assay. Between 0 and 1.
#'
#' @return A list with the corrected estimate, its standard error, and what
#'   the correction assumed.
#' @keywords internal
#' @noRd
.attenuation_correction <- function(estimate, se, reliability) {

  if (!is.finite(reliability) || reliability <= 0 || reliability > 1) {

    stop(paste("Reliability must be between 0 and 1: the share of the",
               "measured variance that is real."), call. = FALSE)

  }

  if (!is.finite(estimate)) return(NULL)

  list(
    estimate = estimate / reliability,
    se = if (is.finite(se)) se / reliability else NA_real_,
    reliability = reliability,
    inflation = 1 / reliability,
    assumptions = c(
      sprintf("Corrected for measurement error assuming reliability %.2f, which came from you and not from the data.",
              reliability),
      "Classical error only: independent of the outcome, the covariates and the true value.",
      "Error in the covariates is not corrected, and causes residual confounding rather than attenuation."
    )
  )

}

# =============================================================================
# Dying of something else is not the same as surviving
# =============================================================================
#
# A Cox model treats everything that is not the event as censoring, and
# censoring means "still at risk, we just stopped looking". For a competing
# event that is false: someone who died of something else is not going to
# have the outcome later. Treating them as censored assumes they could have,
# which in a mortality study over five years is not a small assumption.
#
# What comes out is a cause-specific hazard, which is a real quantity and
# answers a real question. It is simply not the question most readers think
# they are being shown, which is the probability of the event happening.
# =============================================================================

#' What a survival estimate assumes when other causes are possible
#'
#' @param competing Name of a column marking competing events, or NULL.
#' @param n_competing How many people had one.
#' @param n_total How many people there are.
#'
#' @return Assumption text for the edges of a survival analysis.
#' @keywords internal
#' @noRd
.competing_risk_assumptions <- function(competing = NULL, n_competing = 0L,
                                        n_total = 0L) {

  if (is.null(competing)) {

    return(c(
      "Anything that is not the event is treated as censoring, which assumes those people could still have had it.",
      "If they died of another cause they could not have, and this is a cause-specific hazard rather than a risk."
    ))

  }

  share <- if (n_total > 0) n_competing / n_total else NA_real_

  c(
    sprintf("%d of %d people (%.0f%%) had a competing event and were censored at it.",
            n_competing, n_total, 100 * .report_or(share, NA)),
    "This is a cause-specific hazard: the rate among those still able to have the event.",
    "It is not the probability of the event occurring, which would need the competing risk modelled rather than censored.",
    "A cause-specific hazard can rise while the risk falls, if the competing cause rises faster."
  )

}

#' Attach the positivity check and any measurement-error correction
#'
#' Both belong on the edge rather than in a table at the bottom, for the same
#' reason the DAG audit does: nobody cross-references a table against the
#' finding they are reading.
#'
#' @param edges Integrated edges.
#' @param x The analysis matrix.
#' @param covariates Covariate frame, or NULL.
#' @param outcome_name Name of the outcome.
#' @param assume The assumptions the caller declared.
#'
#' @return The edges, with the new fields set where they apply.
#' @keywords internal
#' @noRd
.audit_estimability <- function(edges, x, covariates, outcome_name, assume) {

  reliability <- .report_or(assume$reliability, NULL)

  lapply(edges, function(e) {

    # --- positivity ---------------------------------------------------------

    if (isTRUE(assume$positivity) && identical(e$target, outcome_name) &&
        !is.null(e$source) && e$source %in% colnames(x)) {

      check <- .safe_try(
        .positivity(x[, e$source], covariates, assume$min_residual), NULL)

      if (!is.null(check)) {

        e$positivity <- check$holds
        e$residual_variation <- check$residual
        e$positivity_problems <- check$problems

        if (!check$holds) {

          e$warnings <- c(e$warnings, check$problems)

          e$assumptions <- c(
            e$assumptions,
            "Positivity: at some combinations of the adjustment set this exposure never varies, so part of this estimate is extrapolation.")

        }

      }

    }

    # --- measurement error --------------------------------------------------

    if (!is.null(reliability) && !is.null(e$source) &&
        e$source %in% names(reliability)) {

      r <- unname(reliability[[e$source]])

      corrected <- .safe_try(
        .attenuation_correction(e$estimate, e$se, r), NULL)

      if (!is.null(corrected)) {

        # Reported beside the estimate and never in place of it. The
        # correction rests on a number the caller supplied, so replacing the
        # measured value with it would hide an assumption inside a result.

        e$reliability <- r
        e$corrected_estimate <- corrected$estimate
        e$corrected_ci <- c(corrected$estimate - 1.96 * corrected$se,
                            corrected$estimate + 1.96 * corrected$se)

        e$assumptions <- c(e$assumptions, corrected$assumptions)

        e$notes <- c(e$notes, sprintf(
          "Measurement error would attenuate this by %.0f%%; corrected it is %s rather than %s.",
          100 * (1 - r), format(round(corrected$estimate, 4)),
          format(round(e$estimate, 4))))

      }

    }

    e

  })

}
