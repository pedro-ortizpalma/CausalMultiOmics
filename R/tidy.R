# =============================================================================
# Tidy accessors
#
# Every result object already holds the table a reader wants; reaching it
# meant knowing that it lives in `$graph$edges` and choosing from 27 columns.
# These methods make `as.data.frame()` do what a user of any other R package
# would expect, so that writing a CSV, drawing a custom figure or filtering
# the findings needs no knowledge of the internal layout.
# =============================================================================

# The columns a reader actually uses: who, on what, in which direction, how
# big, how certain, how well identified, and how much evidence stands behind
# it. `all = TRUE` returns everything for anyone who wants the rest.

.cmo_reading_columns <- c(
  "source", "target", "block", "direction", "estimate", "se",
  "p_value", "fdr", "n", "identification", "level", "level_label",
  "evidence_score", "e_value", "n_methods", "methods", "temporal"
)


#' The name of the outcome an analysis was run on
#'
#' The outcome of a \code{CMOResult} is stored as a list, because the engine
#' needs its type, its levels and its values as well as its name. Comparing
#' that list against a character column succeeds by coercion and quietly
#' returns the wrong rows, so reach for the name through this accessor.
#'
#' @param object A \code{CMOResult} object.
#'
#' @return A length-one character vector.
#'
#' @seealso \code{\link{analyze}}, \code{\link{as.data.frame.CMOResult}}
#'
#' @examples
#' data <- simulate_data(n = 60, blocks = list(main = 6), seed = 1)
#' ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)
#' fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
#'                quiet = TRUE,
#'                control = analysis_control(methods = c("association", "conditional")))
#' outcome_name(fit)
#'
#'
#' @export

outcome_name <- function(object) {

  if (inherits(object, "CMOResult")) {

    return(as.character(object$outcome$name))

  }

  if (is.list(object$outcome)) return(as.character(object$outcome$name))

  as.character(object$outcome)

}


.cmo_edge_frame <- function(edges, all = FALSE) {

  if (is.null(edges) || nrow(edges) == 0) return(data.frame())

  if (!isTRUE(all)) {

    keep <- intersect(.cmo_reading_columns, names(edges))

    edges <- edges[, keep, drop = FALSE]

  }

  if ("evidence_score" %in% names(edges)) {

    edges <- edges[order(-edges$evidence_score), , drop = FALSE]

  }

  rownames(edges) <- NULL

  edges

}


#' The relationships an analysis found, as a data frame
#'
#' Returns one row per relationship, ordered by evidence score, carrying the
#' columns a reader uses. The full table the engine produced is available with
#' \code{all = TRUE}.
#'
#' @param x A \code{CMOResult} object.
#' @param ... Ignored.
#' @param all Return every column the engine produced rather than the reading
#'   subset.
#' @param outcome_only Keep only the relationships involving the outcome.
#' @param min_score Drop relationships scoring below this value.
#'
#' @return A \code{data.frame}, ordered by \code{evidence_score}. Empty when
#'   the analysis found nothing.
#'
#' @seealso \code{\link{analyze}}, \code{\link{explain}},
#'   \code{\link{outcome_name}}
#'
#' @examples
#' data <- simulate_data(n = 80, blocks = list(main = 8), seed = 1)
#' ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)
#' fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
#'                quiet = TRUE,
#'                control = analysis_control(methods = c("association", "conditional")))
#'
#' head(as.data.frame(fit))
#'
#' # Only what bears on the outcome, and only the better-supported findings.
#' as.data.frame(fit, outcome_only = TRUE, min_score = 10)
#'
#'
#' @export

as.data.frame.CMOResult <- function(x, ..., all = FALSE, outcome_only = FALSE,
                                    min_score = 0) {

  edges <- x$graph$edges

  if (is.null(edges) || nrow(edges) == 0) return(data.frame())

  if (isTRUE(outcome_only)) {

    outcome <- outcome_name(x)

    edges <- edges[edges$source == outcome | edges$target == outcome, ,
                   drop = FALSE]

  }

  if (is.finite(min_score) && min_score > 0 &&
      "evidence_score" %in% names(edges)) {

    edges <- edges[edges$evidence_score >= min_score, , drop = FALSE]

  }

  .cmo_edge_frame(edges, all = all)

}


#' The edges of an evidence graph, as a data frame
#'
#' @param x An \code{EvidenceGraph} object.
#' @param ... Ignored.
#' @param all Return every column rather than the reading subset.
#'
#' @return A \code{data.frame}, ordered by \code{evidence_score}.
#'
#' @seealso \code{\link{as.data.frame.CMOResult}}
#'
#' @export

as.data.frame.EvidenceGraph <- function(x, ..., all = FALSE) {

  .cmo_edge_frame(x$edges, all = all)

}


#' The analysis variants a sensitivity run fitted, as a data frame
#'
#' One row per preprocessing path, so that the spread of the estimate across
#' defensible choices can be plotted or tabulated directly.
#'
#' @param x A \code{CMOSensitivity} object.
#' @param ... Ignored.
#'
#' @return A \code{data.frame} with the source and target of the relationship
#'   added to every row.
#'
#' @seealso \code{\link{sensitivity}}
#'
#' @export

as.data.frame.CMOSensitivity <- function(x, ...) {

  paths <- x$paths

  if (is.null(paths) || NROW(paths) == 0) return(data.frame())

  out <- as.data.frame(paths, stringsAsFactors = FALSE)

  out$source <- x$source
  out$target <- x$target

  rownames(out) <- NULL

  out

}


#' The counterfactual contrasts, as a data frame
#'
#' @param x A \code{CMOCounterfactual} object.
#' @param ... Ignored.
#'
#' @return A \code{data.frame}, one row per modifiable variable.
#'
#' @seealso \code{\link{counterfactual}}
#'
#' @export

as.data.frame.CMOCounterfactual <- function(x, ...) {

  if (is.null(x$table) || NROW(x$table) == 0) return(data.frame())

  out <- as.data.frame(x$table, stringsAsFactors = FALSE)

  rownames(out) <- NULL

  out

}


#' The block-level audit, as a data frame
#'
#' @param x A \code{CMOValidation} object.
#' @param ... Ignored.
#'
#' @return A \code{data.frame}, one row per block.
#'
#' @seealso \code{\link{check_data}}
#'
#' @export

as.data.frame.CMOValidation <- function(x, ...) {

  summary_table <- x$tables$block_summary

  if (is.null(summary_table) || NROW(summary_table) == 0) return(data.frame())

  out <- as.data.frame(summary_table, stringsAsFactors = FALSE)

  rownames(out) <- NULL

  out

}


#' What each preprocessing stage did, as a data frame
#'
#' One row per stage and block, with the dimensions before and after, so the
#' shape of the data can be traced through the pipeline.
#'
#' @param x A \code{PreprocessingResult} object.
#' @param ... Ignored.
#'
#' @return A \code{data.frame}, one row per recorded step.
#'
#' @seealso \code{\link{preprocess}}
#'
#' @export

as.data.frame.PreprocessingResult <- function(x, ...) {

  steps <- x$tables$steps

  if (is.null(steps) || NROW(steps) == 0) return(data.frame())

  out <- as.data.frame(steps, stringsAsFactors = FALSE)

  rownames(out) <- NULL

  out

}
