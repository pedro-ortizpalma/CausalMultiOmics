# =============================================================================
# Reaching the plots that are already there
#
# Every audit, preprocessing run and analysis records its figures inside the
# object it returns. Without a plot() method, base R answers
# `plot(result)` with "'x' is a list, but does not have components 'x' and
# 'y'", and the only documented route is
# grDevices::replayPlot(x$plots$histograms$clinical) -- correct, but nobody
# guesses it. These methods are a door onto what the object already holds;
# they draw nothing new.
# =============================================================================

.cmo_is_recorded <- function(x) {

  inherits(x, "recordedplot") || inherits(x, "recordedPlot")

}


.cmo_plot_slots <- function(x) {

  plots <- x$plots

  if (is.null(plots) || length(plots) == 0) return(NULL)

  # An object built with plots = FALSE still carries the named slots, all
  # empty. Treating that as "has figures" produces a puzzling error about one
  # slot when the real answer is that nothing was recorded at all.

  recorded <- function(slot) {

    if (.cmo_is_recorded(slot)) return(TRUE)

    is.list(slot) && length(slot) > 0 &&
      any(vapply(slot, .cmo_is_recorded, logical(1)))

  }

  if (!any(vapply(plots, recorded, logical(1)))) return(NULL)

  plots[vapply(plots, recorded, logical(1))]

}


#' Which figures an object carries
#'
#' Lists the recorded plots stored inside a result object, and for the ones
#' that were drawn per block, which blocks are available.
#'
#' @param x A \code{CMOValidation}, \code{PreprocessingResult} or
#'   \code{CMOResult} object.
#'
#' @return The plot names, invisibly. Called for the listing it prints.
#'
#' @seealso \code{\link{plot.CMOValidation}}
#'
#' @examples
#' data <- simulate_data(n = 60, blocks = list(main = 6), seed = 1)
#' audit <- check_data(data)
#' cmo_plots(audit)
#'
#'
#' @export

cmo_plots <- function(x) {

  plots <- .cmo_plot_slots(x)

  if (is.null(plots)) {

    message("This object carries no figures. Run it again with plots = TRUE.")

    return(invisible(character(0)))

  }

  cat(sprintf("\n%d figure(s) available\n\n", length(plots)))

  for (name in names(plots)) {

    slot <- plots[[name]]

    blocks <- if (!.cmo_is_recorded(slot) && is.list(slot) &&
                  !is.null(names(slot)))
      sprintf("   per block: %s", paste(names(slot), collapse = ", ")) else ""

    cat(sprintf("  %-28s%s\n", name, blocks))

  }

  cat(sprintf("\nDraw one with:  plot(x, \"%s\")\n", names(plots)[1]))

  invisible(names(plots))

}


.cmo_draw <- function(x, which, block) {

  plots <- .cmo_plot_slots(x)

  if (is.null(plots)) {

    stop("This object carries no figures. Run it again with plots = TRUE.",
         call. = FALSE)

  }

  # No name asked for: list rather than fail. A user typing plot(result) is
  # asking "what can I see?", and an error teaches them nothing.

  if (is.null(which)) {

    cmo_plots(x)

    return(invisible(NULL))

  }

  if (!which %in% names(plots)) {

    stop(sprintf("There is no figure called '%s'.\n  Available: %s",
                 which, paste(names(plots), collapse = ", ")), call. = FALSE)

  }

  slot <- plots[[which]]

  if (!.cmo_is_recorded(slot) && is.list(slot)) {

    if (is.null(block)) {

      if (length(slot) == 0L) {

        stop(sprintf("No block recorded a '%s' figure.", which),
             call. = FALSE)

      } else if (length(slot) == 1L) {

        slot <- slot[[1]]

      } else {

        stop(sprintf(paste0(
          "'%s' was drawn once per block.\n",
          "  Choose one: plot(x, \"%s\", block = \"%s\")\n",
          "  Blocks: %s"),
          which, which, names(slot)[1],
          paste(names(slot), collapse = ", ")), call. = FALSE)

      }

    } else {

      if (!block %in% names(slot)) {

        stop(sprintf("Block '%s' has no '%s' figure.\n  Available: %s",
                     block, which, paste(names(slot), collapse = ", ")),
             call. = FALSE)

      }

      slot <- slot[[block]]

    }

  }

  if (!.cmo_is_recorded(slot)) {

    stop(sprintf("The '%s' slot holds no recorded figure.", which),
         call. = FALSE)

  }

  grDevices::replayPlot(slot)

  invisible(slot)

}


#' Draw a figure recorded during the audit
#'
#' Called without \code{which}, lists what is available instead of failing.
#'
#' @param x A \code{CMOValidation} object.
#' @param which Name of the figure. Use \code{cmo_plots(x)} to see the names.
#' @param block Block name, for figures drawn once per block.
#' @param ... Ignored.
#'
#' @return The recorded plot, invisibly.
#'
#' @seealso \code{\link{cmo_plots}}, \code{\link{check_data}}
#'
#' @examples
#' data <- simulate_data(n = 60, blocks = list(main = 6), seed = 1)
#' audit <- check_data(data)
#'
#' plot(audit)                                  # what is available
#' plot(audit, "missing_by_block")
#'
#'
#' @export

plot.CMOValidation <- function(x, which = NULL, block = NULL, ...) {

  .cmo_draw(x, which, block)

}


#' Draw a figure recorded during an analysis
#'
#' @inheritParams plot.CMOValidation
#' @param x A \code{CMOResult} object.
#'
#' @return The recorded plot, invisibly.
#'
#' @seealso \code{\link{cmo_plots}}, \code{\link{analyze}}
#'
#' @export

plot.CMOResult <- function(x, which = NULL, block = NULL, ...) {

  .cmo_draw(x, which, block)

}


#' Draw a figure recorded during preprocessing
#'
#' @inheritParams plot.CMOValidation
#' @param x A \code{PreprocessingResult} object.
#'
#' @return The recorded plot, invisibly.
#'
#' @seealso \code{\link{cmo_plots}}, \code{\link{preprocess}}
#'
#' @export

plot.PreprocessingResult <- function(x, which = NULL, block = NULL, ...) {

  .cmo_draw(x, which, block)

}
