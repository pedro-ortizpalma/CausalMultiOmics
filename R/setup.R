# =============================================================================
# What this installation can actually run
#
# The package imports base R only and leaves every modelling dependency in
# Suggests, which keeps installation trivial on any machine. The cost is that
# a newcomer runs three of the nine evidence generators without knowing it:
# the notice exists, but inside summary() of a result, that is, after the
# analysis. This asks the question beforehand.
#
# It matters more than a missing convenience. The evidence score weights
# agreement between methods, so a relationship seen by one generator scores
# below the same relationship seen by four. An incomplete installation does
# not return less evidence; it returns evidence biased downwards.
# =============================================================================

# Optional dependencies of the preprocessing registry. The registry itself
# records `requires` for the generators, but the preprocessing entries that
# need a package are known here, keyed by stage and method.

.cmo_optional_preprocessing <- function() {

  list(
    imputation = c(knn = "VIM", mice = "mice", missForest = "missForest"),
    batch = c(combat = "sva", harmony = "harmony", ruv = "RUVSeq"),
    feature_selection = c(boruta = "Boruta", lasso = "glmnet",
                          relief = "FSelectorRcpp")
  )

}


#' What this installation can run, and what it is missing
#'
#' Reports which evidence generators and which optional preprocessing methods
#' are available on this machine, and prints the \code{install.packages()}
#' call that would enable the rest. The generator list is read from the
#' engine's own registry, so it stays correct as methods are added.
#'
#' @param install Install the missing CRAN packages instead of only naming
#'   them.
#'
#' @return A \code{data.frame} of class \code{cmo_setup}, one row per
#'   generator and per optional preprocessing method, with columns
#'   \code{component}, \code{label}, \code{status}, \code{requires},
#'   \code{missing} and \code{kind}.
#'
#' @seealso \code{\link{analyze}}, \code{\link{analysis_control}}
#'
#' @examples
#' cmo_setup()
#'
#'
#' @export

cmo_setup <- function(install = FALSE) {

  registry <- .evidence_registry()

  generators <- do.call(rbind, lapply(names(registry), function(name) {

    required <- registry[[name]]$requires

    present <- if (length(required) == 0) logical(0) else
      .pkg_available(required)

    data.frame(
      component = name,
      label = registry[[name]]$label,
      status = if (length(required) == 0 || all(present))
        "available" else "unavailable",
      requires = paste(required, collapse = ", "),
      missing = if (length(required) == 0) "" else
        paste(required[!present], collapse = ", "),
      kind = "generator",
      stringsAsFactors = FALSE
    )

  }))

  methods <- .method_registry()
  optional <- .cmo_optional_preprocessing()

  preprocessing <- do.call(rbind, lapply(names(optional), function(stage) {

    do.call(rbind, lapply(names(optional[[stage]]), function(method) {

      if (!method %in% names(methods[[stage]])) return(NULL)

      package <- unname(optional[[stage]][[method]])
      present <- .pkg_available(package)

      data.frame(
        component = paste0(stage, ":", method),
        label = sprintf("%s method", stage),
        status = if (present) "available" else "unavailable",
        requires = package,
        missing = if (present) "" else package,
        kind = "preprocessing",
        stringsAsFactors = FALSE
      )

    }))

  }))

  out <- rbind(generators, preprocessing)

  class(out) <- c("cmo_setup", "data.frame")

  if (isTRUE(install)) {

    needed <- unique(unlist(strsplit(out$missing[nzchar(out$missing)], ", ")))

    if (length(needed) > 0) utils::install.packages(needed)

  }

  out

}


#' Print what this installation can run
#'
#' @param x A \code{cmo_setup} object.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export

print.cmo_setup <- function(x, ...) {

  generators <- x[x$kind == "generator", , drop = FALSE]
  preprocessing <- x[x$kind == "preprocessing", , drop = FALSE]

  available <- sum(generators$status == "available")

  cat("\n")
  cat("CausalMultiOmics setup\n")
  cat("======================\n\n")

  cat(sprintf("Evidence generators: %d of %d available\n\n",
              available, nrow(generators)))

  for (i in seq_len(nrow(generators))) {

    mark <- if (generators$status[i] == "available") "  ok  " else "  --  "

    note <- if (nzchar(generators$missing[i]))
      sprintf("needs %s", generators$missing[i]) else ""

    cat(sprintf("%s%-14s %-30s %s\n", mark, generators$component[i],
                generators$label[i], note))

  }

  unavailable <- sum(preprocessing$status == "unavailable")

  if (unavailable > 0) {

    cat(sprintf("\nOptional preprocessing methods unavailable: %d of %d\n",
                unavailable, nrow(preprocessing)))

    for (i in which(preprocessing$status == "unavailable")) {

      cat(sprintf("  --  %-30s needs %s\n", preprocessing$component[i],
                  preprocessing$missing[i]))

    }

  }

  needed <- unique(unlist(strsplit(x$missing[nzchar(x$missing)], ", ")))

  if (length(needed) == 0) {

    cat("\nNothing is missing: every generator can run.\n")

    return(invisible(x))

  }

  cat("\nTo enable the rest:\n\n")

  cat(sprintf('  install.packages(c(%s))\n',
              paste0('"', needed, '"', collapse = ", ")))

  cat("\n  or:  cmo_setup(install = TRUE)\n")

  cat(paste0(
    "\nAn analysis runs without them, with fewer methods. That is not the\n",
    "same as a smaller answer: the evidence score weights agreement between\n",
    "methods, so a relationship seen by one generator scores below the same\n",
    "relationship seen by four.\n"))

  invisible(x)

}
