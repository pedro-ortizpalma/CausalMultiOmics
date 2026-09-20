# =============================================================================
# Two analyses, side by side
# =============================================================================
#
# Comparing two results is not set intersection, however much it looks like
# it. Two graphs can be compared only if the analyses behind them estimated
# the same quantity on the same kind of outcome with the same adjustment; if
# they did not, an overlap of 60% is a number about two different questions
# and means nothing at all.
#
# So the comparability check comes first and its failures are reported as
# loudly as the comparison itself. This is also why there is no
# meta_analyze(): pooling requires everything comparability requires and
# more, and a function that pooled regardless would be the most dangerous
# thing in the package.
# =============================================================================

#' Compare two analyses of the same question
#'
#' Answers the four questions a reader has when handed two results: which
#' relationships appear in both, which reverse, which vanish, and which
#' survive at a size that no longer means the same thing.
#'
#' @section Why the last one matters most:
#'
#' A relationship that holds in both cohorts at a fifth of the size is the
#' usual way a replication is oversold. Both are statistically significant,
#' both point the same way, and a comparison judged on presence alone calls
#' it agreement. Size is reported as a ratio for that reason.
#'
#' @section What it will not do:
#'
#' It does not pool. Two estimates can be compared without being averaged,
#' and averaging them needs them to be on the same scale with the same
#' adjustment, which the comparability check exists to test rather than
#' assume.
#'
#' @param a,b Two \code{CMOResult} objects.
#' @param names What to call them in the output.
#' @param tolerance How far the second estimate may fall from the first, as a
#'   ratio, before the relationship is reported as agreeing in direction only.
#'   The default of 0.5 flags anything that halved.
#'
#' @return A \code{CMOComparison}.
#'
#' @seealso \code{\link{analyze}}, \code{\link{sensitivity}}
#'
#' @examples
#' \donttest{
#' one <- simulate_data(n = 200, blocks = list(a = c("x", "z")),
#'                      dag = data.frame(from = "x", to = "y", effect = 0.8),
#'                      outcome = "y", seed = 1)
#' two <- simulate_data(n = 200, blocks = list(a = c("x", "z")),
#'                      dag = data.frame(from = "x", to = "y", effect = 0.8),
#'                      outcome = "y", seed = 2)
#'
#' fit <- function(s) {
#'   p <- preprocess(s, check_data(s), plots = FALSE, quiet = TRUE)
#'   analyze(p, "y", effort = "fast", plots = FALSE, quiet = TRUE,
#'           control = analysis_control(methods = "association"))
#' }
#'
#' compare_results(fit(one), fit(two))
#' }
#'
#'
#' @export
compare_results <- function(a, b, names = c("first", "second"),
                            tolerance = 0.5) {

  if (!inherits(a, "CMOResult") || !inherits(b, "CMOResult")) {
    stop("'a' and 'b' must both be CMOResult objects.", call. = FALSE)
  }

  if (length(names) != 2) {
    stop("'names' must be two labels.", call. = FALSE)
  }

  out <- structure(
    list(names = as.character(names), tolerance = tolerance,
         comparable = list(), edges = data.frame(), agreement = list(),
         notes = character()),
    class = "CMOComparison"
  )

  out$comparable <- .comparability(a, b)

  edges_a <- .comparison_edges(a)
  edges_b <- .comparison_edges(b)

  if (nrow(edges_a) == 0 || nrow(edges_b) == 0) {

    out$notes <- "At least one of the two analyses found no relationships."

    return(out)

  }

  merged <- merge(edges_a, edges_b, by = c("source", "target"),
                  all = TRUE, suffixes = c("_a", "_b"))

  merged$in_a <- !is.na(merged$estimate_a)
  merged$in_b <- !is.na(merged$estimate_b)

  merged$same_direction <- with(merged,
    in_a & in_b & is.finite(estimate_a) & is.finite(estimate_b) &
      sign(estimate_a) == sign(estimate_b))

  merged$ratio <- with(merged, ifelse(
    in_a & in_b & is.finite(estimate_a) & estimate_a != 0,
    estimate_b / estimate_a, NA_real_))

  merged$status <- vapply(seq_len(nrow(merged)), function(i) {

    if (!merged$in_a[i]) return(sprintf("only in the %s", out$names[2]))
    if (!merged$in_b[i]) return(sprintf("only in the %s", out$names[1]))

    if (!isTRUE(merged$same_direction[i])) return("reverses")

    r <- merged$ratio[i]

    if (!is.finite(r)) return("in both")

    if (r < tolerance) return("in both, much smaller")
    if (r > 1 / tolerance) return("in both, much larger")

    "in both, consistent"

  }, character(1))

  merged <- merged[order(-pmax(.report_or(merged$score_a, 0),
                               .report_or(merged$score_b, 0),
                               na.rm = TRUE)), ]

  rownames(merged) <- NULL

  out$edges <- merged

  key <- paste(merged$source, "->", merged$target)

  out$agreement <- list(
    n_a = nrow(edges_a),
    n_b = nrow(edges_b),
    both = sum(merged$in_a & merged$in_b),
    only_a = key[merged$in_a & !merged$in_b],
    only_b = key[!merged$in_a & merged$in_b],
    reversed = key[merged$status == "reverses"],
    shrunk = key[merged$status == "in both, much smaller"],
    grown = key[merged$status == "in both, much larger"],
    jaccard = sum(merged$in_a & merged$in_b) / nrow(merged)
  )

  out$notes <- c(
    sprintf("%d relationship(s) in both, %d only in the %s, %d only in the %s.",
            out$agreement$both, length(out$agreement$only_a), out$names[1],
            length(out$agreement$only_b), out$names[2]),

    paste("A relationship missing from one of two analyses has not been",
          "refuted by it. It may simply not have cleared the threshold there,",
          "and the two lists are not a test of each other.")
  )

  if (!isTRUE(out$comparable$comparable)) {

    out$notes <- c(
      "The two analyses did not ask the same question; see the warnings above. Everything below compares numbers that may not be on the same scale.",
      out$notes)

  }

  out

}

#' Are these two results even comparable?
#'
#' The check that has to come first. Two graphs can be compared only if the
#' analyses behind them estimated the same quantity on the same kind of
#' outcome with the same adjustment. Sixty per cent overlap between two
#' different questions is a number about nothing.
#'
#' @param a,b Two results.
#'
#' @return A list with a verdict and the reasons for it.
#' @keywords internal
#' @noRd
.comparability <- function(a, b) {

  problems <- character()

  outcome_a <- .report_or(a$outcome$name, NA_character_)
  outcome_b <- .report_or(b$outcome$name, NA_character_)

  if (!identical(outcome_a, outcome_b)) {
    problems <- c(problems, sprintf(
      "Different outcomes: '%s' and '%s'. These are answers to two questions.",
      outcome_a, outcome_b))
  }

  type_a <- .report_or(a$outcome$type, NA_character_)
  type_b <- .report_or(b$outcome$type, NA_character_)

  if (!identical(type_a, type_b)) {
    problems <- c(problems, sprintf(
      "Different kinds of outcome: %s and %s, so the estimates are not on the same scale.",
      type_a, type_b))
  }

  design_a <- .report_or(a$design$type, NA_character_)
  design_b <- .report_or(b$design$type, NA_character_)

  if (!identical(design_a, design_b)) {
    problems <- c(problems, sprintf(
      "Different designs: %s and %s, so different methods applied.",
      design_a, design_b))
  }

  adjust <- function(r) sort(unique(unlist(
    lapply(r$evidence, function(e) e$adjustment_set))))

  set_a <- adjust(a)
  set_b <- adjust(b)

  if (!identical(set_a, set_b)) {

    problems <- c(problems, sprintf(
      "Different adjustment: %s against %s. An estimate adjusted for different things is a different estimate.",
      if (length(set_a) == 0) "nothing" else paste(set_a, collapse = ", "),
      if (length(set_b) == 0) "nothing" else paste(set_b, collapse = ", ")))

  }

  families <- function(r) sort(unique(stats::na.omit(vapply(
    r$evidence, function(e) .report_or(e$quantity_family, NA_character_),
    character(1)))))

  fam_a <- families(a)
  fam_b <- families(b)

  if (length(intersect(fam_a, fam_b)) == 0 &&
      length(fam_a) > 0 && length(fam_b) > 0) {

    problems <- c(problems, sprintf(
      "No quantity in common: %s against %s. There is nothing here to compare.",
      paste(fam_a, collapse = ", "), paste(fam_b, collapse = ", ")))

  }

  list(
    comparable = length(problems) == 0,
    problems = problems,
    outcome = c(outcome_a, outcome_b),
    design = c(design_a, design_b)
  )

}

#' The two columns a comparison needs from a result
#'
#' @param r A CMOResult.
#'
#' @return A data.frame of source, target, estimate and score.
#' @keywords internal
#' @noRd
.comparison_edges <- function(r) {

  if (length(r$evidence) == 0) {
    return(data.frame(source = character(), target = character(),
                      estimate = numeric(), score = numeric(),
                      stringsAsFactors = FALSE))
  }

  data.frame(
    source = vapply(r$evidence, function(e) e$source, character(1)),
    target = vapply(r$evidence, function(e) e$target, character(1)),
    estimate = vapply(r$evidence, function(e)
      .report_or(e$estimate, NA_real_), numeric(1)),
    score = vapply(r$evidence, function(e)
      .report_or(e$evidence_score, NA_real_), numeric(1)),
    stringsAsFactors = FALSE
  )

}

#' Print a comparison of two analyses
#'
#' @param x A \code{CMOComparison}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export
print.CMOComparison <- function(x, ...) {

  cat("\n")
  cat("CMOComparison\n")
  cat("=============\n\n")

  cat(sprintf("  %s  vs  %s\n\n", x$names[1], x$names[2]))

  # Before anything else, because a comparison of two different questions is
  # a number about nothing and the reader has to know that first.

  if (!isTRUE(x$comparable$comparable)) {

    cat("These two analyses did not ask the same question\n")
    cat(strrep("-", 66), "\n", sep = "")

    for (p in x$comparable$problems) {
      cat(paste(strwrap(p, width = 66, prefix = "    ", initial = "  ! "),
                collapse = "\n"), "\n", sep = "")
    }

    cat("\n")

  } else {

    cat("  Same outcome, same design, same adjustment: comparable.\n\n")

  }

  if (!is.data.frame(x$edges) || nrow(x$edges) == 0) {

    for (n in x$notes) cat("  ", n, "\n", sep = "")
    cat("\n")

    return(invisible(x))

  }

  a <- x$agreement

  cat(sprintf("%-28s %d\n", sprintf("Found in the %s:", x$names[1]), a$n_a))
  cat(sprintf("%-28s %d\n", sprintf("Found in the %s:", x$names[2]), a$n_b))
  cat(sprintf("%-28s %d\n", "In both:", a$both))
  cat(sprintf("%-28s %s\n", "Overlap (Jaccard):", fmt_num(a$jaccard, 2)))

  section <- function(title, keys, explanation) {

    if (length(keys) == 0) return(invisible(NULL))

    cat("\n", title, "\n", sep = "")
    cat(strrep("-", 66), "\n", sep = "")

    for (k in utils::head(keys, 10)) cat("  ", k, "\n", sep = "")

    if (length(keys) > 10)
      cat("  ... and ", length(keys) - 10, " more\n", sep = "")

    cat(paste(strwrap(explanation, width = 66, prefix = "  "),
              collapse = "\n"), "\n", sep = "")

  }

  section("Reverses direction", a$reversed,
          "The clearest disagreement there is: one analysis says higher and the other says lower.")

  section("Present in both, much smaller in the second", a$shrunk,
          "The usual way a replication is oversold. Both significant, both the same direction, and the effect is a fraction of what it was.")

  section("Present in both, much larger in the second", a$grown,
          "Worth the same suspicion in the other direction.")

  section(sprintf("Only in the %s", x$names[1]), a$only_a,
          "Not refuted by the second analysis. It may simply not have cleared the threshold there.")

  section(sprintf("Only in the %s", x$names[2]), a$only_b,
          "Nothing in the first analysis rules these out either.")

  cat("\nHow to read this\n")
  cat(strrep("-", 66), "\n", sep = "")

  for (n in x$notes) {
    cat(paste(strwrap(n, width = 66, prefix = "  "), collapse = "\n"),
        "\n", sep = "")
  }

  cat("\n  Nothing here is pooled. Comparing two estimates does not average\n")
  cat("  them, and averaging needs them on one scale with one adjustment.\n")

  cat("\n")

  invisible(x)

}
