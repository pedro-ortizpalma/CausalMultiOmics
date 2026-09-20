# =============================================================================
# Data with a known answer
# =============================================================================
#
# Real data cannot show that an analysis is right. It has no answer to check
# against, so it can demonstrate machinery running and nothing more. Every
# claim this package makes about recovering a mediator, catching an artefact
# of imputation or separating a latent process from its members has to be
# checked against a truth someone planted on purpose.
#
# That checking was being done by thirty-five small generators written one at
# a time across the test suite and the walkthrough, each planting its own
# truth its own way. This is that, once.
# =============================================================================

#' Generate a multi-block study with a known answer
#'
#' Builds a \code{MultiOmicsData} from a causal structure you specify, and
#' records what it did so the result can be checked rather than admired.
#'
#' @section How the data is generated:
#'
#' Variables are produced in topological order over the supplied structure.
#' A variable with no parents is standard normal; a variable with parents is
#' the sum of its parents times their effects, plus noise. So an effect of
#' 0.8 means eight tenths of a standard deviation in the child per standard
#' deviation in the parent, before noise, which is the reading a regression
#' coefficient will recover.
#'
#' Anything named in the structure is created. Names listed in \code{blocks}
#' become measured features in that block; the outcome becomes the outcome;
#' anything else becomes a metadata column, which is how covariates and
#' effect modifiers arrive.
#'
#' @section What the truth records:
#'
#' \code{result$misc$simulation} holds the structure, the effects, which
#' features are pure noise, which cells were blanked, module membership and
#' the seed. A test that asserts a mediator was found can then assert it
#' against the mediator that was planted, rather than against whichever
#' variable came out on top.
#'
#' @param n Number of people.
#' @param blocks Named list. Each element is either a character vector of
#'   feature names to place in that block, or a single number meaning that
#'   many pure-noise features. Names used in \code{dag} must appear here to
#'   be measured; anything else in \code{dag} becomes metadata.
#' @param dag A data.frame with columns \code{from}, \code{to} and optionally
#'   \code{effect} (default 0.6). Both structure and magnitude in one object,
#'   because they are the same claim.
#' @param outcome Name of the outcome variable, which must appear in
#'   \code{dag} unless \code{effects_on_outcome} is used.
#' @param outcome_type \code{"continuous"}, \code{"binary"} or
#'   \code{"survival"}. Survival adds a time and a status column.
#' @param modules Named list of latent processes: each element is a character
#'   vector of feature names that will be driven by one unobserved variable.
#'   This is the shape a real biological process has, and it is not
#'   expressible as a DAG between measured variables.
#' @param module_loading How strongly each module drives its members.
#' @param missing Fraction of values to blank, either one number for every
#'   block or a named vector per block.
#' @param missing_pattern \code{"random"} blanks at random;
#'   \code{"by_outcome"} blanks preferentially where the outcome is high,
#'   which is the pattern imputation handles worst and the one that
#'   manufactures findings.
#' @param batch Number of batches, or 0 for none.
#' @param batch_effect Size of the shift between batches.
#' @param coverage Fraction of people each block measured, either one number
#'   or a named vector. Below 1 the blocks stop sharing a population, which
#'   is what makes sample alignment a real problem.
#' @param noise Standard deviation of the noise added to every generated
#'   variable.
#' @param seed Passed to \code{set.seed()}. The caller's random number
#'   generator is restored afterwards.
#'
#' @return A \code{MultiOmicsData} with \code{misc$simulation} describing what
#'   was planted.
#'
#' @seealso \code{\link{load_data}}, \code{\link{check_data}}, \code{\link{analyze}}
#'
#' @examples
#' # A confounder, a mediator, and a variable the outcome causes.
#' structure <- data.frame(
#'   from   = c("age", "age", "protein", "inflammation", "disease"),
#'   to     = c("protein", "disease", "inflammation", "disease", "biomarker"),
#'   effect = c(0.5, 0.4, 0.8, 0.6, 0.9)
#' )
#'
#' sim <- simulate_data(
#'   n = 200,
#'   blocks = list(blood = c("protein", "inflammation", "biomarker"),
#'                 other = 5),
#'   dag = structure,
#'   outcome = "disease"
#' )
#'
#' sim$misc$simulation$noise_features
#'
#'
#' @export
simulate_data <- function(n = 200,
                          blocks = list(main = 10),
                          dag = NULL,
                          outcome = "y",
                          outcome_type = c("continuous", "binary", "survival"),
                          modules = NULL,
                          module_loading = 0.9,
                          missing = 0,
                          missing_pattern = c("random", "by_outcome"),
                          batch = 0,
                          batch_effect = 0.5,
                          coverage = 1,
                          noise = 1,
                          seed = 1) {

  outcome_type <- match.arg(outcome_type)
  missing_pattern <- match.arg(missing_pattern)

  if (!is.numeric(n) || n < 20) {
    stop("'n' must be at least 20; below that nothing can be recovered.",
         call. = FALSE)
  }

  if (!is.list(blocks) || length(blocks) == 0 || is.null(names(blocks))) {
    stop("'blocks' must be a named list of feature names or feature counts.",
         call. = FALSE)
  }

  .with_preserved_seed({

    set.seed(seed)

    ids <- sprintf("S%0*d", nchar(n), seq_len(n))

    # --- which names go where ------------------------------------------------

    named <- lapply(names(blocks), function(b) {

      spec <- blocks[[b]]

      if (is.character(spec)) spec
      else if (is.numeric(spec) && length(spec) == 1)
        sprintf("%s_%02d", b, seq_len(spec))
      else stop(sprintf(
        "blocks[['%s']] must be feature names or a single count.", b),
        call. = FALSE)

    })

    names(named) <- names(blocks)

    measured <- unlist(named, use.names = FALSE)

    if (anyDuplicated(measured)) {
      stop("Feature names must be unique across blocks: ",
           paste(unique(measured[duplicated(measured)]), collapse = ", "),
           call. = FALSE)
    }

    # --- generate every variable the structure mentions ----------------------

    structure_df <- .simulation_structure(dag)

    values <- .simulate_from_dag(structure_df, n, noise, outcome,
                                 outcome_type)

    planted <- setdiff(names(values), outcome)

    # --- latent processes ----------------------------------------------------

    module_values <- list()

    if (!is.null(modules)) {

      if (is.null(names(modules))) {
        stop("'modules' must be a named list of feature names.", call. = FALSE)
      }

      for (m in names(modules)) {

        latent <- stats::rnorm(n)

        module_values[[m]] <- latent

        for (f in modules[[m]]) {
          values[[f]] <- module_loading * latent +
            stats::rnorm(n, sd = noise * 0.5)
        }

      }

    }

    # --- everything measured that nothing generated is noise -----------------

    generated <- names(values)

    noise_features <- setdiff(measured, generated)

    for (f in noise_features) values[[f]] <- stats::rnorm(n, sd = noise)

    # --- assemble ------------------------------------------------------------

    assays <- lapply(names(named), function(b) {

      cols <- named[[b]]

      m <- do.call(cbind, lapply(cols, function(f) values[[f]]))

      dimnames(m) <- list(ids, cols)

      m

    })

    names(assays) <- names(named)

    # Anything the structure created that no block claims is metadata: that
    # is how covariates and effect modifiers arrive, without needing their
    # own argument.

    metadata_names <- setdiff(planted, measured)

    metadata <- data.frame(sample_id = ids, stringsAsFactors = FALSE)

    for (v in metadata_names) metadata[[v]] <- values[[v]]

    metadata <- cbind(metadata,
                      .simulation_outcome(values[[outcome]], outcome,
                                          outcome_type, n))

    # --- batch ---------------------------------------------------------------

    batch_labels <- NULL

    if (isTRUE(batch > 1)) {

      batch_labels <- factor(rep(sprintf("B%d", seq_len(batch)),
                                 length.out = n))

      metadata$batch <- as.character(batch_labels)

      shift <- stats::rnorm(batch, sd = batch_effect)

      assays <- lapply(assays, function(m) {
        m + shift[as.integer(batch_labels)]
      })

    }

    # --- coverage: which people each block actually measured -----------------

    coverage <- .simulation_per_block(coverage, names(assays), "coverage",
                                     neutral = 1)

    covered <- list()

    for (b in names(assays)) {

      keep <- if (coverage[[b]] >= 1) ids else
        sort(sample(ids, max(10L, round(coverage[[b]] * n))))

      covered[[b]] <- keep

      assays[[b]] <- assays[[b]][keep, , drop = FALSE]

    }

    # --- missingness ---------------------------------------------------------

    missing <- .simulation_per_block(missing, names(assays), "missing",
                                    neutral = 0)

    blanked <- list()

    for (b in names(assays)) {

      if (missing[[b]] <= 0) { blanked[[b]] <- 0L; next }

      m <- assays[[b]]

      k <- round(missing[[b]] * length(m))

      if (k < 1) { blanked[[b]] <- 0L; next }

      cells <- if (identical(missing_pattern, "random")) {

        sample(length(m), k)

      } else {

        # Blanked where the outcome is high. Imputation then fills those
        # cells from people whose outcome is low, which is how a finding
        # gets manufactured out of nothing.

        y <- values[[outcome]][match(rownames(m), ids)]

        weight <- rep(rank(y) / length(y), times = ncol(m))

        sample(length(m), k, prob = weight)

      }

      m[cells] <- NA

      assays[[b]] <- m

      blanked[[b]] <- length(cells)

    }

    object <- load_data(assays, metadata = metadata)

    object$misc$simulation <- list(
      n = n,
      seed = seed,
      structure = structure_df,
      outcome = outcome,
      outcome_type = outcome_type,
      blocks = named,
      noise_features = noise_features,
      planted_features = intersect(planted, measured),
      metadata_variables = metadata_names,
      modules = modules,
      module_latent = module_values,
      coverage = coverage,
      covered = covered,
      missing = missing,
      missing_pattern = missing_pattern,
      cells_blanked = blanked,
      batch = if (is.null(batch_labels)) NULL else as.character(batch_labels)
    )

    object$history <- c(
      object$history,
      sprintf("simulate_data(n = %d, seed = %d): %d block(s), %d planted feature(s), %d noise feature(s)",
              n, seed, length(assays), length(intersect(planted, measured)),
              length(noise_features))
    )

    object

  }, seed = seed)

}

#' Normalise the structure argument
#'
#' @param dag A data.frame, or NULL.
#'
#' @return A data.frame with from, to and effect.
#' @keywords internal
#' @noRd
.simulation_structure <- function(dag) {

  if (is.null(dag)) {
    return(data.frame(from = character(), to = character(),
                      effect = numeric(), stringsAsFactors = FALSE))
  }

  if (!is.data.frame(dag) || !all(c("from", "to") %in% names(dag))) {
    stop("'dag' must be a data.frame with 'from' and 'to' columns.",
         call. = FALSE)
  }

  out <- data.frame(from = as.character(dag$from),
                    to = as.character(dag$to),
                    stringsAsFactors = FALSE)

  out$effect <- if ("effect" %in% names(dag)) as.numeric(dag$effect) else 0.6

  if (any(!is.finite(out$effect))) {
    stop("Every effect must be a finite number.", call. = FALSE)
  }

  out

}

#' Generate variables in topological order
#'
#' A variable with no parents is standard normal. A variable with parents is
#' the weighted sum of them plus noise, so an effect of 0.8 is eight tenths
#' of a standard deviation in the child per standard deviation in the parent
#' and a regression will recover roughly that number.
#'
#' @param structure_df from/to/effect.
#' @param n Sample size.
#' @param noise Noise standard deviation.
#' @param outcome Name of the outcome.
#' @param outcome_type Its type.
#'
#' @return A named list of numeric vectors.
#' @keywords internal
#' @noRd
.simulate_from_dag <- function(structure_df, n, noise, outcome, outcome_type) {

  nodes <- unique(c(structure_df$from, structure_df$to, outcome))

  values <- list()

  remaining <- nodes

  # Kahn's algorithm, with a guard: a cycle means the structure is not a DAG
  # and generating from it would loop forever rather than fail.

  guard <- 0L

  while (length(remaining) > 0) {

    guard <- guard + 1L

    if (guard > length(nodes) + 1L) {
      stop(paste("The structure contains a cycle, so it is not a DAG and",
                 "cannot be generated from:",
                 paste(remaining, collapse = ", ")),
           call. = FALSE)
    }

    ready <- Filter(function(v) {
      parents <- structure_df$from[structure_df$to == v]
      all(parents %in% names(values))
    }, remaining)

    if (length(ready) == 0) {
      stop(paste("The structure contains a cycle, so it is not a DAG and",
                 "cannot be generated from:",
                 paste(remaining, collapse = ", ")),
           call. = FALSE)
    }

    for (v in ready) {

      parents <- structure_df[structure_df$to == v, , drop = FALSE]

      values[[v]] <- if (nrow(parents) == 0) {

        stats::rnorm(n)

      } else {

        Reduce(`+`, lapply(seq_len(nrow(parents)), function(i)
          parents$effect[i] * values[[parents$from[i]]])) +
          stats::rnorm(n, sd = noise)

      }

    }

    remaining <- setdiff(remaining, ready)

  }

  values

}

#' Turn the outcome's latent value into the requested kind of outcome
#'
#' @param value The generated numeric outcome.
#' @param outcome Its name.
#' @param outcome_type Which kind.
#' @param n Sample size.
#'
#' @return A data.frame of the outcome columns.
#' @keywords internal
#' @noRd
.simulation_outcome <- function(value, outcome, outcome_type, n) {

  if (identical(outcome_type, "continuous")) {

    out <- data.frame(value, stringsAsFactors = FALSE)
    names(out) <- outcome

    return(out)

  }

  if (identical(outcome_type, "binary")) {

    out <- data.frame(stats::rbinom(n, 1, stats::plogis(value)),
                      stringsAsFactors = FALSE)
    names(out) <- outcome

    return(out)

  }

  # Survival: a time drawn against the linear predictor, and censoring, so
  # the pair behaves like follow-up rather than like a number called time.

  rate <- exp(0.5 * scale(value)[, 1]) * 0.05

  event_time <- stats::rexp(n, rate)
  censor_time <- stats::rexp(n, 0.02)

  observed <- pmin(event_time, censor_time)

  out <- data.frame(
    round(observed, 2),
    as.integer(event_time <= censor_time),
    stringsAsFactors = FALSE
  )

  names(out) <- c(paste0(outcome, "_time"), outcome)

  out

}

#' Accept one number or one per block
#'
#' @param value Scalar or named vector.
#' @param blocks Block names.
#' @param label What it is, for the error message.
#' @param neutral What an unnamed block gets. Not the same for every
#'   argument: no missingness is 0 and full coverage is 1, so a shared
#'   default of zero silently reduced every unnamed block to ten people.
#'
#' @return A named list, one entry per block.
#' @keywords internal
#' @noRd
.simulation_per_block <- function(value, blocks, label, neutral = 0) {

  if (length(value) == 1 && is.null(names(value))) {
    return(stats::setNames(as.list(rep(value, length(blocks))), blocks))
  }

  unknown <- setdiff(names(value), blocks)

  if (length(unknown) > 0) {
    stop(sprintf("'%s' names blocks that do not exist: %s", label,
                 paste(unknown, collapse = ", ")), call. = FALSE)
  }

  out <- stats::setNames(as.list(rep(neutral, length(blocks))), blocks)

  for (b in names(value)) out[[b]] <- unname(value[[b]])

  out

}
