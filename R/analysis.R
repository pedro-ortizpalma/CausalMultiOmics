# =============================================================================
# analysis.R
# Causal evidence engine
# =============================================================================
#
# analyze() does not fit a model and hand it back. It runs many methods, each
# of which emits EvidenceEdge objects, and then integrates them into a single
# scored graph. What the user reads is the graph and its interpretation, not
# the models.
#
# The integration is the point. One method finding an association is a
# result; six methods finding it, one of them longitudinal, with a stable
# bootstrap and a declared adjustment set, is evidence. The engine keeps
# those two apart.
#
# It also keeps apart two things that are easy to confuse. Agreement between
# methods measures STABILITY. It does not measure validity: methods sharing
# an unmeasured confounder agree with each other and are wrong together. So
# every edge carries the identification strategy that would license reading
# it causally, and the score is named for what it actually measures.
#
# =============================================================================

# =============================================================================
# Design detection
# =============================================================================

#' Work out what kind of study this is
#'
#' The available evidence generators depend on it: repeated measures unlock
#' longitudinal models and give temporal precedence real meaning, a
#' time/status pair unlocks survival models, and a single cross-section
#' supports neither.
#'
#' @keywords internal

.detect_design <- function(metadata, outcome, time = NULL, subject = NULL) {

  design <- list(
    type = "cross-sectional",
    longitudinal = FALSE,
    survival = FALSE,
    subject = subject,
    time = time,
    n_subjects = NA_integer_,
    n_timepoints = NA_integer_,
    notes = character()
  )

  if (!is.null(subject) && subject %in% colnames(metadata)) {

    subjects <- metadata[[subject]]
    design$n_subjects <- length(unique(subjects[!is.na(subjects)]))

    repeated <- any(table(subjects[!is.na(subjects)]) > 1)

    if (repeated) {

      design$longitudinal <- TRUE
      design$type <- "longitudinal"

      design$notes <- c(design$notes,
                        "Repeated measures detected: subject-level random effects apply.")

    }

  }

  if (!is.null(time) && time %in% colnames(metadata)) {

    values <- suppressWarnings(as.numeric(metadata[[time]]))
    design$n_timepoints <- length(unique(values[!is.na(values)]))

  }

  # A time variable paired with a binary status outcome is a survival design
  # rather than a longitudinal one.

  if (!is.null(time) && identical(outcome$type, "binary") && !design$longitudinal) {

    design$survival <- TRUE
    design$type <- "survival"

    design$notes <- c(design$notes,
                      "Time-to-event structure detected: survival models apply.")

  }

  # A supplied `time` that ends up driving nothing is user-visible misuse of
  # the call, not a property of the data, so it is signalled rather than
  # quietly recorded: the caller asked for something that did not happen.

  if (!is.null(time) && !design$survival && !design$longitudinal) {

    design$notes <- c(design$notes, sprintf(
      "'%s' was supplied as a time variable but no time structure could be used.",
      time
    ))

    warning(
      sprintf(
        paste0(
          "'time = \"%s\"' was ignored: a time variable only changes the ",
          "analysis when paired with a binary event outcome (survival) or ",
          "with repeated measures per subject (longitudinal).\n",
          "  The analysis ran as cross-sectional."
        ),
        time
      ),
      call. = FALSE
    )

  }

  design

}

#' Resolve the outcome from metadata
#' @keywords internal

.resolve_outcome <- function(metadata, outcome, sample_ids) {

  if (!(outcome %in% colnames(metadata))) {

    stop(
      sprintf("Outcome '%s' is not a column of the metadata.", outcome),
      call. = FALSE
    )

  }

  values <- metadata[[outcome]][match(sample_ids, metadata$sample_id)]

  usable <- sum(!is.na(values))

  if (usable < 10) {

    stop(
      sprintf("Outcome '%s' has only %d usable value(s) among the analysed samples.",
              outcome, usable),
      call. = FALSE
    )

  }

  levels_present <- unique(stats::na.omit(values))

  if (length(levels_present) < 2) {

    stop(sprintf("Outcome '%s' does not vary.", outcome), call. = FALSE)

  }

  # A nominal outcome with more than two categories has no numeric coding
  # that means anything: as.factor() would order it alphabetically and every
  # model downstream would silently treat "blue < green < red < yellow" as a
  # scale. Refusing is the only honest option, because the analysis would run
  # and produce results that look fine.

  if (length(levels_present) > 2 && !is.numeric(values)) {

    stop(
      sprintf(
        paste0(
          "Outcome '%s' has %d unordered categories (%s).\n",
          "  There is no meaningful way to place them on a numeric scale, so ",
          "the analysis would be invalid.\n",
          "  Recode it as one category against the rest, or supply an ",
          "ordered numeric variable."
        ),
        outcome, length(levels_present),
        paste(utils::head(as.character(levels_present), 4), collapse = ", ")
      ),
      call. = FALSE
    )

  }

  type <- if (length(levels_present) == 2) "binary" else "continuous"

  numeric_values <- if (identical(type, "binary")) {
    as.numeric(as.factor(values)) - 1
  } else {
    as.numeric(values)
  }

  list(
    name = outcome,
    type = type,
    values = numeric_values,
    raw = values,
    levels = levels_present,
    n = usable
  )

}

# =============================================================================
# Sample alignment
# =============================================================================

#' Turn categorical columns into something every method can read
#'
#' A factor cannot go into a correlation, a penalised fit or a structure
#' search as it stands. Each level except one becomes its own yes/no column,
#' contrasted against the level left out. That reference level is not
#' arbitrary decoration: every estimate for this variable means "compared to
#' the reference", and the reference has to travel with the column or the
#' number cannot be read.
#'
#' The commonest level is used as the reference, because contrasts against a
#' rare category are estimated from very few people and come out wide.
#'
#' @param block A data.frame.
#' @param max_levels Beyond this a column is treated as an identifier rather
#'   than a variable and left out.
#' @param min_per_level Levels rarer than this are folded into nothing: they
#'   produce an estimate no one should read.
#'
#' @return A list of encoded columns and a record of what each one means.
#' @noRd
.encode_categorical <- function(block, max_levels = 10, min_per_level = 5) {

  columns <- list()
  encoding <- list()
  notes <- character(0)

  candidates <- setdiff(names(block), .numeric_columns(block))

  for (v in candidates) {

    values <- block[[v]]

    if (inherits(values, "Surv")) next

    values <- as.character(values)
    observed <- values[!is.na(values)]

    counts <- sort(table(observed), decreasing = TRUE)

    if (length(counts) < 2) {

      notes <- c(notes, sprintf(
        "'%s' has a single value and carries no information.", v))
      next

    }

    if (length(counts) > max_levels) {

      notes <- c(notes, sprintf(
        "'%s' has %d categories, which is more than %d: treated as an identifier and left out.",
        v, length(counts), max_levels))
      next

    }

    reference <- names(counts)[1]

    usable <- names(counts)[counts >= min_per_level]
    usable <- setdiff(usable, reference)

    dropped <- setdiff(setdiff(names(counts), reference), usable)

    if (length(dropped) > 0) {

      notes <- c(notes, sprintf(
        "'%s': categories %s have fewer than %d observations and were left out.",
        v, paste(dropped, collapse = ", "), min_per_level))

    }

    if (length(usable) == 0) {

      notes <- c(notes, sprintf(
        "'%s' has no category with enough observations to compare.", v))
      next

    }

    for (level in usable) {

      name <- paste0(v, "=", level)

      indicator <- as.numeric(values == level)
      indicator[is.na(values)] <- NA_real_

      columns[[name]] <- indicator

      encoding[[name]] <- list(
        variable = v,
        level = level,
        reference = reference,
        n_level = unname(counts[[level]]),
        n_reference = unname(counts[[reference]])
      )

    }

  }

  list(columns = columns, encoding = encoding, notes = notes)

}

#' Join the analysed blocks onto one sample x feature matrix
#'
#' Blocks need not share samples, and preprocessing may have removed
#' different ones from each. Analysis needs a rectangle, so the intersection
#' is taken and reported: silently analysing whatever survived would hide how
#' much of the cohort the result actually rests on.
#'
#' @noRd
.align_blocks <- function(assays, blocks, min_samples = 10, original = NULL) {

  blocks <- intersect(blocks, names(assays))

  if (length(blocks) == 0) {
    stop("No usable blocks to analyse.", call. = FALSE)
  }

  id_sets <- lapply(assays[blocks], rownames)

  shared <- Reduce(intersect, id_sets)

  if (length(shared) < min_samples) {

    # Naming the count and stopping leaves the user to work out which of
    # twenty blocks broke it, and the pairwise overlap matrix they are most
    # likely to consult will not tell them: every pair can share everything
    # while all of them together share nothing.

    stop(
      paste(c(
        sprintf(paste0("Only %d sample(s) are shared by all %d block(s), ",
                       "and at least %d are needed."),
                length(shared), length(blocks), min_samples),
        .explain_no_overlap(id_sets, min_samples, original)
      ), collapse = "\n"),
      call. = FALSE
    )

  }

  shared <- sort(shared)

  columns <- list()
  feature_block <- character()
  encoding <- list()
  notes <- character(0)

  for (block in blocks) {

    x <- assays[[block]][shared, , drop = FALSE]

    add <- function(name, values) {

      # Feature names can collide between blocks; qualify only when they do,
      # so the common case keeps readable names.

      final <- if (name %in% names(columns)) paste(block, name, sep = ".")
      else name

      columns[[final]] <<- values
      feature_block[final] <<- block

      final

    }

    for (v in .numeric_columns(x)) add(v, x[[v]])

    # Categorical columns survive loading and preprocessing untouched, which
    # is right: nothing sensible comes of scaling a category. They have to be
    # encoded here or they would be dropped without anyone being told.

    encoded <- .encode_categorical(x)

    for (name in names(encoded$columns)) {

      final <- add(name, encoded$columns[[name]])
      encoding[[final]] <- encoded$encoding[[name]]

    }

    if (length(encoded$notes) > 0) {
      notes <- c(notes, sprintf("[%s] %s", block, encoded$notes))
    }

  }

  if (length(columns) == 0) {

    stop(
      paste("No usable features found in the selected blocks: every column",
            "was either non-numeric with too many categories, or constant."),
      call. = FALSE
    )

  }

  matrix_data <- as.matrix(as.data.frame(columns, check.names = FALSE))
  rownames(matrix_data) <- shared

  list(
    x = matrix_data,
    samples = shared,
    feature_block = feature_block,
    encoding = encoding,
    encoding_notes = notes,
    blocks = blocks,
    dropped = setdiff(unique(unlist(id_sets)), shared)
  )

}

#' Covariate matrix aligned to the analysed samples
#' @keywords internal

.build_covariates <- function(metadata, covariates, sample_ids) {

  if (is.null(covariates) || length(covariates) == 0) return(NULL)

  missing_cols <- setdiff(covariates, colnames(metadata))

  if (length(missing_cols) > 0) {

    stop(
      sprintf("Covariate(s) not found in metadata: %s.",
              paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )

  }

  frame <- metadata[match(sample_ids, metadata$sample_id), covariates, drop = FALSE]

  rownames(frame) <- sample_ids

  frame

}

# =============================================================================
# Screening
# =============================================================================

#' Share the screening budget out between blocks
#'
#' Ranking every feature together and taking the best hands the whole budget
#' to whichever block has the most columns. Twenty thousand transcripts
#' against five clinical variables is not a fair contest: the clinical block
#' disappears from the analysis entirely, and with it every cross-block
#' mediation that would have run through it.
#'
#' Each block gets a floor first, so a small block survives intact, and what
#' is left over is shared in proportion to size.
#'
#' @param feature_block Named vector mapping feature to block.
#' @param max_features Total budget.
#' @param min_per_block Floor per block, or the whole block when smaller.
#'
#' @return A named integer vector of quotas.
#' @noRd
.allocate_screening <- function(feature_block, max_features,
                                min_per_block = 10) {

  sizes <- table(feature_block)
  blocks <- names(sizes)
  sizes <- as.integer(sizes)
  names(sizes) <- blocks

  if (sum(sizes) <= max_features) return(sizes)

  # One feature per block is inviolable. Losing a block entirely is the
  # failure this allocation exists to prevent, so when the budget cannot
  # stretch that far the budget yields, not the block. The caller is told
  # what it actually cost rather than being handed a silent gap.

  quota <- pmin(sizes, 1L)

  if (sum(quota) >= max_features) return(quota)

  give <- function(quota, room, n) {

    while (n > 0 && any(room > 0)) {
      j <- which.max(room)
      quota[j] <- quota[j] + 1L
      room[j] <- room[j] - 1L
      n <- n - 1L
    }

    quota

  }

  # Raise everyone towards the floor before anyone goes above it, so a small
  # block is filled before a large one takes a second helping.

  target <- pmin(sizes, min_per_block)

  quota <- give(quota, target - quota, max_features - sum(quota))

  remaining <- max_features - sum(quota)

  if (remaining <= 0) return(quota)

  # Whatever is left over is shared in proportion to how much room each
  # block still has.

  room <- sizes - quota
  share <- if (sum(room) > 0) room / sum(room) else rep(0, length(room))

  quota <- quota + pmin(room, floor(remaining * share))

  give(quota, sizes - quota, max_features - sum(quota))

}

#' Reduce the feature space before the pairwise stage
#'
#' Pairwise evidence over every feature of every block is quadratic: twenty
#' thousand features is four hundred million tests, which is neither
#' computable nor interpretable. Features are screened against the outcome
#' first and the graph is built among the survivors. The screen is recorded
#' so the reader knows what was never examined.
#'
#' The budget is shared between blocks rather than handed to whichever has
#' the most columns. See \code{.allocate_screening()}.
#'
#' @noRd
.screen_features <- function(x, outcome, max_features, feature_block = NULL,
                             min_per_block = 10, quiet = FALSE) {

  p <- ncol(x)

  if (p <= max_features) {

    return(list(keep = colnames(x), screened = FALSE, p_values = NULL,
                tested = p, retained = p, allocation = NULL))

  }

  y <- outcome$values

  p_values <- apply(x, 2, function(col) {

    ok <- is.finite(col) & is.finite(y)

    if (sum(ok) < 5 || stats::sd(col[ok]) == 0) return(NA_real_)

    .safe_try(stats::cor.test(col[ok], y[ok])$p.value, NA_real_)

  })

  if (is.null(feature_block)) {

    ranked <- order(p_values, na.last = TRUE)
    keep <- colnames(x)[ranked[seq_len(max_features)]]
    allocation <- NULL

  } else {

    blocks <- feature_block[colnames(x)]

    allocation <- .allocate_screening(blocks, max_features, min_per_block)

    keep <- unlist(lapply(names(allocation), function(b) {

      members <- colnames(x)[blocks == b]

      if (length(members) == 0) return(character(0))

      ranked <- members[order(p_values[members], na.last = TRUE)]

      utils::head(ranked, allocation[[b]])

    }), use.names = FALSE)

  }

  if (!isTRUE(quiet)) {

    cat(sprintf("  Screened %d features down to %d.\n", p, length(keep)))

    if (!is.null(allocation)) {
      cat(sprintf("    %s\n",
                  paste(sprintf("%s: %d", names(allocation), allocation),
                        collapse = ", ")))
    }

  }

  list(
    keep = keep,
    screened = TRUE,
    p_values = p_values,
    tested = p,
    retained = length(keep),
    allocation = allocation
  )

}


# =============================================================================
# What kind of number a method reported
# =============================================================================
#
# Every model says something about a relationship, but they do not say the same
# kind of thing. A logistic regression reports a log-odds ratio, a Cox model a
# log-hazard ratio, a random forest the loss in accuracy when a variable is
# shuffled. Those live on different scales, and averaging across them produces
# a number that means nothing: the median of a log-odds of 0.29, an arc
# strength of 9715 and a permutation importance of 0.72 is not an estimate of
# anything.
#
# So each observation declares WHAT it measured, and the integrator pools only
# within families that share a scale. Everything else still counts towards
# agreement, because two methods pointing the same way is informative even when
# their magnitudes cannot be combined.
#
# This is also what makes the engine extensible. It does not know that Cox
# models exist; it knows how to pool log-ratios. A new method plugs in by
# declaring its quantity, and nothing in the integrator changes.
# =============================================================================

#' The kinds of quantity a method can report
#'
#' Each entry declares the scale the number lives on, which other quantities it
#' can be pooled with, whether its sign carries direction, and what value means
#' "no relationship".
#'
#' @return A named list of quantity definitions.
#' @noRd
.quantity_registry <- function() {

  list(

    beta = list(
      label = "regression coefficient",
      family = "linear",
      scale = "outcome units per unit of the variable",
      signed = TRUE, null_value = 0, poolable = TRUE
    ),

    log_or = list(
      label = "log odds ratio",
      family = "log_ratio",
      scale = "log-odds",
      signed = TRUE, null_value = 0, poolable = TRUE
    ),

    log_hr = list(
      label = "log hazard ratio",
      family = "log_ratio",
      scale = "log-hazard",
      signed = TRUE, null_value = 0, poolable = TRUE
    ),

    partial_correlation = list(
      label = "partial correlation",
      family = "correlation",
      scale = "-1 to 1, holding the other features fixed",
      signed = TRUE, null_value = 0, poolable = TRUE
    ),

    correlation = list(
      label = "correlation",
      family = "correlation",
      scale = "-1 to 1",
      signed = TRUE, null_value = 0, poolable = TRUE
    ),

    indirect_effect = list(
      label = "indirect effect",
      family = "linear",
      scale = "outcome units carried through the mediator",
      signed = TRUE, null_value = 0, poolable = TRUE
    ),

    # Shrunk towards zero by design, so it is not an unbiased estimate of the
    # same thing an unpenalised coefficient estimates. Kept in its own family
    # rather than pooled with beta, which would drag the pooled value down by
    # an amount that depends on the penalty rather than on the data.
    penalised_beta = list(
      label = "penalised coefficient",
      family = "penalised",
      scale = "shrunk towards zero by the penalty",
      signed = TRUE, null_value = 0, poolable = FALSE
    ),

    # Unsigned: permutation importance measures how much worse predictions get,
    # which is a magnitude with no direction. The sign carried alongside it is
    # imputed from the marginal correlation and is not part of the measurement.
    importance = list(
      label = "permutation importance",
      family = "importance",
      scale = "loss of predictive accuracy, unbounded above",
      signed = FALSE, null_value = 0, poolable = FALSE
    ),

    arc_strength = list(
      label = "network arc strength",
      family = "structure",
      scale = "change in network score, magnitude depends on sample size",
      signed = FALSE, null_value = 0, poolable = FALSE
    )

  )

}

#' Look up one quantity, tolerating an unregistered name
#' @noRd
.quantity <- function(name) {

  registry <- .quantity_registry()

  if (is.null(name) || is.na(name) || !(name %in% names(registry))) {

    return(list(label = if (is.null(name) || is.na(name)) "unspecified" else name,
                family = "unknown", scale = "unknown",
                signed = TRUE, null_value = 0, poolable = FALSE))

  }

  registry[[name]]

}

#' Everything the engine can currently integrate
#'
#' @return A data.frame, one row per registered quantity.
#' @noRd
.available_quantities <- function() {

  registry <- .quantity_registry()

  do.call(rbind, lapply(names(registry), function(n) {

    q <- registry[[n]]

    data.frame(quantity = n, label = q$label, family = q$family,
               scale = q$scale, signed = q$signed, poolable = q$poolable,
               stringsAsFactors = FALSE)

  }))

}

#' One quantity, reported by one method, about one relationship
#'
#' The unit the integration engine actually works with. A model does not emit
#' evidence: it emits an observation, and evidence is what several observations
#' become once they have been reconciled.
#'
#' Emitting one of these is how a new method joins the engine. Declare the
#' quantity and the integrator handles the rest; it has no knowledge of which
#' models exist.
#'
#' @param source,target The two variables.
#' @param quantity One of \code{.available_quantities()$quantity}.
#' @param estimate The number the method reported.
#' @param generator,method Who reported it.
#' @param se,p_value,n,ci The usual accompaniments, where the method provides
#'   them.
#' @param identification What would license a causal reading.
#' @param ... Further fields stored verbatim.
#'
#' @return An Observation.
#' @noRd
observation <- function(source, target, quantity, estimate,
                        generator, method, se = NA_real_,
                        p_value = NA_real_, n = NA_integer_,
                        ci = c(NA_real_, NA_real_),
                        identification = "none", ...) {

  .new_edge(source = source, target = target, estimate = estimate,
            generator = generator, method = method, quantity = quantity,
            se = se, p_value = p_value, n = n, ci = ci,
            identification = identification, ...)

}
# =============================================================================
# Edge construction
# =============================================================================

#' Build one EvidenceEdge
#' @keywords internal

.new_edge <- function(source, target, estimate, generator, method,
                      quantity = NA_character_,
                      se = NA_real_, p_value = NA_real_, n = NA_integer_,
                      ci = c(NA_real_, NA_real_), identification = "none",
                      adjustment_set = character(), assumptions = character(),
                      temporal = NA, source_block = NA_character_,
                      target_block = NA_character_, effect_size = NA_real_,
                      notes = character()) {

  e <- EvidenceEdge()

  spec <- .quantity(quantity)

  e$quantity <- quantity
  e$quantity_family <- spec$family
  e$quantity_label <- spec$label

  e$source <- source
  e$target <- target
  e$source_block <- source_block
  e$target_block <- target_block
  e$direction <- if (is.na(estimate)) NA_character_ else
    if (estimate >= 0) "positive" else "negative"

  e$estimate <- estimate
  e$se <- se
  e$ci_lower <- ci[1]
  e$ci_upper <- ci[2]
  e$p_value <- p_value
  e$effect_size <- if (is.na(effect_size)) abs(estimate) else effect_size
  e$n <- n

  e$method <- method
  e$generator <- generator

  e$identification <- identification
  e$adjustment_set <- adjustment_set
  e$assumptions <- assumptions
  e$temporal <- temporal

  e$supporting_methods <- method
  e$notes <- notes

  e

}

#' Assumptions implied by an identification strategy
#' @keywords internal

.identification_assumptions <- function(identification, adjustment_set = character()) {

  switch(

    identification,

    none = c(
      "No adjustment was made; the estimate is a marginal association.",
      "Any shared cause of the two variables is fully reflected in the estimate."
    ),

    adjustment = c(
      sprintf("Conditioned on: %s.",
              if (length(adjustment_set) > 0)
                paste(adjustment_set, collapse = ", ") else "nothing"),
      "Causal only if that set blocks every backdoor path, which the data cannot verify.",
      "No unmeasured confounding; no adjustment for a collider or a mediator."
    ),

    temporal = c(
      "The exposure was measured before the outcome, which rules out reverse causation.",
      "Temporal order does not address confounding: a shared prior cause remains possible."
    ),

    instrument = c(
      "The instrument affects the outcome only through the exposure (exclusion restriction).",
      "The instrument is independent of unmeasured confounders.",
      "The instrument is associated with the exposure."
    ),

    character()

  )

}

# =============================================================================
# Evidence generators
# =============================================================================
#
# Every generator takes the analysis context and returns a list of
# EvidenceEdge objects. `applies` decides whether the generator is meaningful
# for the detected design. Adding a method to the engine means adding an
# entry here.
# =============================================================================

#' The registry of evidence generators
#' @keywords internal

.evidence_registry <- function() {

  list(

    association = list(
      label = "Adjusted association",
      requires = character(),
      applies = function(design, outcome) TRUE,
      generate = .evidence_association
    ),

    conditional = list(
      label = "Conditional independence",
      requires = character(),
      applies = function(design, outcome) TRUE,
      generate = .evidence_conditional
    ),

    survival = list(
      label = "Cox proportional hazards",
      requires = "survival",
      applies = function(design, outcome) isTRUE(design$survival),
      generate = .evidence_survival
    ),

    longitudinal = list(
      label = "Linear mixed model",
      requires = "lme4",
      applies = function(design, outcome) isTRUE(design$longitudinal),
      generate = .evidence_longitudinal
    ),

    mediation = list(
      label = "Bootstrap mediation",
      requires = character(),
      applies = function(design, outcome) TRUE,
      generate = .evidence_mediation
    ),

    elasticnet = list(
      label = "Elastic net selection",
      requires = "glmnet",
      applies = function(design, outcome) TRUE,
      generate = .evidence_elasticnet
    ),

    randomforest = list(
      label = "Random forest importance",
      requires = "ranger",
      applies = function(design, outcome) TRUE,
      generate = .evidence_randomforest
    ),

    bayesnet = list(
      label = "Bayesian network structure",
      requires = "bnlearn",
      applies = function(design, outcome) TRUE,
      generate = .evidence_bayesnet
    ),

    sem = list(
      label = "Structural equation model",
      requires = "lavaan",
      applies = function(design, outcome) TRUE,
      generate = .evidence_sem
    )

  )

}

# -----------------------------------------------------------------------------
# Adjusted association
# -----------------------------------------------------------------------------

#' Generate association evidence edges per feature
#'
#' @keywords internal
#' @noRd
.evidence_association <- function(context) {

  x <- context$x
  outcome <- context$outcome
  covariates <- context$covariates

  identification <- if (is.null(covariates)) "none" else "adjustment"
  adjustment_set <- if (is.null(covariates)) character() else names(covariates)

  assumptions <- .identification_assumptions(identification, adjustment_set)

  binary <- identical(outcome$type, "binary")

  edges <- list()

  for (v in colnames(x)) {

    frame <- data.frame(.y = outcome$values, .x = x[, v])

    if (!is.null(covariates)) frame <- cbind(frame, covariates)

    frame <- frame[stats::complete.cases(frame), , drop = FALSE]

    if (nrow(frame) < 10 || stats::sd(frame$.x) == 0) next

    fit <- .safe_try(
      if (binary) {
        stats::glm(.y ~ ., data = frame, family = stats::binomial())
      } else {
        stats::lm(.y ~ ., data = frame)
      },
      NULL
    )

    if (is.null(fit)) next

    coefs <- .safe_try(summary(fit)$coefficients, NULL)

    if (is.null(coefs) || !(".x" %in% rownames(coefs))) next

    estimate <- coefs[".x", 1]
    se <- coefs[".x", 2]
    p <- coefs[".x", 4]

    edges[[length(edges) + 1L]] <- .new_edge(
      source = v, target = outcome$name,
      estimate = estimate, se = se, p_value = p, n = nrow(frame),
      ci = c(estimate - 1.96 * se, estimate + 1.96 * se),
      generator = "association",
      method = if (binary) "logistic regression" else "linear regression",
      quantity = if (binary) "log_or" else "beta",
      identification = identification,
      adjustment_set = adjustment_set,
      assumptions = assumptions,
      source_block = context$feature_block[[v]],
      target_block = "outcome"
    )

  }

  edges

}

# -----------------------------------------------------------------------------
# Conditional independence between features
# -----------------------------------------------------------------------------

#' Compute partial correlation evidence between features
#'
#' @keywords internal
#' @noRd
.evidence_conditional <- function(context) {

  x <- context$x

  if (ncol(x) < 2) return(list())

  complete <- stats::complete.cases(x)
  m <- x[complete, , drop = FALSE]

  if (nrow(m) < ncol(m) + 5) {

    # Partial correlation needs more samples than features to invert the
    # covariance matrix; marginal correlation is reported instead and the
    # weaker claim is recorded on every edge.

    correlation <- .safe_try(stats::cor(m, use = "pairwise.complete.obs"), NULL)
    partial <- FALSE

  } else {

    correlation <- .safe_try({
      precision <- solve(stats::cor(m))
      d <- sqrt(diag(precision))
      -precision / outer(d, d)
    }, NULL)

    partial <- TRUE

  }

  if (is.null(correlation)) return(list())

  n <- nrow(m)
  k <- if (partial) ncol(m) - 2 else 0

  edges <- list()

  features <- colnames(m)

  for (i in seq_along(features)) {

    for (j in seq_along(features)) {

      if (j <= i) next

      r <- correlation[i, j]

      if (!is.finite(r) || abs(r) < context$params$min_correlation) next

      # Fisher z for the (partial) correlation.
      df <- n - k - 3

      if (df <= 0) next

      z <- 0.5 * log((1 + r) / (1 - r)) * sqrt(df)
      p <- 2 * stats::pnorm(-abs(z))

      edges[[length(edges) + 1L]] <- .new_edge(
        source = features[i], target = features[j],
        estimate = r, p_value = p, n = n,
        generator = "conditional",
        method = if (partial) "partial correlation" else "correlation",
        quantity = if (partial) "partial_correlation" else "correlation",
        identification = if (partial) "adjustment" else "none",
        adjustment_set = if (partial) setdiff(features, features[c(i, j)]) else character(),
        assumptions = if (partial) {
          c("Conditioned on every other analysed feature.",
            "Linear Gaussian dependence; an unmeasured common cause remains possible.")
        } else {
          c("Marginal correlation: no adjustment was made.",
            "Too few samples relative to features to estimate partial correlation.")
        },
        source_block = context$feature_block[[features[i]]],
        target_block = context$feature_block[[features[j]]]
      )

    }

  }

  edges

}

# -----------------------------------------------------------------------------
# Survival
# -----------------------------------------------------------------------------

#' Generate survival evidence with temporal identification
#'
#' @keywords internal
#' @noRd
.evidence_survival <- function(context) {

  time_values <- context$time_values
  status <- context$outcome$values

  if (is.null(time_values)) return(list())

  covariates <- context$covariates

  identification <- if (is.null(covariates)) "temporal" else "temporal"
  adjustment_set <- if (is.null(covariates)) character() else names(covariates)

  assumptions <- c(
    .identification_assumptions("temporal"),
    if (length(adjustment_set) > 0)
      sprintf("Additionally conditioned on: %s.",
              paste(adjustment_set, collapse = ", ")) else character(),
    "Proportional hazards over follow-up."
  )

  edges <- list()

  for (v in colnames(context$x)) {

    frame <- data.frame(.time = time_values, .status = status, .x = context$x[, v])

    if (!is.null(covariates)) frame <- cbind(frame, covariates)

    frame <- frame[stats::complete.cases(frame) & frame$.time > 0, , drop = FALSE]

    if (nrow(frame) < 10 || stats::sd(frame$.x) == 0) next

    fit <- .safe_try(
      survival::coxph(
        survival::Surv(frame$.time, frame$.status) ~ .,
        data = frame[, setdiff(names(frame), c(".time", ".status")), drop = FALSE]
      ),
      NULL
    )

    if (is.null(fit)) next

    coefs <- .safe_try(summary(fit)$coefficients, NULL)

    if (is.null(coefs) || !(".x" %in% rownames(coefs))) next

    estimate <- coefs[".x", "coef"]
    se <- coefs[".x", "se(coef)"]
    p <- coefs[".x", ncol(coefs)]

    edges[[length(edges) + 1L]] <- .new_edge(
      source = v, target = context$outcome$name,
      estimate = estimate, se = se, p_value = p, n = nrow(frame),
      ci = c(estimate - 1.96 * se, estimate + 1.96 * se),
      generator = "survival", method = "Cox proportional hazards",
      quantity = "log_hr",
      identification = identification,
      adjustment_set = adjustment_set,
      assumptions = assumptions,
      temporal = TRUE,
      source_block = context$feature_block[[v]],
      target_block = "outcome"
    )

  }

  edges

}

# -----------------------------------------------------------------------------
# Longitudinal
# -----------------------------------------------------------------------------

#' Generate longitudinal evidence from mixed models
#'
#' @keywords internal
#' @noRd
.evidence_longitudinal <- function(context) {

  subject <- context$subject_values

  if (is.null(subject)) return(list())

  covariates <- context$covariates
  adjustment_set <- if (is.null(covariates)) character() else names(covariates)

  assumptions <- c(
    "Repeated measures within subject modelled as a random intercept.",
    "Between-subject confounding is not removed by the random effect.",
    if (length(adjustment_set) > 0)
      sprintf("Conditioned on: %s.", paste(adjustment_set, collapse = ", "))
    else "No covariate adjustment was applied."
  )

  edges <- list()

  for (v in colnames(context$x)) {

    frame <- data.frame(.y = context$outcome$values, .x = context$x[, v],
                        .subject = factor(subject))

    if (!is.null(covariates)) frame <- cbind(frame, covariates)

    frame <- frame[stats::complete.cases(frame), , drop = FALSE]

    if (nrow(frame) < 10 || stats::sd(frame$.x) == 0) next
    if (length(unique(frame$.subject)) < 3) next

    predictors <- setdiff(names(frame), c(".y", ".subject"))

    formula <- stats::as.formula(
      paste(".y ~", paste(predictors, collapse = " + "), "+ (1 | .subject)")
    )

    # A singular fit is expected when a feature carries little within-subject
    # variation; the estimate is still usable and the message is noise here.

    fit <- .safe_try(
      suppressMessages(lme4::lmer(formula, data = frame, REML = FALSE)), NULL
    )

    if (is.null(fit)) next

    coefs <- .safe_try(summary(fit)$coefficients, NULL)

    if (is.null(coefs) || !(".x" %in% rownames(coefs))) next

    estimate <- coefs[".x", 1]
    se <- coefs[".x", 2]

    # lmerMod reports no p-value; the Wald approximation is used and said so.
    p <- 2 * stats::pnorm(-abs(estimate / se))

    edges[[length(edges) + 1L]] <- .new_edge(
      source = v, target = context$outcome$name,
      estimate = estimate, se = se, p_value = p, n = nrow(frame),
      ci = c(estimate - 1.96 * se, estimate + 1.96 * se),
      generator = "longitudinal", method = "linear mixed model",
      quantity = "beta",
      identification = "adjustment",
      adjustment_set = c(adjustment_set, "subject (random intercept)"),
      assumptions = assumptions,
      temporal = TRUE,
      source_block = context$feature_block[[v]],
      target_block = "outcome",
      notes = "p-value from the Wald approximation; lmer reports no exact denominator df."
    )

  }

  edges

}

# -----------------------------------------------------------------------------
# Mediation
# -----------------------------------------------------------------------------

#' Test mediation evidence via bootstrapped triples
#'
#' @keywords internal
#' @noRd
.evidence_mediation <- function(context) {

  x <- context$x
  y <- context$outcome$values

  candidates <- context$params$mediation_candidates

  if (length(candidates) < 2) return(list())

  n_boot <- context$params$bootstrap

  edges <- list()

  # Bootstrapping every candidate triple is quadratic in the candidates and
  # linear in the resamples: twenty candidates cost forty thousand model fits
  # and about forty seconds. The point estimate is cheap, so all triples are
  # scored first and only the strongest are bootstrapped.

  max_tests <- if (is.null(context$params$max_mediation_tests)) 30L else
    context$params$max_mediation_tests

  triples <- list()

  # Cross-block triples only: a mediator inside the same block as its
  # exposure is usually collinearity rather than mechanism.

  for (exposure in candidates) {

    for (mediator in candidates) {

      if (identical(exposure, mediator)) next

      block_a <- context$feature_block[[exposure]]
      block_b <- context$feature_block[[mediator]]

      if (identical(block_a, block_b)) next

      frame <- data.frame(.y = y, .x = x[, exposure], .m = x[, mediator])
      frame <- frame[stats::complete.cases(frame), , drop = FALSE]

      if (nrow(frame) < 20) next
      if (stats::sd(frame$.x) == 0 || stats::sd(frame$.m) == 0) next

      indirect <- .safe_try({

        a <- stats::coef(stats::lm(.m ~ .x, data = frame))[[2]]
        b <- stats::coef(stats::lm(.y ~ .x + .m, data = frame))[[3]]

        a * b

      }, NA_real_)

      if (is.na(indirect)) next

      triples[[length(triples) + 1L]] <- list(
        exposure = exposure, mediator = mediator,
        block_a = block_a, block_b = block_b,
        frame = frame, indirect = indirect
      )

    }

  }

  if (length(triples) == 0) return(list())

  strength <- vapply(triples, function(t) abs(t$indirect), numeric(1))
  triples <- triples[order(strength, decreasing = TRUE)]
  triples <- triples[seq_len(min(max_tests, length(triples)))]

  for (triple in triples) {

    exposure <- triple$exposure
    mediator <- triple$mediator
    block_a <- triple$block_a
    block_b <- triple$block_b
    frame <- triple$frame
    indirect <- triple$indirect

    {

      boot <- .with_preserved_seed({

        vapply(seq_len(n_boot), function(i) {

          idx <- sample(nrow(frame), replace = TRUE)
          resample <- frame[idx, , drop = FALSE]

          .safe_try({
            a <- stats::coef(stats::lm(.m ~ .x, data = resample))[[2]]
            b <- stats::coef(stats::lm(.y ~ .x + .m, data = resample))[[3]]
            a * b
          }, NA_real_)

        }, numeric(1))

      }, seed = context$params$seed)

      boot <- boot[is.finite(boot)]

      if (length(boot) < n_boot / 2) next

      ci <- stats::quantile(boot, c(0.025, 0.975))

      # Only report a mediation edge when the indirect effect excludes zero.
      if (ci[1] <= 0 && ci[2] >= 0) next

      total <- .safe_try(
        stats::coef(stats::lm(.y ~ .x, data = frame))[[2]], NA_real_
      )

      edges[[length(edges) + 1L]] <- .new_edge(
        source = exposure, target = mediator,
        estimate = indirect, n = nrow(frame),
        ci = c(ci[1], ci[2]),
        p_value = mean(sign(boot) != sign(indirect)) * 2,
        generator = "mediation", method = "bootstrap mediation",
        quantity = "indirect_effect",
        identification = "adjustment",
        adjustment_set = "mediator path",
        assumptions = c(
          "No unmeasured exposure-mediator, exposure-outcome or mediator-outcome confounding.",
          "The mediator precedes the outcome and the exposure precedes the mediator.",
          "No exposure-mediator interaction was modelled."
        ),
        source_block = block_a, target_block = block_b,
        effect_size = abs(indirect),
        notes = sprintf("Indirect effect; total effect = %s, mediated proportion = %s.",
                        format(round(total, 4)),
                        if (is.finite(total) && total != 0)
                          format(round(indirect / total, 3)) else "NA")
      )

    }

  }

  edges

}

# -----------------------------------------------------------------------------
# Elastic net
# -----------------------------------------------------------------------------

#' Generate evidence via elastic net regression
#'
#' @keywords internal
#' @noRd
.evidence_elasticnet <- function(context) {

  x <- context$x
  outcome <- context$outcome

  complete <- stats::complete.cases(x) & is.finite(outcome$values)

  m <- x[complete, , drop = FALSE]
  y <- outcome$values[complete]

  if (nrow(m) < 20 || ncol(m) < 2) return(list())

  keep <- apply(m, 2, function(col) stats::sd(col) > 0)
  m <- m[, keep, drop = FALSE]

  if (ncol(m) < 2) return(list())

  family <- if (identical(outcome$type, "binary")) "binomial" else "gaussian"

  fit <- .with_preserved_seed(
    .safe_try(
      glmnet::cv.glmnet(m, y, alpha = 0.5, family = family, nfolds = 5),
      NULL
    ),
    seed = context$params$seed
  )

  if (is.null(fit)) return(list())

  coefs <- .safe_try(as.matrix(stats::coef(fit, s = "lambda.1se")), NULL)

  if (is.null(coefs)) return(list())

  selected <- rownames(coefs)[coefs[, 1] != 0]
  selected <- setdiff(selected, "(Intercept)")

  assumptions <- c(
    "Penalised multivariable fit: every selected feature is conditioned on the others.",
    "Selection is not inference; the penalty biases coefficients towards zero.",
    "Correlated features compete, so an omitted feature is not evidence of no effect."
  )

  lapply(selected, function(v) {

    .new_edge(
      source = v, target = outcome$name,
      estimate = coefs[v, 1], n = nrow(m),
      generator = "elasticnet", method = "elastic net",
      quantity = "penalised_beta",
      identification = "adjustment",
      adjustment_set = setdiff(selected, v),
      assumptions = assumptions,
      source_block = context$feature_block[[v]],
      target_block = "outcome"
    )

  })

}

# -----------------------------------------------------------------------------
# Random forest
# -----------------------------------------------------------------------------

#' Generate evidence via random forest importance
#'
#' @keywords internal
#' @noRd
.evidence_randomforest <- function(context) {

  x <- context$x
  outcome <- context$outcome

  complete <- stats::complete.cases(x) & is.finite(outcome$values)

  m <- x[complete, , drop = FALSE]
  y <- outcome$values[complete]

  if (nrow(m) < 20 || ncol(m) < 2) return(list())

  frame <- as.data.frame(m, check.names = FALSE)
  frame$.y <- if (identical(outcome$type, "binary")) factor(y) else y

  fit <- .with_preserved_seed(
    .safe_try(
      ranger::ranger(
        dependent.variable.name = ".y", data = frame,
        importance = "permutation", num.trees = context$params$trees,
        probability = identical(outcome$type, "binary")
      ),
      NULL
    ),
    seed = context$params$seed
  )

  if (is.null(fit)) return(list())

  importance <- .safe_try(ranger::importance(fit), NULL)

  if (is.null(importance) || length(importance) == 0) return(list())

  # Permutation importance is unsigned; the direction comes from the marginal
  # correlation so the edge still carries one.

  positive <- importance[importance > 0]

  if (length(positive) == 0) return(list())

  assumptions <- c(
    "Permutation importance is a predictive quantity, not an effect estimate.",
    "Importance is conditional on the other features present in the forest.",
    "Correlated features share importance, which dilutes both."
  )

  lapply(names(positive), function(v) {

    direction <- .safe_try(
      stats::cor(m[, v], y, use = "pairwise.complete.obs"), 0
    )

    .new_edge(
      source = v, target = outcome$name,
      estimate = sign(direction) * unname(positive[[v]]),
      n = nrow(m),
      effect_size = unname(positive[[v]]),
      generator = "randomforest", method = "random forest importance",
      quantity = "importance",
      identification = "adjustment",
      adjustment_set = setdiff(colnames(m), v),
      assumptions = assumptions,
      source_block = context$feature_block[[v]],
      target_block = "outcome"
    )

  })

}

# -----------------------------------------------------------------------------
# Bayesian network
# -----------------------------------------------------------------------------

#' Generate evidence via Bayesian network structure
#'
#' @keywords internal
#' @noRd
.evidence_bayesnet <- function(context) {

  x <- context$x
  outcome <- context$outcome

  complete <- stats::complete.cases(x) & is.finite(outcome$values)

  m <- x[complete, , drop = FALSE]

  if (nrow(m) < 20 || ncol(m) < 2) return(list())

  keep <- apply(m, 2, function(col) stats::sd(col) > 0)
  m <- m[, keep, drop = FALSE]

  if (ncol(m) < 2) return(list())

  # Structure learning is superexponential in the node count; the graph is
  # built on the strongest features only.
  if (ncol(m) > context$params$max_network_nodes) {

    strength <- abs(apply(m, 2, function(col)
      .safe_try(stats::cor(col, outcome$values[complete]), 0)))

    m <- m[, order(strength, decreasing = TRUE)[
      seq_len(context$params$max_network_nodes)], drop = FALSE]

  }

  frame <- as.data.frame(m, check.names = FALSE)
  frame[[outcome$name]] <- outcome$values[complete]

  # bnlearn needs syntactic names; the mapping is undone on the way out.
  original <- names(frame)
  names(frame) <- make.names(original, unique = TRUE)
  lookup <- stats::setNames(original, names(frame))

  fit <- .with_preserved_seed(
    .safe_try(bnlearn::hc(frame), NULL),
    seed = context$params$seed
  )

  if (is.null(fit) || nrow(fit$arcs) == 0) return(list())

  strength <- .safe_try(
    bnlearn::arc.strength(fit, frame), NULL
  )

  assumptions <- c(
    "Structure learned from observational data: the DAG is identified only up to its Markov equivalence class.",
    "Arc directions within an equivalence class are not empirically distinguishable.",
    "Causal sufficiency: no unmeasured common cause of any two nodes.",
    "Linear Gaussian dependence."
  )

  lapply(seq_len(nrow(fit$arcs)), function(i) {

    from <- lookup[[fit$arcs[i, "from"]]]
    to <- lookup[[fit$arcs[i, "to"]]]

    weight <- if (!is.null(strength)) {
      row <- strength[strength$from == fit$arcs[i, "from"] &
                        strength$to == fit$arcs[i, "to"], ]
      if (nrow(row) > 0) row$strength[1] else NA_real_
    } else NA_real_

    .new_edge(
      source = from, target = to,
      estimate = if (is.finite(weight)) -weight else 1,
      p_value = if (is.finite(weight) && weight <= 1 && weight >= 0) weight else NA_real_,
      n = nrow(frame),
      effect_size = if (is.finite(weight)) abs(weight) else NA_real_,
      generator = "bayesnet", method = "Bayesian network (hill climbing)",
      quantity = "arc_strength",
      identification = "adjustment",
      adjustment_set = "learned parent set",
      assumptions = assumptions,
      source_block = if (identical(from, outcome$name)) "outcome" else
        context$feature_block[[from]],
      target_block = if (identical(to, outcome$name)) "outcome" else
        context$feature_block[[to]]
    )

  })

}

# -----------------------------------------------------------------------------
# Structural equation model
# -----------------------------------------------------------------------------

#' Generate evidence via structural equation modeling
#'
#' @keywords internal
#' @noRd
.evidence_sem <- function(context) {

  candidates <- context$params$sem_candidates

  if (length(candidates) < 2) return(list())

  x <- context$x
  outcome <- context$outcome

  complete <- stats::complete.cases(x[, candidates, drop = FALSE]) &
    is.finite(outcome$values)

  frame <- as.data.frame(x[complete, candidates, drop = FALSE], check.names = FALSE)

  if (nrow(frame) < 20) return(list())

  original <- names(frame)
  names(frame) <- make.names(original, unique = TRUE)
  lookup <- stats::setNames(original, names(frame))

  outcome_name <- make.names(outcome$name)
  frame[[outcome_name]] <- outcome$values[complete]

  model <- paste(
    outcome_name, "~", paste(names(lookup), collapse = " + ")
  )

  fit <- .safe_try(
    lavaan::sem(model, data = frame, warn = FALSE), NULL
  )

  if (is.null(fit)) return(list())

  estimates <- .safe_try(lavaan::parameterEstimates(fit), NULL)

  if (is.null(estimates)) return(list())

  regressions <- estimates[estimates$op == "~", , drop = FALSE]

  if (nrow(regressions) == 0) return(list())

  assumptions <- c(
    "The path structure was specified, not discovered; a different structure would give different estimates.",
    "No unmeasured common cause of any two modelled variables.",
    "Linear relationships and multivariate normality.",
    "Model fit indicates consistency with the data, not correctness of the structure."
  )

  lapply(seq_len(nrow(regressions)), function(i) {

    row <- regressions[i, ]

    source_name <- if (row$rhs %in% names(lookup)) lookup[[row$rhs]] else row$rhs

    .new_edge(
      source = source_name, target = outcome$name,
      estimate = row$est, se = row$se, p_value = row$pvalue,
      ci = c(row$ci.lower, row$ci.upper), n = nrow(frame),
      generator = "sem", method = "structural equation model",
      quantity = "beta",
      identification = "adjustment",
      adjustment_set = setdiff(original, source_name),
      assumptions = assumptions,
      source_block = context$feature_block[[source_name]],
      target_block = "outcome"
    )

  })

}

# =============================================================================
# Evidence integration
# =============================================================================
#
# Generators emit edges independently, so the same relationship arrives many
# times with different estimates. The integrator merges them, resolves the
# conflicts and scores what is left.
# =============================================================================

#' Key identifying a relationship regardless of which method reported it
#' @keywords internal
.edge_key <- function(edge) paste(edge$source, "->", edge$target)

#' Merge the edges describing the same relationship and score them
#'
#' Three scores come out, and they measure different things on purpose:
#'
#' \describe{
#'   \item{strength}{How large the effect is, standardised across methods,
#'     combined with how stable it is.}
#'   \item{confidence}{How precisely it was estimated: sample size, interval
#'     width, multiplicity-adjusted significance.}
#'   \item{consistency}{How many of the methods that could see the
#'     relationship agreed on its sign.}
#' }
#'
#' Consistency deliberately does NOT feed a causal claim. Methods that share
#' an unmeasured confounder agree with each other while all being biased in
#' the same direction, so agreement is evidence of stability and nothing
#' more. The identification strategy is tracked separately and reported
#' beside the score.
#'
#' @keywords internal

.integrate_evidence <- function(edges, params) {

  if (length(edges) == 0) return(list())

  keys <- vapply(edges, .edge_key, character(1))

  # Multiplicity is corrected within each generator, because the number of
  # tests a generator ran is what defines its own family.

  generators <- vapply(edges, function(e) e$generator, character(1))

  for (g in unique(generators)) {

    idx <- which(generators == g)
    p <- vapply(edges[idx], function(e) e$p_value, numeric(1))

    if (all(is.na(p))) next

    fdr <- stats::p.adjust(p, method = "BH")

    for (k in seq_along(idx)) edges[[idx[k]]]$fdr <- fdr[k]

  }

  merged <- list()

  for (key in unique(keys)) {

    group <- edges[keys == key]

    base <- group[[1]]

    estimates <- vapply(group, function(e) e$estimate, numeric(1))
    methods <- vapply(group, function(e) e$method, character(1))
    generators_here <- vapply(group, function(e) e$generator, character(1))

    finite <- is.finite(estimates)

    if (!any(finite)) next

    signs <- sign(estimates[finite])
    dominant <- if (sum(signs > 0) >= sum(signs < 0)) 1 else -1

    agreeing <- which(finite)[signs == dominant]
    disagreeing <- which(finite)[signs != dominant]

    # --- consistency -------------------------------------------------------
    #
    # Weighted by the epistemic level of each contributing method rather than
    # counted. An unweighted vote makes five predictive methods outrank one
    # longitudinal model plus one mediation analysis, which inverts the
    # actual strength of the evidence.

    levels_here <- vapply(group, function(e) .evidence_level(e$generator),
                          integer(1))

    weights <- .level_weight(levels_here)

    consistency <- sum(weights[agreeing]) / sum(weights[which(finite)])

    # --- strength ----------------------------------------------------------
    #
    # Effect sizes are not comparable across methods (a Cox coefficient and a
    # permutation importance live on different scales), so each is turned
    # into a rank-free bounded quantity before averaging.

    effect_sizes <- vapply(group, function(e) e$effect_size, numeric(1))
    effect_sizes <- effect_sizes[is.finite(effect_sizes)]

    strength <- if (length(effect_sizes) == 0) 0.5 else {
      mean(vapply(effect_sizes, function(v) v / (1 + v), numeric(1)))
    }

    # --- confidence --------------------------------------------------------

    fdrs <- vapply(group, function(e) e$fdr, numeric(1))
    fdrs <- fdrs[is.finite(fdrs)]

    significance <- if (length(fdrs) == 0) 0.5 else 1 - min(fdrs)

    ns <- vapply(group, function(e) as.numeric(e$n), numeric(1))
    ns <- ns[is.finite(ns)]

    size_term <- if (length(ns) == 0) 0.5 else {
      n <- max(ns)
      n / (n + 50)
    }

    widths <- vapply(group, function(e) {
      if (is.finite(e$ci_lower) && is.finite(e$ci_upper) && is.finite(e$estimate) &&
          e$estimate != 0) {
        abs(e$ci_upper - e$ci_lower) / abs(e$estimate)
      } else NA_real_
    }, numeric(1))

    widths <- widths[is.finite(widths)]

    precision <- if (length(widths) == 0) 0.5 else 1 / (1 + stats::median(widths))

    confidence <- (significance + size_term + precision) / 3

    # --- identification ----------------------------------------------------
    #
    # The strongest strategy any contributing method could claim wins, and
    # the assumptions of every contributing method are kept.

    # The level a relationship reaches is the level of the strongest method
    # that found it, and the identification strategy follows from that plus
    # whatever was declared. Deriving one from the other keeps them from
    # drifting apart, which two independent fields inevitably would.

    top_level <- max(levels_here)

    declared <- vapply(group, function(e) e$identification, character(1))
    ranking <- c(none = 0, adjustment = 1, temporal = 2, instrument = 3)

    # Index back into `declared`, not into `ranking`: which.max() returns a
    # position within the contributing methods, and reading it as a position
    # in the lookup table silently reported every edge as unidentified.

    declared_best <- declared[[which.max(ranking[declared])]]

    from_level <- .level_identification(
      top_level,
      has_adjustment = any(vapply(group, function(e)
        length(e$adjustment_set) > 0, logical(1))),
      has_temporal = any(vapply(group, function(e)
        isTRUE(e$temporal), logical(1)))
    )

    best <- if (ranking[[from_level]] >= ranking[[declared_best]])
      from_level else declared_best

    integrated <- base

    # --- the reported estimate ---------------------------------------------
    #
    # Pool only within a family that shares a scale. Taking the median across
    # everything, as this used to, mixed a log-odds of 0.29 with an arc
    # strength of 9715 and reported the middle one as if it estimated
    # something.
    #
    # Where several families are available the one from the strongest kind of
    # evidence wins, since that is the estimate a reader should be quoting.
    # Observations outside it still count towards agreement: two methods
    # pointing the same way is informative even when their magnitudes cannot
    # be combined.

    families <- vapply(group, function(e) {
      f <- .quantity(e$quantity)$family
      if (isTRUE(.quantity(e$quantity)$poolable)) f else NA_character_
    }, character(1))

    usable <- which(finite & !is.na(families))

    if (length(usable) > 0) {

      by_family <- split(usable, families[usable])

      # Not named `best`: that belongs to the identification block below, and
      # reusing it here silently overwrote every edge's identification with
      # the name of a quantity family.
      best_family <- names(by_family)[which.max(vapply(
        by_family, function(idx) max(levels_here[idx]), numeric(1)
      ))]

      chosen <- by_family[[best_family]]

      integrated$estimate <- stats::median(estimates[chosen])
      integrated$quantity <- group[[chosen[1]]]$quantity
      integrated$quantity_family <- best_family
      integrated$quantity_label <- .quantity(integrated$quantity)$label
      integrated$pooled_from <- length(chosen)

      integrated$not_pooled <- unique(vapply(
        group[setdiff(seq_along(group), chosen)],
        function(e) .quantity(e$quantity)$label, character(1)
      ))

    } else {

      # Nothing poolable: report the strongest single observation rather than
      # a mixture, and say which one it was.

      pick <- which(finite)[which.max(levels_here[finite])]

      integrated$estimate <- estimates[pick]
      integrated$quantity <- group[[pick]]$quantity
      integrated$quantity_family <- .quantity(integrated$quantity)$family
      integrated$quantity_label <- .quantity(integrated$quantity)$label
      integrated$pooled_from <- 1L

      integrated$not_pooled <- unique(vapply(
        group[setdiff(seq_along(group), pick)],
        function(e) .quantity(e$quantity)$label, character(1)
      ))

    }

    integrated$direction <- if (dominant > 0) "positive" else "negative"
    integrated$effect_size <- if (length(effect_sizes) == 0) NA_real_ else
      stats::median(effect_sizes)
    # min(all-NA, na.rm = TRUE) is Inf, not NA, and Inf then travels into the
    # reported tables as if it were a p-value.

    p_values <- vapply(group, function(e) e$p_value, numeric(1))

    integrated$p_value <- if (all(is.na(p_values))) NA_real_ else
      min(p_values, na.rm = TRUE)
    integrated$fdr <- if (length(fdrs) == 0) NA_real_ else min(fdrs)
    integrated$n <- if (length(ns) == 0) NA_integer_ else as.integer(max(ns))

    integrated$identification <- best
    integrated$adjustment_set <- unique(unlist(
      lapply(group, function(e) e$adjustment_set)
    ))
    integrated$assumptions <- unique(unlist(
      lapply(group, function(e) e$assumptions)
    ))
    integrated$temporal <- any(vapply(group, function(e) isTRUE(e$temporal),
                                      logical(1)))

    integrated$method <- paste(unique(methods), collapse = " + ")
    integrated$generator <- paste(unique(generators_here), collapse = " + ")
    integrated$supporting_methods <- unique(methods[agreeing])
    integrated$conflicting_methods <- unique(methods[disagreeing])

    # Keep what each method said on its own. The merged estimate is a median
    # across methods measured on different scales, which is useful as a
    # summary and impossible to check without the parts it came from.

    integrated$contributions <- do.call(rbind, lapply(seq_along(group), function(i) {

      e <- group[[i]]

      data.frame(
        method = e$method,
        generator = e$generator,
        level = levels_here[i],
        level_label = .level_label(levels_here[i]),

        # What kind of number this row holds, and whether it went into the
        # pooled estimate. Without it the reader is looking at a column of
        # numbers on four different scales with nothing to say so.
        quantity = .quantity(e$quantity)$label,
        scale = .quantity(e$quantity)$scale,
        pooled = isTRUE(.quantity(e$quantity)$poolable),

        estimate = e$estimate,
        se = e$se,
        ci_lower = e$ci_lower,
        ci_upper = e$ci_upper,
        p_value = e$p_value,
        fdr = e$fdr,
        n = e$n,
        direction = if (!is.finite(e$estimate)) NA_character_ else
          if (e$estimate >= 0) "positive" else "negative",
        agrees = i %in% agreeing,
        identification = e$identification,
        stringsAsFactors = FALSE
      )

    }))

    rownames(integrated$contributions) <- NULL

    integrated$strength <- strength
    integrated$confidence <- confidence
    integrated$consistency <- consistency
    integrated$stability <- consistency

    integrated$level <- top_level
    integrated$level_label <- .level_label(top_level)

    integrated$evidence_score <- 100 * strength * confidence * consistency

    # How much unmeasured confounding it would take to erase this. Computed
    # here because it needs only the estimate and its standard error, at no
    # cost, and it answers the question the identification label raises.

    se_here <- vapply(group, function(e) e$se, numeric(1))
    se_here <- se_here[is.finite(se_here)]

    scale <- if (any(vapply(group, function(e)
      grepl("Cox|logistic", e$method), logical(1)))) "log" else "standardised"

    ev <- .e_value(integrated$estimate,
                   if (length(se_here) > 0) stats::median(se_here) else NA_real_,
                   scale)

    integrated$e_value <- ev$e_value
    integrated$e_value_ci <- ev$e_value_ci

    if (length(disagreeing) > 0) {

      integrated$warnings <- c(
        integrated$warnings,
        sprintf("Methods disagree on the sign: %s report the opposite direction.",
                paste(unique(methods[disagreeing]), collapse = ", "))
      )

      integrated$conflict_summary <- .summarise_conflict(
        group, agreeing, disagreeing
      )

    }

    if (!identical(best, "temporal") && !identical(best, "instrument")) {

      integrated$notes <- c(
        integrated$notes,
        "No temporal or instrumental identification: this is an adjusted association."
      )

    }

    merged[[key]] <- integrated

  }

  # ---------------------------------------------------------------------------
  # Reciprocal edges
  # ---------------------------------------------------------------------------
  #
  # Different methods can report A -> B and B -> A. Observational data cannot
  # orient both, and keeping the pair puts a 2-cycle in the graph, which then
  # produces paths that read as mechanisms but are artefacts ("B -> A -> Y"
  # alongside "A -> B -> Y"). The better-supported direction is kept and the
  # disagreement is recorded on it rather than dropped silently.

  for (key in names(merged)) {

    edge <- merged[[key]]

    if (is.null(edge)) next

    reverse_key <- paste(edge$target, "->", edge$source)
    reverse <- merged[[reverse_key]]

    if (is.null(reverse)) next

    if (edge$evidence_score >= reverse$evidence_score) {

      dc <- .direction_confidence(edge$evidence_score, reverse$evidence_score,
                                  edge$temporal, edge$level)

      edge$direction_confidence <- dc$confidence
      edge$direction_basis <- dc$basis

      edge$warnings <- c(edge$warnings, sprintf(
        "The reverse direction was also reported (score %.1f, by %s); direction confidence is %.2f.",
        reverse$evidence_score,
        paste(reverse$supporting_methods, collapse = ", "),
        dc$confidence
      ))

      edge$notes <- c(edge$notes, "Direction chosen by score, not established.")

      merged[[key]] <- edge
      merged[[reverse_key]] <- NULL

    }

  }

  # Edges with no rival direction still need the field filled: nothing argued
  # against the orientation, but nothing tested it either, and those are not
  # the same as a confident call.

  for (key in names(merged)) {

    edge <- merged[[key]]

    if (is.null(edge) || is.finite(edge$direction_confidence)) next

    dc <- .direction_confidence(edge$evidence_score, NA_real_,
                                edge$temporal, edge$level)

    edge$direction_confidence <- dc$confidence
    edge$direction_basis <- dc$basis

    merged[[key]] <- edge

  }

  # Keep only what clears the reporting floor.

  merged <- Filter(
    function(e) isTRUE(e$evidence_score >= params$min_evidence_score),
    merged
  )

  merged[order(vapply(merged, function(e) e$evidence_score, numeric(1)),
                decreasing = TRUE)]

}

# =============================================================================
# Evidence graph
# =============================================================================

#' Turn integrated evidence into a scored directed graph
#' @keywords internal

.build_evidence_graph <- function(evidence, feature_block, outcome_name, params) {

  graph <- EvidenceGraph()

  if (length(evidence) == 0) return(graph)

  edges <- do.call(rbind, lapply(evidence, function(e) {

    data.frame(
      source = e$source,
      target = e$target,
      direction = .report_or(e$direction, NA_character_),
      estimate = e$estimate,
      p_value = e$p_value,
      fdr = e$fdr,
      n = e$n,
      identification = e$identification,
      level = e$level,
      level_label = e$level_label,
      source_block = .report_or(e$source_block, NA_character_),
      target_block = .report_or(e$target_block, NA_character_),

      # Reported beside the score, never inside it. Crossing measurement
      # layers makes a relationship more interesting, not better supported,
      # and folding that preference into evidence_score would mix a thematic
      # judgement into a measurement of size, precision and agreement.
      cross_block = .is_cross_block(e$source_block, e$target_block),

      temporal = isTRUE(e$temporal),
      strength = round(e$strength, 4),
      confidence = round(e$confidence, 4),
      consistency = round(e$consistency, 4),
      evidence_score = round(e$evidence_score, 2),

      # Scales evidence_score, so it belongs beside it: a reader comparing
      # two rows needs to see that one was discounted for resting on filled-in
      # values rather than for being a weaker relationship.
      data_quality = round(e$data_quality, 3),

      # How much of the cohort carries this. A relationship halved by
      # removing three people is a different object from one that survives
      # losing half of them, and the score cannot tell them apart.
      share_driving_effect = round(e$share_driving_effect, 3),

      heterogeneity_fdr = round(e$heterogeneity_fdr, 4),

      e_value = round(e$e_value, 3),
      direction_confidence = round(e$direction_confidence, 3),
      bootstrap_stability = round(e$bootstrap_stability, 3),
      n_methods = length(e$supporting_methods),
      methods = paste(e$supporting_methods, collapse = "; "),
      conflicts = paste(e$conflicting_methods, collapse = "; "),
      stringsAsFactors = FALSE
    )

  }))

  rownames(edges) <- NULL

  node_names <- unique(c(edges$source, edges$target))

  nodes <- data.frame(
    name = node_names,
    block = vapply(node_names, function(v) {
      if (identical(v, outcome_name)) "outcome"
      else if (v %in% names(feature_block)) feature_block[[v]]
      else "unknown"
    }, character(1)),
    stringsAsFactors = FALSE
  )

  nodes$out_degree <- vapply(nodes$name, function(v) sum(edges$source == v), integer(1))
  nodes$in_degree <- vapply(nodes$name, function(v) sum(edges$target == v), integer(1))
  nodes$degree <- nodes$out_degree + nodes$in_degree

  nodes$evidence <- vapply(nodes$name, function(v) {
    involved <- edges$evidence_score[edges$source == v | edges$target == v]
    if (length(involved) == 0) 0 else sum(involved)
  }, numeric(1))

  graph$nodes <- nodes
  graph$edges <- edges
  graph$evidence <- evidence
  graph$parameters <- params

  # ---------------------------------------------------------------------------
  # Topology
  # ---------------------------------------------------------------------------

  if (requireNamespace("igraph", quietly = TRUE)) {

    g <- .safe_try(
      igraph::graph_from_data_frame(
        edges[, c("source", "target", "evidence_score")],
        directed = TRUE, vertices = nodes["name"]
      ),
      NULL
    )

    if (!is.null(g)) {

      graph$igraph <- g

      graph$metrics <- list(
        betweenness = .safe_try(igraph::betweenness(g), NULL),
        closeness = .safe_try(igraph::closeness(g), NULL),
        hub_score = .safe_try(igraph::hits_scores(g)$hub, NULL),
        density = .safe_try(igraph::edge_density(g), NA_real_)
      )

      if (!is.null(graph$metrics$betweenness)) {
        nodes$betweenness <- round(
          graph$metrics$betweenness[nodes$name], 4
        )
        graph$nodes <- nodes
      }

      # Louvain shuffles the node order, so it is both a source of
      # irreproducibility and a leak of the caller's RNG state.

      communities <- .with_preserved_seed(
        .safe_try(
          igraph::cluster_louvain(igraph::as_undirected(g, mode = "collapse")),
          NULL
        ),
        seed = if (is.null(params$seed)) 1L else params$seed
      )

      if (!is.null(communities)) {

        membership <- igraph::membership(communities)

        graph$communities <- list(
          membership = membership,
          sizes = as.integer(table(membership)),
          modularity = .safe_try(igraph::modularity(communities), NA_real_),
          method = "Louvain"
        )

        graph$nodes$community <- as.integer(membership[graph$nodes$name])

      }

      graph$adjacency <- .safe_try(
        as.matrix(igraph::as_adjacency_matrix(g, attr = "evidence_score")), NULL
      )

    }

  }

  path_search <- .extract_paths(graph, outcome_name, params)

  graph$paths <- path_search$paths
  graph$parameters$path_search <- path_search$search

  graph

}

#' Rank the evidence paths that reach the outcome
#' @keywords internal

.extract_paths <- function(graph, outcome_name, params) {

  empty <- list(paths = data.frame(), search = list(trimmed = FALSE))

  if (is.null(graph$igraph)) return(empty)

  g <- graph$igraph

  if (!(outcome_name %in% igraph::V(g)$name)) return(empty)

  # Enumerating simple paths is exponential in the density of the graph: a
  # fifteen-node dense graph already takes twenty seconds, and a real one is
  # bigger. The search therefore runs on the highest-scoring subgraph rather
  # than on everything, and says so when it had to cut.

  max_edges <- if (is.null(params$max_path_edges)) 60L else params$max_path_edges

  search <- list(trimmed = FALSE, edges_available = nrow(graph$edges),
                 edges_searched = nrow(graph$edges))

  if (nrow(graph$edges) > max_edges) {

    ranked <- graph$edges[order(-graph$edges$evidence_score), ]

    # Every path has to end at the outcome, so the edges reaching it are kept
    # whatever they score. Ranking globally would throw them away first:
    # relationships between two variables of the same block routinely score
    # far higher than anything reaching the outcome, because two haematology
    # measures track each other much more closely than either tracks death.
    # Trimming on score alone therefore leaves the outcome unreachable and
    # the search returns nothing at all.

    reaching <- ranked[ranked$target == outcome_name, , drop = FALSE]
    others <- ranked[ranked$target != outcome_name, , drop = FALSE]

    room <- max(max_edges - nrow(reaching), 0L)

    kept <- rbind(reaching, utils::head(others, room))

    search$outcome_edges_kept <- nrow(reaching)

    vertices <- unique(c(kept$source, kept$target))

    g <- .safe_try(
      igraph::graph_from_data_frame(
        kept[, c("source", "target", "evidence_score")],
        directed = TRUE, vertices = data.frame(name = vertices)
      ),
      NULL
    )

    if (is.null(g) || !(outcome_name %in% igraph::V(g)$name)) return(empty)

    search$trimmed <- TRUE
    search$edges_searched <- nrow(kept)

  }

  sources <- setdiff(igraph::V(g)$name, outcome_name)

  rows <- list()

  for (from in sources) {

    paths <- .safe_try(
      suppressWarnings(igraph::all_simple_paths(
        g, from = from, to = outcome_name, mode = "out",
        cutoff = params$max_path_length
      )),
      list()
    )

    for (path in paths) {

      names_in_path <- igraph::V(g)$name[as.integer(path)]

      if (length(names_in_path) < 3) next

      scores <- numeric(0)

      for (i in seq_len(length(names_in_path) - 1)) {

        edge <- graph$edges[
          graph$edges$source == names_in_path[i] &
            graph$edges$target == names_in_path[i + 1], ]

        scores <- c(scores, if (nrow(edge) > 0) edge$evidence_score[1] else 0)

      }

      if (length(scores) == 0 || any(scores == 0)) next

      rows[[length(rows) + 1L]] <- data.frame(
        path = paste(names_in_path, collapse = " -> "),
        origin = names_in_path[1],
        length = length(names_in_path) - 1L,
        mediators = paste(names_in_path[-c(1, length(names_in_path))],
                          collapse = "; "),

        # A path is only as good as its weakest link, so the minimum is the
        # honest summary; the mean is kept alongside for ranking ties.
        weakest_link = round(min(scores), 2),
        mean_evidence = round(mean(scores), 2),
        stringsAsFactors = FALSE
      )

    }

  }

  if (length(rows) == 0) return(list(paths = data.frame(), search = search))

  out <- do.call(rbind, rows)
  out <- out[order(-out$weakest_link, -out$mean_evidence), ]
  rownames(out) <- NULL

  list(paths = utils::head(out, params$max_paths), search = search)

}

# =============================================================================
# Importance, interpretation and performance
# =============================================================================

#' Consensus importance across every generator that produced one
#' @keywords internal

.consensus_importance <- function(edges, outcome_name, feature_block) {

  to_outcome <- Filter(function(e) identical(e$target, outcome_name), edges)

  if (length(to_outcome) == 0) return(data.frame())

  generators <- unique(vapply(to_outcome, function(e) e$generator, character(1)))
  features <- unique(vapply(to_outcome, function(e) e$source, character(1)))

  ranks <- matrix(NA_real_, nrow = length(features), ncol = length(generators),
                  dimnames = list(features, generators))

  for (g in generators) {

    subset <- Filter(function(e) identical(e$generator, g), to_outcome)

    sizes <- vapply(subset, function(e) {
      value <- e$effect_size
      if (is.finite(value)) value else abs(e$estimate)
    }, numeric(1))

    names(sizes) <- vapply(subset, function(e) e$source, character(1))

    sizes <- sizes[is.finite(sizes)]

    if (length(sizes) == 0) next

    # Ranks rather than raw values: importances from different methods are
    # not on a common scale and averaging them directly is meaningless.
    scaled <- rank(sizes) / length(sizes)

    ranks[names(scaled), g] <- scaled

  }

  consensus <- data.frame(
    feature = features,
    block = vapply(features, function(v)
      if (v %in% names(feature_block)) feature_block[[v]] else NA_character_,
      character(1)),
    n_methods = apply(ranks, 1, function(r) sum(!is.na(r))),
    consensus_importance = round(apply(ranks, 1, mean, na.rm = TRUE), 4),
    stringsAsFactors = FALSE
  )

  consensus <- consensus[is.finite(consensus$consensus_importance), ]
  consensus <- consensus[order(-consensus$consensus_importance), ]

  rownames(consensus) <- NULL

  consensus

}

#' Extract readable findings from the graph
#' @keywords internal

.interpret_graph <- function(graph, importance, outcome_name, params) {

  interpretation <- list(
    drivers = data.frame(),
    mediators = character(),
    hubs = character(),
    bridges = character(),
    protective = character(),
    risk = character(),
    biomarkers = character(),
    targets = character(),
    statements = character()
  )

  if (nrow(graph$edges) == 0) return(interpretation)

  edges <- graph$edges

  # --- direct drivers of the outcome -----------------------------------------

  direct <- edges[edges$target == outcome_name, ]

  # Score first, but a relationship two methods agreed on outranks one that a
  # single method reported by a tenth of a point. Rounding the score to whole
  # points before breaking ties on method count keeps the ordering from
  # turning on differences too small to mean anything.

  direct <- direct[order(-round(direct$evidence_score),
                         -direct$n_methods,
                         -direct$evidence_score), ]

  interpretation$drivers <- utils::head(direct, params$top_n)

  if (nrow(direct) > 0) {

    interpretation$risk <- direct$source[direct$direction == "positive"]
    interpretation$protective <- direct$source[direct$direction == "negative"]

  }

  # --- mediators: appear in the middle of a path -----------------------------

  if (nrow(graph$paths) > 0) {

    mediators <- unlist(strsplit(graph$paths$mediators, "; "))
    mediators <- mediators[nzchar(mediators)]

    if (length(mediators) > 0) {
      interpretation$mediators <- names(sort(table(mediators), decreasing = TRUE))
    }

  }

  # --- hubs: most connected --------------------------------------------------

  nodes <- graph$nodes[graph$nodes$name != outcome_name, ]

  if (nrow(nodes) > 0) {

    interpretation$hubs <- utils::head(
      nodes$name[order(-nodes$degree)], params$top_n
    )

  }

  # --- bridges: edges that cross blocks --------------------------------------

  crossing <- edges[
    edges$source %in% graph$nodes$name & edges$target %in% graph$nodes$name, ]

  block_of <- stats::setNames(graph$nodes$block, graph$nodes$name)

  if (nrow(crossing) > 0) {

    is_bridge <- block_of[crossing$source] != block_of[crossing$target] &
      crossing$target != outcome_name

    interpretation$bridges <- unique(crossing$source[which(is_bridge)])

  }

  # --- candidates ------------------------------------------------------------

  if (nrow(importance) > 0 && nrow(direct) > 0) {

    strong <- direct$source[direct$evidence_score >= stats::median(direct$evidence_score)]

    interpretation$biomarkers <- utils::head(
      intersect(importance$feature, strong), params$top_n
    )

    # A therapeutic target should sit upstream, not merely correlate: it has
    # to reach the outcome through something.
    interpretation$targets <- utils::head(
      intersect(interpretation$biomarkers, interpretation$bridges), params$top_n
    )

  }

  # --- statements ------------------------------------------------------------

  statements <- character()

  if (nrow(interpretation$drivers) > 0) {

    best <- interpretation$drivers[1, ]

    statements <- c(statements, sprintf(
      "%s shows the strongest evidence for a relationship with %s (score %.1f, %d method(s), %s).",
      best$source, outcome_name, best$evidence_score, best$n_methods,
      switch(best$identification,
             temporal = "temporal precedence established",
             adjustment = "adjusted association",
             instrument = "instrumented",
             "unadjusted association")
    ))

  }

  if (length(interpretation$mediators) > 0) {

    statements <- c(statements, sprintf(
      "%s appears as a mediator on the highest-scoring paths.",
      interpretation$mediators[1]
    ))

  }

  n_temporal <- sum(edges$temporal)

  statements <- c(statements, sprintf(
    "%d of %d integrated relationships carry temporal precedence; the rest are adjusted associations and cannot rule out confounding.",
    n_temporal, nrow(edges)
  ))

  interpretation$statements <- statements

  interpretation

}

# =============================================================================
# analyze()
# =============================================================================

#' Build quantified causal evidence from preprocessed multi-block data
#'
#' \code{analyze()} does not return a model. It runs every applicable
#' analysis method, collects the relationships each one reports as
#' \code{EvidenceEdge} objects, integrates them into a single scored directed
#' graph, and extracts the findings that graph supports.
#'
#' @section What the scores mean:
#'
#' Three scores are reported per relationship and they measure different
#' things. \strong{Strength} is how large the effect is. \strong{Confidence}
#' is how precisely it was estimated. \strong{Consistency} is how many of the
#' methods that could see the relationship agreed on its direction.
#'
#' Consistency is not validity. Methods that share an unmeasured confounder
#' agree with one another while all being biased in the same direction, so
#' agreement is evidence of stability and nothing more. Every edge therefore
#' also carries \code{identification} — the strategy that would license a
#' causal reading — and \code{assumptions}, the conditions that would have to
#' hold. An edge identified as \code{"none"} or \code{"adjustment"} is an
#' association, however high its score.
#'
#' @param object A \code{PreprocessingResult}, as returned by
#'   \code{preprocess()}. Preprocessing decisions are already fixed, which is
#'   why analysis starts here rather than from a \code{MultiOmicsData}.
#' @param outcome Name of the outcome column in the metadata.
#' @param time Optional name of a time or follow-up column. Combined with a
#'   binary outcome this makes the design a survival one.
#' @param subject Optional name of a subject identifier column. Repeated
#'   values make the design longitudinal.
#' @param covariates Optional character vector of metadata columns to adjust
#'   for. These form the declared adjustment set.
#' @param blocks Blocks to analyse, or \code{"all"}.
#' @param goal Either \code{"causal"} (default) or \code{"predictive"}. The
#'   goal selects which generators run.
#' @param methods Optional character vector naming the generators to run,
#'   overriding \code{goal}.
#' @param effort How much computation to spend. \code{"fast"} runs the
#'   generators once and nothing else; \code{"standard"} adds model
#'   diagnostics and 50 resamples; \code{"thorough"} adds null calibration;
#'   \code{"exhaustive"} resamples every generator 500 times. Each component
#'   can be overridden individually by the arguments below, which take
#'   precedence.
#' @param resample Number of resamples used to test whether a relationship
#'   survives a different sample. \code{0} switches it off. Cost is linear in
#'   this number.
#' @param resample_scheme Either \code{"bootstrap"} or \code{"cv"}.
#' @param resample_methods Which generators to re-run on each resample. The
#'   cheap ones by default, because structure learning and SEM are what make
#'   resampling expensive.
#' @param permutations Number of outcome permutations used to measure how many
#'   relationships the engine finds when there is nothing to find. \code{0}
#'   switches it off.
#' @param diagnostics Whether to compute fit quality and assumption checks for
#'   the models behind the leading relationships.
#' @param heterogeneity Optional metadata columns to test as effect modifiers.
#' @param negative_controls Optional feature names that should not appear in
#'   the graph. If they do, the run is flagged.
#' @param dag Optional causal structure, as a \code{dagitty} object, a
#'   dagitty specification string, or a data.frame with \code{from} and
#'   \code{to} columns. Supplying one lets the engine check whether the
#'   adjustment actually identifies each effect, rather than only recording
#'   that something was adjusted for. It never changes an estimate; it changes
#'   what may be claimed about one. See \code{\link{check_dag}}.
#' @param modifiable Optional variables or blocks the user could plausibly
#'   act on. When supplied, the report expresses the leading relationships as
#'   contrasts in the original measurement units. Nothing is included by
#'   default: a contrast for a genotype is arithmetic without meaning. See
#'   \code{\link{counterfactual}}.
#' @param max_features Cap on the number of features carried into the
#'   pairwise stage. Features are screened against the outcome first, and the
#'   budget is shared out between blocks rather than given to whichever block
#'   has the most columns.
#' @param min_per_block Floor on how many features each block keeps through
#'   screening, or the whole block when it is smaller. Without it a block of
#'   twenty thousand transcripts takes every slot and a five-variable
#'   clinical block disappears from the analysis. Every block keeps at least
#'   one feature even when that pushes the total past \code{max_features}:
#'   losing a block entirely is worse than exceeding a target set for speed.
#' @param bootstrap Bootstrap resamples used for mediation.
#' @param min_evidence_score Relationships scoring below this are not
#'   reported.
#' @param plots Whether to render diagnostic plots.
#' @param seed Seed used for every random procedure. The caller's random
#'   number generator is restored afterwards.
#' @param quiet Whether to suppress progress messages.
#'
#' @return A \code{CMOResult} object.
#'
#' @seealso \code{\link{preprocess}}, \code{\link{annotate_evidence}}
#'
#' @export

analyze <- function(object,
                    outcome,
                    time = NULL,
                    subject = NULL,
                    covariates = NULL,
                    blocks = "all",
                    goal = c("causal", "predictive"),
                    methods = NULL,
                    effort = c("standard", "fast", "thorough", "exhaustive"),
                    resample = NULL,
                    resample_scheme = c("bootstrap", "cv"),
                    resample_methods = NULL,
                    permutations = NULL,
                    diagnostics = NULL,
                    heterogeneity = NULL,
                    negative_controls = NULL,
                    modifiable = NULL,
                    dag = NULL,
                    max_features = 150,
                    min_per_block = 10,
                    bootstrap = 200,
                    min_evidence_score = 1,
                    plots = TRUE,
                    seed = 1L,
                    quiet = FALSE) {

  started <- Sys.time()

  goal <- match.arg(goal)
  effort <- match.arg(effort)
  resample_scheme <- match.arg(resample_scheme)

  # The expensive parts are opt-in through a single dial, with every piece
  # separately overridable. Resampling and permutation are the only things
  # here that cost real time, and both scale linearly in their count, so the
  # user can trade patience for rigour explicitly instead of discovering the
  # cost after an afternoon.

  preset <- switch(
    effort,
    fast       = list(resample = 0L,   permutations = 0L,   diagnostics = FALSE),
    standard   = list(resample = 50L,  permutations = 0L,   diagnostics = TRUE),
    thorough   = list(resample = 200L, permutations = 50L,  diagnostics = TRUE),
    exhaustive = list(resample = 500L, permutations = 200L, diagnostics = TRUE)
  )

  if (is.null(resample)) resample <- preset$resample
  if (is.null(permutations)) permutations <- preset$permutations
  if (is.null(diagnostics)) diagnostics <- preset$diagnostics

  if (is.null(resample_methods)) {
    resample_methods <- if (identical(effort, "exhaustive"))
      names(.evidence_registry()) else .fast_generators()
  }

  # An analysis is a read-only act and must not move the caller's random
  # number generator. The generators guard themselves, but third-party code
  # reached from here does not always: igraph's community detection shuffles
  # node order, and a future generator might do something similar. This is
  # the backstop.

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {

    .saved_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", .saved_seed, envir = globalenv()), add = TRUE)

  } else {

    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)

  }

  if (!inherits(object, "PreprocessingResult")) {

    stop(
      paste("'object' must be a PreprocessingResult. Run preprocess() first:",
            "analysis starts from a fixed preprocessing plan."),
      call. = FALSE
    )

  }

  data <- object$data

  if (is.null(data) || length(data$assays) == 0) {
    stop("The preprocessing result carries no data.", call. = FALSE)
  }

  metadata <- data$metadata

  if (is.null(metadata) || !("sample_id" %in% colnames(metadata))) {

    stop(
      "Analysis needs metadata with a 'sample_id' column to locate the outcome.",
      call. = FALSE
    )

  }

  selected <- if (identical(blocks, "all")) names(data$assays) else blocks

  unknown <- setdiff(selected, names(data$assays))

  if (length(unknown) > 0) {
    stop(sprintf("Unknown block(s): %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Alignment, outcome and design
  # ---------------------------------------------------------------------------

  # Cheap argument checks come before the expensive join. Aligning first meant
  # that asking for a column that does not exist was answered with a
  # complaint about sample overlap, which sends the reader to look at the
  # wrong thing entirely.

  if (!(outcome %in% colnames(metadata))) {

    stop(
      sprintf("Outcome '%s' is not a column of the metadata.\n  Available: %s",
              outcome,
              paste(utils::head(setdiff(colnames(metadata), "sample_id"), 12),
                    collapse = ", ")),
      call. = FALSE
    )

  }

  for (argument in c("time", "subject")) {

    value <- get(argument)

    if (!is.null(value) && !(value %in% colnames(metadata))) {

      stop(sprintf("'%s = \"%s\"' is not a column of the metadata.",
                   argument, value), call. = FALSE)

    }

  }

  if (!is.null(methods)) {

    unknown_methods <- setdiff(methods, names(.evidence_registry()))

    if (length(unknown_methods) > 0) {

      stop(
        sprintf("Unknown generator(s): %s.\n  Available: %s",
                paste(unknown_methods, collapse = ", "),
                paste(names(.evidence_registry()), collapse = ", ")),
        call. = FALSE
      )

    }

  }

  if (!isTRUE(quiet)) cat("Aligning blocks...\n")

  aligned <- .align_blocks(data$assays, selected,
                           original = .report_or(object$input$assays, NULL))

  outcome_spec <- .resolve_outcome(metadata, outcome, aligned$samples)

  design <- .detect_design(
    metadata[match(aligned$samples, metadata$sample_id), , drop = FALSE],
    outcome_spec, time, subject
  )

  covariate_frame <- .build_covariates(metadata, covariates, aligned$samples)

  time_values <- if (!is.null(time) && time %in% colnames(metadata)) {
    suppressWarnings(as.numeric(
      metadata[[time]][match(aligned$samples, metadata$sample_id)]
    ))
  } else NULL

  subject_values <- if (!is.null(subject) && subject %in% colnames(metadata)) {
    metadata[[subject]][match(aligned$samples, metadata$sample_id)]
  } else NULL

  if (!isTRUE(quiet)) {
    cat(sprintf("  %d sample(s), %d feature(s), design: %s\n",
                length(aligned$samples), ncol(aligned$x), design$type))
  }

  # ---------------------------------------------------------------------------
  # Screening
  # ---------------------------------------------------------------------------

  screen <- .screen_features(aligned$x, outcome_spec, max_features,
                             feature_block = aligned$feature_block,
                             min_per_block = min_per_block, quiet = quiet)

  x <- aligned$x[, screen$keep, drop = FALSE]

  # ---------------------------------------------------------------------------
  # Context
  # ---------------------------------------------------------------------------

  top_features <- utils::head(screen$keep, min(20, length(screen$keep)))

  params <- list(
    goal = goal,
    seed = seed,
    bootstrap = bootstrap,
    trees = 500,
    min_correlation = 0.3,
    min_evidence_score = min_evidence_score,
    max_network_nodes = 25,
    max_path_length = 4,
    max_paths = 50,
    top_n = 10,
    mediation_candidates = top_features,
    sem_candidates = utils::head(top_features, 8)
  )

  context <- list(
    x = x,
    outcome = outcome_spec,
    covariates = covariate_frame,
    time_values = time_values,
    subject_values = subject_values,
    design = design,
    feature_block = aligned$feature_block,
    encoding = aligned$encoding,
    metadata = metadata,
    params = params
  )

  # ---------------------------------------------------------------------------
  # Run the generators
  # ---------------------------------------------------------------------------

  registry <- .evidence_registry()

  wanted <- if (!is.null(methods)) {

    unknown_methods <- setdiff(methods, names(registry))

    if (length(unknown_methods) > 0) {

      stop(
        sprintf("Unknown generator(s): %s.\n  Available: %s",
                paste(unknown_methods, collapse = ", "),
                paste(names(registry), collapse = ", ")),
        call. = FALSE
      )

    }

    methods

  } else if (identical(goal, "predictive")) {

    c("association", "elasticnet", "randomforest")

  } else {

    names(registry)

  }

  # A contrast between categories rests on the smaller of the two cells, not
  # on the whole cohort. Left uncorrected, a category holding 1% of the
  # sample inherits the precision of the other 99% and can top the ranking on
  # a handful of people.
  effective_n <- function(edges) {

    if (length(aligned$encoding) == 0) return(edges)

    lapply(edges, function(e) {

      coded <- aligned$encoding[[e$source]]

      if (is.null(coded)) return(e)

      cell <- min(coded$n_level, coded$n_reference)

      if (is.finite(e$n)) e$n <- min(e$n, cell) else e$n <- cell

      e

    })

  }

  all_edges <- list()
  models <- list()
  logs <- character()

  for (name in wanted) {

    entry <- registry[[name]]

    if (!entry$applies(design, outcome_spec)) {

      logs <- c(logs, sprintf("%s: skipped, not applicable to a %s design.",
                              name, design$type))
      next

    }

    missing_pkgs <- entry$requires[
      !vapply(entry$requires, requireNamespace, logical(1), quietly = TRUE)
    ]

    if (length(missing_pkgs) > 0) {

      logs <- c(logs, sprintf("%s: skipped, package(s) %s not installed.",
                              name, paste(missing_pkgs, collapse = ", ")))
      next

    }

    if (!isTRUE(quiet)) cat(sprintf("Running %s...\n", entry$label))

    edges <- .safe_try(entry$generate(context), NULL)

    if (is.null(edges)) {

      logs <- c(logs, sprintf("%s: failed and produced no evidence.", name))
      next

    }

    logs <- c(logs, sprintf("%s: %d relationship(s).", name, length(edges)))

    all_edges <- c(all_edges, effective_n(edges))
    models[[name]] <- list(label = entry$label, n_edges = length(edges))

  }

  # ---------------------------------------------------------------------------
  # Integrate
  # ---------------------------------------------------------------------------

  if (!isTRUE(quiet)) cat("Integrating evidence...\n")

  evidence <- .integrate_evidence(all_edges, params)

  # ---------------------------------------------------------------------------
  # Charge every relationship for the part of it that was reconstructed
  # ---------------------------------------------------------------------------
  #
  # Applied before anything that reads evidence_score, since it rescales it.

  feature_quality <- .safe_try(.feature_quality(object), NULL)

  if (!is.null(feature_quality)) {

    evidence <- .quality_edges(evidence, feature_quality, covariates)

    # Re-sort: the discount can move a relationship built on filled-in data
    # below one that was measured, and leaving the old order would show a
    # ranking that disagrees with the scores printed beside it.

    evidence <- evidence[order(
      vapply(evidence, function(e) .report_or(e$evidence_score, 0), numeric(1)),
      decreasing = TRUE)]

    imputed_edges <- sum(vapply(evidence, function(e)
      isTRUE(e$data_quality < 1), logical(1)))

    if (imputed_edges > 0) {

      logs <- c(logs, sprintf(
        "Data quality: %d of %d relationships rest partly on imputed values; their scores were scaled down.",
        imputed_edges, length(evidence)))

      if (!isTRUE(quiet)) {
        cat(sprintf("  Quality: %d relationship(s) involve imputed data.\n",
                    imputed_edges))
      }

    }

    # Does the finding survive on the rows that were actually measured?

    if (isTRUE(diagnostics)) {

      evidence <- .safe_try(
        .complete_case_sensitivity(
          evidence, x, outcome_spec, covariate_frame,
          .observed_mask(object, aligned$samples, colnames(x))
        ),
        evidence
      )

      reversed <- sum(vapply(evidence, function(e)
        isFALSE(e$complete_case_agrees), logical(1)))

      if (reversed > 0) {

        logs <- c(logs, sprintf(
          "Complete-case check: %d relationship(s) reverse direction when only measured rows are used.",
          reversed))

        if (!isTRUE(quiet)) {
          cat(sprintf("  Quality: %d relationship(s) reverse without the imputed rows.\n",
                      reversed))
        }

      }

    }

  }

  # ---------------------------------------------------------------------------
  # Check the adjustment against the stated structure, if one was given
  # ---------------------------------------------------------------------------

  causal_structure <- .parse_dag(dag)

  if (!is.null(causal_structure)) {

    evidence <- .audit_edges(evidence, causal_structure, covariates)

    identified <- sum(vapply(evidence,
                             function(e) isTRUE(e$identifiable), logical(1)))

    downgraded <- sum(vapply(evidence, function(e)
      length(e$adjustment_problems) > 0, logical(1)))

    logs <- c(logs, sprintf(
      "DAG audit: %d of %d relationships identified; %d adjusted for something the DAG says they should not be.",
      identified, length(evidence), downgraded))

    if (!isTRUE(quiet)) {
      cat(sprintf("  DAG: %d identified, %d with a harmful adjustment.\n",
                  identified, downgraded))
    }

  }

  # ---------------------------------------------------------------------------
  # Does one number describe everybody?
  # ---------------------------------------------------------------------------
  #
  # Applied to the edges before the graph is built from them, so the tables
  # and figures downstream carry the answer rather than contradicting it.

  reported_features <- unique(vapply(
    Filter(function(e) identical(e$target, outcome_spec$name), evidence),
    function(e) e$source, character(1)))

  if (isTRUE(diagnostics)) {

    evidence <- .safe_try(
      .edge_concentration(evidence, x, outcome_spec, covariate_frame),
      evidence
    )

    fragile <- sum(vapply(evidence, function(e)
      isTRUE(e$share_driving_effect <= 0.05), logical(1)))

    if (fragile > 0) {

      logs <- c(logs, sprintf(
        "Effect concentration: %d relationship(s) are halved by removing 5%% or less of the cohort.",
        fragile))

      if (!isTRUE(quiet)) {
        cat(sprintf("  Concentration: %d relationship(s) rest on a small minority.\n",
                    fragile))
      }

    }

  }

  # Testing against a named modifier needs the user to have guessed what
  # modifies the effect, so it only runs when they say.

  subgroups <- if (!is.null(heterogeneity)) {

    .safe_try(
      .heterogeneity(context, heterogeneity,
                     utils::head(reported_features, 20), quiet),
      list(available = FALSE)
    )

  } else list(available = FALSE)

  if (isTRUE(subgroups$available)) {

    evidence <- .safe_try(
      .edge_heterogeneity(evidence, subgroups, outcome_spec$name), evidence)

    differing <- sum(vapply(evidence, function(e)
      isTRUE(e$heterogeneity_fdr < 0.05), logical(1)))

    if (differing > 0) {

      logs <- c(logs, sprintf(
        "Heterogeneity: %d relationship(s) differ between the groups tested.",
        differing))

      if (!isTRUE(quiet)) {
        cat(sprintf("  Heterogeneity: %d relationship(s) are an average over groups that disagree.\n",
                    differing))
      }

    }

  }

  # ---------------------------------------------------------------------------
  # The same data at the resolution between a feature and a block
  # ---------------------------------------------------------------------------

  modules <- .safe_try(
    .build_module_graph(x, outcome_spec, covariate_frame,
                        aligned$feature_block),
    ModuleGraph()
  )

  if (is.data.frame(modules$modules) && nrow(modules$modules) > 0) {

    coherent <- sum(modules$modules$coherent)
    crossing <- sum(modules$modules$cross_block)

    logs <- c(logs, sprintf(
      "Modules: %d group(s) of features move together; %d cohere well enough to summarise, %d span more than one block.",
      nrow(modules$modules), coherent, crossing))

    if (!isTRUE(quiet)) {
      cat(sprintf("  Modules: %d found, %d coherent, %d crossing blocks.\n",
                  nrow(modules$modules), coherent, crossing))
    }

  }

  # ---------------------------------------------------------------------------
  # Resampling: would these relationships survive a different sample?
  # ---------------------------------------------------------------------------

  resampling <- list(available = FALSE)

  if (resample >= 2) {

    if (!isTRUE(quiet)) {

      per_replicate <- as.numeric(difftime(Sys.time(), started, units = "secs")) /
        max(length(models), 1)

      cat(sprintf("Resampling (%d %s replicates, roughly %s)...\n",
                  resample, resample_scheme,
                  .resample_estimate(per_replicate * 0.6, resample)))

    }

    resampling <- .safe_try(
      .resample_evidence(context, resample_methods, resample,
                         scheme = resample_scheme, quiet = quiet),
      list(available = FALSE)
    )

    if (isTRUE(resampling$available)) {

      for (i in seq_along(evidence)) {

        key <- .edge_key(evidence[[i]])
        hit <- resampling$edges[[key]]

        if (is.null(hit)) {

          # Never recovered in any resample, which is itself the finding.
          evidence[[i]]$bootstrap_stability <- 0
          evidence[[i]]$bootstrap_found <- 0L
          evidence[[i]]$bootstrap_evaluable <- resampling$replicates
          next

        }

        evidence[[i]]$bootstrap_stability <- hit$stability
        evidence[[i]]$bootstrap_found <- hit$found
        evidence[[i]]$bootstrap_evaluable <- hit$evaluable

      }

    }

  }

  # ---------------------------------------------------------------------------
  # The graph across resamples, not just its edges one at a time
  # ---------------------------------------------------------------------------

  consensus <- .safe_try(
    .build_consensus_graph(resampling, evidence), ConsensusGraph()
  )

  if (consensus$replicates > 0) {

    logs <- c(logs, sprintf(
      "Consensus graph: %d of %d reported relationships recur in at least %.0f%% of resamples (Jaccard %.2f).",
      consensus$agreement$both, consensus$agreement$reported,
      100 * consensus$threshold, .report_or(consensus$agreement$jaccard, NA)))

    if (!isTRUE(quiet)) {
      cat(sprintf("  Consensus: %d/%d reported relationships recur; %d recur but were not reported.\n",
                  consensus$agreement$both, consensus$agreement$reported,
                  length(consensus$agreement$consensus_only)))
    }

  }

  graph <- .build_evidence_graph(
    evidence, aligned$feature_block, outcome_spec$name, params
  )

  importance <- .consensus_importance(
    all_edges, outcome_spec$name, aligned$feature_block
  )

  interpretation <- .interpret_graph(
    graph, importance, outcome_spec$name, params
  )

  # ---------------------------------------------------------------------------
  # Is any of this more than the engine would find on noise?
  # ---------------------------------------------------------------------------

  calibration <- list(available = FALSE)

  if (permutations >= 2) {

    if (!isTRUE(quiet))
      cat(sprintf("Calibrating against the null (%d permutations)...\n",
                  permutations))

    calibration <- .safe_try(
      .null_calibration(context, resample_methods, permutations,
                        evidence, quiet),
      list(available = FALSE)
    )

  }

  # ---------------------------------------------------------------------------
  # The remaining assessments, all cheap
  # ---------------------------------------------------------------------------

  top_features <- utils::head(
    if (nrow(importance) > 0) importance$feature else colnames(x), 15
  )

  model_diagnostics <- if (isTRUE(diagnostics)) {
    .safe_try(.model_diagnostics(context, top_features, quiet),
              list(available = FALSE))
  } else list(available = FALSE)

  controls <- .check_negative_controls(evidence, negative_controls,
                                       outcome_spec$name)

  robustness <- .safe_try(
    .network_robustness(graph, aligned$feature_block, outcome_spec$name),
    list(available = FALSE)
  )

  # ---------------------------------------------------------------------------
  # The same evidence, read at the level of whole blocks
  # ---------------------------------------------------------------------------

  block_evidence <- .safe_try(
    .block_evidence(evidence, outcome_spec$name), data.frame())

  block_importance <- .safe_try(
    .block_importance(evidence, graph, aligned$feature_block,
                      outcome_spec$name), data.frame())

  block_scores <- .safe_try(
    .block_scores(all_edges, aligned$feature_block, outcome_spec$name),
    data.frame())

  communities <- .safe_try(
    .community_composition(graph, outcome_spec$name), data.frame())

  block_graph <- .safe_try(
    .block_graph(block_evidence, block_importance),
    list(nodes = data.frame(), edges = data.frame(), igraph = NULL))

  standing <- .safe_try(
    .predictive_versus_causal(all_edges, outcome_spec$name,
                              aligned$feature_block),
    data.frame()
  )

  missingness <- .safe_try(
    .missing_report(aligned$x, aligned$dropped, screen),
    list()
  )

  # ---------------------------------------------------------------------------
  # Assemble
  # ---------------------------------------------------------------------------

  result <- CMOResult()

  result$input <- object
  result$data <- list(
    x = x, samples = aligned$samples,
    feature_block = aligned$feature_block,
    encoding = aligned$encoding,
    dropped_samples = aligned$dropped,
    screening = screen
  )

  if (length(aligned$encoding_notes) > 0) {
    logs <- c(logs, aligned$encoding_notes)
  }

  if (length(aligned$encoding) > 0 && !isTRUE(quiet)) {
    cat(sprintf("  %d categorical column(s) encoded as %d comparison(s).\n",
                length(unique(vapply(aligned$encoding,
                                     function(e) e$variable, character(1)))),
                length(aligned$encoding)))
  }

  result$design <- design
  result$outcome <- outcome_spec
  result$models <- models

  result$effects <- if (length(all_edges) > 0) {

    do.call(rbind, lapply(all_edges, function(e) {
      data.frame(
        source = e$source, target = e$target, generator = e$generator,
        method = e$method, estimate = e$estimate, se = e$se,
        p_value = e$p_value, fdr = e$fdr, n = e$n,
        identification = e$identification,
        stringsAsFactors = FALSE
      )
    }))

  } else data.frame()

  result$evidence <- evidence
  result$graph <- graph
  result$causal_paths <- graph$paths
  result$importance <- importance
  result$consensus <- consensus
  result$modules <- modules

  result$network <- list(
    communities = graph$communities,
    metrics = graph$metrics,
    n_nodes = nrow(graph$nodes),
    n_edges = nrow(graph$edges),

    # The block-level reading of the same graph.
    blocks = list(
      evidence = block_evidence,
      importance = block_importance,
      scores = block_scores,
      communities = communities,
      graph = block_graph
    )
  )

  result$interpretation <- interpretation

  result$performance <- list(
    generators_run = length(models),
    edges_generated = length(all_edges),
    edges_integrated = length(evidence),
    features_screened = screen$tested,
    features_retained = screen$retained,
    samples = length(aligned$samples),
    temporal_edges = if (nrow(graph$edges) > 0) sum(graph$edges$temporal) else 0L
  )

  result$diagnostics <- list(
    design = design,
    dropped_samples = aligned$dropped,
    screening = screen,
    models = model_diagnostics,
    missing_data = missingness,
    resampling = resampling,
    null_calibration = calibration,
    heterogeneity = subgroups,
    negative_controls = controls,
    network_robustness = robustness
  )

  result$tables <- list(
    evidence = graph$edges,
    nodes = graph$nodes,
    paths = graph$paths,
    importance = importance,
    effects = result$effects,
    drivers = interpretation$drivers,
    standing = standing,
    block_evidence = block_evidence,
    block_importance = block_importance,
    block_scores = block_scores,
    communities = communities,
    model_diagnostics = model_diagnostics$table,
    heterogeneity = subgroups$table,
    negative_controls = controls$flagged,
    leave_one_block_out = robustness$leave_one_block_out
  )

  if (isTRUE(plots)) {

    result$plots <- .safe_try(
      .build_analysis_plots(graph, importance, block_evidence,
                            outcome_spec$name, consensus, modules, evidence),
      list()
    )

    # Figures were asked for and none came back. Without this the failure is
    # completely silent: the caller gets an empty list and a report with no
    # pictures, and nothing anywhere says why.

    if (length(result$plots) == 0 && nrow(graph$edges) > 0) {

      logs <- c(logs, "Plots were requested but none could be drawn.")

      if (!isTRUE(quiet)) cat("  Warning: no plots could be drawn.\n")

    }

  }

  result$report <- .build_evidence_report(result)

  result$parameters <- c(params, list(
    outcome = outcome, time = time, subject = subject,
    covariates = covariates, blocks = selected, goal = goal,
    generators = wanted, modifiable = modifiable,
    effort = effort, resample = resample, permutations = permutations
  ))

  finished <- Sys.time()

  result$execution <- list(
    runtime = as.numeric(difftime(finished, started, units = "secs")),
    started = started,
    finished = finished,
    package_version = .safe_try(
      as.character(utils::packageVersion("CausalMultiOmics")), NA_character_
    ),
    engine_version = "1.0.0",
    seed = seed,
    session = utils::sessionInfo()$R.version$version.string
  )

  result$logs <- logs
  result$history <- sprintf("analyze() run on %s", format(started))
  result$timestamp <- Sys.time()

  if (!isTRUE(quiet)) {
    cat(sprintf("Done: %d relationship(s) from %d method(s) in %.2f s\n",
                length(evidence), length(models), result$execution$runtime))
  }

  result

}

# =============================================================================
# annotate_evidence()
# =============================================================================

#' Attach external biological support to an evidence graph
#'
#' Kept out of \code{analyze()} on purpose. Pathway and interaction databases
#' are queried over the network, so folding them into the analysis would make
#' the same data produce different scores on different days, fail without a
#' connection, and depend on a database version that is not recorded anywhere
#' in the result.
#'
#' Called separately, the annotation is an explicit, dated act: the source and
#' the retrieval time are stored on every edge it touches.
#'
#' @param result A \code{CMOResult}.
#' @param annotations A data.frame with columns \code{source}, \code{target}
#'   and optionally \code{support} (0-1) and \code{database}. Supply this to
#'   annotate from a local export, which is the reproducible option.
#' @param weight How much the biological support contributes to the
#'   recomputed evidence score, between 0 and 1.
#'
#' @return The \code{CMOResult} with biological support attached.
#'
#' @export

annotate_evidence <- function(result, annotations, weight = 0.1) {

  if (!inherits(result, "CMOResult")) {
    stop("'result' must be a CMOResult object.", call. = FALSE)
  }

  if (!is.data.frame(annotations) ||
      !all(c("source", "target") %in% names(annotations))) {

    stop("'annotations' must be a data.frame with 'source' and 'target' columns.",
         call. = FALSE)

  }

  if (!isTRUE(weight >= 0 && weight <= 1)) {
    stop("'weight' must be between 0 and 1.", call. = FALSE)
  }

  if (!("support" %in% names(annotations))) annotations$support <- 1
  if (!("database" %in% names(annotations))) annotations$database <- "user-supplied"

  retrieved <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  key <- paste(annotations$source, "->", annotations$target)

  for (i in seq_along(result$evidence)) {

    edge <- result$evidence[[i]]

    hit <- which(key == paste(edge$source, "->", edge$target))

    if (length(hit) == 0) next

    support <- max(annotations$support[hit])

    edge$biological_support <- support
    edge$biological_sources <- unique(annotations$database[hit])
    edge$notes <- c(
      edge$notes,
      sprintf("Biological support %.2f from %s, retrieved %s.",
              support, paste(edge$biological_sources, collapse = ", "), retrieved)
    )

    # Support raises the score but cannot create one: an edge with no
    # statistical evidence stays where it was.
    edge$evidence_score <- edge$evidence_score * (1 - weight) +
      100 * support * weight * (edge$evidence_score > 0)

    result$evidence[[i]] <- edge

  }

  result$graph <- .build_evidence_graph(
    result$evidence, result$data$feature_block,
    result$outcome$name, result$parameters
  )

  result$tables$evidence <- result$graph$edges
  result$causal_paths <- result$graph$paths

  result$history <- c(
    result$history,
    sprintf("annotate_evidence() applied on %s", retrieved)
  )

  result

}

# =============================================================================
# Report, plots and tables
# =============================================================================

#' Compile limitations for the evidence report
#'
#' @keywords internal
#' @noRd
.build_evidence_report <- function(result) {

  graph <- result$graph
  interpretation <- result$interpretation

  limitations <- c(
    "Observational data: no relationship reported here is identified by design.",
    sprintf("%d of %d relationships carry temporal precedence.",
            if (nrow(graph$edges) > 0) sum(graph$edges$temporal) else 0L,
            nrow(graph$edges)),
    "Agreement between methods measures stability, not validity: methods sharing an unmeasured confounder agree while all being biased.",
    sprintf("Analysis rests on %d sample(s) shared by every analysed block; %d were dropped by the intersection.",
            length(result$data$samples), length(result$data$dropped_samples))
  )

  calibration <- result$diagnostics$null_calibration

  if (isTRUE(calibration$available)) {

    limitations <- c(limitations, sprintf(
      "On %d runs with the outcome shuffled the engine found %.1f relationship(s) on average against %d here (p = %.3f). %s",
      calibration$permutations, calibration$null_edges_mean,
      calibration$observed_edges, calibration$p_more_edges,
      calibration$verdict
    ))

  } else {

    limitations <- c(limitations, paste(
      "The graph was not calibrated against a null. Without that there is no",
      "way to know how many of these relationships the engine would find on",
      "data with no signal at all; run with permutations > 0."
    ))

  }

  controls <- result$diagnostics$negative_controls

  if (length(controls$declared) > 0) {
    limitations <- c(limitations, controls$notes)
  }

  if (isTRUE(result$diagnostics$resampling$available)) {

    stabilities <- vapply(result$evidence,
                          function(e) e$bootstrap_stability, numeric(1))

    limitations <- c(limitations, sprintf(
      "Under %d %s resamples, %d of %d relationships were recovered less than half the time.",
      result$diagnostics$resampling$replicates,
      result$diagnostics$resampling$scheme,
      sum(stabilities < 0.5, na.rm = TRUE), length(stabilities)
    ))

  }

  if (isTRUE(result$data$screening$screened)) {

    limitations <- c(limitations, sprintf(
      "%d of %d features were screened out against the outcome before the pairwise stage and were never examined for indirect roles.",
      result$data$screening$tested - result$data$screening$retained,
      result$data$screening$tested
    ))

  }

  list(
    main_findings = interpretation$statements,
    drivers = interpretation$drivers,
    pathways = utils::head(graph$paths, 10),
    mediators = interpretation$mediators,
    network = sprintf("%d nodes, %d edges, %d communities",
                      nrow(graph$nodes), nrow(graph$edges),
                      length(graph$communities$sizes)),
    limitations = limitations
  )

}

#' Build plots for evidence scores and importance
#'
#' @keywords internal
#' @noRd
.build_analysis_plots <- function(graph, importance, block_evidence = NULL,
                                  outcome_name = NULL, consensus = NULL,
                                  modules = NULL, evidence = NULL) {

  plots <- list()

  # The one figure that shows the whole answer at once: which layers were
  # measured, how large each is, and where the evidence runs between them.

  if (!is.null(outcome_name)) {

    plots$circos <- .safe_try(.plot_circos(graph, outcome_name), NULL)

    plots$circos_blocks <- .safe_try(
      .plot_circos_blocks(block_evidence, outcome_name), NULL)

  }

  if (nrow(graph$edges) > 0) {

    top <- utils::head(graph$edges[order(-graph$edges$evidence_score), ], 20)

    plots$evidence_scores <- .plot_bar(
      top$evidence_score,
      paste(top$source, "->", top$target),
      main = "Top relationships by evidence score",
      ylab = "Evidence score"
    )

    plots$score_components <- .safe_record(function() {

      graphics::par(mar = c(4, 4, 3, 1))

      graphics::plot(
        graph$edges$strength, graph$edges$confidence,
        xlab = "Strength", ylab = "Confidence",
        main = "Strength against confidence",
        pch = 19, cex = 0.6 + 2 * graph$edges$consistency,
        col = grDevices::adjustcolor("#4C72B0", 0.6)
      )

    })

    plots$identification <- .plot_bar(
      as.numeric(table(graph$edges$identification)),
      names(table(graph$edges$identification)),
      main = "Relationships by identification strategy",
      ylab = "Count"
    )

  }

  if (!is.null(consensus) && is.data.frame(consensus$edges) &&
      nrow(consensus$edges) > 0) {

    plots$consensus <- .safe_try(
      .plot_consensus(consensus, .report_or(outcome_name, "the outcome")), NULL)

  }

  if (!is.null(modules) && !is.null(evidence)) {

    plots$modules <- .safe_try(
      .plot_modules(modules, evidence,
                    .report_or(outcome_name, "the outcome")), NULL)

  }

  if (nrow(importance) > 0) {

    top <- utils::head(importance, 20)

    plots$importance <- .plot_bar(
      top$consensus_importance, top$feature,
      main = "Consensus importance", ylab = "Mean rank across methods"
    )

  }

  if (!is.null(graph$igraph) && requireNamespace("igraph", quietly = TRUE)) {

    plots$network <- .safe_record(function() {

      g <- graph$igraph

      graphics::par(mar = c(1, 1, 3, 1))

      igraph::plot.igraph(
        g,
        vertex.size = 6,
        vertex.label.cex = 0.6,
        vertex.color = "#DD8452",
        edge.arrow.size = 0.3,
        edge.width = 1 + 3 * (igraph::E(g)$evidence_score /
                                max(igraph::E(g)$evidence_score, na.rm = TRUE)),
        main = "Evidence graph"
      )

    })

  }

  plots[!vapply(plots, is.null, logical(1))]

}


# =============================================================================
# Evidence hierarchy
# =============================================================================
#
# Not every method that reports a relationship is making the same kind of
# claim. A random forest says a variable helps predict; a Cox model on
# exposures measured before the event says something considerably stronger.
# Counting those as one vote each makes five predictive methods agreeing look
# like stronger evidence than one longitudinal model plus one mediation
# analysis, which is backwards.
#
# So every generator carries a level, and the level is what `identification`
# is derived from. One axis, not two: a parallel "level" field alongside
# `identification` would inevitably drift out of step with it.
# =============================================================================

#' Epistemic level of an evidence generator
#'
#' @param generator Generator name.
#'
#' @return An integer from 1 to 5.
#' @noRd
.evidence_level <- function(generator) {

  switch(

    generator,

    # 1 - predictive. Says the variable carries information about the
    #     outcome; says nothing about the shape or direction of any effect.
    randomforest = 1L,
    elasticnet = 1L,

    # 2 - associational. Estimates an effect, conditional at best on what was
    #     measured.
    association = 2L,
    conditional = 2L,
    bayesnet = 2L,

    # 3 - temporal. The exposure precedes the outcome, which rules out
    #     reverse causation.
    survival = 3L,
    longitudinal = 3L,

    # 4 - mechanistic. Commits to a path structure and estimates its parts.
    #
    # Mediation qualifies: it tests a specific three-variable path and
    # bootstraps the indirect effect. The SEM generator does not, despite the
    # name: the structure it fits is `outcome ~ every candidate`, which is a
    # multivariable regression written in lavaan syntax. Calling that
    # mechanistic would let one generator promote every edge in the graph to
    # the second-highest level and empty the hierarchy of meaning. It moves
    # up only if a user-specified structure is ever supported.
    mediation = 4L,
    sem = 2L,

    # 5 - identified. An instrument or a validated adjustment set.
    iv = 5L,

    2L

  )

}

#' Name of an evidence level
#'
#' @noRd
.level_label <- function(level) {

  c("predictive", "associational", "temporal", "mechanistic",
    "identified")[level]

}

#' Weight a level contributes to the agreement score
#'
#' Deliberately not linear. The step from "predicts" to "estimates an effect"
#' matters less than the step from "estimates an effect" to "the exposure came
#' first", because only the latter removes a rival explanation.
#'
#' @noRd
.level_weight <- function(level) {

  c(0.4, 1.0, 2.0, 2.5, 4.0)[level]

}

#' Identification strategy, from the level and what the design supports
#'
#' The level is not enough on its own, and treating it as a ladder to
#' identification is wrong. Mediation sits at level 4 because it commits to a
#' path structure, but on cross-sectional data it only assumes an ordering, it
#' does not observe one. Reading "level 4" as "temporal" would let a mediation
#' scan on a single time point claim measurement order it never had.
#'
#' Temporality is a property of the design and is carried separately by the
#' generators that genuinely have it.
#'
#' @noRd
.level_identification <- function(level, has_adjustment, has_temporal = FALSE) {

  if (level >= 5L) return("instrument")

  if (isTRUE(has_temporal)) return("temporal")

  if (isTRUE(has_adjustment)) "adjustment" else "none"

}

#' What each method actually does, for a reader who has not met it
#'
#' A report that says a finding was "found by structural equation model" has
#' told a specialist something and a general reader nothing. Worse, an
#' unfamiliar name reads as authority. These descriptions say what the method
#' looks at and, more usefully, what it cannot see.
#'
#' @param generator Generator name.
#'
#' @return A list with a short label, what it does, and its main limitation.
#' @noRd
.method_explanation <- function(generator) {

  switch(

    generator,

    association = list(
      what = "Fits a straight line between the variable and the outcome, after subtracting the effect of any factors you asked it to account for.",
      cannot = "It cannot tell which one came first, and it can only account for factors that were actually measured."
    ),

    conditional = list(
      what = "Asks whether two variables still move together once every other measured variable is held fixed. If they do not, their apparent link ran through something else.",
      cannot = "It needs more samples than variables to work properly, and it says nothing about direction."
    ),

    survival = list(
      what = "Relates the variable to how long it took for the event to happen, using people who never had the event as well.",
      cannot = "It assumes the effect stays constant over the whole follow-up period."
    ),

    longitudinal = list(
      what = "Uses repeated measurements from the same person, so each person acts partly as their own comparison.",
      cannot = "It removes differences between people, but not a factor that varies within a person over time."
    ),

    mediation = list(
      what = "Tests a three-step chain: the first variable moves the second, and the second moves the outcome. The indirect part is re-estimated on hundreds of resamples to see how stable it is.",
      cannot = "It assumes the chain runs in the order given. On data measured at one time point, that order is an assumption, not an observation."
    ),

    elasticnet = list(
      what = "Fits every variable at once and shrinks the useless ones to exactly zero, so what survives is a short list that predicts the outcome together.",
      cannot = "When two variables carry the same information it keeps one and drops the other, so being dropped is not evidence of having no effect."
    ),

    randomforest = list(
      what = "Builds hundreds of decision trees and measures how much worse the predictions get when one variable is scrambled. A big drop means the variable carried real information.",
      cannot = "It measures usefulness for prediction, not the size or the direction of an effect. Something downstream of the outcome predicts it beautifully."
    ),

    bayesnet = list(
      what = "Searches for a network of arrows that explains the observed pattern of correlations with as few connections as possible.",
      cannot = "Several different arrangements of arrows explain the same data equally well, so the direction it picks is often one of many that fit."
    ),

    sem = list(
      what = "Fits all the candidate variables against the outcome simultaneously, so each estimate is adjusted for the others.",
      cannot = "The structure it fits was written automatically, not derived from biology. It is a multivariable regression, despite the name."
    ),

    list(what = "No description available.", cannot = "")

  )

}

#' What each reported quantity means and where it comes from
#'
#' @noRd
.quantity_explanations <- function() {

  list(

    list(term = "Evidence score",
         short = "0 to 100",
         what = "Size, Precision and Agreement multiplied together, then scaled. All three must be reasonable for the score to be high: a large effect measured badly scores low, and so does a tiny effect measured perfectly.",
         watch = "It is a summary of how well supported a relationship is, not of how important it is biologically."),

    list(term = "Size",
         short = "0 to 1",
         what = "How large the relationship is, put on a common scale so that methods measuring different things can be averaged.",
         watch = "A big number here means the variables move together a lot, not that one causes the other."),

    list(term = "Precision",
         short = "0 to 1",
         what = "How tightly the number was pinned down. Built from the number of samples, the width of the confidence interval, and the significance after correcting for how many things were tested.",
         watch = "High precision on a biased estimate just means confidently wrong."),

    list(term = "Agreement",
         short = "0 to 1",
         what = "How many of the methods that looked at this relationship saw it in the same direction, weighted so that stronger kinds of evidence count for more than weaker ones.",
         watch = "Methods sharing the same blind spot agree with each other. This measures stability, not truth."),

    list(term = "Estimate",
         short = "any number",
         what = "The size of the relationship in the units that particular method works in. A Cox model reports it on a log scale, a random forest reports a drop in prediction quality: they are not comparable across rows.",
         watch = "Compare estimates within a method, never between methods."),

    list(term = "95% CI",
         short = "a range",
         what = "The range of values compatible with the data. Narrow means well determined.",
         watch = "If the range crosses zero, the data does not exclude there being no relationship at all."),

    list(term = "p-value",
         short = "0 to 1",
         what = "How surprising this result would be if there were really no relationship. Small means surprising.",
         watch = "It says nothing about how big or how important the relationship is."),

    list(term = "FDR",
         short = "0 to 1",
         what = "The p-value corrected for having tested many things at once. Test a thousand variables and fifty will look significant by luck; this accounts for that.",
         watch = "Always read the FDR rather than the raw p-value here."),

    list(term = "E-value",
         short = "1 upwards",
         what = "How strong a hidden factor would have to be, on both the variable and the outcome, to explain the whole relationship away. Near 1 means a trivial one would do it.",
         watch = "It does not say a hidden factor exists. It says how much would be needed."),

    list(term = "Recovered in",
         short = "a percentage",
         what = "The analysis was repeated on many random re-samples of the same people. This is how often the relationship survived.",
         watch = "A relationship that disappears in half the resamples is fragile even if its score is high."),

    list(term = "Direction confidence",
         short = "0.5 to 1",
         what = "When methods disagree about which way the arrow points, how one-sided that disagreement was. 0.5 means it was a coin flip.",
         watch = "Only measurement order settles direction. Everything else is inference."),

    list(term = "n",
         short = "a count",
         what = "How many samples that particular method actually used, after dropping any with missing values.",
         watch = "Different rows can rest on different numbers of people.")

  )

}


# =============================================================================
# How much of a relationship was measured rather than reconstructed
# =============================================================================
#
# Every interval this package reports is conditional on the numbers behind it
# being real numbers. Imputation breaks that quietly: it fills missing cells
# with plausible values, and from that moment on nothing downstream can tell
# a measurement from a reconstruction. A variable that arrived 40% empty
# reports the same precision as one that was fully observed, because the
# uncertainty was discarded at the moment of filling and never came back.
#
# So quality here means one specific thing: the share of the data behind a
# relationship that was actually measured. Rows dropped for being incomplete
# do not count against it. They cost sample size, `confidence` already
# reflects that through n, and charging for them twice would be double
# counting. Only fabricated values count.
#
# This never touches an estimate either. It scales the score the estimate is
# ranked by, and says why in words.
# =============================================================================

#' How much of an imputed value survives as information
#'
#' Imputation methods are not interchangeable. Filling with the column mean
#' collapses every missing cell onto a single number: the variance of that
#' part of the column becomes zero and any association computed through it is
#' pulled toward the null. Nearest-neighbour imputation borrows from
#' correlated features and keeps most of the structure, so more of the filled
#' portion is still carrying information.
#'
#' These are deliberately coarse. The point is that the ranking between
#' methods is right and the direction of the penalty is right, not that 0.35
#' is measurable to two digits.
#'
#' @param method The imputation method recorded in the preprocessing step.
#'
#' @return A factor between 0 and 1 applied to the imputed share of a
#'   variable.
#' @keywords internal
#' @noRd
.imputation_fidelity <- function(method) {

  if (length(method) == 0 || is.na(method)) return(0.5)

  switch(as.character(method),
         mean = 0.35, median = 0.35, mode = 0.35,
         pseudocount = 0.6,
         knn = 0.75,
         0.5)

}

#' What preprocessing did to every feature that reached the analysis
#'
#' Reads the original blocks and the executed steps, and returns one row per
#' surviving feature saying how much of it was measured, how much was filled
#' in, by what, and what else happened to its block.
#'
#' @param prep A \code{PreprocessingResult}.
#'
#' @return A data.frame keyed by feature name, or \code{NULL} when the
#'   provenance is not recoverable.
#' @keywords internal
#' @noRd
.feature_quality <- function(prep) {

  if (!inherits(prep, "PreprocessingResult")) return(NULL)

  original <- .report_or(prep$input$assays, NULL)
  final <- .report_or(prep$data$assays, NULL)

  if (is.null(original) || is.null(final)) return(NULL)

  rows <- list()

  for (block in names(final)) {

    kept <- colnames(final[[block]])

    if (length(kept) == 0) next

    raw <- original[[block]]

    # A block can reach the analysis without having been in the input under
    # the same name (a derived block, say). Nothing can be claimed about its
    # provenance, so it is reported as unknown rather than as clean.

    missing_pct <- if (is.null(raw)) {
      stats::setNames(rep(NA_real_, length(kept)), kept)
    } else {
      m <- 100 * colMeans(is.na(as.matrix(raw)))
      m[match(kept, names(m))]
    }

    steps <- Filter(function(s) identical(s$block, block),
                    .report_or(prep$steps, list()))

    stage_of <- function(stage) {
      hit <- Filter(function(s) identical(s$stage, stage), steps)
      if (length(hit) == 0) NULL else hit[[1]]
    }

    imputation <- stage_of("imputation")
    batch <- stage_of("batch")

    method <- if (is.null(imputation)) NA_character_ else
      as.character(imputation$method)

    fidelity <- .imputation_fidelity(method)

    for (i in seq_along(kept)) {

      pct <- unname(missing_pct[i])

      # Missing but never imputed means the rows were dropped by whichever
      # model used the feature. That costs n, not honesty.
      imputed_here <- !is.null(imputation) && isTRUE(pct > 0)

      complete <- if (is.na(pct)) NA_real_ else 1 - pct / 100

      quality <- if (is.na(complete)) NA_real_ else
        if (imputed_here) complete + (1 - complete) * fidelity else 1

      flags <- character()

      if (isTRUE(imputed_here)) {
        flags <- c(flags, sprintf(
          "%.0f%% of this variable was missing and was filled in by %s.",
          pct, .imputation_label(method)))
      }

      if (!is.null(batch)) {
        flags <- c(flags, sprintf(
          "Block '%s' was corrected for batch effects. That helps unless batch and the outcome vary together, in which case some real signal went with it.",
          block))
      }

      rows[[length(rows) + 1L]] <- data.frame(
        feature = kept[i],
        block = block,
        missing_percent = pct,
        imputed = isTRUE(imputed_here),
        imputation_method = method,
        quality = quality,
        flags = I(list(flags)),
        stringsAsFactors = FALSE
      )

    }

  }

  if (length(rows) == 0) return(NULL)

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out

}

#' Name an imputation method the way a reader would say it
#'
#' @param method The registry name.
#'
#' @return A phrase that completes "was filled in by ...".
#' @keywords internal
#' @noRd
.imputation_label <- function(method) {

  switch(as.character(method),
         mean = "the column mean",
         median = "the column median",
         mode = "the commonest value in the column",
         knn = "k-nearest-neighbours, borrowing from correlated features",
         pseudocount = "a fixed pseudocount",
         sprintf("the '%s' method", method))

}

#' Charge every relationship for the part of it that was invented
#'
#' An edge is worth no more than its worst-measured ingredient, so the score
#' takes the weakest link across both endpoints and everything adjusted for.
#' Taking an average instead would let a clean outcome and four clean
#' covariates hide an exposure that was two-thirds imputed, which is the one
#' thing a reader needs to be told.
#'
#' @param edges Integrated edges.
#' @param ftab The table from \code{.feature_quality()}.
#' @param covariates Names of the variables adjusted for.
#'
#' @return The edges, with quality fields set and \code{evidence_score}
#'   scaled.
#' @keywords internal
#' @noRd
.quality_edges <- function(edges, ftab, covariates = character()) {

  if (is.null(ftab) || nrow(ftab) == 0) return(edges)

  lookup <- function(name) {
    hit <- which(ftab$feature == name)
    if (length(hit) == 0) NULL else ftab[hit[1], ]
  }

  lapply(edges, function(e) {

    involved <- unique(c(e$source, e$target, covariates, e$adjustment_set))

    known <- Filter(Negate(is.null), lapply(involved, lookup))

    # Nothing known about any ingredient. Reporting 1 would assert the data
    # was clean; NA says the provenance was not recoverable, which is true.
    if (length(known) == 0) return(e)

    qualities <- vapply(known, function(r) r$quality, numeric(1))
    names(qualities) <- vapply(known, function(r) r$feature, character(1))

    usable <- qualities[is.finite(qualities)]

    if (length(usable) == 0) return(e)

    e$data_quality <- unname(min(usable))
    e$quality_limited_by <- names(usable)[which.min(usable)]

    e$quality_flags <- unique(unlist(lapply(known, function(r) r$flags[[1]])))

    # The score is a product of things that are each a fraction of what they
    # could be. Quality belongs in the product for the same reason precision
    # does: a relationship measured on invented data is not as good as the
    # same relationship measured on real data, and the ranking is what the
    # reader acts on. On a dataset with nothing imputed every quality is 1
    # and no score moves.

    if (is.finite(e$evidence_score)) {
      e$evidence_score <- e$evidence_score * e$data_quality
    }

    e

  })

}

#' Which cells of the analysis matrix were measured rather than filled in
#'
#' @param prep A \code{PreprocessingResult}.
#' @param samples Sample identifiers in the order the analysis uses.
#' @param features Feature names in the order the analysis uses.
#'
#' @return A logical matrix, \code{TRUE} where the original value was
#'   observed, or \code{NULL} when the original data is not recoverable.
#' @keywords internal
#' @noRd
.observed_mask <- function(prep, samples, features) {

  if (!inherits(prep, "PreprocessingResult")) return(NULL)

  original <- .report_or(prep$input$assays, NULL)

  if (is.null(original)) return(NULL)

  mask <- matrix(TRUE, nrow = length(samples), ncol = length(features),
                 dimnames = list(samples, features))

  for (block in names(original)) {

    raw <- as.matrix(original[[block]])

    here <- intersect(features, colnames(raw))

    if (length(here) == 0) next

    rows <- match(samples, rownames(raw))

    # A sample the block never had is not an imputed value, it is a sample
    # this block cannot speak to. Left as TRUE so it does not masquerade as
    # fabricated data.
    seen <- !is.na(rows)

    if (!any(seen)) next

    mask[seen, here] <- !is.na(raw[rows[seen], here, drop = FALSE])

  }

  mask

}

#' Refit the strongest relationships using only rows that were measured
#'
#' The cheapest honest answer to "did this come out of the imputation?".
#' Multiple imputation with Rubin's rules would be the thorough version and
#' costs a full analysis per replicate; this costs one model fit per
#' relationship and catches the case that matters, which is a finding that
#' exists only because the gaps were filled.
#'
#' Only relationships with the outcome are checked, because only those have a
#' single model that can be refitted without re-deriving the whole graph.
#'
#' @param edges Integrated edges.
#' @param x The analysis matrix.
#' @param outcome The resolved outcome.
#' @param covariates The covariate frame, or \code{NULL}.
#' @param mask The matrix from \code{.observed_mask()}.
#' @param top_n How many of the highest-scoring relationships to check.
#'
#' @return The edges, with the complete-case fields set where applicable.
#' @keywords internal
#' @noRd
.complete_case_sensitivity <- function(edges, x, outcome, covariates, mask,
                                       top_n = 20L) {

  if (is.null(mask) || length(edges) == 0) return(edges)

  # Nothing was imputed anywhere: the complete-case fit is the fit.
  if (all(mask)) return(edges)

  candidates <- which(vapply(edges, function(e) {
    identical(e$target, outcome$name) &&
      !is.null(e$source) && e$source %in% colnames(x) &&
      e$source %in% colnames(mask) &&
      any(!mask[, e$source])
  }, logical(1)))

  if (length(candidates) == 0) return(edges)

  scores <- vapply(edges[candidates], function(e)
    .report_or(e$evidence_score, 0), numeric(1))

  candidates <- candidates[order(-scores)][seq_len(min(top_n, length(candidates)))]

  binary <- identical(outcome$type, "binary")

  for (i in candidates) {

    e <- edges[[i]]

    keep <- mask[, e$source]

    frame <- data.frame(.y = outcome$values, .x = x[, e$source])

    if (!is.null(covariates)) frame <- cbind(frame, covariates)

    frame <- frame[keep, , drop = FALSE]
    frame <- frame[stats::complete.cases(frame), , drop = FALSE]

    if (nrow(frame) < 10 || stats::sd(frame$.x) == 0) next

    fit <- .safe_try(
      if (binary) stats::glm(.y ~ ., data = frame, family = stats::binomial())
      else stats::lm(.y ~ ., data = frame),
      NULL
    )

    if (is.null(fit)) next

    coefs <- .safe_try(summary(fit)$coefficients, NULL)

    if (is.null(coefs) || !(".x" %in% rownames(coefs))) next

    estimate <- coefs[".x", 1]

    e$complete_case_estimate <- estimate
    e$complete_case_n <- nrow(frame)

    agrees <- is.finite(e$estimate) && is.finite(estimate) &&
      sign(estimate) == sign(e$estimate)

    e$complete_case_agrees <- agrees

    if (!agrees) {

      e$warnings <- c(e$warnings, sprintf(
        paste("Using only the %d samples where %s was actually measured, the",
              "relationship reverses direction. It may be an artefact of the",
              "imputation."),
        nrow(frame), e$source))

    }

    edges[[i]] <- e

  }

  edges

}

# =============================================================================
# The resolution between a feature and a block
# =============================================================================
#
# Measured variables are rarely independent things. Fifty transcripts moving
# together are one process measured fifty times: testing each separately
# answers a question nobody asked, pays the multiplicity penalty fifty times,
# and reports fifty findings where there is one.
#
# The two resolutions already here are the feature and the block. The block
# is whatever the user called a block, which is a decision about how the data
# arrived rather than about biology. The missing level is the one the data
# can define for itself.
#
# Nothing here is additional evidence. It is the same measurements
# re-expressed, so a module agreeing with its own members is arithmetic, and
# every part of this has to say so rather than let it read as replication.
# =============================================================================

#' Group features that move together
#'
#' Correlation, not the evidence graph. A community detected on the evidence
#' graph groups features that each have a link to the outcome, which they can
#' do while being uncorrelated with each other; the first principal component
#' of such a group summarises nothing. A module has to be a set of variables
#' that vary together before summarising it means anything.
#'
#' @param x The analysis matrix.
#' @param min_correlation Features join a module at this average correlation.
#' @param min_size Modules smaller than this are left as individual features.
#' @param max_features Above this the correlation matrix is too large to be
#'   worth computing, and the modules would be too many to read.
#'
#' @return A named integer vector of module assignments, or \code{NULL}.
#' @keywords internal
#' @noRd
.detect_modules <- function(x, min_correlation = 0.5, min_size = 3L,
                            max_features = 2000L) {

  if (!is.matrix(x) && !is.data.frame(x)) return(NULL)

  x <- as.matrix(x)

  varying <- apply(x, 2, function(col) {
    col <- col[is.finite(col)]
    length(col) > 2 && stats::sd(col) > 0
  })

  x <- x[, varying, drop = FALSE]

  # Enough columns to form one module, not twice that: five variables that
  # are all one thing is a perfectly good answer, and requiring room for two
  # modules threw it away.

  if (ncol(x) < min_size || ncol(x) > max_features) return(NULL)

  correlation <- .safe_try(
    stats::cor(x, use = "pairwise.complete.obs"), NULL)

  if (is.null(correlation)) return(NULL)

  correlation[!is.finite(correlation)] <- 0

  # Distance on the absolute correlation: two features that move exactly
  # opposite each other are measuring one thing, and the sign is settled
  # later when the module gets a direction.

  tree <- .safe_try(
    stats::hclust(stats::as.dist(1 - abs(correlation)), method = "average"),
    NULL)

  if (is.null(tree)) return(NULL)

  membership <- stats::cutree(tree, h = 1 - min_correlation)

  sizes <- table(membership)

  keep <- names(sizes)[sizes >= min_size]

  if (length(keep) == 0) return(NULL)

  membership[!(as.character(membership) %in% keep)] <- NA_integer_

  # Renumbered so the labels run 1..k with no gaps: "module 7" out of four
  # modules is a puzzle for the reader and a bug report waiting to happen.

  present <- sort(unique(stats::na.omit(membership)))
  renumbered <- match(membership, present)
  names(renumbered) <- names(membership)

  renumbered

}

#' Summarise each module by the one variable that best represents it
#'
#' The first principal component, on standardised features so that a module
#' is not dominated by whichever of its members happens to have the largest
#' units.
#'
#' Its sign is arbitrary as \code{prcomp} returns it, which would make the
#' direction of every module's relationship with the outcome a coin flip. It
#' is fixed here so that a higher score means higher on the majority of the
#' module's features, which is the only reading that lets the direction be
#' reported at all.
#'
#' @param x The analysis matrix.
#' @param membership Module assignments from \code{.detect_modules()}.
#'
#' @return A list with the score matrix and, per module, the share of its own
#'   variance the score accounts for.
#' @keywords internal
#' @noRd
.module_latent <- function(x, membership) {

  x <- as.matrix(x)

  modules <- sort(unique(stats::na.omit(membership)))

  if (length(modules) == 0) return(NULL)

  scores <- matrix(NA_real_, nrow = nrow(x), ncol = length(modules),
                   dimnames = list(rownames(x), paste0("module_", modules)))

  explained <- stats::setNames(rep(NA_real_, length(modules)),
                               colnames(scores))

  for (i in seq_along(modules)) {

    members <- names(membership)[which(membership == modules[i])]
    members <- intersect(members, colnames(x))

    if (length(members) < 2) next

    block <- x[, members, drop = FALSE]

    complete <- stats::complete.cases(block)

    if (sum(complete) < 10) next

    pca <- .safe_try(
      stats::prcomp(block[complete, , drop = FALSE], center = TRUE,
                    scale. = TRUE),
      NULL)

    if (is.null(pca)) next

    pc1 <- drop(pca$x[, 1])

    # Sign: a component is defined up to a sign, so without this the
    # direction reported for a module is whichever way prcomp happened to
    # point. Aligned to the average of its own standardised members.

    average <- rowMeans(scale(block[complete, , drop = FALSE]))

    if (isTRUE(stats::cor(pc1, average) < 0)) pc1 <- -pc1

    # Scaled to unit variance, which is what makes the module's coefficient
    # mean the same thing as a feature's. A raw first component has a
    # standard deviation of roughly the square root of its eigenvalue, so a
    # ten-feature module arrives about three times wider than its own members
    # and its coefficient comes out three times smaller for no reason but
    # arithmetic. Read side by side, that looks like the module disagreeing
    # with the features it is made of.

    spread <- stats::sd(pc1)

    if (is.finite(spread) && spread > 0) pc1 <- pc1 / spread

    scores[complete, i] <- pc1

    explained[i] <- pca$sdev[1]^2 / sum(pca$sdev^2)

  }

  list(scores = scores, explained = explained)

}

#' Test each module against the outcome the way a feature is tested
#'
#' @param latent The score matrix.
#' @param outcome The resolved outcome.
#' @param covariates Covariate frame, or \code{NULL}.
#'
#' @return A data.frame of module-level estimates.
#' @keywords internal
#' @noRd
.module_edges <- function(latent, outcome, covariates = NULL) {

  binary <- identical(outcome$type, "binary")

  rows <- list()

  for (m in colnames(latent)) {

    frame <- data.frame(.y = outcome$values, .x = latent[, m])

    if (!is.null(covariates)) frame <- cbind(frame, covariates)

    frame <- frame[stats::complete.cases(frame), , drop = FALSE]

    if (nrow(frame) < 10 || stats::sd(frame$.x) == 0) next

    fit <- .safe_try(
      if (binary) stats::glm(.y ~ ., data = frame, family = stats::binomial())
      else stats::lm(.y ~ ., data = frame),
      NULL)

    if (is.null(fit)) next

    coefs <- .safe_try(summary(fit)$coefficients, NULL)

    if (is.null(coefs) || !(".x" %in% rownames(coefs))) next

    estimate <- coefs[".x", 1]
    se <- coefs[".x", 2]

    rows[[length(rows) + 1L]] <- data.frame(
      module = m,
      estimate = estimate,
      se = se,
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      p_value = coefs[".x", 4],
      n = nrow(frame),
      direction = if (estimate >= 0) "positive" else "negative",
      stringsAsFactors = FALSE
    )

  }

  if (length(rows) == 0) return(data.frame())

  out <- do.call(rbind, rows)
  out$fdr <- stats::p.adjust(out$p_value, method = "BH")
  out <- out[order(out$p_value), ]
  rownames(out) <- NULL

  out

}

#' Assemble the module-level reading of the same data
#'
#' @param x The analysis matrix.
#' @param outcome The resolved outcome.
#' @param covariates Covariate frame, or \code{NULL}.
#' @param feature_block Which block each feature came from.
#' @param min_correlation Passed to \code{.detect_modules()}.
#' @param min_variance_explained Below this a module's first component is not
#'   a summary of it, and the module is reported but not treated as coherent.
#'
#' @return A \code{ModuleGraph}.
#' @keywords internal
#' @noRd
.build_module_graph <- function(x, outcome, covariates = NULL,
                                feature_block = list(),
                                min_correlation = 0.5,
                                min_variance_explained = 0.5) {

  out <- ModuleGraph()
  out$method <- "average-linkage clustering on absolute correlation"
  out$height <- min_correlation

  membership <- .safe_try(.detect_modules(x, min_correlation), NULL)

  if (is.null(membership) || all(is.na(membership))) {

    out$notes <- paste(
      "No group of features moved together closely enough to be summarised",
      "as one thing, so every relationship in this analysis is between the",
      "outcome and a single variable.")

    return(out)

  }

  latent <- .safe_try(.module_latent(x, membership), NULL)

  if (is.null(latent)) {
    out$notes <- "Modules were found but could not be summarised."
    return(out)
  }

  usable <- colSums(!is.na(latent$scores)) > 0

  latent$scores <- latent$scores[, usable, drop = FALSE]
  latent$explained <- latent$explained[usable]

  if (ncol(latent$scores) == 0) {
    out$notes <- "Modules were found but could not be summarised."
    return(out)
  }

  out$membership <- membership
  out$latent <- latent$scores

  modules <- sort(unique(stats::na.omit(membership)))

  rows <- lapply(seq_along(modules), function(i) {

    name <- paste0("module_", modules[i])

    if (!(name %in% colnames(latent$scores))) return(NULL)

    members <- names(membership)[which(membership == modules[i])]

    blocks <- unlist(feature_block[members], use.names = FALSE)
    blocks <- blocks[!is.na(blocks)]

    composition <- if (length(blocks) == 0) "unknown" else {
      counts <- sort(table(blocks), decreasing = TRUE)
      paste(sprintf("%d%% %s", round(100 * as.integer(counts) / length(blocks)),
                    names(counts)), collapse = ", ")
    }

    explained <- unname(latent$explained[name])

    data.frame(
      module = name,
      size = length(members),
      blocks = length(unique(blocks)),
      composition = composition,

      # A module spanning layers is the interesting case: it is the only
      # thing here that could be a mechanism rather than a measurement
      # artefact of one platform.
      cross_block = length(unique(blocks)) > 1,

      variance_explained = explained,

      # Below this the first component is not a summary of the module, it is
      # one direction through a cloud. Reported rather than hidden, because a
      # module that failed to cohere is itself a finding about the data.
      coherent = isTRUE(explained >= min_variance_explained),

      members = paste(utils::head(members, 8), collapse = ", "),
      stringsAsFactors = FALSE
    )

  })

  out$modules <- do.call(rbind, Filter(Negate(is.null), rows))

  if (!is.null(out$modules)) {
    out$modules <- out$modules[order(-out$modules$size), ]
    rownames(out$modules) <- NULL
  }

  edges <- .safe_try(.module_edges(latent$scores, outcome, covariates),
                     data.frame())

  if (nrow(edges) > 0 && !is.null(out$modules)) {

    edges <- merge(
      edges,
      out$modules[, c("module", "size", "blocks", "cross_block",
                      "variance_explained", "coherent")],
      by = "module", all.x = TRUE)

    edges <- edges[order(edges$p_value), ]
    rownames(edges) <- NULL

  }

  out$edges <- edges

  out$notes <- c(
    sprintf(paste("%d module(s) covering %d of %d features. The rest move",
                  "independently enough that summarising them would lose",
                  "what makes them different."),
            nrow(.report_or(out$modules, data.frame())),
            sum(!is.na(membership)), length(membership)),

    paste("A module and its own members are the same measurements at two",
          "resolutions, not two findings. If both appear, they agree by",
          "construction and neither confirms the other.")
  )

  out

}

# =============================================================================
# Whether one number describes everybody
# =============================================================================
#
# Every estimate this package reports is an average over the people who were
# measured, and an average says nothing about whether they resemble each
# other. A coefficient of 0.4 is compatible with 0.4 in everyone and with 2.0
# in a tenth of them and nothing in the rest, and those two findings have
# almost nothing in common: the first is a property of the cohort, the second
# is a property of ten people nobody has identified.
#
# Testing that against a named moderator is the standard approach and needs
# the user to have guessed right about what modifies the effect. The check
# here needs no guess. It asks how much of the cohort would have to be
# removed to halve the estimate, which is answerable from the fit alone.
# =============================================================================

#' How few people it would take to halve a relationship
#'
#' For a relationship that holds across the cohort, removing a few people
#' barely moves the estimate: that is what an average does. For one produced
#' by a small unusual group, removing that group collapses it. The number of
#' samples that separates those two cases is a more useful description of
#' robustness than any p-value, and it is not a p-value in disguise: a
#' finding can be overwhelmingly significant and rest entirely on nine
#' people.
#'
#' The candidate set is ordered by influence on the coefficient, so this is
#' the worst case rather than a typical one. That is deliberate. A reader
#' deciding whether to believe a result wants to know how fragile it could
#' be, not how fragile a random deletion would make it.
#'
#' @param y Outcome values.
#' @param x The exposure column.
#' @param covariates Covariate frame, or \code{NULL}.
#' @param binary Whether the outcome is binary.
#'
#' @return A list with the count and the share of the cohort it represents,
#'   or \code{NULL} when the model could not be fitted.
#' @keywords internal
#' @noRd
.effect_concentration <- function(y, x, covariates = NULL, binary = FALSE) {

  frame <- data.frame(.y = y, .x = x)

  if (!is.null(covariates)) frame <- cbind(frame, covariates)

  frame <- frame[stats::complete.cases(frame), , drop = FALSE]

  n <- nrow(frame)

  if (n < 20 || stats::sd(frame$.x) == 0) return(NULL)

  fit_it <- function(d) {
    .safe_try(
      if (binary) stats::glm(.y ~ ., data = d, family = stats::binomial())
      else stats::lm(.y ~ ., data = d),
      NULL
    )
  }

  fit <- fit_it(frame)

  if (is.null(fit)) return(NULL)

  beta <- .safe_try(stats::coef(fit)[[".x"]], NA_real_)

  if (!is.finite(beta) || beta == 0) return(NULL)

  # An estimate indistinguishable from zero is trivially fragile: halving
  # nothing costs nothing, and the metric would report every null result as
  # driven by a handful of people. Concentration is a statement about a
  # relationship that exists, so there has to be one first.

  se <- .safe_try(summary(fit)$coefficients[".x", 2], NA_real_)

  if (!is.finite(se) || se <= 0 || abs(beta) < 2 * se) return(NULL)

  # One-step influence: how much each observation pulls the coefficient. Exact
  # for lm, a good approximation for glm, and it costs one call rather than n
  # refits.

  influence <- .safe_try(stats::dfbeta(fit)[, ".x"], NULL)

  if (is.null(influence) || length(influence) != n) return(NULL)

  # Samples pulling the estimate away from zero, worst first. Removing one of
  # these lowers the coefficient by roughly its own dfbeta.

  pulling <- influence * sign(beta)

  ordered <- order(pulling, decreasing = TRUE)

  cumulative <- cumsum(pulling[ordered])

  target <- abs(beta) / 2

  # Unnamed: `which()` carries the sample name across, and a count that
  # prints as `S41 44` is a puzzle rather than an answer.
  k <- unname(which(cumulative >= target)[1])

  if (is.na(k)) {

    # No prefix of the cohort halves it, which is the strongest possible
    # answer: the relationship is not carried by any identifiable minority.

    return(list(k = n, share = 1, confirmed = NA_real_))

  }

  # dfbetas are not additive, so the running total is an estimate. One refit
  # at the chosen k turns it into a measurement.

  refit <- fit_it(frame[-ordered[seq_len(k)], , drop = FALSE])

  confirmed <- if (is.null(refit)) NA_real_ else
    .safe_try(stats::coef(refit)[[".x"]], NA_real_)

  list(k = as.integer(k), share = k / n, confirmed = confirmed)

}

#' Attach the concentration check to the relationships that carry the report
#'
#' Restricted to relationships with the outcome, because only those have one
#' model that can be refitted without re-deriving the whole graph.
#'
#' @param edges Integrated edges.
#' @param x The analysis matrix.
#' @param outcome The resolved outcome.
#' @param covariates Covariate frame, or \code{NULL}.
#' @param top_n How many of the highest-scoring relationships to check.
#'
#' @return The edges, with the concentration fields set where applicable.
#' @keywords internal
#' @noRd
.edge_concentration <- function(edges, x, outcome, covariates, top_n = 20L) {

  if (length(edges) == 0) return(edges)

  candidates <- which(vapply(edges, function(e) {
    identical(e$target, outcome$name) && !is.null(e$source) &&
      e$source %in% colnames(x)
  }, logical(1)))

  if (length(candidates) == 0) return(edges)

  scores <- vapply(edges[candidates], function(e)
    .report_or(e$evidence_score, 0), numeric(1))

  candidates <- candidates[order(-scores)][
    seq_len(min(top_n, length(candidates)))]

  binary <- identical(outcome$type, "binary")

  for (i in candidates) {

    conc <- .safe_try(
      .effect_concentration(outcome$values, x[, edges[[i]]$source],
                            covariates, binary),
      NULL
    )

    if (is.null(conc)) next

    edges[[i]]$samples_driving_effect <- conc$k
    edges[[i]]$share_driving_effect <- conc$share

    if (conc$share <= 0.05) {

      edges[[i]]$warnings <- c(edges[[i]]$warnings, sprintf(
        paste("Removing the %d most influential sample(s), %.0f%% of the",
              "cohort, halves this estimate. It describes those people more",
              "than it describes the group."),
        conc$k, 100 * conc$share))

    }

  }

  edges

}

# =============================================================================
# Checking an adjustment against a stated causal structure
# =============================================================================
#
# Without a DAG the engine can say that something was adjusted for, and no
# more. That is a weak statement: conditioning on the wrong variable does not
# merely fail to help, it can make the estimate worse than doing nothing.
# Adjusting for a mediator removes part of the very effect being measured.
# Adjusting for a collider opens a path that was closed and manufactures an
# association out of nothing.
#
# A DAG is the only thing that can tell those apart, and it has to come from
# the user: it is a claim about biology, not something recoverable from the
# data. Supplying one turns "we adjusted for age" into "the adjustment closes
# every backdoor path, so this is identified" or into "you conditioned on a
# collider and this estimate is worse than the unadjusted one".
#
# Nothing here changes an estimate. It changes what may be claimed about one.
# =============================================================================

#' Refuse a causal structure that parsed into nothing
#'
#' @param parsed The result of a \code{dagitty} parse, possibly \code{NULL}.
#'
#' @return The structure, unchanged, or an error.
#' @keywords internal
#' @noRd
.validate_dag <- function(parsed) {

  # dagitty accepts text that is not a DAG at all without complaining: it
  # returns a graph holding a single unnamed node. A silent empty graph would
  # then audit every edge as "not in the DAG", which reads like a modelling
  # result rather than the typo it is.

  usable <- !is.null(parsed) &&
    length(setdiff(names(parsed), c("", NA))) > 0

  if (!usable) {
    stop(paste0(
      "Could not parse the DAG specification: no named variables were found.\n",
      "  Expected something like: dag { age -> protein; age -> disease }"
    ), call. = FALSE)
  }

  parsed

}

#' Accept a causal structure in whichever form the user has it
#'
#' A dagitty object, a dagitty string, or the simplest thing a biologist
#' actually has to hand: a table of arrows.
#'
#' @param dag A dagitty object, a character DAG specification, or a data.frame
#'   with \code{from} and \code{to} columns.
#'
#' @return A dagitty object, or NULL when nothing usable was supplied.
#' @keywords internal
#' @noRd
.parse_dag <- function(dag) {

  if (is.null(dag)) return(NULL)

  if (!requireNamespace("dagitty", quietly = TRUE)) {

    stop(
      paste("Supplying a DAG needs the 'dagitty' package.\n",
            " install.packages(\"dagitty\")"),
      call. = FALSE
    )

  }

  if (inherits(dag, "dagitty")) return(dag)

  if (is.data.frame(dag)) {

    if (!all(c("from", "to") %in% names(dag))) {

      stop("A DAG given as a data.frame needs 'from' and 'to' columns.",
           call. = FALSE)

    }

    if (nrow(dag) == 0) return(NULL)

    spec <- paste0(
      "dag { ",
      paste(sprintf("%s -> %s", dag$from, dag$to), collapse = "; "),
      " }"
    )

    return(.validate_dag(.safe_try(dagitty::dagitty(spec), NULL)))

  }

  if (is.character(dag) && length(dag) == 1) {

    return(.validate_dag(.safe_try(dagitty::dagitty(dag), NULL)))

  }

  stop(
    paste("'dag' must be a dagitty object, a dagitty specification string,",
          "or a data.frame with 'from' and 'to' columns."),
    call. = FALSE
  )

}

#' Is this effect identified by this adjustment, and if not, why not
#'
#' @param dag A dagitty object.
#' @param exposure,outcome Node names.
#' @param adjusted What was actually conditioned on.
#'
#' @return A list with the verdict, the reason, what would have sufficed, and
#'   any variable that should not have been adjusted for.
#' @noRd
.dag_audit <- function(dag, exposure, outcome, adjusted = character()) {

  unknown <- list(identifiable = NA, reason = character(),
                  required = character(), problems = character(),
                  harmful = character())

  if (is.null(dag)) return(unknown)

  nodes <- names(dag)

  # A variable the user never put in the DAG cannot be reasoned about. Saying
  # so beats guessing.
  if (!(exposure %in% nodes) || !(outcome %in% nodes)) {

    # Built by replacement, not by c(): concatenating two lists that both
    # carry a `reason` leaves two of them, and `$reason` picks the first,
    # which is the empty one.

    unknown$reason <- sprintf(
      "%s is not in the supplied DAG, so nothing can be said about it.",
      if (!(exposure %in% nodes)) exposure else outcome
    )

    return(unknown)

  }

  adjusted <- intersect(adjusted, nodes)

  descendants_of_exposure <- setdiff(
    .safe_try(dagitty::descendants(dag, exposure), character(0)), exposure)

  descendants_of_outcome <- setdiff(
    .safe_try(dagitty::descendants(dag, outcome), character(0)), outcome)

  ancestors_of_outcome <- setdiff(
    .safe_try(dagitty::ancestors(dag, outcome), character(0)), outcome)

  # --- variables that should not have been conditioned on --------------------

  mediators <- intersect(intersect(adjusted, descendants_of_exposure),
                         ancestors_of_outcome)

  post_outcome <- intersect(adjusted, descendants_of_outcome)

  post_exposure <- setdiff(intersect(adjusted, descendants_of_exposure),
                           c(mediators, post_outcome))

  problems <- character()
  harmful <- character()

  if (length(mediators) > 0) {
    problems <- c(problems, sprintf(
      "Conditioned on %s, which lies on the path from %s to %s: part of the effect being measured has been removed.",
      paste(mediators, collapse = ", "), exposure, outcome))
    harmful <- c(harmful, mediators)
  }

  if (length(post_outcome) > 0) {
    problems <- c(problems, sprintf(
      "Conditioned on %s, which %s affects: this can open a path that was closed and create an association where there is none.",
      paste(post_outcome, collapse = ", "), outcome))
    harmful <- c(harmful, post_outcome)
  }

  if (length(post_exposure) > 0) {
    problems <- c(problems, sprintf(
      "Conditioned on %s, which %s affects: adjusting after the exposure distorts the estimate.",
      paste(post_exposure, collapse = ", "), exposure))
    harmful <- c(harmful, post_exposure)
  }

  # --- what would have sufficed ---------------------------------------------

  sets <- .safe_try(
    lapply(dagitty::adjustmentSets(dag, exposure, outcome), as.character),
    list()
  )

  required <- if (length(sets) == 0) character(0) else sets[[1]]

  # --- the verdict -----------------------------------------------------------

  sufficient <- .safe_try(
    isTRUE(dagitty::isAdjustmentSet(dag, adjusted, exposure, outcome)),
    NA
  )

  reason <- character()

  if (length(sets) == 0 && !isTRUE(sufficient)) {

    return(list(
      identifiable = FALSE,
      reason = sprintf(
        "No set of the measured variables closes every backdoor path from %s to %s, so this effect cannot be identified from this data whatever is adjusted for.",
        exposure, outcome),
      required = character(0), problems = problems, harmful = unique(harmful)
    ))

  }

  if (isTRUE(sufficient) && length(harmful) == 0) {

    reason <- sprintf(
      "The adjustment closes every backdoor path from %s to %s under the DAG supplied.",
      exposure, outcome)

    return(list(identifiable = TRUE, reason = reason,
                required = required, problems = problems,
                harmful = character()))

  }

  if (isTRUE(sufficient) && length(harmful) > 0) {

    # dagitty can call a set sufficient while it also contains something that
    # should not be there; the harm is real either way.
    return(list(
      identifiable = FALSE,
      reason = "The backdoor paths are closed, but the adjustment also includes a variable that should not be conditioned on.",
      required = required, problems = problems, harmful = unique(harmful)
    ))

  }

  missing <- setdiff(required, adjusted)

  # Lead with the harm when there is one. "Does not close every backdoor path"
  # sends the reader looking for a missing confounder when the real problem is
  # a variable that should never have been in the set.

  reason <- if (length(harmful) > 0) {

    sprintf("Conditioning on %s is what breaks this: %s should not be in the adjustment set at all.",
            paste(harmful, collapse = ", "),
            if (length(harmful) == 1) "it" else "they")

  } else if (length(missing) > 0) {

    sprintf("A backdoor path from %s to %s is left open. Adjusting for %s would close it.",
            exposure, outcome, paste(missing, collapse = ", "))

  } else {

    sprintf("The adjustment does not close every backdoor path from %s to %s.",
            exposure, outcome)

  }

  list(identifiable = FALSE, reason = reason, required = required,
       problems = problems, harmful = unique(harmful))

}

#' Apply the audit to every relationship the DAG can speak about
#'
#' @noRd
.audit_edges <- function(edges, dag, covariates) {

  if (is.null(dag)) return(edges)

  lapply(edges, function(e) {

    audit <- .safe_try(
      .dag_audit(dag, e$source, e$target,
                 adjusted = unique(c(covariates, e$adjustment_set))),
      NULL
    )

    if (is.null(audit)) return(e)

    e$identifiable <- audit$identifiable
    e$identification_reason <- audit$reason
    e$required_adjustment <- audit$required
    e$adjustment_problems <- audit$problems

    # Conditioning on a mediator or on something the outcome affects makes the
    # estimate worse than leaving it alone, so the edge cannot go on claiming
    # that it was adjusted. Saying "none" here is not pedantry: it stops a
    # number that is actively misleading from being read as a careful one.

    if (length(audit$harmful) > 0) {

      e$identification <- "none"

      e$warnings <- c(e$warnings, sprintf(
        "Adjustment includes %s, which the supplied DAG says should not be conditioned on.",
        paste(audit$harmful, collapse = ", ")))

      e$assumptions <- c(
        "This estimate is worse than the unadjusted one under the DAG supplied.",
        e$assumptions)

    } else if (isFALSE(audit$identifiable) &&
               identical(e$identification, "adjustment")) {

      # Nothing harmful was conditioned on, but the adjustment does not close
      # the backdoor paths either. "Adjustment" names a strategy that worked;
      # keeping the label on an adjustment that did not is the report telling
      # the reader the opposite of what the audit just found.

      e$identification <- "none"

      e$assumptions <- c(
        "The adjustment made does not identify this effect under the DAG supplied.",
        e$assumptions)

    }

    e

  })

}
# =============================================================================
# Sensitivity to unmeasured confounding
# =============================================================================

#' How strong an unmeasured confounder would have to be
#'
#' The E-value (VanderWeele and Ding, 2017) is the minimum strength of
#' association, on the risk-ratio scale, that an unmeasured confounder would
#' need with both the exposure and the outcome to explain away an observed
#' association entirely.
#'
#' This does not say whether confounding is present. It says how much would
#' be required, which is a different and more useful question: an E-value of
#' 1.1 means a trivially weak confounder suffices, while 5 means the
#' confounder would have to be stronger than most measured risk factors.
#'
#' Continuous effects are put on the risk-ratio scale through the
#' approximation \code{RR = exp(0.91 * d)} for a standardised difference
#' \code{d}, which is the conversion the original authors propose.
#'
#' @param estimate Effect estimate.
#' @param se Standard error, used for the limit of the confidence interval.
#' @param scale Either \code{"log"} for coefficients already on a log scale
#'   (Cox, logistic) or \code{"standardised"} for standardised differences.
#'
#' @return A list with the point E-value and the E-value for the confidence
#'   limit closest to the null.
#' @noRd
.e_value <- function(estimate, se = NA_real_, scale = "standardised") {

  if (!is.finite(estimate)) {
    return(list(e_value = NA_real_, e_value_ci = NA_real_, rr = NA_real_))
  }

  to_rr <- function(x) {
    if (identical(scale, "log")) exp(x) else exp(0.91 * x)
  }

  from_rr <- function(rr) {

    if (!is.finite(rr) || rr <= 0) return(NA_real_)

    # The formula is stated for RR > 1; a protective effect is inverted first.
    if (rr < 1) rr <- 1 / rr

    if (rr <= 1) return(1)

    rr + sqrt(rr * (rr - 1))

  }

  point <- from_rr(to_rr(estimate))

  ci_limit <- if (is.finite(se) && se > 0) {

    lower <- estimate - 1.96 * se
    upper <- estimate + 1.96 * se

    # The bound closest to the null is what the E-value for the interval uses;
    # an interval spanning the null has no bound to report.
    if (lower <= 0 && upper >= 0) {
      1
    } else {
      from_rr(to_rr(if (abs(lower) < abs(upper)) lower else upper))
    }

  } else NA_real_

  list(e_value = point, e_value_ci = ci_limit, rr = to_rr(estimate))

}

#' Plain-language reading of an E-value
#' @noRd
.e_value_reading <- function(e) {

  if (!is.finite(e)) return("not computable")

  if (e < 1.25) "a very weak unmeasured confounder would explain this away"
  else if (e < 2) "a modest unmeasured confounder would explain this away"
  else if (e < 3) "explaining this away needs a fairly strong confounder"
  else if (e < 5) "explaining this away needs a strong confounder"
  else "explaining this away needs an implausibly strong confounder"

}

# =============================================================================
# Direction confidence
# =============================================================================

#' How well the data supports the orientation of an edge
#'
#' Observational data rarely identifies which way an arrow points. When one
#' method reports A to B and another reports B to A, the engine keeps the
#' better-supported direction, and this records how close the call was: 0.5
#' means the two directions were equally supported and the orientation is
#' arbitrary.
#'
#' Temporal evidence is the exception. If the exposure was measured before the
#' outcome, the orientation is not in question.
#'
#' @noRd
.direction_confidence <- function(forward_score, reverse_score, temporal,
                                  level) {

  # Only measurement order settles orientation. A high level does not: a
  # mediation model assumes its ordering rather than observing it.

  if (isTRUE(temporal)) {
    return(list(confidence = 1, basis = "measurement order"))
  }

  if (!is.finite(reverse_score) || reverse_score <= 0) {

    # Nothing argued for the other direction, but nothing tested it either.
    return(list(confidence = NA_real_, basis = "untested"))

  }

  total <- forward_score + reverse_score

  if (!is.finite(total) || total <= 0) {
    return(list(confidence = NA_real_, basis = "untested"))
  }

  list(confidence = forward_score / total, basis = "relative support")

}

# =============================================================================
# Score decomposition
# =============================================================================

#' Open up an evidence score into the parts it was built from
#'
#' The single number is convenient and hides everything. This returns the
#' components, each on a 0 to 1 scale, so a reader can see whether a score of
#' 70 rests on a large effect measured imprecisely or a small one measured
#' well.
#'
#' Biological support is reported here but never folded into the primary
#' score. Letting prior knowledge raise a score would make already-published
#' relationships outrank novel ones, which is confirmation bias inside the
#' metric of a tool meant for discovery.
#'
#' @param edge An \code{EvidenceEdge}.
#'
#' @return A data.frame of components with their values and weights.
#' @noRd
.decompose_evidence <- function(edge) {

  component <- function(name, value, weight, note) {
    data.frame(component = name, value = value, weight = weight,
               note = note, stringsAsFactors = FALSE)
  }

  rows <- list(

    component("Effect size", edge$strength, 0.34,
              "how large the relationship is"),

    component("Precision", edge$confidence, 0.33,
              "how tightly it was estimated"),

    component("Method agreement", edge$consistency, 0.33,
              "weighted by the epistemic level of each method"),

    component("Data quality", edge$data_quality, NA_real_,
              if (!is.finite(edge$data_quality)) "provenance unknown"
              else if (edge$data_quality >= 1) "nothing here was imputed"
              else sprintf("scales the score; limited by %s",
                           .report_or(edge$quality_limited_by, "an ingredient"))),

    component("Spread across the cohort", edge$share_driving_effect, NA_real_,
              if (!is.finite(edge$share_driving_effect)) "not checked"
              else sprintf("halved by removing %d sample(s)",
                           .report_or(edge$samples_driving_effect, NA))),

    component("Evidence level", edge$level / 5, NA_real_,
              sprintf("highest level reached: %s", .level_label(edge$level))),

    component("Temporal support", if (isTRUE(edge$temporal)) 1 else 0, NA_real_,
              if (isTRUE(edge$temporal)) "exposure measured first" else
                "no measurement order"),

    component("Direction confidence", edge$direction_confidence, NA_real_,
              .report_or(edge$direction_basis, "untested")),

    component("Bootstrap stability", edge$bootstrap_stability, NA_real_,
              if (is.finite(edge$bootstrap_stability))
                sprintf("recovered in %s of resamples",
                        format(round(edge$bootstrap_stability, 2))) else
                          "resampling not run"),

    component("Sensitivity (E-value)",
              if (is.finite(edge$e_value)) min(edge$e_value / 5, 1) else NA_real_,
              NA_real_,
              if (is.finite(edge$e_value))
                sprintf("E = %.2f, %s", edge$e_value,
                        .e_value_reading(edge$e_value)) else "not computable"),

    component("Biological support", edge$biological_support, NA_real_,
              "reported alongside, never inside the score")

  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out

}

# =============================================================================
# Contradiction summary
# =============================================================================

#' Explain why methods disagreed about a relationship
#'
#' Recording that a conflict exists is not much use on its own. What a reader
#' needs is the shape of the disagreement: whether the dissenters are all
#' predictive methods, whether the relationship only appears once covariates
#' are added, or whether a single method is out on its own.
#'
#' @noRd
.summarise_conflict <- function(group, agreeing, disagreeing) {

  if (length(disagreeing) == 0) return(character(0))

  methods_for <- vapply(group[agreeing], function(e) e$method, character(1))
  methods_against <- vapply(group[disagreeing], function(e) e$method,
                            character(1))

  levels_for <- vapply(group[agreeing], function(e)
    .evidence_level(e$generator), integer(1))
  levels_against <- vapply(group[disagreeing], function(e)
    .evidence_level(e$generator), integer(1))

  notes <- sprintf("%d method(s) report one direction, %d the other.",
                   length(agreeing), length(disagreeing))

  # A dissent from a higher level is a different situation from a dissent by a
  # predictive method, and the reader should not have to work that out.
  if (max(levels_against) > max(levels_for)) {

    notes <- c(notes, sprintf(
      "The dissenting evidence is of a higher kind (%s) than the majority (%s), so the majority should not simply be preferred.",
      .level_label(max(levels_against)), .level_label(max(levels_for))
    ))

  } else if (max(levels_against) < max(levels_for)) {

    notes <- c(notes, sprintf(
      "The dissent comes only from %s evidence, which carries less weight here.",
      .level_label(max(levels_against))
    ))

  }

  if (length(disagreeing) == 1L) {
    notes <- c(notes, sprintf("Only %s disagrees.", methods_against[1]))
  }

  adjusted <- vapply(group, function(e)
    length(e$adjustment_set) > 0, logical(1))

  if (any(adjusted[agreeing]) && !any(adjusted[disagreeing])) {

    notes <- c(notes, paste(
      "The relationship appears once covariates are accounted for and not",
      "before, which can mean the covariate was masking it or that",
      "adjusting for it induced the association."
    ))

  }

  notes

}

# =============================================================================
# Negative controls
# =============================================================================

#' Check whether variables that should not appear, do
#'
#' A negative control is something the user knows has no business being part
#' of the mechanism. If it shows up with a strong score, the finding is not
#' the control's fault: it is a signal that the pipeline is picking up
#' structure that is not biological.
#'
#' @param evidence Integrated evidence.
#' @param controls Character vector of feature names.
#' @param outcome_name Name of the outcome.
#'
#' @return A list with the offending edges and a verdict.
#' @noRd
.check_negative_controls <- function(evidence, controls, outcome_name) {

  if (length(controls) == 0) {
    return(list(declared = character(0), flagged = data.frame(),
                passed = NA, notes = character(0)))
  }

  hits <- Filter(
    function(e) (e$source %in% controls || e$target %in% controls),
    evidence
  )

  if (length(hits) == 0) {

    return(list(
      declared = controls,
      flagged = data.frame(),
      passed = TRUE,
      notes = sprintf(
        "None of the %d declared negative control(s) appear in the graph, which is what should happen.",
        length(controls))
    ))

  }

  flagged <- do.call(rbind, lapply(hits, function(e) {
    data.frame(source = e$source, target = e$target,
               evidence_score = round(e$evidence_score, 2),
               reaches_outcome = identical(e$target, outcome_name),
               stringsAsFactors = FALSE)
  }))

  flagged <- flagged[order(-flagged$evidence_score), ]
  rownames(flagged) <- NULL

  strong <- flagged[flagged$evidence_score >= 20, , drop = FALSE]

  notes <- sprintf(
    "%d relationship(s) involve a declared negative control.",
    nrow(flagged)
  )

  if (nrow(strong) > 0) {

    notes <- c(notes, paste(
      "At least one scores highly. A negative control should not, so treat",
      "the whole graph with caution: something other than biology may be",
      "driving these relationships, such as a batch effect, a technical",
      "artefact, or a variable that encodes the outcome indirectly."
    ))

  }

  list(declared = controls, flagged = flagged,
       passed = nrow(strong) == 0, notes = notes)

}

# =============================================================================
# Predictive versus causal standing
# =============================================================================

#' Separate how useful a variable is from how well supported it is
#'
#' These come apart more often than people expect. A variable can predict
#' well because it is a downstream consequence of the outcome, and a variable
#' with a well-supported effect can predict poorly because its effect is
#' small. Reporting them in one column invites the reader to conflate them.
#'
#' @noRd
.predictive_versus_causal <- function(edges, outcome_name, feature_block) {

  to_outcome <- Filter(function(e) identical(e$target, outcome_name), edges)

  if (length(to_outcome) == 0) return(data.frame())

  features <- unique(vapply(to_outcome, function(e) e$source, character(1)))

  rows <- lapply(features, function(v) {

    mine <- Filter(function(e) identical(e$source, v), to_outcome)

    levels <- vapply(mine, function(e) .evidence_level(e$generator), integer(1))

    predictive <- mine[levels == 1L]
    causal <- mine[levels >= 3L]
    associational <- mine[levels == 2L]

    scale01 <- function(x) if (length(x) == 0) 0 else {
      v <- max(abs(vapply(x, function(e) e$effect_size, numeric(1))),
               na.rm = TRUE)
      if (!is.finite(v)) 0 else v / (1 + v)
    }

    data.frame(
      feature = v,
      block = if (v %in% names(feature_block)) feature_block[[v]] else NA_character_,
      predictive_support = round(scale01(predictive), 3),
      associational_support = round(scale01(associational), 3),
      higher_support = round(scale01(causal), 3),
      highest_level = if (length(levels) == 0) NA_character_ else
        .level_label(max(levels)),
      stringsAsFactors = FALSE
    )

  })

  out <- do.call(rbind, rows)

  # The interesting rows are the mismatches, so they are labelled explicitly.
  out$standing <- ifelse(
    out$predictive_support > 0.3 & out$higher_support == 0,
    "predicts well, no higher evidence",
    ifelse(out$higher_support > 0 & out$predictive_support < 0.1,
           "supported but weakly predictive",
           "consistent across kinds")
  )

  out <- out[order(-out$higher_support, -out$predictive_support), ]
  rownames(out) <- NULL

  out

}

# =============================================================================
# Missing data
# =============================================================================

#' Describe the missingness the analysis had to work around
#'
#' Deliberately descriptive. Whether data is missing at random rather than
#' missing not at random cannot be decided from the observed data at all: the
#' evidence that would settle it is the part that is absent. Little's test
#' addresses only the stricter question of whether missingness is completely
#' at random, and even that is reported as a test result rather than a verdict.
#'
#' @noRd
.missing_report <- function(x, dropped_samples, screening) {

  n_cells <- length(x)
  n_missing <- sum(is.na(x))

  by_feature <- colMeans(is.na(x))
  by_sample <- rowMeans(is.na(x))

  complete <- sum(stats::complete.cases(x))

  patterns <- apply(is.na(x), 1, function(r) paste(as.integer(r), collapse = ""))
  pattern_table <- sort(table(patterns), decreasing = TRUE)

  # Little's MCAR test, reduced to what base R can do honestly: compare the
  # observed means of each variable between the rows that are complete and
  # the rows that are not. A difference is evidence against MCAR.
  mcar_p <- NA_real_

  if (n_missing > 0 && complete > 5 && complete < nrow(x) - 5) {

    incomplete <- !stats::complete.cases(x)

    ps <- vapply(seq_len(ncol(x)), function(j) {

      a <- x[!incomplete, j]
      b <- x[incomplete, j]

      a <- a[is.finite(a)]; b <- b[is.finite(b)]

      if (length(a) < 3 || length(b) < 3) return(NA_real_)

      .safe_try(stats::t.test(a, b)$p.value, NA_real_)

    }, numeric(1))

    ps <- ps[is.finite(ps)]

    # Fisher's method to combine the per-variable comparisons.
    if (length(ps) > 0) {
      stat <- -2 * sum(log(pmax(ps, 1e-300)))
      mcar_p <- stats::pchisq(stat, df = 2 * length(ps), lower.tail = FALSE)
    }

  }

  list(
    cells = n_cells,
    missing_cells = n_missing,
    missing_percent = 100 * n_missing / max(n_cells, 1),
    complete_cases = complete,
    complete_case_percent = 100 * complete / max(nrow(x), 1),
    worst_features = utils::head(sort(by_feature, decreasing = TRUE), 10),
    worst_samples = utils::head(sort(by_sample, decreasing = TRUE), 10),
    n_patterns = length(pattern_table),
    commonest_patterns = utils::head(pattern_table, 5),
    mcar_test_p = mcar_p,
    samples_dropped_by_alignment = length(dropped_samples),
    features_never_examined = screening$tested - screening$retained,
    notes = c(
      if (is.finite(mcar_p)) sprintf(
        "Test against missing-completely-at-random: p = %.4f. %s",
        mcar_p,
        if (mcar_p < 0.05)
          "Missingness is related to the observed values, so it is not completely at random."
        else
          "No evidence against complete randomness, which is weak reassurance rather than proof."
      ) else "Too few incomplete cases to test for complete randomness.",
      paste("Whether the data is missing at random rather than missing not at",
            "random cannot be determined from the observed data. No test can",
            "settle it, because the information needed is the part that is",
            "absent.")
    )
  )

}

# =============================================================================
# Resampling
# =============================================================================
#
# Method agreement says several methods saw the same thing in this sample.
# Resampling asks a different question: would they still see it in a slightly
# different sample? A relationship that six methods agree on but that
# disappears in a third of bootstrap resamples is not a robust finding.
#
# Cost is the constraint. A full analyze() takes seconds even on toy data, so
# a hundred replicates of everything runs into hours on a real dataset. Only
# the cheap generators are resampled by default and the choice is exposed, so
# the user decides how much computation to spend rather than discovering the
# answer after an afternoon.
# =============================================================================

#' Generators cheap enough to resample by default
#' @noRd
.fast_generators <- function() {
  c("association", "conditional", "survival", "longitudinal")
}

#' Estimate how long resampling will take before committing to it
#' @noRd
.resample_estimate <- function(seconds_per_replicate, n_replicates) {

  total <- seconds_per_replicate * n_replicates

  if (total < 90) sprintf("%.0f s", total)
  else if (total < 5400) sprintf("%.1f min", total / 60)
  else sprintf("%.1f h", total / 3600)

}

#' Re-run the cheap generators on resampled data
#'
#' Reports, for every relationship, how often it was recovered AND how often
#' it could have been: a feature that goes constant in a resample makes the
#' edge untestable there, and counting that as a failure would understate
#' stability. The denominator is returned rather than hidden.
#'
#' @param context The analysis context.
#' @param generators Generator names to re-run.
#' @param replicates How many resamples.
#' @param scheme Either "bootstrap" or "cv".
#' @param folds Folds, when scheme is "cv".
#' @param quiet Suppress progress.
#'
#' @return A list keyed by edge, each with found, evaluable and sign counts.
#' @noRd
.resample_evidence <- function(context, generators, replicates,
                               scheme = "bootstrap", folds = 10,
                               quiet = FALSE) {

  registry <- .evidence_registry()
  generators <- intersect(generators, names(registry))

  n <- nrow(context$x)

  if (n < 20 || length(generators) == 0 || replicates < 2) {
    return(list(available = FALSE, edges = list(), replicates = 0L))
  }

  found <- new.env(parent = emptyenv())
  evaluable <- new.env(parent = emptyenv())
  positive <- new.env(parent = emptyenv())

  # Structure, as opposed to individual edges. Recorded here because the
  # replicate graphs are built anyway and then discarded; keeping them costs
  # nothing and is the only way to say whether the picture is stable rather
  # than whether its parts are.

  ranks <- new.env(parent = emptyenv())
  sizes <- integer(0)

  bump <- function(env, key, by = 1L) {
    assign(key, (if (exists(key, envir = env)) get(key, envir = env) else 0L) + by,
           envir = env)
  }

  assignments <- .with_preserved_seed({

    if (identical(scheme, "cv")) {

      fold_id <- sample(rep(seq_len(folds), length.out = n))
      lapply(seq_len(replicates), function(r) {
        which(fold_id != ((r - 1L) %% folds) + 1L)
      })

    } else {

      lapply(seq_len(replicates), function(r) sample(n, replace = TRUE))

    }

  }, seed = context$params$seed)

  done <- 0L

  for (idx in assignments) {

    sub <- context
    sub$x <- context$x[idx, , drop = FALSE]
    sub$outcome <- context$outcome
    sub$outcome$values <- context$outcome$values[idx]

    if (!is.null(context$covariates))
      sub$covariates <- context$covariates[idx, , drop = FALSE]
    if (!is.null(context$time_values))
      sub$time_values <- context$time_values[idx]
    if (!is.null(context$subject_values))
      sub$subject_values <- context$subject_values[idx]

    # A feature with no variation left in this resample cannot be tested, and
    # that has to be recorded separately from being tested and not found.
    varying <- colnames(sub$x)[
      apply(sub$x, 2, function(col) {
        col <- col[is.finite(col)]
        length(col) > 2 && stats::sd(col) > 0
      })
    ]

    replicate_edges <- list()

    for (g in generators) {

      entry <- registry[[g]]

      if (!entry$applies(context$design, context$outcome)) next

      edges <- .safe_try(suppressWarnings(entry$generate(sub)), NULL)

      if (!is.null(edges)) replicate_edges <- c(replicate_edges, edges)

    }

    # Counting raw emissions would make every stability equal to one: the
    # association generator reports an edge for every feature it can fit,
    # significant or not. What has to survive the resample is a relationship
    # that would actually have been reported, so the replicate goes through
    # the same integration and threshold as the real run.

    integrated <- .safe_try(
      .integrate_evidence(replicate_edges, context$params), list()
    )

    # The replicate's own ranking, which is what a reader would have been
    # shown had this resample been the sample that was collected.

    ordered <- integrated[order(
      vapply(integrated, function(e) .report_or(e$evidence_score, 0),
             numeric(1)),
      decreasing = TRUE)]

    sizes <- c(sizes, length(ordered))

    for (i in seq_along(ordered)) {

      e <- ordered[[i]]
      key <- .edge_key(e)

      bump(found, key)
      if (isTRUE(e$estimate >= 0)) bump(positive, key)

      assign(key,
             c(if (exists(key, envir = ranks)) get(key, envir = ranks)
               else integer(0), i),
             envir = ranks)

    }

    # Every pair of varying features was testable in this replicate.
    for (v in varying) {
      bump(evaluable, paste(v, "->", context$outcome$name))
    }

    done <- done + 1L

    if (!isTRUE(quiet) && done %% 25 == 0)
      cat(sprintf("    %d/%d replicates\n", done, replicates))

  }

  keys <- ls(found)

  out <- lapply(keys, function(k) {

    n_found <- get(k, envir = found)
    n_eval <- if (exists(k, envir = evaluable)) get(k, envir = evaluable) else
      replicates

    n_pos <- if (exists(k, envir = positive)) get(k, envir = positive) else 0L

    seen_at <- if (exists(k, envir = ranks)) get(k, envir = ranks) else integer(0)

    list(
      found = n_found,
      evaluable = max(n_eval, n_found),
      stability = n_found / max(max(n_eval, n_found), 1L),
      sign_agreement = max(n_pos, n_found - n_pos) / max(n_found, 1L),
      ranks = seen_at
    )

  })

  names(out) <- keys

  list(available = TRUE, edges = out, replicates = replicates,
       scheme = scheme, generators = generators, sizes = sizes)

}

# =============================================================================
# The graph across resamples
# =============================================================================

#' Assemble what the resampling says about the shape of the graph
#'
#' The reported graph is one draw. This is the distribution it came from:
#' which relationships recur, which way they point when they do, where they
#' rank, and how much of the reported picture would survive being collected
#' again.
#'
#' The reported graph is deliberately not treated as the truth the resamples
#' are scored against. It is one of them, and the interesting cases are the
#' two disagreements: relationships that were reported but rarely recur, and
#' relationships that recur constantly but did not make the report.
#'
#' @param resampling The list returned by \code{.resample_evidence()}.
#' @param evidence The integrated edges from the full sample.
#' @param threshold Fraction of replicates a relationship must appear in to
#'   join the consensus.
#' @param top_n How many of the reported relationships to track by rank.
#'
#' @return A \code{ConsensusGraph}.
#' @keywords internal
#' @noRd
.build_consensus_graph <- function(resampling, evidence, threshold = 0.5,
                                   top_n = 10L) {

  out <- ConsensusGraph()

  if (!isTRUE(resampling$available) || length(resampling$edges) == 0) {
    out$notes <- "Resampling did not run, so nothing can be said about stability."
    return(out)
  }

  replicates <- resampling$replicates

  out$replicates <- replicates
  out$scheme <- .report_or(resampling$scheme, NA_character_)
  out$threshold <- threshold
  out$sizes <- resampling$sizes

  keys <- names(resampling$edges)

  parts <- strsplit(keys, " -> ", fixed = TRUE)

  reported <- vapply(evidence, .edge_key, character(1))

  rows <- lapply(seq_along(keys), function(i) {

    hit <- resampling$edges[[i]]
    seen <- hit$ranks

    # How often the two variables appeared in this order rather than the
    # other. An edge recovered every time but pointing whichever way the
    # resample felt like is not a stable finding about direction.
    reverse_key <- paste(parts[[i]][2], "->", parts[[i]][1])
    reverse_n <- if (reverse_key %in% keys)
      resampling$edges[[reverse_key]]$found else 0L

    data.frame(
      source = parts[[i]][1],
      target = parts[[i]][2],
      frequency = hit$found / max(replicates, 1L),

      # Recurrence alone is not recovery. A relationship that turns up in
      # nine resamples out of ten pointing a different way each time has been
      # found nine times and established nothing, so consensus membership is
      # decided on the share of replicates that agreed on a sign as well as
      # on turning up at all.
      consistent_frequency =
        (hit$found / max(replicates, 1L)) * hit$sign_agreement,

      found = hit$found,
      median_rank = if (length(seen) > 0) stats::median(seen) else NA_real_,
      best_rank = if (length(seen) > 0) min(seen) else NA_integer_,
      worst_rank = if (length(seen) > 0) max(seen) else NA_integer_,
      sign_agreement = hit$sign_agreement,
      orientation_agreement = hit$found / max(hit$found + reverse_n, 1L),
      reported = keys[i] %in% reported,
      stringsAsFactors = FALSE
    )

  })

  edges <- do.call(rbind, rows)
  edges <- edges[order(-edges$consistent_frequency, edges$median_rank), ]
  rownames(edges) <- NULL

  out$edges <- edges
  out$consensus <- edges[edges$consistent_frequency >= threshold, , drop = FALSE]
  rownames(out$consensus) <- NULL

  # The caveat travels with the object, because this is the number most
  # likely to be quoted out of context. A bootstrap resamples the people who
  # were actually measured, so it reports how much the picture depends on
  # which of them ended up in the study. It says nothing about whether a
  # relationship is real: a variable with no connection to the outcome that
  # happens to correlate with it in this sample will correlate with it in
  # almost every resample too, and will look perfectly stable here.

  out$notes <- c(
    out$notes,
    paste("Stability here means the finding does not depend on which people",
          "were sampled. It is not evidence that the relationship is real:",
          "a chance correlation in this sample recurs in almost every",
          "resample of it. Null calibration, at effort = 'thorough', is what",
          "addresses that.")
  )

  # --- how much of the reported picture is in the consensus -----------------

  in_consensus <- paste(out$consensus$source, "->", out$consensus$target)

  out$agreement <- list(
    reported = length(reported),
    consensus = nrow(out$consensus),
    both = length(intersect(reported, in_consensus)),
    reported_only = setdiff(reported, in_consensus),
    consensus_only = setdiff(in_consensus, reported),
    jaccard = if (length(union(reported, in_consensus)) == 0) NA_real_ else
      length(intersect(reported, in_consensus)) /
        length(union(reported, in_consensus))
  )

  # --- did the headline findings stay at the top ----------------------------

  top <- utils::head(reported, top_n)

  if (length(top) > 0) {

    out$rank_stability <- do.call(rbind, lapply(seq_along(top), function(i) {

      hit <- resampling$edges[[top[i]]]

      seen <- if (is.null(hit)) integer(0) else hit$ranks

      data.frame(
        source = sub(" ->.*$", "", top[i]),
        target = sub("^.*-> ", "", top[i]),
        reported_rank = i,
        frequency = if (is.null(hit)) 0 else hit$found / max(replicates, 1L),
        median_rank = if (length(seen) > 0) stats::median(seen) else NA_real_,
        # How often it stayed in the top group it was reported in. Being
        # third instead of second is not instability; falling out of the
        # top ten is.
        kept_top = if (length(seen) == 0) 0 else
          mean(seen <= length(top)),
        stringsAsFactors = FALSE
      )

    }))

    rownames(out$rank_stability) <- NULL

  }

  out

}

# =============================================================================
# Null calibration
# =============================================================================

#' How many relationships the engine finds when there is nothing to find
#'
#' The single most useful number a reader can have about a graph. Twenty-five
#' edges sounds like a result until you learn the same engine produces
#' twenty-two on the same data with the outcome shuffled.
#'
#' The outcome is permuted, which destroys any relationship between the
#' features and the outcome while leaving the correlation structure among the
#' features intact. Feature-to-feature edges therefore survive permutation by
#' construction, and only edges reaching the outcome are counted.
#'
#' @noRd
.null_calibration <- function(context, generators, permutations,
                              observed_edges, quiet = FALSE) {

  registry <- .evidence_registry()
  generators <- intersect(generators, names(registry))

  if (permutations < 2 || length(generators) == 0) {
    return(list(available = FALSE))
  }

  outcome_name <- context$outcome$name
  n <- nrow(context$x)

  observed <- sum(vapply(observed_edges,
                         function(e) identical(e$target, outcome_name),
                         logical(1)))

  counts <- integer(permutations)
  best <- numeric(permutations)

  permuted_orders <- .with_preserved_seed(
    lapply(seq_len(permutations), function(i) sample(n)),
    seed = context$params$seed + 1L
  )

  for (i in seq_along(permuted_orders)) {

    sub <- context
    sub$outcome$values <- context$outcome$values[permuted_orders[[i]]]

    if (!is.null(context$time_values))
      sub$time_values <- context$time_values[permuted_orders[[i]]]

    edges <- list()

    for (g in generators) {

      entry <- registry[[g]]

      if (!entry$applies(context$design, context$outcome)) next

      got <- .safe_try(suppressWarnings(entry$generate(sub)), NULL)

      if (!is.null(got)) edges <- c(edges, got)

    }

    integrated <- .safe_try(.integrate_evidence(edges, context$params), list())

    reaching <- Filter(function(e) identical(e$target, outcome_name), integrated)

    counts[i] <- length(reaching)
    best[i] <- if (length(reaching) == 0) 0 else
      max(vapply(reaching, function(e) e$evidence_score, numeric(1)))

    if (!isTRUE(quiet) && i %% 25 == 0)
      cat(sprintf("    %d/%d permutations\n", i, permutations))

  }

  observed_best <- {
    reaching <- Filter(function(e) identical(e$target, outcome_name),
                       observed_edges)
    if (length(reaching) == 0) 0 else
      max(vapply(reaching, function(e) e$evidence_score, numeric(1)))
  }

  list(
    available = TRUE,
    permutations = permutations,
    observed_edges = observed,
    null_edges_mean = mean(counts),
    null_edges_sd = stats::sd(counts),
    null_edges_max = max(counts),
    observed_best_score = observed_best,
    null_best_mean = mean(best),

    # Empirical p, with the usual +1 correction so it can never be zero.
    p_more_edges = (sum(counts >= observed) + 1) / (permutations + 1),
    p_better_score = (sum(best >= observed_best) + 1) / (permutations + 1),

    excess = observed - mean(counts),

    verdict = if (observed <= mean(counts)) {
      "The engine finds as many relationships on shuffled data as on the real data. Treat the whole graph as noise."
    } else if ((sum(counts >= observed) + 1) / (permutations + 1) > 0.05) {
      "The number of relationships found is not clearly more than chance produces."
    } else {
      "More relationships were found than shuffling the outcome produces, so the graph as a whole carries signal."
    }
  )

}

# =============================================================================
# Heterogeneity
# =============================================================================

#' Does a relationship hold in every subgroup
#'
#' Interaction tests are badly underpowered: a study sized to detect a main
#' effect is usually nowhere near able to detect that the effect differs
#' between groups. Subgroup findings from a scan like this are hypotheses at
#' best, and the wording says so rather than announcing that something works
#' "only in women".
#'
#' @noRd
.heterogeneity <- function(context, moderators, features, quiet = FALSE) {

  metadata <- context$metadata

  if (is.null(metadata) || length(moderators) == 0) {
    return(list(available = FALSE, table = data.frame()))
  }

  present <- intersect(moderators, colnames(metadata))

  if (length(present) == 0) {
    return(list(available = FALSE, table = data.frame(),
                notes = "None of the requested moderators are in the metadata."))
  }

  ids <- rownames(context$x)
  y <- context$outcome$values

  rows <- list()

  for (m in present) {

    values <- metadata[[m]][match(ids, metadata$sample_id)]

    groups <- unique(stats::na.omit(values))

    if (length(groups) < 2 || length(groups) > 6) next

    for (v in features) {

      frame <- data.frame(.y = y, .x = context$x[, v], .m = factor(values))
      frame <- frame[stats::complete.cases(frame), , drop = FALSE]

      if (nrow(frame) < 20) next
      if (min(table(frame$.m)) < 8) next
      if (stats::sd(frame$.x) == 0) next

      fit <- .safe_try(stats::lm(.y ~ .x * .m, data = frame), NULL)

      if (is.null(fit)) next

      coefs <- .safe_try(summary(fit)$coefficients, NULL)

      if (is.null(coefs)) next

      interaction_rows <- grep("^\\.x:", rownames(coefs))

      if (length(interaction_rows) == 0) next

      p <- min(coefs[interaction_rows, 4], na.rm = TRUE)

      # Per-group slopes, so a reader can see the shape of the difference.
      slopes <- vapply(levels(frame$.m), function(g) {
        sub <- frame[frame$.m == g, , drop = FALSE]
        if (nrow(sub) < 5 || stats::sd(sub$.x) == 0) return(NA_real_)
        .safe_try(stats::coef(stats::lm(.y ~ .x, data = sub))[[2]], NA_real_)
      }, numeric(1))

      rows[[length(rows) + 1L]] <- data.frame(
        feature = v, moderator = m,
        interaction_p = p,
        groups = paste(levels(frame$.m), collapse = " / "),
        slopes = paste(sprintf("%.3f", slopes), collapse = " / "),
        same_sign = all(is.na(slopes)) ||
          length(unique(sign(slopes[is.finite(slopes)]))) <= 1,

        # Kept as structured data as well as the printable string. The string
        # is for a console; anything that wants to put the groups in a table
        # had to parse it back out, which is not a thing to ask of a caller.
        detail = I(list(data.frame(
          group = levels(frame$.m),
          n = as.integer(table(frame$.m)),
          slope = unname(slopes),
          stringsAsFactors = FALSE
        ))),

        stringsAsFactors = FALSE
      )

    }

  }

  if (length(rows) == 0) {
    return(list(available = FALSE, table = data.frame(),
                notes = "No feature-moderator pair had enough data to test."))
  }

  out <- do.call(rbind, rows)
  out$interaction_fdr <- stats::p.adjust(out$interaction_p, method = "BH")
  out <- out[order(out$interaction_fdr), ]
  rownames(out) <- NULL

  n_sig <- sum(out$interaction_fdr < 0.05)

  list(
    available = TRUE,
    table = out,
    n_tested = nrow(out),
    n_heterogeneous = n_sig,
    notes = c(
      sprintf("%d of %d feature-moderator pairs show an interaction surviving correction.",
              n_sig, nrow(out)),
      paste("Interaction tests need far more data than main-effect tests.",
            "Treat any subgroup finding here as a hypothesis to test in a",
            "study designed for it, not as a conclusion about that subgroup.")
    )
  )

}

#' Put the subgroup result on the relationship it belongs to
#'
#' The table is a fine thing to read on its own and a poor way to be told
#' that the finding in front of you is an average over groups that disagree.
#' Nobody cross-references a table at the bottom of a report against the card
#' they are reading, so the finding has to carry it.
#'
#' Where a feature was tested against several moderators, the strongest
#' interaction wins the slot. Reporting the weakest would bury the point.
#'
#' @param edges Integrated edges.
#' @param heterogeneity The list returned by \code{.heterogeneity()}.
#' @param outcome_name Name of the outcome.
#'
#' @return The edges, with the heterogeneity fields set where applicable.
#' @keywords internal
#' @noRd
.edge_heterogeneity <- function(edges, heterogeneity, outcome_name) {

  if (!isTRUE(heterogeneity$available) ||
      !is.data.frame(heterogeneity$table) ||
      nrow(heterogeneity$table) == 0) {
    return(edges)
  }

  tab <- heterogeneity$table

  lapply(edges, function(e) {

    if (!identical(e$target, outcome_name)) return(e)

    hits <- tab[tab$feature == e$source, , drop = FALSE]

    if (nrow(hits) == 0) return(e)

    hits <- hits[order(hits$interaction_fdr), , drop = FALSE]

    best <- hits[1, ]

    e$heterogeneity_moderator <- best$moderator
    e$heterogeneity_p <- best$interaction_p
    e$heterogeneity_fdr <- best$interaction_fdr
    e$consistent_across_groups <- isTRUE(best$same_sign)

    if (!is.null(best$detail)) e$effect_by_group <- best$detail[[1]]

    if (isTRUE(best$interaction_fdr < 0.05)) {

      e$warnings <- c(e$warnings, sprintf(
        paste("This relationship differs by %s (FDR %.3g), so the single",
              "estimate above is an average over groups that do not agree."),
        best$moderator, best$interaction_fdr))

      if (!isTRUE(best$same_sign)) {

        e$warnings <- c(e$warnings, sprintf(
          "The direction itself reverses between levels of %s.",
          best$moderator))

      }

    }

    e

  })

}

# =============================================================================
# Model diagnostics
# =============================================================================

#' Fit quality for the models behind the top relationships
#'
#' Assumption checks are reported for what they are: a failed check does not
#' invalidate an estimate on its own, but it does tell the reader which
#' numbers to distrust.
#'
#' @noRd
.model_diagnostics <- function(context, features, quiet = FALSE) {

  y <- context$outcome$values
  binary <- identical(context$outcome$type, "binary")

  rows <- list()

  for (v in features) {

    frame <- data.frame(.y = y, .x = context$x[, v])

    if (!is.null(context$covariates))
      frame <- cbind(frame, context$covariates)

    frame <- frame[stats::complete.cases(frame), , drop = FALSE]

    if (nrow(frame) < 15 || stats::sd(frame$.x) == 0) next

    fit <- .safe_try(
      if (binary) stats::glm(.y ~ ., data = frame, family = stats::binomial())
      else stats::lm(.y ~ ., data = frame),
      NULL
    )

    if (is.null(fit)) next

    resid <- stats::residuals(fit)

    r2 <- if (binary) {
      1 - fit$deviance / fit$null.deviance          # McFadden
    } else {
      .safe_try(summary(fit)$r.squared, NA_real_)
    }

    auc <- if (binary) {

      .safe_try({
        p <- stats::fitted(fit)
        pos <- p[frame$.y == 1]; neg <- p[frame$.y == 0]
        mean(outer(pos, neg, ">") + 0.5 * outer(pos, neg, "=="))
      }, NA_real_)

    } else NA_real_

    # Breusch-Pagan for non-constant residual variance.
    hetero_p <- if (!binary) {
      .safe_try({
        aux <- stats::lm(resid^2 ~ stats::fitted(fit))
        stats::pf(summary(aux)$fstatistic[1],
                  summary(aux)$fstatistic[2],
                  summary(aux)$fstatistic[3], lower.tail = FALSE)
      }, NA_real_)
    } else NA_real_

    # Variance inflation for the feature itself, against the covariates.
    vif <- if (ncol(frame) > 2) {
      .safe_try({
        others <- setdiff(names(frame), c(".y", ".x"))
        aux <- stats::lm(
          stats::as.formula(paste(".x ~", paste(others, collapse = " + "))),
          data = frame)
        1 / (1 - summary(aux)$r.squared)
      }, NA_real_)
    } else NA_real_

    cooks <- .safe_try(stats::cooks.distance(fit), NULL)

    rows[[length(rows) + 1L]] <- data.frame(
      feature = v,
      n = nrow(frame),
      r_squared = round(r2, 4),
      aic = round(stats::AIC(fit), 1),
      bic = round(stats::BIC(fit), 1),
      auc = round(auc, 3),
      rmse = round(sqrt(mean(resid^2)), 4),
      residual_normality_p = round(.shapiro_p(resid), 4),
      heteroscedasticity_p = round(hetero_p, 4),
      vif = round(vif, 2),
      influential_points = if (is.null(cooks)) NA_integer_ else
        sum(cooks > 4 / nrow(frame), na.rm = TRUE),
      stringsAsFactors = FALSE
    )

  }

  if (length(rows) == 0) return(list(available = FALSE, table = data.frame()))

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Proportional hazards is the assumption the temporal claim rests on, so a
  # violation matters more here than the others.
  ph <- NULL

  if (isTRUE(context$design$survival) &&
      requireNamespace("survival", quietly = TRUE)) {

    ph <- do.call(rbind, lapply(features, function(v) {

      frame <- data.frame(.time = context$time_values,
                          .status = context$outcome$values,
                          .x = context$x[, v])
      frame <- frame[stats::complete.cases(frame) & frame$.time > 0, ,
                     drop = FALSE]

      if (nrow(frame) < 15) return(NULL)

      p <- .safe_try({
        fit <- survival::coxph(
          survival::Surv(frame$.time, frame$.status) ~ frame$.x)
        survival::cox.zph(fit)$table["GLOBAL", "p"]
      }, NA_real_)

      data.frame(feature = v, ph_assumption_p = round(p, 4),
                 stringsAsFactors = FALSE)

    }))

  }

  list(
    available = TRUE,
    table = out,
    proportional_hazards = ph,
    notes = c(
      sprintf("%d model(s) with residuals departing from normality (p < 0.05).",
              sum(out$residual_normality_p < 0.05, na.rm = TRUE)),
      sprintf("%d model(s) with non-constant residual variance.",
              sum(out$heteroscedasticity_p < 0.05, na.rm = TRUE)),
      if (any(out$vif > 5, na.rm = TRUE))
        "Some features are strongly collinear with the covariates, which inflates their standard errors."
      else "No serious collinearity with the covariates.",
      if (!is.null(ph) && any(ph$ph_assumption_p < 0.05, na.rm = TRUE))
        "Proportional hazards is violated for at least one feature; its temporal claim is weaker than the label suggests."
      else NULL
    )
  )

}

# =============================================================================
# Network robustness
# =============================================================================

#' How much the picture depends on any one part of it
#'
#' If dropping a whole assay barely changes the graph, the graph was not
#' really using it. If dropping one node collapses it, the result rests on a
#' single measurement.
#'
#' @noRd
.network_robustness <- function(graph, feature_block, outcome_name) {

  edges <- graph$edges

  if (nrow(edges) == 0) return(list(available = FALSE))

  block_of <- function(v) {
    if (identical(v, outcome_name)) "outcome"
    else if (v %in% names(feature_block)) feature_block[[v]] else "unknown"
  }

  total_score <- sum(edges$evidence_score)

  blocks <- setdiff(unique(vapply(c(edges$source, edges$target), block_of,
                                  character(1))), "outcome")

  leave_block <- do.call(rbind, lapply(blocks, function(b) {

    keep <- vapply(seq_len(nrow(edges)), function(i)
      block_of(edges$source[i]) != b && block_of(edges$target[i]) != b,
      logical(1))

    data.frame(
      removed = b,
      edges_lost = sum(!keep),
      edges_remaining = sum(keep),
      evidence_lost_percent = round(
        100 * (1 - sum(edges$evidence_score[keep]) / total_score), 1),
      paths_lost = if (nrow(graph$paths) > 0)
        sum(vapply(graph$paths$path, function(p)
          any(vapply(strsplit(p, " -> ")[[1]], block_of, character(1)) == b),
          logical(1))) else 0L,
      stringsAsFactors = FALSE
    )

  }))

  leave_block <- leave_block[order(-leave_block$evidence_lost_percent), ]
  rownames(leave_block) <- NULL

  nodes <- setdiff(unique(c(edges$source, edges$target)), outcome_name)

  leave_node <- do.call(rbind, lapply(nodes, function(v) {

    keep <- edges$source != v & edges$target != v

    data.frame(
      removed = v,
      edges_lost = sum(!keep),
      evidence_lost_percent = round(
        100 * (1 - sum(edges$evidence_score[keep]) / total_score), 1),
      stringsAsFactors = FALSE
    )

  }))

  leave_node <- leave_node[order(-leave_node$evidence_lost_percent), ]
  rownames(leave_node) <- NULL

  critical <- leave_node$removed[leave_node$evidence_lost_percent > 30]

  list(
    available = TRUE,
    leave_one_block_out = leave_block,
    leave_one_node_out = utils::head(leave_node, 15),
    critical_nodes = critical,
    notes = c(
      if (nrow(leave_block) > 0) sprintf(
        "Removing %s costs the most: %.1f%% of the total evidence.",
        leave_block$removed[1], leave_block$evidence_lost_percent[1]) else NULL,
      if (length(critical) > 0) sprintf(
        "The graph leans heavily on %s; a measurement error there would change the conclusions.",
        paste(critical, collapse = ", "))
      else "No single variable carries more than a third of the evidence."
    )
  )

}

# =============================================================================
# explain()
# =============================================================================

#' Explain why a variable ended up where it did
#'
#' Assembles everything the engine knows about one variable into a single
#' account: which methods supported it, at what epistemic level, how stable it
#' was under resampling, what a confounder would have to look like to explain
#' it away, and which paths it sits on.
#'
#' @param object A \code{CMOResult}.
#' @param feature Name of the variable to explain.
#'
#' @return A list, printed as a readable account.
#'
#' @examples
#' tr <- data.frame(
#'   G1 = rnorm(40), G2 = rnorm(40),
#'   row.names = paste0("S", 1:40)
#' )
#'
#' meta <- data.frame(sample_id = paste0("S", 1:40), y = rnorm(40))
#'
#' x <- load_data(assays = list(block = tr), metadata = meta)
#' prep <- preprocess(x, check_data(x), plots = FALSE, quiet = TRUE)
#'
#' result <- analyze(prep, outcome = "y", effort = "fast",
#'                   plots = FALSE, quiet = TRUE)
#'
#' explain(result, "G1")
#'
#' @export

explain <- function(object, feature) {

  if (!inherits(object, "CMOResult")) {
    stop("'object' must be a CMOResult object.", call. = FALSE)
  }

  involved <- Filter(
    function(e) identical(e$source, feature) || identical(e$target, feature),
    object$evidence
  )

  if (length(involved) == 0) {

    known <- unique(c(object$graph$nodes$name))

    stop(
      sprintf("'%s' has no relationships in this graph.%s", feature,
              if (length(known) > 0)
                sprintf("\n  Available: %s",
                        paste(utils::head(known, 15), collapse = ", ")) else ""),
      call. = FALSE
    )

  }

  to_outcome <- Filter(function(e) identical(e$target, object$outcome$name),
                       involved)

  paths <- if (nrow(object$causal_paths) > 0) {
    object$causal_paths[grepl(feature, object$causal_paths$path, fixed = TRUE), ]
  } else data.frame()

  importance <- if (nrow(object$importance) > 0) {
    object$importance[object$importance$feature == feature, ]
  } else data.frame()

  structure(
    list(
      feature = feature,
      outcome = object$outcome$name,
      edges = involved,
      to_outcome = to_outcome,
      paths = paths,
      encoding = .feature_encoding(object, feature),
      importance = importance,
      standing = if (nrow(.report_or(object$tables$standing, data.frame())) > 0)
        object$tables$standing[object$tables$standing$feature == feature, ] else
          data.frame(),
      roles = c(
        if (feature %in% object$interpretation$hubs) "hub" else NULL,
        if (feature %in% object$interpretation$mediators) "mediator" else NULL,
        if (feature %in% object$interpretation$bridges) "bridge" else NULL,
        if (feature %in% object$interpretation$risk) "associated with a higher outcome" else NULL,
        if (feature %in% object$interpretation$protective) "associated with a lower outcome" else NULL
      )
    ),
    class = "CMOExplanation"
  )

}

#' Print a readable account of one variable
#'
#' @param x A \code{CMOExplanation}.
#' @param ... Ignored.
#'
#' @return The explanation, invisibly.
#'
#' @export
print.CMOExplanation <- function(x, ...) {

  cat("\n")
  cat("Why does ", x$feature, " appear in this analysis?\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  if (length(x$roles) > 0) {
    cat("\nRole: ", paste(x$roles, collapse = ", "), "\n", sep = "")
  }

  if (length(x$to_outcome) > 0) {

    e <- x$to_outcome[[1]]

    cat("\nRelationship with ", x$outcome, "\n", sep = "")
    cat(strrep("-", 40), "\n", sep = "")

    cat(sprintf("  %-24s %s\n", "Direction",
                .result_plain_direction(e$direction, x$feature, x$outcome,
                                        x$encoding)))
    cat(sprintf("  %-24s %.1f / 100\n", "Evidence score", e$evidence_score))
    cat(sprintf("  %-24s %s (level %d)\n", "Highest evidence",
                .level_label(e$level), e$level))
    cat(sprintf("  %-24s %s\n", "Identification", e$identification))

    if (!is.na(e$identifiable)) {

      cat(sprintf("  %-24s %s\n", "Per the supplied DAG",
                  if (isTRUE(e$identifiable)) "identified"
                  else "NOT identified"))

      if (length(e$identification_reason) > 0) {
        cat(paste(strwrap(e$identification_reason, width = 70,
                          prefix = "    "), collapse = "\n"), "\n", sep = "")
      }

      for (p in e$adjustment_problems) {
        cat(paste(strwrap(p, width = 70, prefix = "    ", initial = "    ! "),
                  collapse = "\n"), "\n", sep = "")
      }

      missing <- setdiff(e$required_adjustment, e$adjustment_set)

      if (length(missing) > 0) {
        cat("    Missing from the adjustment set: ",
            paste(missing, collapse = ", "), "\n", sep = "")
      }

    }

    if (is.finite(e$e_value)) {
      cat(sprintf("  %-24s %.2f - %s\n", "E-value", e$e_value,
                  .e_value_reading(e$e_value)))
    }

    if (is.finite(e$bootstrap_stability)) {
      cat(sprintf("  %-24s %.0f%% of resamples\n", "Recovered in",
                  100 * e$bootstrap_stability))
    }

    if (is.finite(e$share_driving_effect)) {

      cat(sprintf("  %-24s %d sample(s), %.0f%% of the cohort\n",
                  "Halved by removing", e$samples_driving_effect,
                  100 * e$share_driving_effect))

    }

    if (is.finite(e$heterogeneity_fdr)) {

      cat(sprintf("  %-24s %s (FDR %.3g)\n", "Differs by",
                  e$heterogeneity_moderator, e$heterogeneity_fdr))

      if (is.data.frame(e$effect_by_group) && nrow(e$effect_by_group) > 0) {

        for (g in seq_len(nrow(e$effect_by_group))) {
          cat(sprintf("    %-16s n = %-6d slope %s\n",
                      e$effect_by_group$group[g],
                      e$effect_by_group$n[g],
                      format(round(e$effect_by_group$slope[g], 4))))
        }

      }

    }

    if (is.finite(e$data_quality) && e$data_quality < 1) {

      cat(sprintf("  %-24s %.2f (score scaled by this)\n", "Data quality",
                  e$data_quality))

      for (f in e$quality_flags) {
        cat(paste(strwrap(f, width = 70, prefix = "    "),
                  collapse = "\n"), "\n", sep = "")
      }

      if (is.finite(e$complete_case_estimate)) {
        cat(sprintf(
          "    On the %d measured rows only: %s (vs %s overall) - %s\n",
          e$complete_case_n,
          format(round(e$complete_case_estimate, 4)),
          format(round(e$estimate, 4)),
          if (isTRUE(e$complete_case_agrees)) "same direction"
          else "DIRECTION REVERSES"))
      }

    }

    if (is.finite(e$direction_confidence)) {
      cat(sprintf("  %-24s %.2f (%s)\n", "Direction confidence",
                  e$direction_confidence, e$direction_basis))
    }

    if (is.data.frame(e$contributions) && nrow(e$contributions) > 0) {

      cat("\n  What each method found on its own\n")

      display <- e$contributions[, c("method", "quantity", "pooled",
                                     "estimate", "ci_lower", "ci_upper",
                                     "p_value", "n", "agrees")]

      names(display) <- c("method", "measured", "pooled", "estimate",
                          "CI low", "CI high", "p", "n", "agrees")

      print(display, row.names = FALSE, digits = 3)

      cat(sprintf(
        "\n  Headline estimate: %s, pooled from %d observation(s).\n",
        .report_or(e$quantity_label, "unspecified"), e$pooled_from))

      if (length(e$not_pooled) > 0) {
        cat("  Not combined, being different measurements: ",
            paste(e$not_pooled, collapse = ", "), "\n", sep = "")
      }

      cat("\n  What those methods are\n")

      for (g in unique(e$contributions$generator)) {

        info <- .method_explanation(g)

        cat("\n    ", g, "\n", sep = "")
        cat(paste(strwrap(info$what, width = 70, prefix = "      "),
                  collapse = "\n"), "\n")

        if (nzchar(info$cannot)) {
          cat(paste(strwrap(paste("Limitation:", info$cannot), width = 70,
                            prefix = "      "), collapse = "\n"), "\n")
        }

      }

    } else {

      cat("\n  Supported by:\n")
      for (m in e$supporting_methods) cat("    + ", m, "\n", sep = "")

      if (length(e$conflicting_methods) > 0) {
        cat("\n  Disagreeing:\n")
        for (m in e$conflicting_methods) cat("    ! ", m, "\n", sep = "")
      }

    }

    if (length(e$conflict_summary) > 0) {
      cat("\n")
      cat(paste0("  ", e$conflict_summary), sep = "\n")
    }

    cat("\n  Score, opened up\n")
    print(.decompose_evidence(e), row.names = FALSE)

    cat("\n  What would have to be true\n")
    for (a in e$assumptions) {
      cat(paste(strwrap(a, width = 72, prefix = "      ", initial = "    - "),
                collapse = "\n"), "\n")
    }

  } else {

    cat("\n  No direct relationship with the outcome.\n")

  }

  if (nrow(x$paths) > 0) {
    cat("\nPaths it sits on\n")
    cat(strrep("-", 40), "\n", sep = "")
    print(x$paths[, c("path", "weakest_link")], row.names = FALSE)
  }

  if (nrow(x$importance) > 0) {
    cat("\nRanked by ", x$importance$n_methods[1],
        " method(s), combined ranking ",
        format(round(x$importance$consensus_importance[1], 3)), "\n", sep = "")
  }

  cat("\n")

  invisible(x)

}

# =============================================================================
# Counterfactual contrasts
# =============================================================================
#
# The question everyone actually wants answered: if this variable were higher
# or lower by some amount, what happens to the outcome, in the units the
# variable was measured in.
#
# Two things make it harder than it looks.
#
# The first is scale. Analysis runs on preprocessed data, so a coefficient is
# per unit of a transformed, normalized, scaled quantity, not per mg/dL.
# Because preprocess() stores a fitted model for every step, the original
# value can be pushed through the same pipeline and the change measured on
# the analysis scale. Non-linear steps mean the answer depends on where you
# start, so every contrast is anchored at a stated reference value.
#
# The second is that this is an interventional claim. "Risk falls by 15%"
# says something about acting on the world, and an adjusted association does
# not support that. The wording is therefore driven by the identification
# label rather than chosen freely, and variables the user has not declared
# modifiable are left out: computing a contrast for a genotype invites a
# reading of it that nothing can justify.
# =============================================================================

#' Push a value through the stored preprocessing pipeline
#'
#' Replays the feature-level steps recorded for one block, so a value in
#' original units can be located on the scale the analysis worked in.
#'
#' @noRd
.replay_value <- function(block_data, steps, block_name) {

  x <- block_data

  for (step in steps) {

    if (!identical(step$block, block_name)) next
    if (!isTRUE(step$replay)) next

    if (step$stage %in% c("remove_features", "filter")) {
      x <- .apply_keep(x, step$model)
      next
    }

    # Batch correction needs labels this synthetic row does not have, and
    # applying it would shift the contrast by an amount that depends on which
    # batch we pretended the row belonged to.
    if (identical(step$stage, "batch")) next

    entry <- .safe_try(.get_method(step$stage, step$method), NULL)

    if (is.null(entry)) next

    x <- .safe_try(entry$apply(x, step$model), x)

  }

  x

}

#' Locate a feature's original name inside its block
#' @noRd
.original_feature <- function(feature, block, assays) {

  columns <- colnames(assays[[block]])

  if (feature %in% columns) return(feature)

  # .align_blocks() qualifies a name only when two blocks collide.
  stripped <- sub(paste0("^", block, "\\."), "", feature)

  if (stripped %in% columns) return(stripped)

  NA_character_

}

#' Which methods report a coefficient that means something per unit
#'
#' A permutation importance and a network arc strength are not effects per
#' unit of anything, so they cannot answer this question at all.
#'
#' @noRd
.interpretable_generators <- function() {
  c("survival", "longitudinal", "association", "sem", "elasticnet")
}

#' Translate an effect on the model scale into a statement about the outcome
#' @noRd
.outcome_contrast <- function(effect, se, outcome_type, design,
                              baseline_risk = NULL) {

  if (isTRUE(design$survival)) {

    hr <- exp(effect)

    return(list(
      measure = "hazard ratio",
      value = hr,
      ci = if (is.finite(se)) exp(effect + c(-1.96, 1.96) * se) else
        c(NA_real_, NA_real_),
      percent = 100 * (hr - 1),
      unit = "hazard of the event"
    ))

  }

  if (identical(outcome_type, "binary")) {

    odds_ratio <- exp(effect)

    # An odds ratio is not a risk ratio unless the event is rare, so the
    # conversion is done explicitly against a stated baseline risk rather
    # than left for the reader to do wrongly.
    rr <- if (is.finite(baseline_risk) && baseline_risk > 0 &&
              baseline_risk < 1) {
      odds_ratio / (1 - baseline_risk + baseline_risk * odds_ratio)
    } else NA_real_

    return(list(
      measure = "odds ratio",
      value = odds_ratio,
      ci = if (is.finite(se)) exp(effect + c(-1.96, 1.96) * se) else
        c(NA_real_, NA_real_),
      percent = if (is.finite(rr)) 100 * (rr - 1) else 100 * (odds_ratio - 1),
      risk_ratio = rr,
      baseline_risk = baseline_risk,
      unit = if (is.finite(rr)) "risk of the event" else "odds of the event"
    ))

  }

  list(
    measure = "difference in the outcome",
    value = effect,
    ci = if (is.finite(se)) effect + c(-1.96, 1.96) * se else
      c(NA_real_, NA_real_),
    percent = NA_real_,
    unit = "units of the outcome"
  )

}

#' Wording that matches what the evidence actually supports
#'
#' The verb is chosen by the identification label, not by the author. An
#' adjusted association gets "is associated with" however large its score,
#' because "would reduce" is a claim about intervening and nothing in an
#' observational adjustment licenses it.
#'
#' @noRd
.contrast_sentence <- function(variable, direction_word, amount, units,
                               reference, contrast, identification,
                               outcome_name) {

  verb <- switch(
    identification,
    instrument = "is estimated to change",
    temporal   = "is associated with",
    adjustment = "is associated with",
    "is associated with"
  )

  magnitude <- if (is.finite(contrast$percent)) {

    sprintf("a %.1f%% %s %s", abs(contrast$percent),
            if (contrast$percent < 0) "lower" else "higher", contrast$unit)

  } else {

    sprintf("a change of %.3f in %s", contrast$value, outcome_name)

  }

  sentence <- sprintf(
    "%s %s in %s%s %s %s.",
    direction_word,
    if (is.finite(amount)) sprintf("%.4g%s", amount,
                                   if (nzchar(units)) paste0(" ", units) else "")
    else "one unit",
    variable,
    if (is.finite(reference)) sprintf(" from %.4g", reference) else "",
    verb, magnitude
  )

  caveat <- switch(
    identification,
    instrument = "An instrument supports reading this as an effect of acting on the variable.",
    temporal   = "The variable was measured before the outcome, so the relationship cannot run backwards. It can still be produced by something that was never measured.",
    adjustment = "This is an association after accounting for the measured factors. It is not a prediction of what would happen if the variable were changed.",
    "This is an unadjusted association. It is not a prediction of what would happen if the variable were changed."
  )

  list(sentence = sentence, caveat = caveat)

}

#' What the results imply about changing a variable
#'
#' Expresses each relationship as a contrast in the units the variable was
#' originally measured in: move this variable by this much, and the outcome
#' differs by this much.
#'
#' @section What this is and is not:
#'
#' The arithmetic is a contrast implied by a fitted model, not a prediction of
#' what an intervention would do. Those coincide only when the effect is
#' identified, which for observational data it usually is not. Every row
#' therefore carries its identification label and a caveat written to match
#' it, and the wording never says "would reduce" unless an instrument
#' supports that reading.
#'
#' Only variables named in \code{modifiable} are included. A contrast for a
#' genotype or an ancestry component is arithmetic without meaning, and
#' printing one invites exactly the reading it cannot support.
#'
#' @param object A \code{CMOResult}.
#' @param modifiable Variables or block names to consider. Anything the user
#'   could plausibly act on: diet, a metabolite, a treatable protein. Required.
#' @param change How much to move each variable, in its original units.
#'   \code{"sd"} (the default) uses one standard deviation, \code{"iqr"} the
#'   interquartile range, a number applies that amount to everything, and a
#'   named vector sets it per variable.
#' @param direction Either \code{"increase"} or \code{"decrease"}.
#' @param baseline Reference point the change starts from, since non-linear
#'   preprocessing makes the answer depend on it. \code{"median"} or a number.
#' @param baseline_risk Observed risk of the event, used to turn an odds ratio
#'   into a risk ratio for a binary outcome. Taken from the data when absent.
#' @param min_identification Lowest identification strength to report:
#'   \code{"none"}, \code{"adjustment"}, \code{"temporal"} or
#'   \code{"instrument"}.
#'
#' @return A \code{CMOCounterfactual} object, printed as readable statements.
#'
#' @examples
#' set.seed(1)
#' n <- 60
#' protein <- rnorm(n, 120, 15)
#' y <- 0.05 * protein + rnorm(n, 0, 1)
#' ids <- paste0("S", seq_len(n))
#'
#' x <- load_data(
#'   assays = list(blood = data.frame(APOA1 = protein, row.names = ids)),
#'   metadata = data.frame(sample_id = ids, HDL = y)
#' )
#'
#' prep <- preprocess(x, check_data(x), plots = FALSE, quiet = TRUE)
#' res <- analyze(prep, outcome = "HDL", effort = "fast",
#'                plots = FALSE, quiet = TRUE)
#'
#' counterfactual(res, modifiable = "APOA1")
#'
#' @export

counterfactual <- function(object,
                           modifiable,
                           change = "sd",
                           direction = c("increase", "decrease"),
                           baseline = "median",
                           baseline_risk = NULL,
                           min_identification = c("none", "adjustment",
                                                  "temporal", "instrument")) {

  if (!inherits(object, "CMOResult")) {
    stop("'object' must be a CMOResult object.", call. = FALSE)
  }

  if (missing(modifiable) || length(modifiable) == 0) {

    stop(
      paste("'modifiable' must name the variables or blocks you could",
            "plausibly act on.\n  Nothing is included by default: a contrast",
            "for a genotype is arithmetic without meaning, and printing one",
            "invites a reading it cannot support."),
      call. = FALSE
    )

  }

  direction <- match.arg(direction)
  min_identification <- match.arg(min_identification)

  preprocessing <- object$input

  if (!inherits(preprocessing, "PreprocessingResult")) {
    stop("The result does not carry the preprocessing it came from.",
         call. = FALSE)
  }

  original <- preprocessing$input$assays
  feature_block <- object$data$feature_block
  outcome_name <- object$outcome$name

  # ---------------------------------------------------------------------------
  # Which variables are on the table
  # ---------------------------------------------------------------------------

  blocks_named <- intersect(modifiable, unique(unname(feature_block)))

  # A categorical variable is several columns by the time it reaches here, so
  # naming "Sex" has to reach "Sex=M" and "Sex=other" without the user having
  # to know how it was encoded.

  encoding <- .report_or(object$data$encoding, list())

  from_categories <- names(encoding)[
    vapply(encoding, function(e) e$variable %in% modifiable, logical(1))
  ]

  wanted <- unique(c(
    intersect(modifiable, names(feature_block)),
    names(feature_block)[feature_block %in% blocks_named],
    from_categories
  ))

  if (length(wanted) == 0) {

    stop(
      sprintf("None of %s is a variable or block in this analysis.\n  Available blocks: %s",
              paste(modifiable, collapse = ", "),
              paste(unique(unname(feature_block)), collapse = ", ")),
      call. = FALSE
    )

  }

  ranking <- c(none = 0, adjustment = 1, temporal = 2, instrument = 3)

  if (is.null(baseline_risk) && identical(object$outcome$type, "binary")) {
    baseline_risk <- mean(object$outcome$values, na.rm = TRUE)
  }

  # ---------------------------------------------------------------------------
  # One contrast per qualifying relationship
  # ---------------------------------------------------------------------------

  rows <- list()
  statements <- list()
  skipped <- character(0)

  for (edge in object$evidence) {

    if (!identical(edge$target, outcome_name)) next
    if (!(edge$source %in% wanted)) next

    if (ranking[[edge$identification]] < ranking[[min_identification]]) {
      skipped <- c(skipped, sprintf("%s (identification: %s)", edge$source,
                                    edge$identification))
      next
    }

    contributions <- edge$contributions

    if (!is.data.frame(contributions) || nrow(contributions) == 0) next

    usable <- contributions[
      contributions$generator %in% .interpretable_generators() &
        is.finite(contributions$estimate), , drop = FALSE]

    if (nrow(usable) == 0) {
      skipped <- c(skipped, sprintf("%s (no method reports a per-unit effect)",
                                    edge$source))
      next
    }

    # Prefer the strongest kind of evidence that can express an effect.
    usable <- usable[order(-usable$level), , drop = FALSE]
    chosen <- usable[1, ]

    block <- feature_block[[edge$source]]

    # A category comparison has no units and no reference value to move from.
    # The contrast it supports is the only one there is: being this category
    # rather than the one it was compared against.

    coded <- object$data$encoding[[edge$source]]

    if (!is.null(coded)) {

      effect <- chosen$estimate
      se <- if (is.finite(chosen$se)) abs(chosen$se) else NA_real_

      contrast <- .outcome_contrast(effect, se, object$outcome$type,
                                    object$design, baseline_risk)

      verb <- if (identical(edge$identification, "instrument"))
        "is estimated to change" else "is associated with"

      magnitude <- if (is.finite(contrast$percent)) {
        sprintf("a %.1f%% %s %s", abs(contrast$percent),
                if (contrast$percent < 0) "lower" else "higher", contrast$unit)
      } else {
        sprintf("a change of %.3f in %s", contrast$value, outcome_name)
      }

      sentence <- sprintf("Being %s rather than %s %s %s.",
                          coded$level, coded$reference, verb, magnitude)

      caveat <- .contrast_sentence("", "", NA_real_, "", NA_real_, contrast,
                                   edge$identification, outcome_name)$caveat

      rows[[length(rows) + 1L]] <- data.frame(
        variable = coded$variable,
        block = block,
        from = coded$reference,
        to = coded$level,
        change = sprintf("category (%d vs %d people)",
                         coded$n_level, coded$n_reference),
        measure = contrast$measure,
        value = round(contrast$value, 4),
        ci_lower = round(contrast$ci[1], 4),
        ci_upper = round(contrast$ci[2], 4),
        percent_change = round(contrast$percent, 2),
        method = chosen$method,
        identification = edge$identification,
        evidence_score = round(edge$evidence_score, 1),
        stringsAsFactors = FALSE
      )

      statements[[length(statements) + 1L]] <- list(
        sentence = sentence, caveat = caveat, variable = edge$source,
        evidence_score = edge$evidence_score,
        identification = edge$identification
      )

      next

    }

    raw_name <- .original_feature(edge$source, block, original)

    if (is.na(raw_name)) next

    raw_values <- original[[block]][[raw_name]]
    raw_values <- raw_values[is.finite(raw_values)]

    if (length(raw_values) < 3) next

    reference <- if (identical(baseline, "median")) stats::median(raw_values)
    else if (is.numeric(baseline)) baseline else stats::median(raw_values)

    amount <- if (is.numeric(change)) {
      if (!is.null(names(change)) && edge$source %in% names(change))
        change[[edge$source]] else change[1]
    } else if (identical(change, "iqr")) {
      stats::IQR(raw_values)
    } else {
      stats::sd(raw_values)
    }

    if (!is.finite(amount) || amount <= 0) next

    signed <- if (identical(direction, "increase")) amount else -amount

    # ---- locate both values on the analysis scale --------------------------

    template <- original[[block]][
      rep(which.min(abs(raw_values - reference))[1], 2), , drop = FALSE]

    if (nrow(template) < 2) next

    template[[raw_name]] <- c(reference, reference + signed)
    rownames(template) <- c("from", "to")

    processed <- .safe_try(
      .replay_value(template, preprocessing$steps, block), NULL
    )

    if (is.null(processed) || !(raw_name %in% names(processed))) {
      skipped <- c(skipped, sprintf("%s (could not be mapped back to its units)",
                                    edge$source))
      next
    }

    delta <- processed[[raw_name]][2] - processed[[raw_name]][1]

    if (!is.finite(delta) || delta == 0) {
      skipped <- c(skipped, sprintf("%s (preprocessing flattened the change)",
                                    edge$source))
      next
    }

    effect <- chosen$estimate * delta
    se <- if (is.finite(chosen$se)) abs(chosen$se * delta) else NA_real_

    contrast <- .outcome_contrast(effect, se, object$outcome$type,
                                  object$design, baseline_risk)

    wording <- .contrast_sentence(
      edge$source,
      if (identical(direction, "increase")) "An increase of" else "A decrease of",
      amount, "", reference, contrast, edge$identification, outcome_name
    )

    rows[[length(rows) + 1L]] <- data.frame(
      variable = edge$source,
      block = block,
      from = round(reference, 4),
      to = round(reference + signed, 4),
      change = round(signed, 4),
      measure = contrast$measure,
      value = round(contrast$value, 4),
      ci_lower = round(contrast$ci[1], 4),
      ci_upper = round(contrast$ci[2], 4),
      percent_change = round(contrast$percent, 2),
      method = chosen$method,
      identification = edge$identification,
      evidence_score = round(edge$evidence_score, 1),
      stringsAsFactors = FALSE
    )

    statements[[length(statements) + 1L]] <- c(
      wording, list(variable = edge$source,
                    evidence_score = edge$evidence_score,
                    identification = edge$identification)
    )

  }

  table <- if (length(rows) > 0) {
    out <- do.call(rbind, rows)
    out <- out[order(-abs(.report_or(out$percent_change, 0)),
                     -out$evidence_score), ]
    rownames(out) <- NULL
    out
  } else data.frame()

  structure(
    list(
      table = table,
      statements = statements,
      outcome = object$outcome,
      design = object$design,
      modifiable = modifiable,
      considered = wanted,
      skipped = unique(skipped),
      direction = direction,
      change = change,
      baseline = baseline,
      baseline_risk = baseline_risk,
      min_identification = min_identification
    ),
    class = "CMOCounterfactual"
  )

}

#' Print counterfactual contrasts as readable statements
#'
#' @param x A \code{CMOCounterfactual}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export
print.CMOCounterfactual <- function(x, ...) {

  cat("\n")
  cat("What the results imply about changing these variables\n")
  cat(strrep("=", 68), "\n", sep = "")

  cat(sprintf("\nOutcome  : %s (%s)\n", x$outcome$name, x$outcome$type))
  cat(sprintf("Design   : %s\n", x$design$type))
  cat(sprintf("Change   : %s of %s, from the %s\n",
              x$direction,
              if (is.numeric(x$change)) format(x$change) else
                switch(x$change, sd = "one standard deviation",
                       iqr = "the interquartile range", x$change),
              if (is.numeric(x$baseline)) format(x$baseline) else x$baseline))

  if (is.finite(.report_or(x$baseline_risk, NA))) {
    cat(sprintf("Baseline : %.1f%% of people had the event\n",
                100 * x$baseline_risk))
  }

  if (nrow(x$table) == 0) {

    cat("\nNo variable qualified.\n")

    if (length(x$skipped) > 0) {
      cat("\nLeft out:\n")
      cat(paste0("  - ", x$skipped), sep = "\n")
    }

    return(invisible(x))

  }

  cat("\n")

  for (s in x$statements) {

    cat(paste(strwrap(s$sentence, width = 68, prefix = "  "),
              collapse = "\n"), "\n", sep = "")

    cat(sprintf("    evidence score %.0f/100, %s\n",
                s$evidence_score, s$identification))

    cat(paste(strwrap(s$caveat, width = 66, prefix = "    "),
              collapse = "\n"), "\n\n", sep = "")

  }

  cat(strrep("-", 68), "\n", sep = "")

  print(x$table[, c("variable", "from", "to", "measure", "value",
                    "percent_change", "identification")],
        row.names = FALSE)

  if (length(x$skipped) > 0) {
    cat("\nLeft out:\n")
    cat(paste0("  - ", x$skipped), sep = "\n")
  }

  cat("\n")

  invisible(x)

}

# =============================================================================
# The block as a unit of evidence
# =============================================================================
#
# A block is more than a group of columns: it is a layer of biology measured
# with one technology. Reading a hundred variable-to-variable edges is hard;
# reading "microbiome to metabolome carries 67 relationships at a mean score
# of 82" is not, and it is often the question that was actually being asked.
#
# Everything here aggregates evidence that already exists. None of it changes
# a score. Whether a relationship crosses blocks makes it more interesting,
# not better supported, and folding that preference into evidence_score would
# mix a thematic judgement into a measurement of size, precision and
# agreement.
# =============================================================================

#' Does this relationship cross between measurement layers
#'
#' Only meaningful between two features. An edge into the outcome always
#' crosses from a block to the outcome, so calling that cross-block would
#' make the field true for almost everything and mean nothing.
#'
#' @noRd
.is_cross_block <- function(source_block, target_block) {

  if (is.na(source_block) || is.na(target_block)) return(NA)

  if (identical(source_block, "outcome") || identical(target_block, "outcome")) {
    return(NA)
  }

  !identical(source_block, target_block)

}

#' Evidence summarised between every pair of blocks
#'
#' @param evidence Integrated evidence.
#' @param outcome_name Name of the outcome.
#'
#' @return A data.frame, one row per ordered pair of blocks.
#' @noRd
.block_evidence <- function(evidence, outcome_name) {

  if (length(evidence) == 0) return(data.frame())

  rows <- do.call(rbind, lapply(evidence, function(e) {

    data.frame(
      from = .report_or(e$source_block, NA_character_),
      to = .report_or(e$target_block, NA_character_),
      score = e$evidence_score,
      consistency = e$consistency,
      level = e$level,
      temporal = isTRUE(e$temporal),
      stringsAsFactors = FALSE
    )

  }))

  rows <- rows[!is.na(rows$from) & !is.na(rows$to), , drop = FALSE]

  if (nrow(rows) == 0) return(data.frame())

  key <- paste(rows$from, rows$to, sep = "\r")

  out <- do.call(rbind, lapply(unique(key), function(k) {

    group <- rows[key == k, , drop = FALSE]

    data.frame(
      from = group$from[1],
      to = group$to[1],
      relationships = nrow(group),
      mean_score = round(mean(group$score), 2),
      max_score = round(max(group$score), 2),
      total_evidence = round(sum(group$score), 1),
      mean_consistency = round(mean(group$consistency), 3),
      highest_level = .level_label(max(group$level)),
      temporal = sum(group$temporal),
      crosses_blocks = !identical(group$from[1], group$to[1]),
      reaches_outcome = identical(group$to[1], "outcome"),
      stringsAsFactors = FALSE
    )

  }))

  out <- out[order(-out$total_evidence), ]
  rownames(out) <- NULL

  out

}

#' How much of the total evidence each block accounts for
#'
#' Answers the question a variable-level ranking cannot: which layer of
#' biology is carrying the result. A block can hold no single strong variable
#' and still dominate through many moderate ones, or the reverse.
#'
#' @noRd
.block_importance <- function(evidence, graph, feature_block, outcome_name) {

  blocks <- unique(unname(feature_block))

  if (length(blocks) == 0 || length(evidence) == 0) return(data.frame())

  total <- sum(vapply(evidence, function(e) e$evidence_score, numeric(1)))

  if (!is.finite(total) || total <= 0) return(data.frame())

  # A relationship between two blocks belongs to both, so counting it whole
  # in each would make the shares add up to more than everything. Its score
  # is split between them; a relationship inside one block, or from a block
  # to the outcome, belongs entirely to that block.

  block_of <- function(v) {
    if (identical(v, outcome_name)) "outcome"
    else if (v %in% names(feature_block)) feature_block[[v]] else NA_character_
  }

  credit <- stats::setNames(numeric(length(blocks)), blocks)

  for (e in evidence) {

    ends <- c(block_of(e$source), block_of(e$target))
    ends <- ends[!is.na(ends) & ends != "outcome"]
    ends <- unique(ends)

    if (length(ends) == 0) next

    for (b in ends) {
      if (b %in% names(credit)) credit[[b]] <- credit[[b]] +
        e$evidence_score / length(ends)
    }

  }

  rows <- lapply(blocks, function(b) {

    members <- names(feature_block)[feature_block == b]

    involved <- Filter(
      function(e) e$source %in% members || e$target %in% members, evidence)

    to_outcome <- Filter(
      function(e) e$source %in% members && identical(e$target, outcome_name),
      evidence)

    scores <- vapply(involved, function(e) e$evidence_score, numeric(1))

    levels <- if (length(involved) == 0) integer(0) else
      vapply(involved, function(e) e$level, integer(1))

    data.frame(
      block = b,
      features_analysed = length(members),
      relationships = length(involved),
      direct_to_outcome = length(to_outcome),
      total_evidence = round(sum(scores), 1),

      # Shares add to 100 because cross-block relationships are split.
      share_percent = round(100 * credit[[b]] / total, 1),

      mean_score = if (length(scores) == 0) NA_real_ else round(mean(scores), 2),
      best_score = if (length(scores) == 0) NA_real_ else round(max(scores), 2),
      highest_level = if (length(levels) == 0) NA_character_ else
        .level_label(max(levels)),
      temporal = sum(vapply(involved, function(e) isTRUE(e$temporal),
                            logical(1))),
      stringsAsFactors = FALSE
    )

  })

  out <- do.call(rbind, rows)
  out <- out[order(-out$share_percent), ]
  rownames(out) <- NULL

  out

}

#' Per-block summary across the dimensions the engine measures
#'
#' Lets two blocks be compared on the same footing: one may predict well
#' while another carries the relationships with any claim to temporality.
#'
#' @noRd
.block_scores <- function(all_edges, feature_block, outcome_name) {

  blocks <- unique(unname(feature_block))

  if (length(blocks) == 0 || length(all_edges) == 0) return(data.frame())

  scale01 <- function(v) {
    v <- v[is.finite(v)]
    if (length(v) == 0) return(NA_real_)
    m <- mean(abs(v))
    round(m / (1 + m), 3)
  }

  rows <- lapply(blocks, function(b) {

    members <- names(feature_block)[feature_block == b]

    mine <- Filter(function(e) e$source %in% members &&
                     identical(e$target, outcome_name), all_edges)

    if (length(mine) == 0) {
      return(data.frame(block = b, predictive = NA_real_,
                        associational = NA_real_, temporal = NA_real_,
                        mechanistic = NA_real_, stringsAsFactors = FALSE))
    }

    levels <- vapply(mine, function(e) .evidence_level(e$generator), integer(1))
    sizes <- vapply(mine, function(e) e$effect_size, numeric(1))

    data.frame(
      block = b,
      predictive = scale01(sizes[levels == 1L]),
      associational = scale01(sizes[levels == 2L]),
      temporal = scale01(sizes[levels == 3L]),
      mechanistic = scale01(sizes[levels >= 4L]),
      stringsAsFactors = FALSE
    )

  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out

}

#' What each detected community is made of
#'
#' "Community 3" tells a reader nothing. "80% proteins, 15% metabolites" tells
#' them whether the engine found a module inside one layer or a genuinely
#' mixed one, which is the interesting case.
#'
#' @noRd
.community_composition <- function(graph, outcome_name) {

  if (length(graph$communities$membership) == 0 || nrow(graph$nodes) == 0) {
    return(data.frame())
  }

  membership <- graph$communities$membership
  nodes <- graph$nodes

  nodes$community <- as.integer(membership[nodes$name])
  nodes <- nodes[!is.na(nodes$community), , drop = FALSE]

  if (nrow(nodes) == 0) return(data.frame())

  rows <- lapply(sort(unique(nodes$community)), function(k) {

    members <- nodes[nodes$community == k, , drop = FALSE]

    composition <- sort(table(members$block), decreasing = TRUE)
    percent <- round(100 * as.integer(composition) / nrow(members))

    blocks_present <- setdiff(names(composition), "outcome")

    data.frame(
      community = k,
      size = nrow(members),
      composition = paste(sprintf("%d%% %s", percent, names(composition)),
                          collapse = ", "),
      blocks = length(blocks_present),
      kind = if (length(blocks_present) <= 1) "single layer" else
        if (percent[1] >= 80) "dominated by one layer" else "mixed layers",
      contains_outcome = "outcome" %in% names(composition),
      members = paste(utils::head(members$name, 8), collapse = ", "),
      stringsAsFactors = FALSE
    )

  })

  out <- do.call(rbind, rows)
  out <- out[order(-out$size), ]
  rownames(out) <- NULL

  out

}

#' The same graph, with whole blocks as the nodes
#'
#' Small enough to take in at a glance, which the variable-level graph
#' usually is not.
#'
#' @noRd
.block_graph <- function(block_evidence, block_importance) {

  if (nrow(block_evidence) == 0) {
    return(list(nodes = data.frame(), edges = data.frame(), igraph = NULL))
  }

  edges <- block_evidence[block_evidence$from != block_evidence$to, , drop = FALSE]

  nodes <- data.frame(
    name = unique(c(block_evidence$from, block_evidence$to)),
    stringsAsFactors = FALSE
  )

  nodes$share_percent <- vapply(nodes$name, function(b) {
    hit <- block_importance$share_percent[block_importance$block == b]
    if (length(hit) == 0) NA_real_ else hit[1]
  }, numeric(1))

  nodes$internal_relationships <- vapply(nodes$name, function(b) {
    hit <- block_evidence$relationships[block_evidence$from == b &
                                          block_evidence$to == b]
    if (length(hit) == 0) 0L else as.integer(hit[1])
  }, integer(1))

  g <- NULL

  if (nrow(edges) > 0 && requireNamespace("igraph", quietly = TRUE)) {

    g <- .safe_try(
      igraph::graph_from_data_frame(
        edges[, c("from", "to", "mean_score")],
        directed = TRUE, vertices = nodes["name"]),
      NULL
    )

  }

  list(nodes = nodes, edges = edges, igraph = g)

}

# =============================================================================
# The whole result in one figure
# =============================================================================
#
# Every other plot shows one facet. This shows the shape of the answer:
# which layers were measured, how big each is, where the relationships run,
# and how much they are worth. Blocks become sectors around the rim, features
# become ticks inside them, and every relationship becomes a chord.
#
# Drawn in base graphics like the rest of the package, so it costs no new
# dependency and works wherever R does.
# =============================================================================

#' Points along a circular arc
#' @noRd
.arc_xy <- function(from, to, radius, n = 60) {

  theta <- seq(from, to, length.out = max(n, 2))

  cbind(radius * cos(theta), radius * sin(theta))

}

#' A chord between two angles, bowed towards the centre
#'
#' A straight line would cross the disc and make a dense graph unreadable.
#' The control point sits nearer the centre the further apart the two ends
#' are, so neighbours get a shallow arc and opposites a deep one, which is
#' what makes the bundle legible.
#'
#' @noRd
.chord_xy <- function(a1, a2, radius, n = 80) {

  separation <- abs(a1 - a2)
  separation <- min(separation, 2 * pi - separation)

  pull <- 1 - separation / pi

  p0 <- c(radius * cos(a1), radius * sin(a1))
  p2 <- c(radius * cos(a2), radius * sin(a2))

  midpoint <- (a1 + a2) / 2

  # Averaging angles fails across the wrap-around, so the control point is
  # placed on the bisector that actually lies between the two ends.
  if (abs(a1 - a2) > pi) midpoint <- midpoint + pi

  p1 <- c(radius * pull * cos(midpoint), radius * pull * sin(midpoint))

  t <- seq(0, 1, length.out = n)

  cbind(
    (1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
    (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2]
  )

}

#' Lay every node out around the circle, grouped by block
#'
#' Sector size follows how many features a block contributed, so the figure
#' shows at a glance that a result rests on four hundred microbial taxa and
#' two clinical measurements.
#'
#' @noRd
.circos_layout <- function(nodes, outcome_name, gap = 0.04) {

  # The outcome gets its own sector, placed last so it reads as the
  # destination rather than as one more layer.
  nodes$sector <- ifelse(nodes$name == outcome_name, "outcome", nodes$block)

  sectors <- setdiff(unique(nodes$sector), "outcome")
  sectors <- c(sort(sectors), if ("outcome" %in% nodes$sector) "outcome")

  counts <- vapply(sectors, function(s) sum(nodes$sector == s), integer(1))

  # A sector with one node still needs room for its label.
  weights <- pmax(counts, 2)

  total_gap <- gap * length(sectors)
  span <- (2 * pi - total_gap) * weights / sum(weights)

  starts <- numeric(length(sectors))
  cursor <- pi / 2

  for (i in seq_along(sectors)) {
    starts[i] <- cursor
    cursor <- cursor - span[i] - gap
  }

  names(starts) <- sectors
  names(span) <- sectors

  angle <- numeric(nrow(nodes))

  for (s in sectors) {

    idx <- which(nodes$sector == s)

    if (length(idx) == 0) next

    # Strongest first within the sector, so the eye finds them together.
    idx <- idx[order(-nodes$evidence[idx])]

    inner <- seq(0.08, 0.92, length.out = length(idx))

    angle[idx] <- starts[[s]] - inner * span[[s]]

  }

  nodes$angle <- angle

  list(
    nodes = nodes,
    sectors = sectors,
    start = starts,
    span = span,
    counts = counts
  )

}

#' Each module beside the members it stands for
#'
#' A bar of module-level estimates would show the summaries and hide what
#' they are summaries of. The question a reader has about a module is whether
#' it represents its members or averages over a disagreement among them, and
#' that is answerable only by drawing both.
#'
#' @param modules A \code{ModuleGraph}.
#' @param evidence The integrated edges, for the members' own estimates.
#' @param outcome_name Name of the outcome.
#'
#' @return A recorded plot, or NULL when there is nothing to draw.
#' @noRd
.plot_modules <- function(modules, evidence, outcome_name) {

  if (!is.data.frame(modules$edges) || nrow(modules$edges) == 0) return(NULL)

  member_estimate <- stats::setNames(
    vapply(evidence, function(e) .report_or(e$estimate, NA_real_), numeric(1)),
    vapply(evidence, function(e) if (identical(e$target, outcome_name))
      e$source else NA_character_, character(1))
  )

  member_estimate <- member_estimate[!is.na(names(member_estimate))]

  edges <- modules$edges[order(modules$edges$estimate), , drop = FALSE]

  members_of <- lapply(edges$module, function(m) {
    k <- as.integer(sub("^module_", "", m))
    names(modules$membership)[which(modules$membership == k)]
  })

  values <- lapply(members_of, function(f) {
    v <- member_estimate[intersect(f, names(member_estimate))]
    v[is.finite(v)]
  })

  span <- range(c(edges$ci_lower, edges$ci_upper, unlist(values), 0),
                na.rm = TRUE)

  if (!all(is.finite(span))) return(NULL)

  .safe_record(function() {

    graphics::par(mar = c(5.8, 8, 3, 14))

    y <- seq_len(nrow(edges))

    graphics::plot(
      NA, xlim = span, ylim = c(0.5, nrow(edges) + 0.5),
      xlab = sprintf("Change in %s per standard deviation", outcome_name),
      ylab = "", yaxt = "n",
      main = "Each module and the features it summarises"
    )

    graphics::abline(v = 0, col = "#BBBBBB", lty = 2)

    graphics::axis(2, at = y, labels = edges$module, las = 1, cex.axis = 0.8)

    for (i in y) {

      # Members first, so the module marker sits on top of its own evidence
      # rather than behind it.
      if (length(values[[i]]) > 0) {
        graphics::points(values[[i]], rep(i, length(values[[i]])),
                         pch = 19, cex = 0.7,
                         col = grDevices::adjustcolor("#999999", 0.7))
      }

      graphics::segments(edges$ci_lower[i], i, edges$ci_upper[i], i,
                         col = "#4C72B0", lwd = 2)

      graphics::points(edges$estimate[i], i, pch = 23, cex = 1.4, lwd = 2,
                       bg = if (isTRUE(edges$coherent[i])) "#4C72B0" else "white",
                       col = "#4C72B0")

      graphics::text(
        graphics::par("usr")[2], i,
        sprintf("  %d feature(s), %s%s", edges$size[i],
                if (is.finite(edges$variance_explained[i]))
                  sprintf("%.0f%% captured", 100 * edges$variance_explained[i])
                else "?",
                if (isTRUE(edges$cross_block[i])) ", crosses blocks" else ""),
        adj = 0, xpd = NA, cex = 0.62, col = "#555555")

    }

    graphics::mtext(
      paste("Diamond is the module; grey dots are its members tested one at a",
            "time. A hollow diamond did not cohere."),
      side = 1, line = 4.4, cex = 0.68, col = "#555555")

  })

}

#' Where each relationship landed across the resamples
#'
#' A bar chart of recurrence would say a relationship was found forty times
#' out of fifty and stop there. What a reader needs is whether it was near
#' the top every time or wandered from second to thirtieth, because those
#' produce the same count and completely different reports.
#'
#' Each relationship is one horizontal line spanning its best to its worst
#' rank, with a dot at the median. A short line near the left is a finding
#' that would have been reported whoever happened to be sampled; a long line
#' is one that depended on it.
#'
#' @param consensus A \code{ConsensusGraph}.
#' @param outcome_name Name of the outcome, used only in the title.
#' @param max_edges How many relationships to draw, most consistent first.
#'
#' @return A recorded plot, or NULL when there is nothing to draw.
#' @noRd
.plot_consensus <- function(consensus, outcome_name, max_edges = 25L) {

  edges <- consensus$edges

  if (!is.data.frame(edges) || nrow(edges) == 0) return(NULL)

  edges <- edges[is.finite(edges$median_rank), , drop = FALSE]

  if (nrow(edges) == 0) return(NULL)

  edges <- utils::head(edges, max_edges)

  # Drawn worst-first so the most consistent relationship ends up at the top
  # of the figure, where a reader looks first.
  edges <- edges[rev(seq_len(nrow(edges))), , drop = FALSE]

  labels <- paste(edges$source, "->", edges$target)

  .safe_record(function() {

    graphics::par(mar = c(5.8, max(8, 0.55 * max(nchar(labels))), 3, 6))

    y <- seq_len(nrow(edges))

    graphics::plot(
      NA, xlim = c(0.5, max(edges$worst_rank) + 0.5),
      ylim = c(0.5, nrow(edges) + 0.5),
      xlab = "Rank within the replicate", ylab = "", yaxt = "n",
      main = sprintf("Where each relationship with %s landed, across %d resamples",
                     outcome_name, consensus$replicates)
    )

    graphics::axis(2, at = y, labels = labels, las = 1, cex.axis = 0.7)

    graphics::abline(v = seq_len(max(edges$worst_rank)),
                     col = "#EEEEEE", lwd = 0.5)

    for (i in y) {

      # Colour carries consistency, so a long line that is also pale reads as
      # doubly unreliable rather than needing two glances.
      shade <- grDevices::adjustcolor(
        "#4C72B0", max(0.15, edges$consistent_frequency[i]))

      graphics::segments(edges$best_rank[i], i, edges$worst_rank[i], i,
                         col = shade, lwd = 4, lend = 1)

      graphics::points(edges$median_rank[i], i, pch = 21, bg = "white",
                       col = shade, cex = 1.1, lwd = 2)

      graphics::text(
        graphics::par("usr")[2], i,
        sprintf(" %.0f%%", 100 * edges$consistent_frequency[i]),
        adj = 0, xpd = NA, cex = 0.65, col = "#555555")

    }

    graphics::mtext("seen with a consistent sign", side = 4, line = 4.2,
                    cex = 0.7, col = "#555555")

    graphics::mtext(
      sprintf("Line spans best to worst rank; dot is the median. %d in the consensus at %.0f%%.",
              nrow(consensus$consensus), 100 * consensus$threshold),
      side = 1, line = 4.4, cex = 0.7, col = "#555555")

  })

}

#' One figure showing blocks, features and every relationship between them
#'
#' @param graph An \code{EvidenceGraph}.
#' @param outcome_name Name of the outcome.
#' @param max_chords Chords drawn, strongest first. Beyond a few hundred the
#'   figure stops being readable and starts being decoration.
#' @param label_top How many feature names to print. Every sector is always
#'   labelled.
#'
#' @return A recorded plot, or NULL when there is nothing to draw.
#' @noRd
.plot_circos <- function(graph, outcome_name, max_chords = 150,
                         label_top = 25) {

  if (nrow(graph$edges) == 0 || nrow(graph$nodes) < 3) return(NULL)

  layout <- .circos_layout(graph$nodes, outcome_name)

  nodes <- layout$nodes
  angle_of <- stats::setNames(nodes$angle, nodes$name)

  palette <- grDevices::hcl.colors(max(length(layout$sectors), 3), "Dark 3")
  names(palette) <- layout$sectors

  if ("outcome" %in% layout$sectors) palette[["outcome"]] <- "#333333"

  edges <- graph$edges[order(graph$edges$evidence_score), , drop = FALSE]

  if (nrow(edges) > max_chords) {
    edges <- utils::tail(edges, max_chords)
  }

  strongest <- max(edges$evidence_score, na.rm = TRUE)

  if (!is.finite(strongest) || strongest <= 0) return(NULL)

  .safe_record(function() {

    graphics::par(mar = c(0.5, 0.5, 2.5, 0.5))

    # Room for the outermost labels, which sit beyond the sector arcs.
    graphics::plot(
      NA, xlim = c(-1.62, 1.62), ylim = c(-1.5, 1.5),
      asp = 1, axes = FALSE, xlab = "", ylab = "",
      main = "Every measured layer and the evidence between them"
    )

    # --- chords, weakest first so the strong ones end up on top ------------

    for (i in seq_len(nrow(edges))) {

      a1 <- angle_of[[edges$source[i]]]
      a2 <- angle_of[[edges$target[i]]]

      if (is.null(a1) || is.null(a2) || is.na(a1) || is.na(a2)) next

      weight <- edges$evidence_score[i] / strongest

      block <- nodes$sector[nodes$name == edges$source[i]][1]
      colour <- palette[[block]]

      curve <- .chord_xy(a1, a2, 0.98)

      graphics::lines(
        curve,
        col = grDevices::adjustcolor(colour, alpha.f = 0.15 + 0.55 * weight),
        lwd = 0.6 + 3.4 * weight
      )

    }

    # --- sector arcs -------------------------------------------------------

    for (s in layout$sectors) {

      arc <- .arc_xy(layout$start[[s]], layout$start[[s]] - layout$span[[s]],
                     1.06)

      graphics::lines(arc, col = palette[[s]], lwd = 7, lend = 1)

      middle <- layout$start[[s]] - layout$span[[s]] / 2

      label <- if (identical(s, "outcome")) outcome_name else s

      graphics::text(
        1.20 * cos(middle), 1.20 * sin(middle),
        sprintf("%s\n%d", label, layout$counts[[s]]),
        col = palette[[s]], cex = 0.72, font = 2,
        srt = 0, adj = c(if (cos(middle) < -0.3) 1 else
          if (cos(middle) > 0.3) 0 else 0.5, 0.5)
      )

    }

    # --- feature ticks -----------------------------------------------------

    for (i in seq_len(nrow(nodes))) {

      a <- nodes$angle[i]

      if (is.na(a)) next

      weight <- if (is.finite(nodes$evidence[i]))
        nodes$evidence[i] / max(nodes$evidence, na.rm = TRUE) else 0

      graphics::segments(
        0.99 * cos(a), 0.99 * sin(a),
        (1.01 + 0.03 * weight) * cos(a), (1.01 + 0.03 * weight) * sin(a),
        col = palette[[nodes$sector[i]]], lwd = 1.4
      )

    }

    # --- names, for the ones worth naming ----------------------------------

    # The outcome already has its sector labelled; naming its tick as well
    # prints it twice, once rotated over the other.
    named <- nodes[nodes$name != outcome_name, , drop = FALSE]
    named <- named[order(-named$evidence), ]
    named <- utils::head(named, label_top)

    for (i in seq_len(nrow(named))) {

      a <- named$angle[i]

      if (is.na(a)) next

      graphics::text(
        1.10 * cos(a), 1.10 * sin(a), named$name[i],
        cex = 0.5, col = "#4a4a4a",
        srt = (a * 180 / pi) %% 360 - if (cos(a) < 0) 180 else 0,
        adj = if (cos(a) < 0) 1 else 0
      )

    }

    graphics::text(0, -1.36,
                   sprintf(paste("%d relationships drawn, thickest carries",
                                 "the most evidence"), nrow(edges)),
                   cex = 0.62, col = "#6b7280")

  })

}

#' The same figure with whole blocks as the only nodes
#'
#' Small enough to take in at a glance, which the feature-level version stops
#' being once a block contributes more than a few dozen columns.
#'
#' @noRd
.plot_circos_blocks <- function(block_evidence, outcome_name) {

  if (!is.data.frame(block_evidence) || nrow(block_evidence) == 0) return(NULL)

  sectors <- unique(c(block_evidence$from, block_evidence$to))
  sectors <- c(sort(setdiff(sectors, "outcome")),
               if ("outcome" %in% sectors) "outcome")

  if (length(sectors) < 2) return(NULL)

  weight <- vapply(sectors, function(s) {
    sum(block_evidence$total_evidence[block_evidence$from == s |
                                        block_evidence$to == s])
  }, numeric(1))

  weight[!is.finite(weight) | weight <= 0] <- 1

  gap <- 0.10
  span <- (2 * pi - gap * length(sectors)) * weight / sum(weight)

  starts <- numeric(length(sectors))
  cursor <- pi / 2

  for (i in seq_along(sectors)) {
    starts[i] <- cursor
    cursor <- cursor - span[i] - gap
  }

  names(starts) <- sectors
  names(span) <- sectors

  centre <- starts - span / 2

  palette <- grDevices::hcl.colors(max(length(sectors), 3), "Dark 3")
  names(palette) <- sectors

  if ("outcome" %in% sectors) palette[["outcome"]] <- "#333333"

  crossing <- block_evidence[block_evidence$from != block_evidence$to, ,
                             drop = FALSE]

  if (nrow(crossing) == 0) return(NULL)

  strongest <- max(crossing$total_evidence)

  .safe_record(function() {

    graphics::par(mar = c(0.5, 0.5, 2.5, 0.5))

    graphics::plot(
      NA, xlim = c(-1.7, 1.7), ylim = c(-1.55, 1.55),
      asp = 1, axes = FALSE, xlab = "", ylab = "",
      main = "Evidence between measurement layers"
    )

    for (i in order(crossing$total_evidence)) {

      a1 <- centre[[crossing$from[i]]]
      a2 <- centre[[crossing$to[i]]]

      weight_i <- crossing$total_evidence[i] / strongest

      graphics::lines(
        .chord_xy(a1, a2, 0.94),
        col = grDevices::adjustcolor(palette[[crossing$from[i]]],
                                      alpha.f = 0.25 + 0.5 * weight_i),
        lwd = 1 + 11 * weight_i
      )

    }

    # A narrow sector leaves no angular room for its label, so two small ones
    # side by side collide. Their labels are pushed out alternately.
    narrow <- span < 0.35
    stagger <- cumsum(narrow) %% 2 == 1 & narrow

    for (i in seq_along(sectors)) {

      s <- sectors[i]

      graphics::lines(
        .arc_xy(starts[[s]], starts[[s]] - span[[s]], 1.02),
        col = palette[[s]], lwd = 13, lend = 1
      )

      internal <- block_evidence$relationships[block_evidence$from == s &
                                                 block_evidence$to == s]

      label <- if (identical(s, "outcome")) outcome_name else s

      radius <- if (stagger[i]) 1.34 else 1.20

      if (stagger[i]) {
        graphics::segments(1.06 * cos(centre[[s]]), 1.06 * sin(centre[[s]]),
                           (radius - 0.06) * cos(centre[[s]]),
                           (radius - 0.06) * sin(centre[[s]]),
                           col = palette[[s]], lwd = 1)
      }

      graphics::text(
        radius * cos(centre[[s]]), radius * sin(centre[[s]]),
        if (length(internal) > 0 && internal[1] > 0)
          sprintf("%s\n(%d within)", label, internal[1]) else label,
        col = palette[[s]], cex = 0.8, font = 2,
        adj = c(if (cos(centre[[s]]) < -0.3) 1 else
          if (cos(centre[[s]]) > 0.3) 0 else 0.5, 0.5)
      )

    }

    graphics::text(0, -1.38,
                   "Arc width is total evidence; chord width is the evidence between two layers",
                   cex = 0.62, col = "#6b7280")

  })

}

#' Check what a causal structure implies about an adjustment
#'
#' Answers, before any model is fitted, the question that decides whether an
#' estimate means anything: given this causal structure, does adjusting for
#' these variables identify the effect of this exposure on this outcome?
#'
#' @section Why this is separate from the data:
#'
#' A DAG is a claim about how the world works, not something recoverable from
#' a correlation matrix. Several different structures fit the same data
#' equally well, so the engine cannot derive one and will not pretend to. What
#' it can do is take the structure you are willing to defend and tell you what
#' follows from it.
#'
#' What follows is often uncomfortable. Adjusting for a mediator removes part
#' of the effect being measured. Adjusting for a collider opens a path that
#' was closed and creates an association out of nothing, so the adjusted
#' estimate is worse than the unadjusted one. Both look like diligence and
#' both are damage.
#'
#' @param dag A \code{dagitty} object, a dagitty specification string, or a
#'   data.frame with \code{from} and \code{to} columns.
#' @param exposure,outcome Variable names.
#' @param adjusted What you intend to condition on.
#'
#' @return A \code{CMODagCheck} object, printed as a verdict with reasons.
#'
#' @examples
#' structure <- data.frame(
#'   from = c("age", "age", "protein", "inflammation"),
#'   to   = c("protein", "disease", "inflammation", "disease")
#' )
#'
#' # Adjusting for age closes the backdoor path.
#' check_dag(structure, "protein", "disease", adjusted = "age")
#'
#' # Adjusting for the mediator removes the effect being measured.
#' check_dag(structure, "protein", "disease",
#'           adjusted = c("age", "inflammation"))
#'
#' @export
check_dag <- function(dag, exposure, outcome, adjusted = character()) {

  parsed <- .parse_dag(dag)

  if (is.null(parsed)) {
    stop("No usable causal structure was supplied.", call. = FALSE)
  }

  audit <- .dag_audit(parsed, exposure, outcome, adjusted)

  structure(
    c(audit, list(exposure = exposure, outcome = outcome,
                  adjusted = adjusted, dag = parsed,
                  nodes = names(parsed))),
    class = "CMODagCheck"
  )

}

#' Print the verdict on an adjustment
#'
#' @param x A \code{CMODagCheck}.
#' @param ... Ignored.
#'
#' @return The object, invisibly.
#'
#' @export
print.CMODagCheck <- function(x, ...) {

  cat("\n")
  cat(sprintf("Effect of %s on %s\n", x$exposure, x$outcome))
  cat(strrep("=", 60), "\n", sep = "")

  cat(sprintf("\nAdjusted for : %s\n",
              if (length(x$adjusted) > 0) paste(x$adjusted, collapse = ", ")
              else "nothing"))

  verdict <- if (is.na(x$identifiable)) "cannot be judged" else
    if (isTRUE(x$identifiable)) "IDENTIFIED" else "NOT identified"

  cat(sprintf("Verdict      : %s\n\n", verdict))

  if (length(x$reason) > 0) {
    cat(paste(strwrap(x$reason, width = 60, prefix = "  "),
              collapse = "\n"), "\n", sep = "")
  }

  if (length(x$problems) > 0) {

    cat("\nProblems with the adjustment\n")
    cat(strrep("-", 60), "\n", sep = "")

    for (p in x$problems) {
      cat(paste(strwrap(p, width = 62, prefix = "    ", initial = "  - "),
                collapse = "\n"), "\n", sep = "")
    }

  }

  if (length(x$required) > 0) {

    cat("\nA sufficient adjustment set\n")
    cat(strrep("-", 60), "\n", sep = "")
    cat("  ", paste(x$required, collapse = ", "), "\n", sep = "")

    missing <- setdiff(x$required, x$adjusted)

    if (length(missing) > 0) {
      cat("  missing from yours: ", paste(missing, collapse = ", "), "\n",
          sep = "")
    }

  }

  if (isTRUE(x$identifiable)) {
    cat("\n  This holds only if the structure you supplied is correct.\n")
    cat("  The data cannot confirm it.\n")
  }

  cat("\n")

  invisible(x)

}
# =============================================================================
# End of analysis.R
# =============================================================================
