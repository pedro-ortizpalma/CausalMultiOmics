# =============================================================================
# preprocessing.R
# Preprocessing engine: method registry and recipe execution
# =============================================================================
#
# This file holds the two halves of one feature.
#
# The first half is the method registry. Every preprocessing method is a
# pair of functions:
#
#   fit(x, params, context) -> model
#   apply(x, model)         -> x
#
# `fit` looks at the data and returns everything needed to reproduce the
# operation later; `apply` performs it. preprocess() calls fit then apply,
# apply_preprocessing() calls apply alone against the stored model.
# Splitting them this way is what makes a pipeline replayable on an external
# dataset: a method that cannot express its decision as a model cannot be
# part of a reproducible pipeline, and the split makes that impossible to
# fake.
#
# `x` is always a data.frame block. Non-numeric columns pass through
# untouched unless the method explicitly handles them. Row and column names
# are preserved.
#
# The second half is the orchestrator. preprocess() decides nothing on its
# own: every method, parameter and threshold it applies was already chosen
# by check_data() and written into a PreprocessingRecipe. It only executes
# that plan, in the order the recipe declares, and records what happened.
#
# =============================================================================

# =============================================================================
# Registry
# =============================================================================

#' Build the preprocessing method registry
#'
#' Returns a nested list \code{registry[[stage]][[method]]}, each entry a list
#' with \code{fit}, \code{apply}, an optional \code{requires} vector of
#' package names, and a human-readable \code{label}.
#'
#' Adding a method to the engine means adding an entry here; nothing else in
#' the pipeline needs to change.
#'
#' @return A nested list of method definitions.
#' @keywords internal

.method_registry <- function() {

  list(

    imputation = .imputation_methods(),
    transformation = .transformation_methods(),
    normalization = .normalization_methods(),
    scaling = .scaling_methods(),
    batch = .batch_methods(),
    feature_selection = .feature_selection_methods()

  )

}

#' Look a method up, with a useful error when it is missing
#' @keywords internal

.get_method <- function(stage, method) {

  registry <- .method_registry()

  if (!(stage %in% names(registry))) {

    stop(sprintf("Unknown preprocessing stage '%s'.", stage), call. = FALSE)

  }

  methods <- registry[[stage]]

  if (!(method %in% names(methods))) {

    stop(
      sprintf(
        "Unknown %s method '%s'.\n  Available: %s",
        stage, method, paste(sort(names(methods)), collapse = ", ")
      ),
      call. = FALSE
    )

  }

  entry <- methods[[method]]

  missing_pkgs <- entry$requires[
    !vapply(entry$requires, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {

    stop(
      sprintf(
        "The %s method '%s' needs package(s) %s, which are not installed.",
        stage, method, paste(sprintf("'%s'", missing_pkgs), collapse = ", ")
      ),
      call. = FALSE
    )

  }

  entry

}

#' List everything the engine can execute
#'
#' @return A data.frame with one row per registered method.
#' @keywords internal

.available_methods <- function() {

  registry <- .method_registry()

  do.call(rbind, lapply(names(registry), function(stage) {

    methods <- registry[[stage]]

    data.frame(
      stage = stage,
      method = names(methods),
      label = vapply(methods, function(m) m$label, character(1)),
      requires = vapply(
        methods,
        function(m) paste(m$requires, collapse = ", "),
        character(1)
      ),
      stringsAsFactors = FALSE
    )

  }))

}

#' Declare a method that is recognised but cannot be replayed
#'
#' Some well-known methods do not fit the fit/apply contract: they produce a
#' completed dataset rather than a transferable model. Registering them with
#' an explanatory error is better than leaving them out, because the user
#' gets told why instead of "unknown method".
#'
#' @keywords internal

.unsupported_method <- function(label, reason, alternatives) {

  list(
    label = label,
    requires = character(),
    fit = function(x, params, context) {
      stop(
        sprintf("%s\n  Use one of: %s.", reason,
                paste(alternatives, collapse = ", ")),
        call. = FALSE
      )
    },
    apply = function(x, model) x
  )

}

# =============================================================================
# Shared helpers
# =============================================================================

#' Numeric columns of a block, as a matrix
#' @keywords internal
.stage_matrix <- function(x, cols) {

  as.matrix(x[, cols, drop = FALSE])

}

#' Write a numeric matrix back into the columns it came from
#' @keywords internal
.stage_restore <- function(x, m, cols) {

  for (j in seq_along(cols)) x[[cols[j]]] <- m[, j]

  x

}

#' Column-wise summary that tolerates all-missing columns
#' @keywords internal
.col_stat <- function(m, fun, fallback = 0) {

  out <- apply(m, 2, function(col) {

    col <- col[is.finite(col)]

    if (length(col) == 0) return(fallback)

    value <- fun(col)

    if (!is.finite(value)) fallback else value

  })

  stats::setNames(as.numeric(out), colnames(m))

}

#' Positive row totals, guarding against empty or zero-sum samples
#' @keywords internal
.row_totals <- function(m) {

  totals <- rowSums(m, na.rm = TRUE)

  totals[!is.finite(totals) | totals <= 0] <- 1

  totals

}

#' Reference distribution used to map new values onto training ranks
#' @keywords internal
.rank_reference <- function(x) {

  x <- sort(x[is.finite(x)])

  if (length(x) == 0) NA_real_ else x

}

#' Position of each value within a stored reference distribution
#'
#' Returns a probability in (0, 1). This is what lets rank- and
#' quantile-based transformations be applied to data the model never saw:
#' a new value is placed against the training distribution rather than
#' re-ranked among its own peers.
#'
#' @keywords internal
.rank_position <- function(x, reference) {

  n <- length(reference)

  if (n == 0 || all(is.na(reference))) return(rep(NA_real_, length(x)))

  p <- (findInterval(x, reference, left.open = FALSE) - 0.5) / n

  p[p <= 0] <- 0.5 / n
  p[p >= 1] <- 1 - 0.5 / n

  p[is.na(x)] <- NA_real_

  p

}

# =============================================================================
# Imputation
# =============================================================================

#' Build the imputation method registry
#'
#' @keywords internal
#' @noRd
.imputation_methods <- function() {

  # Centre-based imputation: mean, median and mode differ only in the
  # statistic, so they share one implementation.

  centre_method <- function(label, stat, columns = "numeric") {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {

        cols <- if (identical(columns, "numeric")) {
          .numeric_columns(x)
        } else {
          names(x)
        }

        fills <- lapply(x[cols], function(col) stat(col))
        names(fills) <- cols

        list(method = label, columns = cols, fills = fills)

      },

      apply = function(x, model) {

        for (v in model$columns) {

          if (!(v %in% names(x))) next

          fill <- model$fills[[v]]

          if (is.null(fill) || (length(fill) == 1 && is.na(fill))) next

          na <- is.na(x[[v]])

          if (any(na)) x[[v]][na] <- fill

        }

        x

      }

    )

  }

  numeric_stat <- function(fun) {

    function(col) {

      if (!is.numeric(col)) return(NA)

      col <- col[is.finite(col)]

      if (length(col) == 0) return(NA_real_)

      fun(col)

    }

  }

  mode_stat <- function(col) {

    col <- col[!is.na(col)]

    if (length(col) == 0) return(NA)

    tab <- table(as.character(col))
    winner <- names(tab)[which.max(tab)]

    if (is.numeric(col)) return(as.numeric(winner))
    if (is.factor(col)) return(factor(winner, levels = levels(col)))

    winner

  }

  list(

    none = list(
      label = "none",
      requires = character(),
      fit = function(x, params, context) list(method = "none"),
      apply = function(x, model) x
    ),

    mean = centre_method("mean", numeric_stat(mean)),

    median = centre_method("median", numeric_stat(stats::median)),

    mode = centre_method("mode", mode_stat, columns = "all"),

    pseudocount = list(

      label = "pseudocount",
      requires = character(),

      fit = function(x, params, context) {

        value <- if (is.null(params$pseudocount)) 1 else params$pseudocount

        list(method = "pseudocount", columns = .numeric_columns(x),
             value = value)

      },

      apply = function(x, model) {

        for (v in model$columns) {

          if (!(v %in% names(x))) next

          na <- is.na(x[[v]])

          if (any(na)) x[[v]][na] <- model$value

        }

        x

      }

    ),

    knn = list(

      label = "knn",
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)
        m <- .stage_matrix(x, cols)

        # The training matrix is the model: neighbours for a new sample have
        # to be looked for among the samples the pipeline was fitted on, not
        # among the new samples themselves.

        list(
          method = "knn",
          columns = cols,
          k = if (is.null(params$k)) 5 else params$k,
          reference = m,
          fallback = .col_stat(m, stats::median, NA_real_)
        )

      },

      apply = function(x, model) {

        cols <- intersect(model$columns, names(x))

        if (length(cols) == 0) return(x)

        m <- .stage_matrix(x, cols)
        ref <- model$reference[, cols, drop = FALSE]

        incomplete <- which(apply(m, 1, function(r) any(is.na(r))))

        for (i in incomplete) {

          target <- m[i, ]
          missing_cols <- which(is.na(target))
          observed <- which(!is.na(target))

          filled <- FALSE

          if (length(observed) > 0) {

            # Distance over the features both rows actually observed, scaled
            # so that samples sharing few features are not favoured.

            d <- apply(ref[, observed, drop = FALSE], 1, function(r) {

              shared <- !is.na(r)

              if (!any(shared)) return(NA_real_)

              sqrt(sum((r[shared] - target[observed][shared])^2) / sum(shared))

            })

            usable <- which(is.finite(d))

            if (length(usable) > 0) {

              k <- min(model$k, length(usable))
              nearest <- usable[order(d[usable])[seq_len(k)]]

              for (j in missing_cols) {

                values <- ref[nearest, j]
                values <- values[is.finite(values)]

                if (length(values) > 0) {
                  m[i, j] <- mean(values)
                  filled <- TRUE
                }

              }

            }

          }

          # No usable neighbour: fall back to the training column median.

          still_missing <- which(is.na(m[i, ]))

          if (length(still_missing) > 0) {
            m[i, still_missing] <- model$fallback[cols][still_missing]
          }

          invisible(filled)

        }

        .stage_restore(x, m, cols)

      }

    ),

    mice = .unsupported_method(
      "mice",
      paste("Multiple imputation draws several completed datasets rather",
            "than fitting a model that transfers to new samples, so it",
            "cannot be replayed on an external set. It belongs downstream,",
            "in the analysis stage."),
      c("knn", "median", "mean")
    ),

    missForest = .unsupported_method(
      "missForest",
      paste("missForest imputes by iterating over the dataset it was given",
            "and keeps no model that can be applied to new samples."),
      c("knn", "median", "mean")
    )

  )

}

# =============================================================================
# Transformation
# =============================================================================

#' Build the transformation method registry
#'
#' @keywords internal
#' @noRd
.transformation_methods <- function() {

  # Stateless element-wise transforms. They need no fitted parameters, but
  # still go through the registry so the pipeline record is uniform.

  stateless <- function(label, fn) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {
        list(method = label, columns = .numeric_columns(x))
      },

      apply = function(x, model) {

        for (v in intersect(model$columns, names(x))) x[[v]] <- fn(x[[v]])

        x

      }

    )

  }

  # Log-like transforms need the shift that made the training data positive;
  # recomputing it on new data would put the two on different scales.

  shifted_log <- function(label, base) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)

        shifts <- vapply(x[cols], function(col) {

          mn <- suppressWarnings(min(col, na.rm = TRUE))

          if (!is.finite(mn) || mn > 0) 0 else -mn + 1

        }, numeric(1))

        list(method = label, columns = cols, base = base,
             shifts = stats::setNames(shifts, cols))

      },

      apply = function(x, model) {

        for (v in intersect(model$columns, names(x))) {
          x[[v]] <- log(x[[v]] + model$shifts[[v]], base = model$base)
        }

        x

      }

    )

  }

  # Per-feature location/scale transforms.

  centre_scale <- function(label, centre_fn, scale_fn) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)
        m <- .stage_matrix(x, cols)

        centre <- .col_stat(m, centre_fn, 0)
        scale <- .col_stat(m, scale_fn, 1)
        scale[!is.finite(scale) | scale <= 0] <- 1

        list(method = label, columns = cols, centre = centre, scale = scale)

      },

      apply = function(x, model) {

        for (v in intersect(model$columns, names(x))) {
          x[[v]] <- (x[[v]] - model$centre[[v]]) / model$scale[[v]]
        }

        x

      }

    )

  }

  # Rank-based transforms store the training distribution so a new value can
  # be positioned against it.

  rank_based <- function(label, convert) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)

        references <- lapply(x[cols], .rank_reference)
        names(references) <- cols

        list(method = label, columns = cols, references = references)

      },

      apply = function(x, model) {

        for (v in intersect(model$columns, names(x))) {

          reference <- model$references[[v]]
          p <- .rank_position(x[[v]], reference)

          x[[v]] <- convert(p, reference)

        }

        x

      }

    )

  }

  # Log-ratio transforms for compositional blocks. The zero replacement and
  # the basis are part of the model.

  logratio <- function(label) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)
        m <- .stage_matrix(x, cols)

        positive <- m[is.finite(m) & m > 0]
        replacement <- if (length(positive) > 0) min(positive) / 2 else 1e-6

        model <- list(method = label, columns = cols,
                      replacement = replacement)

        if (identical(label, "alr")) {
          model$reference_column <- cols[length(cols)]
        }

        if (identical(label, "ilr")) {

          d <- length(cols)

          basis <- matrix(0, nrow = d, ncol = max(d - 1, 0))

          for (i in seq_len(d - 1)) {
            basis[1:i, i] <- 1 / i
            basis[i + 1, i] <- -1
            basis[, i] <- basis[, i] * sqrt(i / (i + 1))
          }

          model$basis <- basis
          model$output_columns <- paste0("ILR", seq_len(max(d - 1, 0)))

        }

        model

      },

      apply = function(x, model) {

        cols <- intersect(model$columns, names(x))

        if (length(cols) == 0) return(x)

        m <- .stage_matrix(x, cols)
        m[!is.finite(m) | m <= 0] <- model$replacement

        geometric <- exp(rowMeans(log(m)))
        clr <- log(m / geometric)

        if (identical(model$method, "clr")) {
          return(.stage_restore(x, clr, cols))
        }

        if (identical(model$method, "alr")) {

          reference <- model$reference_column
          keep <- setdiff(cols, reference)

          alr <- log(m[, keep, drop = FALSE] / m[, reference])

          x <- .stage_restore(x, alr, keep)
          x[[reference]] <- NULL

          return(x)

        }

        # ilr
        if (ncol(clr) < 2) return(.stage_restore(x, clr, cols))

        ilr <- clr %*% model$basis
        colnames(ilr) <- model$output_columns

        for (v in cols) x[[v]] <- NULL
        for (j in seq_len(ncol(ilr))) x[[model$output_columns[j]]] <- ilr[, j]

        x

      }

    )

  }

  list(

    identity = list(
      label = "identity",
      requires = character(),
      fit = function(x, params, context) list(method = "identity"),
      apply = function(x, model) x
    ),

    # Every stage answers to "none", which is what .stage_config() falls back
    # to for an empty recipe slot. For transformations that is the identity.

    none = list(
      label = "none",
      requires = character(),
      fit = function(x, params, context) list(method = "none"),
      apply = function(x, model) x
    ),

    log = shifted_log("log", exp(1)),
    log2 = shifted_log("log2", 2),
    log10 = shifted_log("log10", 10),

    sqrt = stateless("sqrt", .t_sqrt),
    cuberoot = stateless("cuberoot", .t_cuberoot),
    vst = stateless("vst", .t_vst),

    arcsin = stateless("arcsin", function(v) asin(sqrt(pmin(pmax(v, 0), 1)))),

    robust = centre_scale("robust", stats::median, stats::mad),

    rank = rank_based("rank", function(p, reference) p * length(reference)),

    quantile = rank_based("quantile", function(p, reference) stats::qnorm(p)),

    boxcox = list(

      label = "boxcox",
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)

        shifts <- numeric(length(cols))
        lambdas <- numeric(length(cols))

        for (j in seq_along(cols)) {

          col <- x[[cols[j]]]
          col <- col[is.finite(col)]

          mn <- if (length(col) > 0) min(col) else 1
          shifts[j] <- if (mn > 0) 0 else -mn + 1

          shifted <- col + shifts[j]

          lambdas[j] <- if (length(shifted) < 3 || stats::sd(shifted) == 0) {
            1
          } else {
            .safe_try(
              stats::optimize(
                function(lambda) {
                  xt <- if (abs(lambda) < 1e-6) log(shifted) else
                    (shifted^lambda - 1) / lambda
                  -length(shifted) / 2 * log(stats::var(xt)) +
                    (lambda - 1) * sum(log(shifted))
                },
                interval = c(-2, 2), maximum = TRUE
              )$maximum,
              1
            )
          }

        }

        list(method = "boxcox", columns = cols,
             shifts = stats::setNames(shifts, cols),
             lambdas = stats::setNames(lambdas, cols))

      },

      apply = function(x, model) {

        for (v in intersect(model$columns, names(x))) {

          shifted <- x[[v]] + model$shifts[[v]]
          lambda <- model$lambdas[[v]]

          x[[v]] <- if (abs(lambda) < 1e-6) {
            log(shifted)
          } else {
            (shifted^lambda - 1) / lambda
          }

        }

        x

      }

    ),

    yeojohnson = list(

      label = "yeojohnson",
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)

        lambdas <- vapply(x[cols], function(col) {

          col <- col[is.finite(col)]

          if (length(col) < 3 || stats::sd(col) == 0) return(1)

          .safe_try(
            stats::optimize(
              function(lambda) {
                xt <- vapply(col, .yj_transform, numeric(1), lambda = lambda)
                -length(col) / 2 * log(stats::var(xt)) +
                  (lambda - 1) * sum(sign(col) * log(abs(col) + 1))
              },
              interval = c(-2, 2), maximum = TRUE
            )$maximum,
            1
          )

        }, numeric(1))

        list(method = "yeojohnson", columns = cols,
             lambdas = stats::setNames(lambdas, cols))

      },

      apply = function(x, model) {

        for (v in intersect(model$columns, names(x))) {
          x[[v]] <- vapply(x[[v]], .yj_transform, numeric(1),
                           lambda = model$lambdas[[v]])
        }

        x

      }

    ),

    clr = logratio("clr"),
    alr = logratio("alr"),
    ilr = logratio("ilr")

  )

}

# =============================================================================
# Normalization
# =============================================================================
#
# Normalization is sample-wise: it removes technical differences in overall
# magnitude between samples. Every method stores the training reference so
# that a new sample is put on the training scale rather than its own.
# =============================================================================

#' Build the normalization method registry
#'
#' @keywords internal
#' @noRd
.normalization_methods <- function() {

  # Divide each sample by a per-sample size factor, then restore the training
  # scale. `factor_fn(m, model)` returns one factor per row.

  size_factor <- function(label, fit_reference, factor_fn) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)
        m <- .stage_matrix(x, cols)

        model <- list(method = label, columns = cols)
        model <- utils::modifyList(model, fit_reference(m))

        model

      },

      apply = function(x, model) {

        cols <- intersect(model$columns, names(x))

        if (length(cols) == 0) return(x)

        m <- .stage_matrix(x, cols)

        factors <- factor_fn(m, model)
        factors[!is.finite(factors) | factors <= 0] <- 1

        .stage_restore(x, m / factors, cols)

      }

    )

  }

  list(

    none = list(
      label = "none",
      requires = character(),
      fit = function(x, params, context) list(method = "none"),
      apply = function(x, model) x
    ),

    total_sum_scaling = size_factor(
      "total_sum_scaling",
      function(m) list(target = mean(.row_totals(m))),
      function(m, model) .row_totals(m) / model$target
    ),

    tic = size_factor(
      "tic",
      function(m) list(target = mean(.row_totals(m))),
      function(m, model) .row_totals(m) / model$target
    ),

    cpm = size_factor(
      "cpm",
      function(m) list(),
      function(m, model) .row_totals(m) / 1e6
    ),

    median = size_factor(
      "median",
      function(m) {
        medians <- apply(m, 1, stats::median, na.rm = TRUE)
        medians <- medians[is.finite(medians)]
        list(target = if (length(medians) > 0) stats::median(medians) else 1)
      },
      function(m, model) {
        medians <- apply(m, 1, stats::median, na.rm = TRUE)
        medians / model$target
      }
    ),

    rle = size_factor(
      "rle",
      function(m) {
        # Median-of-ratios: the reference is the per-feature geometric mean
        # over the training samples.
        logs <- log(replace(m, !is.finite(m) | m <= 0, NA))
        list(reference = exp(colMeans(logs, na.rm = TRUE)))
      },
      function(m, model) {
        reference <- model$reference[colnames(m)]
        apply(sweep(m, 2, reference, "/"), 1, function(r) {
          r <- r[is.finite(r) & r > 0]
          if (length(r) == 0) 1 else stats::median(r)
        })
      }
    ),

    pqn = size_factor(
      "pqn",
      function(m) {
        totals <- .row_totals(m)
        scaled <- m / (totals / mean(totals))
        list(reference = .col_stat(scaled, stats::median, NA_real_),
             target = mean(totals))
      },
      function(m, model) {
        reference <- model$reference[colnames(m)]
        totals <- .row_totals(m)
        scaled <- m / (totals / model$target)
        quotients <- apply(sweep(scaled, 2, reference, "/"), 1, function(r) {
          r <- r[is.finite(r) & r > 0]
          if (length(r) == 0) 1 else stats::median(r)
        })
        (totals / model$target) * quotients
      }
    ),

    tmm = size_factor(
      "tmm",
      function(m) {
        # The reference sample is the one whose upper-quartile is closest to
        # the average; new samples are compared against that same profile.
        totals <- .row_totals(m)
        scaled <- m / totals
        quartiles <- apply(scaled, 1, stats::quantile, probs = 0.75,
                           na.rm = TRUE)
        idx <- which.min(abs(quartiles - mean(quartiles, na.rm = TRUE)))

        # target_total has to be stored: deriving it from the batch being
        # transformed would make each sample's factor depend on whichever
        # other samples happened to be passed in alongside it, which is
        # re-fitting on new data by the back door.
        list(reference = scaled[idx, ], target_total = mean(totals))
      },
      function(m, model) {

        reference <- model$reference[colnames(m)]
        totals <- .row_totals(m)
        scaled <- m / totals

        vapply(seq_len(nrow(scaled)), function(i) {

          obs <- scaled[i, ]
          usable <- is.finite(obs) & is.finite(reference) &
            obs > 0 & reference > 0

          if (sum(usable) < 4) return(1)

          logratio <- log2(obs[usable] / reference[usable])
          abundance <- (log2(obs[usable]) + log2(reference[usable])) / 2

          # Trim the extremes of both the ratio and the abundance, which is
          # what makes the estimate robust to a few dominant features.
          keep <- logratio >= stats::quantile(logratio, 0.3) &
            logratio <= stats::quantile(logratio, 0.7) &
            abundance >= stats::quantile(abundance, 0.05) &
            abundance <= stats::quantile(abundance, 0.95)

          if (!any(keep)) return(1)

          2^mean(logratio[keep])

        }, numeric(1)) * (totals / model$target_total)

      }
    ),

    quantile = list(

      label = "quantile",
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)
        m <- .stage_matrix(x, cols)

        # Reference distribution: the average sorted sample. Applying it
        # forces every sample onto the same distribution of values.

        sorted <- t(apply(m, 1, function(r) sort(r, na.last = TRUE)))
        reference <- colMeans(sorted, na.rm = TRUE)

        list(method = "quantile", columns = cols, reference = reference)

      },

      apply = function(x, model) {

        cols <- intersect(model$columns, names(x))

        if (length(cols) < 2) return(x)

        m <- .stage_matrix(x, cols)
        reference <- model$reference

        if (length(reference) != ncol(m)) {

          # Feature set changed since fitting: interpolate the stored
          # distribution onto the current width rather than refusing.
          reference <- stats::approx(
            seq_along(reference), reference,
            xout = seq(1, length(reference), length.out = ncol(m))
          )$y

        }

        for (i in seq_len(nrow(m))) {

          row <- m[i, ]
          observed <- which(!is.na(row))

          if (length(observed) == 0) next

          ranks <- rank(row[observed], ties.method = "average")

          positions <- 1 + (ranks - 1) *
            (length(reference) - 1) / max(length(observed) - 1, 1)

          m[i, observed] <- stats::approx(
            seq_along(reference), reference, xout = positions, rule = 2
          )$y

        }

        .stage_restore(x, m, cols)

      }

    ),

    css = .unsupported_method(
      "css",
      paste("Cumulative sum scaling is not implemented in base R yet."),
      c("total_sum_scaling", "rle", "tmm")
    )

  )

}

# =============================================================================
# Scaling
# =============================================================================
#
# Scaling is feature-wise: it puts variables on a comparable scale. Centre
# and scale come from the training data and are reused verbatim.
# =============================================================================

#' Build the scaling method registry
#'
#' @keywords internal
#' @noRd
.scaling_methods <- function() {

  scaler <- function(label, centre_fn, scale_fn) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)
        m <- .stage_matrix(x, cols)

        centre <- .col_stat(m, centre_fn, 0)
        scale <- .col_stat(m, function(col) scale_fn(col), 1)
        scale[!is.finite(scale) | scale <= 0] <- 1

        list(method = label, columns = cols, centre = centre, scale = scale)

      },

      apply = function(x, model) {

        for (v in intersect(model$columns, names(x))) {
          x[[v]] <- (x[[v]] - model$centre[[v]]) / model$scale[[v]]
        }

        x

      }

    )

  }

  list(

    none = list(
      label = "none",
      requires = character(),
      fit = function(x, params, context) list(method = "none"),
      apply = function(x, model) x
    ),

    `z-score` = scaler("z-score", mean, stats::sd),

    autoscaling = scaler("autoscaling", mean, stats::sd),

    pareto = scaler("pareto", mean, function(col) sqrt(stats::sd(col))),

    vast = scaler("vast", mean, function(col) {
      m <- mean(col)
      s <- stats::sd(col)
      if (!is.finite(m) || m == 0) s else s * s / m
    }),

    range = scaler("range", min, function(col) max(col) - min(col)),

    robust = scaler("robust", stats::median, stats::mad)

  )

}

# =============================================================================
# Batch correction
# =============================================================================

#' Build the batch correction method registry
#'
#' @keywords internal
#' @noRd
.batch_methods <- function() {

  list(

    none = list(
      label = "none",
      requires = character(),
      fit = function(x, params, context) list(method = "none"),
      apply = function(x, model) x
    ),

    mean_centering = list(

      label = "mean_centering",
      requires = character(),

      fit = function(x, params, context) {

        cols <- .numeric_columns(x)
        m <- .stage_matrix(x, cols)

        batch <- .batch_labels(x, context)

        if (is.null(batch)) {
          return(list(method = "mean_centering", columns = cols,
                      offsets = list(), grand = .col_stat(m, mean, 0)))
        }

        grand <- .col_stat(m, mean, 0)

        offsets <- lapply(split(seq_len(nrow(m)), batch), function(idx) {
          .col_stat(m[idx, , drop = FALSE], mean, 0) - grand
        })

        list(method = "mean_centering", columns = cols,
             offsets = offsets, grand = grand,
             variable = context$batch_variable)

      },

      apply = function(x, model) {

        cols <- intersect(model$columns, names(x))

        if (length(cols) == 0 || length(model$offsets) == 0) return(x)

        batch <- attr(x, "cmo_batch")

        if (is.null(batch)) return(x)

        m <- .stage_matrix(x, cols)

        for (level in unique(batch[!is.na(batch)])) {

          offset <- model$offsets[[as.character(level)]]

          # A level the pipeline never saw has no offset to reuse; leaving it
          # alone is the only honest option and the engine logs it.
          if (is.null(offset)) next

          idx <- which(batch == level)
          m[idx, ] <- sweep(m[idx, , drop = FALSE], 2, offset[cols], "-")

        }

        .stage_restore(x, m, cols)

      }

    ),

    combat = .unsupported_method(
      "combat",
      "ComBat requires the 'sva' package and is not wired into the engine yet.",
      c("mean_centering")
    ),

    harmony = .unsupported_method(
      "harmony",
      paste("Harmony corrects an embedding rather than the feature matrix,",
            "so it belongs to the latent-representation stage."),
      c("mean_centering")
    ),

    ruv = .unsupported_method(
      "ruv",
      "RUV requires control features that the recipe does not yet declare.",
      c("mean_centering")
    )

  )

}

#' Batch label per sample, aligned to the rows of a block
#' @keywords internal
.batch_labels <- function(x, context) {

  if (is.null(context$metadata) || is.null(context$batch_variable)) return(NULL)

  metadata <- context$metadata

  if (!("sample_id" %in% colnames(metadata))) return(NULL)
  if (!(context$batch_variable %in% colnames(metadata))) return(NULL)

  labels <- metadata[[context$batch_variable]][
    match(rownames(x), metadata$sample_id)
  ]

  if (all(is.na(labels))) return(NULL)

  as.character(labels)

}

# =============================================================================
# Feature selection
# =============================================================================
#
# Selection is expressed as the set of features to keep, which is exactly
# what a new dataset needs in order to end up with the same columns.
# =============================================================================

#' Build the feature selection method registry
#'
#' @keywords internal
#' @noRd
.feature_selection_methods <- function() {

  keep_model <- function(label, choose) {

    list(

      label = label,
      requires = character(),

      fit = function(x, params, context) {
        list(method = label, keep = choose(x, params))
      },

      apply = function(x, model) {

        keep <- intersect(c(model$keep, .non_numeric_columns(x)), names(x))

        x[, keep, drop = FALSE]

      }

    )

  }

  list(

    none = list(
      label = "none",
      requires = character(),
      fit = function(x, params, context) list(method = "none"),
      apply = function(x, model) x
    ),

    variance_filter = keep_model("variance_filter", function(x, params) {

      cols <- .numeric_columns(x)

      if (length(cols) == 0) return(character(0))

      variances <- vapply(x[cols], function(col) {
        col <- col[is.finite(col)]
        if (length(col) < 2) 0 else stats::var(col)
      }, numeric(1))

      top_n <- if (is.null(params$top_n)) length(cols) else params$top_n

      cols[order(variances, decreasing = TRUE)[seq_len(min(top_n, length(cols)))]]

    }),

    correlation_filter = keep_model("correlation_filter", function(x, params) {

      cols <- .numeric_columns(x)

      if (length(cols) < 2) return(cols)

      threshold <- if (is.null(params$threshold)) 0.95 else params$threshold

      m <- .stage_matrix(x, cols)
      correlation <- .safe_try(
        abs(stats::cor(m, use = "pairwise.complete.obs")), NULL
      )

      if (is.null(correlation)) return(cols)

      drop <- character(0)

      for (i in seq_along(cols)) {

        if (cols[i] %in% drop) next

        partners <- which(correlation[i, ] > threshold)
        partners <- partners[partners > i]

        drop <- c(drop, cols[partners])

      }

      setdiff(cols, unique(drop))

    }),

    boruta = .unsupported_method(
      "boruta",
      "Boruta requires the 'Boruta' package and a declared outcome.",
      c("variance_filter", "correlation_filter")
    ),

    lasso = .unsupported_method(
      "lasso",
      "LASSO selection requires the 'glmnet' package and a declared outcome.",
      c("variance_filter", "correlation_filter")
    ),

    relief = .unsupported_method(
      "relief",
      "Relief requires the 'CORElearn' package and a declared outcome.",
      c("variance_filter", "correlation_filter")
    )

  )

}

#' List the non-numeric columns of a block
#'
#' @keywords internal
#' @noRd
.non_numeric_columns <- function(x) {

  setdiff(names(x), .numeric_columns(x))

}

# =============================================================================
# Step recording
# =============================================================================

#' Record one executed pipeline step
#'
#' @keywords internal
.new_step <- function(index, block, stage, method, parameters = list(),
                      model = NULL, before = c(NA, NA), after = c(NA, NA),
                      removed_samples = character(), removed_features = character(),
                      messages = character(), runtime = NA_real_,
                      replay = TRUE) {

  list(
    step = index,
    block = block,
    stage = stage,
    method = method,
    parameters = parameters,
    model = model,
    samples_before = before[1],
    features_before = before[2],
    samples_after = after[1],
    features_after = after[2],
    removed_samples = removed_samples,
    removed_features = removed_features,
    messages = messages,
    runtime = runtime,

    # Sample-level decisions are fitted to the training cohort and must not
    # be replayed: an external set has its own samples, and dropping them
    # because a training sample was dropped would be meaningless.
    replay = replay
  )

}

#' Dimensions of a block as the step record stores them
#' @keywords internal
.block_dim <- function(x) c(nrow(x), ncol(x))

# =============================================================================
# Filters
# =============================================================================
#
# Filters are rules, not methods: they are expressed as the set of features
# to keep, which is exactly what a new dataset needs to end up with the same
# columns.
# =============================================================================

#' Subset a block to the features a filter kept
#' @keywords internal
.apply_keep <- function(x, model) {

  keep <- intersect(model$keep, names(x))

  x[, keep, drop = FALSE]

}

#' Decide which features the structural filters remove
#'
#' Constant, near-constant, duplicated, empty and mostly-missing features.
#' These are invariant to everything downstream, which is why they run first.
#'
#' @keywords internal
.structural_feature_filter <- function(x, recipe, diagnostic = NULL) {

  drop <- character(0)
  reasons <- character(0)

  add <- function(features, reason) {

    features <- setdiff(intersect(features, names(x)), drop)

    if (length(features) == 0) return(invisible(NULL))

    drop <<- c(drop, features)
    reasons <<- c(reasons, stats::setNames(rep(reason, length(features)), features))

  }

  if (isTRUE(recipe$remove_empty_features)) {
    add(names(x)[vapply(x, function(col) all(is.na(col)), logical(1))], "empty")
  }

  if (isTRUE(recipe$remove_constant_features)) {
    add(.constant_features(x), "constant")
  }

  if (isTRUE(recipe$remove_near_constant_features)) {
    add(.near_constant_features(x), "near-constant")
  }

  if (isTRUE(recipe$remove_duplicate_features)) {
    add(.duplicated_features(x), "duplicated")
  }

  if (!is.null(recipe$feature_missing_threshold) && nrow(x) > 0) {

    fraction <- vapply(x, function(col) mean(is.na(col)), numeric(1))

    add(names(x)[fraction > recipe$feature_missing_threshold],
        sprintf("missing > %.0f%%", 100 * recipe$feature_missing_threshold))

  }

  list(keep = setdiff(names(x), drop), drop = drop, reasons = reasons)

}

#' Decide which samples the structural filters remove
#' @keywords internal
.structural_sample_filter <- function(x, recipe) {

  drop <- character(0)
  reasons <- character(0)

  ids <- rownames(x)

  add <- function(samples, reason) {

    samples <- setdiff(intersect(samples, ids), drop)

    if (length(samples) == 0) return(invisible(NULL))

    drop <<- c(drop, samples)
    reasons <<- c(reasons, stats::setNames(rep(reason, length(samples)), samples))

  }

  if (isTRUE(recipe$remove_empty_samples) && ncol(x) > 0) {
    add(ids[apply(is.na(x), 1, all)], "empty")
  }

  if (isTRUE(recipe$remove_duplicate_samples)) {
    add(.duplicated_samples(x), "duplicated")
  }

  if (!is.null(recipe$sample_missing_threshold) && ncol(x) > 0) {

    fraction <- rowMeans(is.na(x))

    add(ids[fraction > recipe$sample_missing_threshold],
        sprintf("missing > %.0f%%", 100 * recipe$sample_missing_threshold))

  }

  list(keep = setdiff(ids, drop), drop = drop, reasons = reasons)

}

#' Decide which features the statistical filters remove
#'
#' Variance, abundance and prevalence only mean something once the block has
#' been normalized and transformed onto a comparable scale, which is why this
#' runs late rather than alongside the structural filters.
#'
#' @keywords internal
.statistical_feature_filter <- function(x, recipe) {

  cols <- .numeric_columns(x)

  drop <- character(0)
  reasons <- character(0)

  add <- function(features, reason) {

    features <- setdiff(features, drop)

    if (length(features) == 0) return(invisible(NULL))

    drop <<- c(drop, features)
    reasons <<- c(reasons, stats::setNames(rep(reason, length(features)), features))

  }

  if (!is.null(recipe$variance_threshold) && length(cols) > 0) {

    variances <- vapply(x[cols], function(col) {
      col <- col[is.finite(col)]
      if (length(col) < 2) 0 else stats::var(col)
    }, numeric(1))

    add(cols[variances < recipe$variance_threshold], "low variance")

  }

  if (!is.null(recipe$abundance_threshold) && length(cols) > 0) {

    abundance <- vapply(x[cols], function(col) {
      col <- col[is.finite(col)]
      if (length(col) == 0) 0 else mean(abs(col))
    }, numeric(1))

    add(cols[abundance < recipe$abundance_threshold], "low abundance")

  }

  if (!is.null(recipe$prevalence_threshold) && length(cols) > 0) {

    prevalence <- vapply(x[cols], function(col) {
      col <- col[is.finite(col)]
      if (length(col) == 0) 0 else mean(col != 0)
    }, numeric(1))

    add(cols[prevalence < recipe$prevalence_threshold], "low prevalence")

  }

  list(keep = setdiff(names(x), drop), drop = drop, reasons = reasons)

}

#' Flag or drop outlying samples
#'
#' Never enabled automatically: discarding observations is a scientific
#' decision, not a data-cleaning one, so the recipe has to ask for it.
#'
#' @keywords internal
.outlier_filter <- function(x, recipe) {

  handling <- recipe$outlier_handling

  if (is.null(handling) || identical(handling, "none")) {
    return(list(handling = "none", samples = character(0)))
  }

  cols <- .numeric_columns(x)

  if (length(cols) == 0) {
    return(list(handling = handling, samples = character(0)))
  }

  threshold <- if (is.null(recipe$outlier_threshold)) 0.25 else
    recipe$outlier_threshold

  m <- .stage_matrix(x, cols)

  outlying <- vapply(seq_len(nrow(m)), function(i) {

    row <- m[i, ]
    row <- row[is.finite(row)]

    if (length(row) == 0) return(0)

    flagged <- vapply(seq_along(cols), function(j) {

      column <- m[, j]
      column <- column[is.finite(column)]

      if (length(column) < 4) return(FALSE)

      q <- stats::quantile(column, c(0.25, 0.75))
      iqr <- q[2] - q[1]

      if (!isTRUE(iqr > 0)) return(FALSE)

      value <- m[i, j]

      isTRUE(value < q[1] - 3 * iqr || value > q[2] + 3 * iqr)

    }, logical(1))

    mean(flagged, na.rm = TRUE)

  }, numeric(1))

  list(
    handling = handling,
    samples = rownames(x)[outlying > threshold],
    fraction = stats::setNames(outlying, rownames(x))
  )

}

# =============================================================================
# Stage execution
# =============================================================================

#' Method configured for a stage in a recipe
#' @keywords internal
.stage_config <- function(recipe, stage) {

  spec <- switch(

    stage,

    imputation = list(recipe$imputation, recipe$imputation_parameters),
    transformation = list(recipe$transformation, recipe$transformation_parameters),
    normalization = list(recipe$normalization, recipe$normalization_parameters),
    scaling = list(recipe$scaling, recipe$scaling_parameters),
    batch = list(recipe$batch, recipe$batch_parameters),
    feature_selection = list(recipe$feature_selection,
                             recipe$feature_selection_parameters),

    list(NULL, list())

  )

  method <- spec[[1]]
  params <- spec[[2]]

  if (is.null(method) || length(method) == 0 || is.na(method)) method <- "none"

  list(method = as.character(method),
       parameters = if (is.null(params)) list() else params)

}

#' Attach batch labels so the batch method can find them
#' @keywords internal
.with_batch_attr <- function(x, context) {

  attr(x, "cmo_batch") <- .batch_labels(x, context)

  x

}

# =============================================================================
# Per-block pipeline
# =============================================================================

#' Run the full recipe for a single block
#' @keywords internal

.preprocess_block <- function(block_name, x, recipe, context, index_offset = 0L) {

  steps <- list()
  models <- list()
  index <- index_offset

  order <- recipe$stage_order

  if (is.null(order) || length(order) == 0) {
    order <- .stage_order(recipe$data_type)
  }

  record <- function(...) {
    index <<- index + 1L
    steps[[length(steps) + 1L]] <<- .new_step(index, block_name, ...)
  }

  for (stage in order) {

    before <- .block_dim(x)
    started <- Sys.time()

    if (identical(stage, "remove_features")) {

      decision <- .structural_feature_filter(x, recipe)

      if (length(decision$drop) > 0) x <- .apply_keep(x, decision)

      record(stage = stage, method = "structural_filter",
             model = list(keep = decision$keep),
             before = before, after = .block_dim(x),
             removed_features = decision$drop,
             messages = if (length(decision$drop) > 0)
               sprintf("%s (%s)", names(decision$reasons), decision$reasons) else
                 character(0),
             runtime = as.numeric(difftime(Sys.time(), started, units = "secs")))

      next

    }

    if (identical(stage, "remove_samples")) {

      decision <- .structural_sample_filter(x, recipe)

      if (length(decision$drop) > 0) {
        x <- x[decision$keep, , drop = FALSE]
      }

      record(stage = stage, method = "structural_filter",
             model = list(keep = decision$keep),
             before = before, after = .block_dim(x),
             removed_samples = decision$drop,
             messages = if (length(decision$drop) > 0)
               sprintf("%s (%s)", names(decision$reasons), decision$reasons) else
                 character(0),
             runtime = as.numeric(difftime(Sys.time(), started, units = "secs")),
             replay = FALSE)

      next

    }

    if (identical(stage, "filter")) {

      decision <- .statistical_feature_filter(x, recipe)

      if (length(decision$drop) > 0) x <- .apply_keep(x, decision)

      record(stage = stage, method = "statistical_filter",
             model = list(keep = decision$keep),
             before = before, after = .block_dim(x),
             removed_features = decision$drop,
             messages = if (length(decision$drop) > 0)
               sprintf("%s (%s)", names(decision$reasons), decision$reasons) else
                 character(0),
             runtime = as.numeric(difftime(Sys.time(), started, units = "secs")))

      next

    }

    if (identical(stage, "outliers")) {

      decision <- .outlier_filter(x, recipe)

      removed <- character(0)

      if (identical(decision$handling, "remove") && length(decision$samples) > 0) {
        removed <- decision$samples
        x <- x[setdiff(rownames(x), removed), , drop = FALSE]
      }

      if (identical(decision$handling, "none")) next

      record(stage = stage, method = decision$handling,
             model = list(flagged = decision$samples),
             before = before, after = .block_dim(x),
             removed_samples = removed,
             messages = if (length(decision$samples) > 0)
               sprintf("%d outlying sample(s) %s", length(decision$samples),
                       if (length(removed) > 0) "removed" else "flagged") else
                 character(0),
             runtime = as.numeric(difftime(Sys.time(), started, units = "secs")),
             replay = FALSE)

      next

    }

    # ---------------------------------------------------------------------
    # Registry-backed stages
    # ---------------------------------------------------------------------

    config <- .stage_config(recipe, stage)

    if (identical(config$method, "none")) next

    entry <- .get_method(stage, config$method)

    if (identical(stage, "batch")) x <- .with_batch_attr(x, context)

    model <- entry$fit(x, config$parameters, context)
    x <- entry$apply(x, model)

    attr(x, "cmo_batch") <- NULL

    models[[stage]] <- model

    after <- .block_dim(x)

    record(stage = stage, method = config$method,
           parameters = config$parameters,
           model = model,
           before = before, after = after,
           removed_features = if (after[2] < before[2])
             setdiff(model$columns, names(x)) else character(0),
           runtime = as.numeric(difftime(Sys.time(), started, units = "secs")))

  }

  list(block = x, steps = steps, models = models, index = index)

}

# =============================================================================
# Plan resolution
# =============================================================================

#' Turn whatever the user passed as a plan into a named list of recipes
#' @keywords internal

.resolve_plan <- function(plan, object) {

  if (inherits(plan, "CMOValidation")) {

    if (length(plan$recipes) == 0) {
      stop("The validation contains no recipes to execute.", call. = FALSE)
    }

    return(plan$recipes)

  }

  if (inherits(plan, "PreprocessingRecipe")) {

    block <- plan$block

    if (is.null(block) || !nzchar(block)) {
      stop("The recipe does not name the block it applies to.", call. = FALSE)
    }

    return(stats::setNames(list(plan), block))

  }

  if (is.list(plan) &&
      all(vapply(plan, inherits, logical(1), "PreprocessingRecipe"))) {

    if (is.null(names(plan))) {
      names(plan) <- vapply(plan, function(r) as.character(r$block), character(1))
    }

    return(plan)

  }

  stop(
    paste("'plan' must be a CMOValidation, a PreprocessingRecipe, or a named",
          "list of PreprocessingRecipe objects."),
    call. = FALSE
  )

}

#' Refuse to execute a plan that does not describe this object
#' @keywords internal

.check_plan_matches <- function(object, plan, recipes, force) {

  problems <- character(0)

  unknown <- setdiff(names(recipes), names(object$assays))

  if (length(unknown) > 0) {

    problems <- c(problems, sprintf(
      "the plan covers block(s) absent from the object: %s",
      paste(unknown, collapse = ", ")
    ))

  }

  if (inherits(plan, "CMOValidation")) {

    summary_table <- plan$summary$block_summary

    if (is.data.frame(summary_table) && nrow(summary_table) > 0) {

      for (i in seq_len(nrow(summary_table))) {

        block <- summary_table$block[i]

        if (!(block %in% names(object$assays))) next

        actual <- object$assays[[block]]

        if (nrow(actual) != summary_table$samples[i] ||
            ncol(actual) != summary_table$features[i]) {

          problems <- c(problems, sprintf(
            "block '%s' is %dx%d but the validation recorded %dx%d",
            block, nrow(actual), ncol(actual),
            summary_table$samples[i], summary_table$features[i]
          ))

        }

      }

    }

    if (!isTRUE(plan$valid)) {

      problems <- c(problems, sprintf(
        "the validation failed with %d error(s)", length(plan$errors)
      ))

    }

  }

  if (length(problems) == 0) return(invisible(NULL))

  if (isTRUE(force)) {

    return(paste("Executed with force = TRUE despite:",
                 paste(problems, collapse = "; ")))

  }

  stop(
    sprintf(
      "The plan does not match this object:\n%s\n  Re-run check_data(), or pass force = TRUE to execute anyway.",
      paste0("  - ", problems, collapse = "\n")
    ),
    call. = FALSE
  )

}

# =============================================================================
# preprocess()
# =============================================================================

#' Execute a preprocessing plan
#'
#' Runs the \code{PreprocessingRecipe} objects produced by
#' \code{check_data()} against the data they describe. The function decides
#' nothing on its own: every method, parameter and threshold it applies comes
#' from the recipe, and the stage order comes from the recipe's
#' \code{stage_order}.
#'
#' Each stage is fitted and then applied, and the fitted model is kept. That
#' is what makes the result reproducible: \code{apply_preprocessing()} can
#' put an external dataset through the identical pipeline without deriving a
#' single decision from it.
#'
#' Sample-level decisions (empty, duplicated or outlying samples) are marked
#' as non-replayable, because an external cohort has its own samples.
#' Feature-level decisions are replayable and pin the feature set.
#'
#' @param object A \code{MultiOmicsData} object.
#' @param plan A \code{CMOValidation} (the usual case, as returned by
#'   \code{check_data()}), a single \code{PreprocessingRecipe}, or a named
#'   list of recipes.
#' @param blocks Optional character vector restricting execution to some
#'   blocks. Blocks not covered are carried through untouched.
#' @param plots Whether to render before/after diagnostic plots.
#' @param force Execute even when the plan does not match the object, or when
#'   the validation reported errors. The mismatch is recorded in the result.
#' @param quiet Whether to suppress progress messages.
#'
#' @return A \code{PreprocessingResult} object.
#'
#' @seealso \code{\link{apply_preprocessing}} to replay the result on new data.
#'
#' @examples
#' tr <- data.frame(
#'   Gene1 = rnorm(10),
#'   Gene2 = rnorm(10),
#'   Gene3 = rep(1, 10),
#'   row.names = paste0("S", 1:10)
#' )
#'
#' x <- load_data(assays = list(transcriptomics = tr))
#'
#' validation <- check_data(x)
#'
#' result <- preprocess(x, validation, plots = FALSE, quiet = TRUE)
#'
#' result
#'
#' @export

preprocess <- function(object,
                       plan,
                       blocks = NULL,
                       plots = TRUE,
                       force = FALSE,
                       quiet = FALSE) {

  started <- Sys.time()

  if (!inherits(object, "MultiOmicsData")) {
    stop("'object' must be a MultiOmicsData object.", call. = FALSE)
  }

  if (missing(plan)) {
    stop("'plan' must be supplied: preprocess() executes a plan, it does not derive one.",
         call. = FALSE)
  }

  recipes <- .resolve_plan(plan, object)

  forced_note <- .check_plan_matches(object, plan, recipes, force)

  if (!is.null(blocks)) {

    unknown <- setdiff(blocks, names(recipes))

    if (length(unknown) > 0) {
      stop(sprintf("No recipe for block(s): %s.", paste(unknown, collapse = ", ")),
           call. = FALSE)
    }

    recipes <- recipes[blocks]

  }

  result <- PreprocessingResult()

  result$input <- object
  result$recipes <- recipes

  logs <- character(0)

  if (!is.null(forced_note)) logs <- c(logs, forced_note)

  # ---------------------------------------------------------------------------
  # Run every block
  # ---------------------------------------------------------------------------

  processed <- object$assays
  all_steps <- list()
  models <- list()
  index <- 0L

  for (block_name in names(recipes)) {

    recipe <- recipes[[block_name]]

    if (!isTRUE(recipe$enabled)) {

      logs <- c(logs, sprintf("Block '%s' skipped: the recipe is disabled.",
                              block_name))
      next

    }

    if (!(block_name %in% names(processed))) next

    if (!isTRUE(quiet)) cat(sprintf("Preprocessing '%s'...\n", block_name))

    context <- list(
      metadata = object$metadata,
      batch_variable = recipe$batch_variable,
      data_type = recipe$data_type,
      modality = recipe$modality
    )

    outcome <- .safe_try(
      .preprocess_block(block_name, processed[[block_name]], recipe,
                        context, index),
      NULL
    )

    if (is.null(outcome)) {

      logs <- c(logs, sprintf(
        "Block '%s' failed during preprocessing and was left untouched.",
        block_name
      ))

      next

    }

    processed[[block_name]] <- outcome$block
    all_steps <- c(all_steps, outcome$steps)
    models[[block_name]] <- outcome$models
    index <- outcome$index

  }

  # ---------------------------------------------------------------------------
  # Assemble the processed object
  # ---------------------------------------------------------------------------

  data <- object
  data$assays <- processed
  data$history <- c(
    object$history,
    sprintf("preprocess(): %d step(s) over %d block(s) [%s]",
            length(all_steps), length(recipes),
            format(started, "%Y-%m-%d %H:%M:%S"))
  )

  result$data <- data
  result$steps <- all_steps
  result$models <- models

  # ---------------------------------------------------------------------------
  # Derived views
  # ---------------------------------------------------------------------------

  result$removed_samples <- .steps_removed(all_steps, "removed_samples")
  result$removed_features <- .steps_removed(all_steps, "removed_features")

  for (stage in c("imputation", "transformation", "normalization",
                  "scaling", "batch", "feature_selection")) {

    slot <- if (identical(stage, "transformation")) "transformations" else stage

    result[[slot]] <- .steps_by_stage(all_steps, stage)

  }

  result$dimensionality_reduction <- list()

  # ---------------------------------------------------------------------------
  # Final quality control
  # ---------------------------------------------------------------------------

  diagnostics <- list()

  for (block_name in names(recipes)) {

    if (!(block_name %in% names(processed))) next

    block <- processed[[block_name]]

    if (nrow(block) == 0 || ncol(block) == 0) next

    diagnostics[[block_name]] <- .safe_try(
      .build_block_diagnostics(block_name, block,
                               .block_modality(object, block_name)),
      NULL
    )

  }

  diagnostics <- diagnostics[!vapply(diagnostics, is.null, logical(1))]

  result$diagnostics <- diagnostics
  result$quality <- .quality_comparison(object, processed, plan, diagnostics)
  result$statistics <- .preprocessing_statistics(object, processed, all_steps)

  # ---------------------------------------------------------------------------
  # Plots and tables
  # ---------------------------------------------------------------------------

  if (isTRUE(plots)) {

    result$plots <- .safe_try(
      .build_preprocessing_plots(object$assays, processed, diagnostics),
      list()
    )

  }

  result$tables <- list(
    steps = .build_steps_table(all_steps),
    removed_features = .build_removed_table(all_steps, "removed_features", "feature"),
    removed_samples = .build_removed_table(all_steps, "removed_samples", "sample"),
    quality = result$quality$table
  )

  # ---------------------------------------------------------------------------
  # Execution
  # ---------------------------------------------------------------------------

  finished <- Sys.time()

  result$execution <- list(
    runtime = as.numeric(difftime(finished, started, units = "secs")),
    started = started,
    finished = finished,
    package_version = .safe_try(
      as.character(utils::packageVersion("CausalMultiOmics")), NA_character_
    ),
    engine_version = "1.0.0",
    forced = isTRUE(force),
    blocks = names(recipes),
    session = utils::sessionInfo()$R.version$version.string
  )

  result$logs <- logs
  result$history <- sprintf("preprocess() run on %s", format(started))
  result$timestamp <- Sys.time()

  if (!isTRUE(quiet)) {
    cat(sprintf("Done: %d step(s) in %.2f s\n",
                length(all_steps), result$execution$runtime))
  }

  result

}

# =============================================================================
# apply_preprocessing()
# =============================================================================

#' Replay a fitted pipeline on new data
#'
#' Puts a new dataset through exactly the pipeline recorded in a
#' \code{PreprocessingResult}, reusing every fitted model rather than
#' deriving anything from the new data. This is what makes an external
#' validation cohort comparable with the cohort the pipeline was built on.
#'
#' Two rules follow from that and are applied deliberately:
#'
#' \itemize{
#'   \item Feature-level decisions are replayed, so the output carries the
#'     same features in the same order as the training data.
#'   \item Sample-level decisions are not replayed. Removing a sample from a
#'     new cohort because a training sample was removed would be meaningless,
#'     and silently dropping rows from a validation set is a common way to
#'     produce optimistic results.
#' }
#'
#' @param object A \code{MultiOmicsData} object holding the new data.
#' @param result A \code{PreprocessingResult} from \code{preprocess()}.
#' @param blocks Optional character vector restricting the replay.
#' @param quiet Whether to suppress progress messages.
#'
#' @return A \code{MultiOmicsData} object carrying the processed blocks.
#'
#' @seealso \code{\link{preprocess}}
#'
#' @export

apply_preprocessing <- function(object, result, blocks = NULL, quiet = FALSE) {

  if (!inherits(object, "MultiOmicsData")) {
    stop("'object' must be a MultiOmicsData object.", call. = FALSE)
  }

  if (!inherits(result, "PreprocessingResult")) {
    stop("'result' must be a PreprocessingResult object.", call. = FALSE)
  }

  if (length(result$steps) == 0) {
    stop("The result contains no steps to replay.", call. = FALSE)
  }

  step_blocks <- unique(vapply(result$steps, function(s) s$block, character(1)))

  target <- if (is.null(blocks)) step_blocks else intersect(blocks, step_blocks)

  missing_blocks <- setdiff(target, names(object$assays))

  if (length(missing_blocks) > 0) {

    stop(
      sprintf("The new object has no block(s): %s.",
              paste(missing_blocks, collapse = ", ")),
      call. = FALSE
    )

  }

  assays <- object$assays

  for (block_name in target) {

    if (!isTRUE(quiet)) cat(sprintf("Replaying '%s'...\n", block_name))

    x <- assays[[block_name]]

    steps <- Filter(
      function(s) identical(s$block, block_name) && isTRUE(s$replay),
      result$steps
    )

    for (step in steps) {

      if (step$stage %in% c("remove_features", "filter")) {

        x <- .apply_keep(x, step$model)
        next

      }

      entry <- .get_method(step$stage, step$method)

      if (identical(step$stage, "batch")) {

        attr(x, "cmo_batch") <- .batch_labels(
          x, list(metadata = object$metadata,
                  batch_variable = step$model$variable)
        )

      }

      x <- entry$apply(x, step$model)

      attr(x, "cmo_batch") <- NULL

    }

    assays[[block_name]] <- x

  }

  object$assays <- assays
  object$history <- c(
    object$history,
    sprintf("apply_preprocessing(): replayed %d block(s) [%s]",
            length(target), format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )

  object

}

# =============================================================================
# Derived views, statistics and tables
# =============================================================================

#' Collect removed samples or features by block
#'
#' @keywords internal
#' @noRd
.steps_removed <- function(steps, field) {

  out <- list()

  for (step in steps) {

    removed <- step[[field]]

    if (length(removed) == 0) next

    out[[step$block]] <- unique(c(out[[step$block]], removed))

  }

  out

}

#' Extract recorded steps for one pipeline stage
#'
#' @keywords internal
#' @noRd
.steps_by_stage <- function(steps, stage) {

  selected <- Filter(function(s) identical(s$stage, stage), steps)

  out <- lapply(selected, function(s) {
    list(method = s$method, parameters = s$parameters, model = s$model,
         runtime = s$runtime)
  })

  names(out) <- vapply(selected, function(s) s$block, character(1))

  out

}

#' Compute before and after preprocessing statistics per block
#'
#' @keywords internal
#' @noRd
.preprocessing_statistics <- function(before, after, steps) {

  blocks <- intersect(names(before$assays), names(after))

  stats::setNames(lapply(blocks, function(nm) {

    b <- before$assays[[nm]]
    a <- after[[nm]]

    list(
      samples_before = nrow(b),
      samples_after = nrow(a),
      features_before = ncol(b),
      features_after = ncol(a),
      missing_before = 100 * sum(is.na(b)) / max(length(as.matrix(b)), 1),
      missing_after = 100 * sum(is.na(a)) / max(length(as.matrix(a)), 1),
      steps = sum(vapply(steps, function(s) identical(s$block, nm), logical(1)))
    )

  }), blocks)

}

#' Compare quality scores before and after preprocessing
#'
#' @keywords internal
#' @noRd
.quality_comparison <- function(object, processed, plan, diagnostics) {

  before_scores <- list()

  if (inherits(plan, "CMOValidation")) {

    before_scores <- lapply(plan$diagnostics, .compute_quality_score)

  }

  blocks <- names(diagnostics)

  rows <- lapply(blocks, function(nm) {

    before <- object$assays[[nm]]
    after <- processed[[nm]]

    data.frame(
      block = nm,
      samples_before = nrow(before),
      samples_after = nrow(after),
      features_before = ncol(before),
      features_after = ncol(after),
      missing_before = round(100 * sum(is.na(before)) /
                               max(length(as.matrix(before)), 1), 2),
      missing_after = round(100 * sum(is.na(after)) /
                              max(length(as.matrix(after)), 1), 2),
      score_before = if (!is.null(before_scores[[nm]]))
        round(before_scores[[nm]], 1) else NA_real_,
      score_after = round(.compute_quality_score(diagnostics[[nm]]), 1),
      stringsAsFactors = FALSE
    )

  })

  table <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()

  list(
    table = table,
    before = before_scores,
    after = lapply(diagnostics, .compute_quality_score)
  )

}

#' Build a summary table of preprocessing steps
#'
#' @keywords internal
#' @noRd
.build_steps_table <- function(steps) {

  if (length(steps) == 0) return(data.frame())

  do.call(rbind, lapply(steps, function(s) {

    data.frame(
      step = s$step,
      block = s$block,
      stage = s$stage,
      method = s$method,
      samples = sprintf("%s -> %s", s$samples_before, s$samples_after),
      features = sprintf("%s -> %s", s$features_before, s$features_after),
      removed_samples = length(s$removed_samples),
      removed_features = length(s$removed_features),
      runtime = round(s$runtime, 4),
      replay = s$replay,
      stringsAsFactors = FALSE
    )

  }))

}

#' Build a table of removed samples and features
#'
#' @keywords internal
#' @noRd
.build_removed_table <- function(steps, field, label) {

  rows <- list()

  for (s in steps) {

    removed <- s[[field]]

    if (length(removed) == 0) next

    reasons <- s$messages

    rows[[length(rows) + 1L]] <- data.frame(
      block = s$block,
      stage = s$stage,
      item = removed,
      reason = if (length(reasons) == length(removed)) reasons else s$stage,
      stringsAsFactors = FALSE
    )

  }

  if (length(rows) == 0) return(data.frame())

  out <- do.call(rbind, rows)
  names(out)[names(out) == "item"] <- label

  out

}

# =============================================================================
# Plots
# =============================================================================

#' Build diagnostic plots comparing data before and after
#'
#' @keywords internal
#' @noRd
.build_preprocessing_plots <- function(before, after, diagnostics) {

  blocks <- intersect(names(before), names(after))

  keep_named <- function(values) {
    values <- values[!vapply(values, is.null, logical(1))]
    values
  }

  plots <- list()

  plots$comparison <- keep_named(stats::setNames(lapply(blocks, function(nm) {
    .plot_transformation_comparison_block(
      nm, .pool_numeric(before[[nm]]), .pool_numeric(after[[nm]])
    )
  }), blocks))

  plots$density <- keep_named(stats::setNames(lapply(blocks, function(nm) {
    .plot_density_block(nm, .pool_numeric(after[[nm]]))
  }), blocks))

  plots$boxplots <- keep_named(stats::setNames(lapply(blocks, function(nm) {
    .plot_boxplot_block(nm, after[[nm]])
  }), blocks))

  plots$qqplots <- keep_named(stats::setNames(lapply(blocks, function(nm) {
    .plot_qqplot_block(nm, .pool_numeric(after[[nm]]))
  }), blocks))

  plots$missing_before <- keep_named(stats::setNames(lapply(blocks, function(nm) {
    .plot_missing_heatmap_block(nm, before[[nm]])
  }), blocks))

  plots$pca <- keep_named(stats::setNames(lapply(blocks, function(nm) {
    .plot_pca_block(nm, diagnostics[[nm]]$pca)
  }), blocks))

  plots$correlation <- keep_named(stats::setNames(lapply(blocks, function(nm) {
    .plot_correlation_heatmap_block(nm, diagnostics[[nm]]$correlation)
  }), blocks))

  plots

}

# =============================================================================
# End of preprocessing.R
# =============================================================================
