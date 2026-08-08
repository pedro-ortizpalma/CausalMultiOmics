# =============================================================================
# validation.R
# Central validation, diagnostics and automatic preprocessing recommendation
# engine for CausalMultiOmics
# =============================================================================

# =============================================================================
# Internal utilities
# =============================================================================

#' Evaluate an expression, falling back to a default
#'
#' @keywords internal
#' @noRd
.safe_try <- function(expr, default = NULL) {

  result <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) default
  )

  if (is.null(result)) default else result

}

#' Evaluate an expression without leaking RNG state into the caller's session
#'
#' Parts of the diagnostic engine need randomness (fold assignment, Shapiro
#' subsampling) and want it reproducible, but \code{check_data()} is a
#' read-only audit: it must not move the user's random number generator, or
#' every simulation run afterwards would silently change. The current
#' \code{.Random.seed} is saved, the expression is evaluated under
#' \code{seed}, and the original state is put back on exit.
#'
#' @param expr Expression to evaluate.
#' @param seed Integer seed used while evaluating \code{expr}.
#'
#' @keywords internal
.with_preserved_seed <- function(expr, seed = 1L) {

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {

    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)

  } else {

    # No generator has been initialised yet: leave the session exactly as it
    # was found by removing whatever set.seed() creates below.

    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )

  }

  set.seed(seed)

  expr

}

#' Assign a (possibly NULL) value to a list slot without deleting it
#'
#' Ordinary \code{$<-} / \code{[[<-} assignment of \code{NULL} removes the
#' element from a list. Every class defined in classes.R has a fixed set of
#' slots that must always remain present (even when their value is
#' \code{NULL}), so every assignment that might carry a \code{NULL} value
#' goes through this helper instead.
#'
#' @keywords internal
.set_slot <- function(obj, name, value) {

  obj[name] <- list(value)

  obj

}

#' Row count that tolerates NULL and non-rectangular input
#'
#' \code{nrow()} returns \code{NULL} for both, and \code{NULL == 0} is
#' \code{logical(0)}, which blows up the surrounding \code{if}.
#'
#' @keywords internal
.n_rows <- function(x) {

  n <- if (is.null(x)) 0L else nrow(x)

  if (is.null(n)) 0L else n

}

#' Names of numeric columns in a block
#'
#' @keywords internal
#' @noRd
.numeric_columns <- function(block) {

  idx <- vapply(block, function(x) is.numeric(x) || is.integer(x), logical(1))

  names(block)[idx]

}

#' Numeric columns of a block as a matrix
#'
#' @keywords internal
#' @noRd
.numeric_matrix <- function(block) {

  nm <- .numeric_columns(block)

  if (length(nm) == 0) return(matrix(numeric(0), nrow = nrow(block), ncol = 0))

  as.matrix(block[, nm, drop = FALSE])

}

#' Pool finite numeric values from a block
#'
#' @keywords internal
#' @noRd
.pool_numeric <- function(block) {

  m <- .numeric_matrix(block)

  x <- as.numeric(m)

  x[is.finite(x)]

}

#' Modality declared for a block, defaulting to "unknown"
#'
#' Hand-built \code{MultiOmicsData} objects need not carry the slot at all,
#' so every read goes through here.
#'
#' @keywords internal
.block_modality <- function(object, block_name) {

  m <- object$modality

  if (is.null(m) || length(m) == 0) return("unknown")

  if (!(block_name %in% names(m))) return("unknown")

  value <- as.character(m[[block_name]])

  if (length(value) == 0 || is.na(value) || !nzchar(value)) return("unknown")

  value

}

#' Find first metadata column matching patterns
#'
#' @keywords internal
#' @noRd
.find_metadata_column <- function(metadata, patterns) {

  if (is.null(metadata)) return(NULL)

  nm <- colnames(metadata)

  for (p in patterns) {

    hit <- grep(p, nm, ignore.case = TRUE, value = TRUE)

    if (length(hit) > 0) return(hit[1])

  }

  NULL

}

# =============================================================================
# .detect_data_type()
# =============================================================================

#' Detect the statistical nature of a single variable
#' @keywords internal
.detect_variable_type <- function(x) {

  if (inherits(x, "Surv")) return("censored")

  if (is.logical(x)) return("binary")

  if (is.factor(x) || is.character(x)) {

    u <- unique(stats::na.omit(x))

    if (length(u) <= 2) return("binary")

    return("ordinal")

  }

  if (is.numeric(x) || is.integer(x)) {

    xv <- stats::na.omit(x)

    if (length(xv) == 0) return("continuous")

    u <- unique(xv)

    if (length(u) <= 2 && all(u %in% c(0, 1))) return("binary")

    if (all(xv >= 0) && all(abs(xv - round(xv)) < .Machine$double.eps^0.5)) {

      if (all(xv <= 1)) return("proportion")

      return("count")

    }

    if (all(xv >= 0 & xv <= 1)) return("proportion")

    return("continuous")

  }

  "mixed"

}

#' Detect the statistical nature of a whole data block
#'
#' @param block A data.frame representing one data block.
#'
#' @return
#' A character string: one of \code{"continuous"}, \code{"count"},
#' \code{"binary"}, \code{"ordinal"}, \code{"proportion"},
#' \code{"compositional"}, \code{"censored"}, \code{"mixed"}.
#'
#' @keywords internal

.detect_data_type <- function(block) {

  if (ncol(block) == 0) return("mixed")

  is_surv <- vapply(block, function(x) inherits(x, "Surv"), logical(1))

  if (any(is_surv)) return("censored")

  numeric_cols <- .numeric_columns(block)

  if (length(numeric_cols) >= 3 && length(numeric_cols) == ncol(block)) {

    m <- .numeric_matrix(block)

    complete <- stats::complete.cases(m)

    if (sum(complete) >= 3) {

      mm <- m[complete, , drop = FALSE]

      # Counts are never compositional: with many features their row sums
      # become stable purely by the law of large numbers, which must not be
      # mistaken for the constant-sum constraint of a closed composition.

      integer_valued <- all(abs(mm - round(mm)) < .Machine$double.eps^0.5)

      if (all(mm >= 0) && !integer_valued) {

        row_sums <- rowSums(mm)

        if (all(row_sums > 0)) {

          rel_sd <- stats::sd(row_sums) / mean(row_sums)

          if (is.finite(rel_sd) && rel_sd < 1e-3) return("compositional")

        }

      }

    }

  }

  types <- vapply(block, .detect_variable_type, character(1))

  # A column with a single distinct value carries no distributional
  # information, yet .detect_variable_type() still has to label it: a column
  # of 1s reads as "proportion", a column of 7s as "count". Letting those
  # labels vote drags an otherwise homogeneous block to "mixed" and cuts the
  # transformation battery down to the four generic candidates. Constant
  # columns are excluded from the vote instead; they are still reported as
  # constant_features and the recipe filters them out regardless.
  #
  # Unanimity among the informative columns is kept on purpose. A block that
  # is genuinely part counts and part continuous IS mixed, and resolving it
  # by majority would hand negative-valued columns to log-like transforms.

  informative <- !vapply(
    block,
    function(x) length(unique(stats::na.omit(x))) <= 1,
    logical(1)
  )

  if (any(informative)) types <- types[informative]

  u <- unique(types)

  if (length(u) == 1) return(u)

  "mixed"

}

# =============================================================================
# Descriptive statistics helpers
# =============================================================================

#' Compute basic descriptive statistics for a vector
#'
#' @keywords internal
#' @noRd
.compute_basic_stats <- function(x) {

  x <- x[is.finite(x)]

  if (length(x) == 0) {

    return(list(
      mean = NA_real_, median = NA_real_, variance = NA_real_,
      sd = NA_real_, mad = NA_real_, cv = NA_real_, iqr = NA_real_,
      minimum = NA_real_, maximum = NA_real_, range = NA_real_,
      quantiles = stats::setNames(rep(NA_real_, 5),
                                   c("0%", "25%", "50%", "75%", "100%"))
    ))

  }

  m <- mean(x)
  s <- stats::sd(x)

  list(
    mean = m,
    median = stats::median(x),
    variance = stats::var(x),
    sd = s,
    mad = stats::mad(x),
    cv = if (isTRUE(m != 0)) s / m else NA_real_,
    iqr = stats::IQR(x),
    minimum = min(x),
    maximum = max(x),
    range = max(x) - min(x),
    quantiles = stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1))
  )

}

#' Compute sample skewness of a vector
#'
#' @keywords internal
#' @noRd
.skewness <- function(x) {

  x <- x[is.finite(x)]
  n <- length(x)

  if (n < 3) return(NA_real_)

  m <- mean(x)
  s <- stats::sd(x)

  if (!isTRUE(s > 0)) return(NA_real_)

  (sum((x - m)^3) / n) / (s^3)

}

#' Compute excess kurtosis of a vector
#'
#' @keywords internal
#' @noRd
.kurtosis <- function(x) {

  x <- x[is.finite(x)]
  n <- length(x)

  if (n < 4) return(NA_real_)

  m <- mean(x)
  s <- stats::sd(x)

  if (!isTRUE(s > 0)) return(NA_real_)

  (sum((x - m)^4) / n) / (s^4) - 3

}

#' Shapiro-Wilk normality test p-value
#'
#' @keywords internal
#' @noRd
.shapiro_p <- function(x) {

  x <- x[is.finite(x)]

  if (length(x) < 3) return(NA_real_)

  # shapiro.test() refuses more than 5000 values. Subsampling is random, so it
  # runs under a fixed seed that is handed back to the caller untouched.

  if (length(x) > 5000) x <- .with_preserved_seed(sample(x, 5000))

  if (stats::sd(x) == 0) return(NA_real_)

  .safe_try(stats::shapiro.test(x)$p.value, NA_real_)

}

#' Flag outliers using the IQR rule
#'
#' @keywords internal
#' @noRd
.outliers_iqr <- function(x) {

  x <- x[is.finite(x)]

  if (length(x) < 4) return(integer(0))

  q <- stats::quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]

  if (!isTRUE(iqr > 0)) return(integer(0))

  lo <- q[1] - 1.5 * iqr
  hi <- q[2] + 1.5 * iqr

  which(x < lo | x > hi)

}

#' Compute shape statistics and normality label
#'
#' @keywords internal
#' @noRd
.compute_shape_stats <- function(x) {

  x <- x[is.finite(x)]

  p <- .shapiro_p(x)

  list(
    skewness = .skewness(x),
    kurtosis = .kurtosis(x),
    shapiro = p,
    normality = if (is.na(p)) "unknown" else if (p > 0.05) "normal" else "non-normal",
    outliers = .outliers_iqr(x)
  )

}

#' Full distribution profile of a pooled numeric vector
#'
#' Shape statistics plus the location/spread summaries that the
#' \code{PreprocessingRecipe} expected-quality slots are built from.
#'
#' @keywords internal
.distribution_profile <- function(x) {

  x <- x[is.finite(x)]

  c(

    .compute_shape_stats(x),

    list(
      mean = if (length(x) > 0) mean(x) else NA_real_,
      sd = if (length(x) > 1) stats::sd(x) else NA_real_,
      variance = if (length(x) > 1) stats::var(x) else NA_real_
    )

  )

}

#' Compute zero, sign, and sparsity statistics
#'
#' @keywords internal
#' @noRd
.compute_composition_stats <- function(m) {

  x <- as.numeric(m)
  n <- length(x)

  if (n == 0) {

    return(list(zero_percent = NA_real_, negative_percent = NA_real_,
                positive_percent = NA_real_, infinite_percent = NA_real_,
                sparsity = NA_real_, density = NA_real_))

  }

  n_inf <- sum(is.infinite(x))
  xf <- x[is.finite(x)]
  nf <- length(xf)

  zero_pct <- if (nf > 0) 100 * sum(xf == 0) / nf else NA_real_
  neg_pct <- if (nf > 0) 100 * sum(xf < 0) / nf else NA_real_
  pos_pct <- if (nf > 0) 100 * sum(xf > 0) / nf else NA_real_
  inf_pct <- 100 * n_inf / n

  list(
    zero_percent = zero_pct,
    negative_percent = neg_pct,
    positive_percent = pos_pct,
    infinite_percent = inf_pct,
    sparsity = if (!is.na(zero_pct)) zero_pct / 100 else NA_real_,
    density = if (!is.na(zero_pct)) 1 - zero_pct / 100 else NA_real_
  )

}

# =============================================================================
# Missing values
# =============================================================================

#' Compute missingness statistics for a block
#'
#' @keywords internal
#' @noRd
.compute_missing_stats <- function(block) {

  m <- is.na(block)

  total_na <- sum(m)
  total_cells <- length(m)

  by_sample <- rowSums(m)
  by_feature <- colSums(m)

  list(
    missing_values = total_na,
    missing_percent = 100 * total_na / max(total_cells, 1),
    missing_by_sample = stats::setNames(
      100 * by_sample / max(ncol(block), 1),
      rownames(block)
    ),
    missing_by_feature = stats::setNames(
      100 * by_feature / max(nrow(block), 1),
      colnames(block)
    ),
    empty_samples = rownames(block)[by_sample == ncol(block) & ncol(block) > 0],
    empty_features = colnames(block)[by_feature == nrow(block) & nrow(block) > 0]
  )

}

# =============================================================================
# Constant / near-constant / duplicated
# =============================================================================

#' Detect constant features in a block
#'
#' @keywords internal
#' @noRd
.constant_features <- function(block) {

  idx <- vapply(block, function(x) length(unique(stats::na.omit(x))) <= 1, logical(1))

  names(block)[idx]

}

#' Detect near-constant features above a threshold
#'
#' @keywords internal
#' @noRd
.near_constant_features <- function(block, threshold = 0.95) {

  const <- .constant_features(block)

  out <- character(0)

  for (v in names(block)) {

    if (v %in% const) next

    x <- stats::na.omit(block[[v]])

    if (length(x) <= 1) next

    p <- max(table(x)) / length(x)

    if (isTRUE(p > threshold)) out <- c(out, v)

  }

  out

}

#' Detect duplicated feature columns
#'
#' @keywords internal
#' @noRd
.duplicated_features <- function(block) {

  if (ncol(block) < 2) return(character(0))

  sig <- vapply(block, function(x) paste(as.character(x), collapse = "\r"), character(1))

  names(block)[duplicated(sig)]

}

#' Detect duplicated sample rows
#'
#' @keywords internal
#' @noRd
.duplicated_samples <- function(block) {

  if (nrow(block) < 2) return(character(0))

  # A duplicated sample means the same specimen was measured twice, and the
  # evidence for that is an identical profile across many features. With one
  # or two low-cardinality variables identical rows are the expected outcome,
  # not an anomaly: a single genotype column taking three values makes almost
  # every row a "duplicate" of another, and acting on that removes nearly the
  # whole cohort.

  if (ncol(block) < 3) return(character(0))

  sig <- apply(block, 1, function(r) paste(as.character(r), collapse = "\r"))

  ids <- rownames(block)

  if (is.null(ids)) ids <- as.character(seq_len(nrow(block)))

  duplicated_ids <- ids[duplicated(sig)]

  # Even with enough columns, a block of coarse categories produces repeats by
  # chance. Genuine re-measurement affects a handful of samples; when a large
  # share of the cohort looks duplicated the block is simply low-resolution,
  # and the finding is about the variables rather than the samples.

  if (length(duplicated_ids) > 0.2 * nrow(block)) return(character(0))

  duplicated_ids

}

# =============================================================================
# Multivariate: correlation, covariance, PCA
# =============================================================================

#' Compute covariance, correlation, and PCA
#'
#' @keywords internal
#' @noRd
.compute_multivariate <- function(block, max_features = 200) {

  m <- .numeric_matrix(block)

  out <- list(covariance = NULL, correlation = NULL,
              pca = NULL, explained_variance = NULL)

  if (ncol(m) < 2 || nrow(m) < 3) return(out)

  if (ncol(m) > max_features) {

    v <- apply(m, 2, stats::var, na.rm = TRUE)
    keep <- order(v, decreasing = TRUE)[seq_len(max_features)]
    m <- m[, keep, drop = FALSE]

  }

  out$covariance <- .safe_try(stats::cov(m, use = "pairwise.complete.obs"))
  out$correlation <- .safe_try(stats::cor(m, use = "pairwise.complete.obs"))

  complete <- stats::complete.cases(m)
  mm <- m[complete, , drop = FALSE]

  keep_cols <- apply(mm, 2, function(x) isTRUE(stats::sd(x) > 0))
  mm <- mm[, keep_cols, drop = FALSE]

  if (nrow(mm) >= 3 && ncol(mm) >= 2) {

    pca <- .safe_try(stats::prcomp(mm, center = TRUE, scale. = TRUE))

    if (!is.null(pca)) {

      out$pca <- pca
      vars <- pca$sdev^2
      out$explained_variance <- vars / sum(vars)

    }

  }

  out

}

# =============================================================================
# Outcome discovery (used by the transformation benchmark)
# =============================================================================

#' Discover survival outcome columns in metadata
#'
#' @keywords internal
#' @noRd
.discover_outcome <- function(object, block_name) {

  if (is.null(object$metadata)) return(NULL)

  metadata <- object$metadata

  if (!("sample_id" %in% colnames(metadata))) return(NULL)

  block <- object$assays[[block_name]]

  ids <- rownames(block)

  if (is.null(ids)) return(NULL)

  meta_ord <- metadata[match(ids, metadata$sample_id), , drop = FALSE]

  time_col <- .find_metadata_column(metadata, c("^time$", "time_to_event", "os_time", "surv_time"))
  status_col <- .find_metadata_column(metadata, c("^status$", "^event$", "^death$", "os_status"))

  if (!is.null(time_col) && !is.null(status_col)) {

    tt <- suppressWarnings(as.numeric(meta_ord[[time_col]]))
    ss <- suppressWarnings(as.numeric(meta_ord[[status_col]]))

    if (sum(!is.na(tt) & !is.na(ss)) >= 10) {

      return(list(type = "survival", time = tt, status = ss))

    }

  }

  y_col <- .find_metadata_column(metadata, c("^outcome$", "^y$", "^response$", "^target$", "^label$"))

  if (!is.null(y_col)) {

    y <- meta_ord[[y_col]]

    if (sum(!is.na(y)) >= 10) {

      if (length(unique(stats::na.omit(y))) <= 2) {

        return(list(type = "binary", y = as.numeric(as.factor(y)) - 1))

      }

      yv <- suppressWarnings(as.numeric(y))

      if (sum(!is.na(yv)) >= 10) return(list(type = "continuous", y = yv))

    }

  }

  NULL

}

# =============================================================================
# .build_block_diagnostics()
# =============================================================================

#' Build diagnostics summary for one block
#'
#' @keywords internal
#' @noRd
.build_block_diagnostics <- function(block_name, block, modality = "unknown") {

  d <- BlockDiagnostics()

  d$block <- block_name
  d$data_type <- .detect_data_type(block)
  d$modality <- if (is.null(modality)) "unknown" else modality
  d$objective <- "descriptive"
  d$samples <- nrow(block)
  d$features <- ncol(block)
  d$observations <- nrow(block) * ncol(block)

  miss <- .compute_missing_stats(block)

  d$missing_values <- miss$missing_values
  d$missing_percent <- miss$missing_percent
  d$missing_by_sample <- miss$missing_by_sample
  d$missing_by_feature <- miss$missing_by_feature
  d$empty_samples <- miss$empty_samples
  d$empty_features <- miss$empty_features

  m <- .numeric_matrix(block)
  comp <- .compute_composition_stats(m)

  d$zero_percent <- comp$zero_percent
  d$negative_percent <- comp$negative_percent
  d$positive_percent <- comp$positive_percent
  d$infinite_percent <- comp$infinite_percent
  d$sparsity <- comp$sparsity
  d$density <- comp$density

  pooled <- .pool_numeric(block)
  basic <- .compute_basic_stats(pooled)

  d$mean <- basic$mean
  d$median <- basic$median
  d$variance <- basic$variance
  d$sd <- basic$sd
  d$mad <- basic$mad
  d$cv <- basic$cv
  d$iqr <- basic$iqr
  d$minimum <- basic$minimum
  d$maximum <- basic$maximum
  d$range <- basic$range
  d$quantiles <- basic$quantiles

  shape <- .compute_shape_stats(pooled)

  d$skewness <- shape$skewness
  d$kurtosis <- shape$kurtosis
  d$shapiro <- shape$shapiro
  d$normality <- shape$normality
  d$outliers <- shape$outliers

  d$constant_features <- .constant_features(block)
  d$near_constant_features <- .near_constant_features(block)
  d$duplicated_features <- .duplicated_features(block)
  d$duplicated_samples <- .duplicated_samples(block)

  mv <- .compute_multivariate(block)

  d <- .set_slot(d, "covariance", mv$covariance)
  d <- .set_slot(d, "correlation", mv$correlation)
  d <- .set_slot(d, "pca", mv$pca)
  d <- .set_slot(d, "explained_variance", mv$explained_variance)

  d$statistics <- list(
    numeric_features = length(.numeric_columns(block)),
    non_numeric_features = ncol(block) - length(.numeric_columns(block)),
    pooled_n = length(pooled)
  )

  d

}

# =============================================================================
# Candidate transformations per data type
# =============================================================================

#' List candidate transformations for a data type
#'
#' @keywords internal
#' @noRd
.candidate_transformations <- function(data_type) {

  switch(

    data_type,

    continuous = c("identity", "yeojohnson", "boxcox", "log", "sqrt", "rank", "quantile", "robust"),
    count = c("identity", "log", "log2", "log10", "sqrt", "cuberoot", "vst", "rank"),
    proportion = c("identity", "sqrt", "yeojohnson", "rank", "quantile", "robust"),
    compositional = c("identity", "clr", "alr", "ilr"),
    binary = c("identity"),
    ordinal = c("identity", "rank"),
    censored = c("identity"),
    mixed = c("identity", "rank", "quantile", "robust"),

    c("identity")

  )

}

# =============================================================================
# Transformation implementations
# =============================================================================

#' Shift values to be strictly positive
#'
#' @keywords internal
#' @noRd
.t_shift_positive <- function(x) {

  mn <- min(x, na.rm = TRUE)

  if (isTRUE(mn <= 0)) x <- x - mn + 1

  x

}

#' Log transform after positive shift
#'
#' @keywords internal
#' @noRd
.t_log <- function(x, base = exp(1)) log(.t_shift_positive(x), base = base)

#' Square root, floored at zero
#'
#' \code{na.rm} is deliberately left at its default: with \code{na.rm = TRUE}
#' \code{pmax()} does not skip missing values, it replaces them with the other
#' operand (0), which would silently impute every \code{NA} as zero.
#'
#' @keywords internal
.t_sqrt <- function(x) sqrt(pmax(x, 0))

#' Signed cube root transform
#'
#' @keywords internal
#' @noRd
.t_cuberoot <- function(x) sign(x) * abs(x)^(1 / 3)

#' Anscombe variance-stabilizing transform
#'
#' See \code{.t_sqrt()} for why \code{na.rm} is not passed to \code{pmax()}.
#'
#' @keywords internal
.t_vst <- function(x) 2 * sqrt(pmax(x, 0) + 3 / 8)

#' Rank transform with averaged ties
#'
#' @keywords internal
#' @noRd
.t_rank <- function(x) rank(x, na.last = "keep", ties.method = "average")

#' Quantile-normalize via rank to normal
#'
#' @keywords internal
#' @noRd
.t_quantile <- function(x) {

  r <- rank(x, na.last = "keep", ties.method = "average")
  n <- sum(!is.na(x))

  stats::qnorm((r - 0.5) / n)

}

#' Median-MAD robust standardization
#'
#' @keywords internal
#' @noRd
.t_robust <- function(x) {

  med <- stats::median(x, na.rm = TRUE)
  mad <- stats::mad(x, na.rm = TRUE)

  if (!isTRUE(mad > 0)) mad <- 1

  (x - med) / mad

}

#' Box-Cox transform with optimized lambda
#'
#' @keywords internal
#' @noRd
.t_boxcox <- function(x) {

  x <- .t_shift_positive(x)

  ll <- function(lambda) {

    n <- length(x)

    if (abs(lambda) < 1e-6) {

      xt <- log(x)

    } else {

      xt <- (x^lambda - 1) / lambda

    }

    -n / 2 * log(stats::var(xt)) + (lambda - 1) * sum(log(x))

  }

  lambda <- stats::optimize(ll, interval = c(-2, 2), maximum = TRUE)$maximum

  list(
    x = if (abs(lambda) < 1e-6) log(x) else (x^lambda - 1) / lambda,
    lambda = lambda
  )

}

#' Yeo-Johnson transform with optimized lambda
#'
#' @keywords internal
#' @noRd
.t_yeojohnson <- function(x) {

  ll <- function(lambda) {

    n <- length(x)
    xt <- vapply(x, .yj_transform, numeric(1), lambda = lambda)

    -n / 2 * log(stats::var(xt)) +
      (lambda - 1) * sum(sign(x) * log(abs(x) + 1))

  }

  lambda <- stats::optimize(ll, interval = c(-2, 2), maximum = TRUE)$maximum

  list(x = vapply(x, .yj_transform, numeric(1), lambda = lambda), lambda = lambda)

}

#' Apply Yeo-Johnson formula to one value
#'
#' @keywords internal
#' @noRd
.yj_transform <- function(x, lambda) {

  if (is.na(x)) return(NA_real_)

  if (x >= 0) {

    if (abs(lambda) < 1e-6) log(x + 1) else ((x + 1)^lambda - 1) / lambda

  } else {

    if (abs(lambda - 2) < 1e-6) -log(-x + 1) else -((-x + 1)^(2 - lambda) - 1) / (2 - lambda)

  }

}

#' Centered log-ratio transform of a matrix
#'
#' @keywords internal
#' @noRd
.t_clr <- function(m) {

  m <- as.matrix(m)
  m[m <= 0] <- min(m[m > 0], na.rm = TRUE) / 2
  g <- exp(rowMeans(log(m), na.rm = TRUE))

  log(m / g)

}

#' Additive log-ratio transform of a matrix
#'
#' @keywords internal
#' @noRd
.t_alr <- function(m) {

  m <- as.matrix(m)
  m[m <= 0] <- min(m[m > 0], na.rm = TRUE) / 2
  ref <- m[, ncol(m)]

  log(m[, -ncol(m), drop = FALSE] / ref)

}

#' Isometric log-ratio transform of a matrix
#'
#' @keywords internal
#' @noRd
.t_ilr <- function(m) {

  clr <- .t_clr(m)
  d <- ncol(clr)

  if (d < 2) return(clr)

  V <- matrix(0, nrow = d, ncol = d - 1)

  for (i in seq_len(d - 1)) {

    V[1:i, i] <- 1 / i
    V[i + 1, i] <- -1
    V[, i] <- V[, i] * sqrt(i / (i + 1))

  }

  clr %*% V

}

#' Apply a named transformation to a data block
#'
#' Only numeric columns are transformed; every other column is left
#' untouched. Row and column names are preserved.
#'
#' @keywords internal

.apply_transformation <- function(block, method) {

  nm <- .numeric_columns(block)

  info <- list(method = method, parameters = list(), messages = character())

  if (length(nm) == 0 || method == "identity") {

    return(list(block = block, info = info))

  }

  out <- block

  if (method %in% c("clr", "alr", "ilr")) {

    m <- .numeric_matrix(block)

    tm <- switch(
      method,
      clr = .t_clr(m),
      alr = .t_alr(m),
      ilr = .t_ilr(m)
    )

    if (method == "alr") {

      keep <- nm[-length(nm)]

    } else if (method == "ilr") {

      keep <- paste0("ILR", seq_len(ncol(tm)))
      colnames(tm) <- keep

      for (v in nm) out[[v]] <- NULL

      for (j in seq_len(ncol(tm))) out[[keep[j]]] <- tm[, j]

      return(list(block = out, info = info))

    } else {

      keep <- nm

    }

    for (j in seq_along(keep)) out[[keep[j]]] <- tm[, j]

    if (method == "alr") out[[nm[length(nm)]]] <- NULL

    return(list(block = out, info = info))

  }

  lambda_store <- list()

  for (v in nm) {

    x <- block[[v]]

    res_v <- .safe_try(

      switch(

        method,
        log = list(x = .t_log(x), lambda = NULL),
        log2 = list(x = .t_log(x, base = 2), lambda = NULL),
        log10 = list(x = .t_log(x, base = 10), lambda = NULL),
        sqrt = list(x = .t_sqrt(x), lambda = NULL),
        cuberoot = list(x = .t_cuberoot(x), lambda = NULL),
        vst = list(x = .t_vst(x), lambda = NULL),
        rank = list(x = .t_rank(x), lambda = NULL),
        quantile = list(x = .t_quantile(x), lambda = NULL),
        robust = list(x = .t_robust(x), lambda = NULL),
        boxcox = {
          r <- .t_boxcox(x)
          list(x = r$x, lambda = r$lambda)
        },
        yeojohnson = {
          r <- .t_yeojohnson(x)
          list(x = r$x, lambda = r$lambda)
        },
        list(x = x, lambda = NULL)

      ),

      default = list(x = x, lambda = NULL)

    )

    if (length(res_v$x) == length(x)) out[[v]] <- res_v$x

    if (!is.null(res_v$lambda)) lambda_store[[v]] <- res_v$lambda

  }

  info$parameters <- lambda_store

  list(block = out, info = info)

}

# =============================================================================
# Transformation metrics
# =============================================================================

#' Compute quality metrics for a transformation
#'
#' @keywords internal
#' @noRd
.transformation_metrics <- function(before, after) {

  before <- before[is.finite(before)]
  after <- after[is.finite(after)]

  shape <- .compute_shape_stats(after)

  # The two pools only correspond cell by cell when the transformation kept
  # the shape of the block. Log-ratio transforms (alr, ilr) drop a column, and
  # dropping non-finite values can shorten one side on its own, so comparing
  # by position would correlate unrelated cells and report a confident number
  # for something that was never measured. Truncating to the shorter vector
  # hides that; refusing to answer does not. A neutral 0.5 is substituted by
  # .composite_score(), which keeps the ranking driven by the other metrics.

  order_pres <- if (length(before) == length(after) && length(after) >= 3) {

    .safe_try(stats::cor(before, after,
                          method = "spearman", use = "pairwise.complete.obs"),
              NA_real_)

  } else NA_real_

  cv_after <- if (isTRUE(mean(after) != 0)) stats::sd(after) / mean(after) else NA_real_
  stability <- if (is.na(cv_after)) 0.5 else 1 / (1 + abs(cv_after))

  list(
    skewness = shape$skewness,
    kurtosis = shape$kurtosis,
    variance = if (length(after) > 1) stats::var(after) else NA_real_,
    outliers_pct = 100 * length(shape$outliers) / max(length(after), 1),
    normality_p = shape$shapiro,
    stability = stability,
    order_preservation = order_pres
  )

}

#' Composite score for ranking a transformation
#'
#' @keywords internal
#' @noRd
.composite_score <- function(metrics, outcome_perf = NA_real_) {

  skew_term <- 1 / (1 + abs(ifelse(is.na(metrics$skewness), 5, metrics$skewness)))
  norm_term <- ifelse(is.na(metrics$normality_p), 0.5, metrics$normality_p)
  out_term <- 1 - pmin(metrics$outliers_pct, 100) / 100
  stab_term <- ifelse(is.na(metrics$stability), 0.5, metrics$stability)
  ord_term <- ifelse(is.na(metrics$order_preservation), 0.5, abs(metrics$order_preservation))

  if (!is.na(outcome_perf)) {

    100 * (0.20 * skew_term + 0.15 * norm_term + 0.15 * out_term +
             0.10 * stab_term + 0.10 * ord_term + 0.30 * outcome_perf)

  } else {

    100 * (0.30 * skew_term + 0.25 * norm_term + 0.20 * out_term +
             0.15 * stab_term + 0.10 * ord_term)

  }

}

# =============================================================================
# Outcome-based relevance (used when an outcome is available)
# =============================================================================

#' Fold-averaged association between a transformed block and the outcome
#'
#' The sample is split into \code{k} folds and the feature-outcome
#' association is measured inside each fold, then averaged. Splitting keeps
#' the estimate from being dominated by a handful of influential samples, so
#' the result is a measure of how *stably* the association survives a given
#' transformation.
#'
#' This is deliberately NOT cross-validation: no model is fitted on the
#' training folds and evaluated on a held-out one. Every quantity below is
#' computed within the fold it is measured on, so it carries the optimistic
#' bias of an in-sample statistic and must not be read as out-of-sample
#' predictive performance. It is used only to rank transformations against
#' each other, where that bias applies equally to every candidate.
#'
#' @param m Numeric matrix of transformed features (rows = samples).
#' @param outcome Outcome descriptor from \code{.discover_outcome()}.
#'
#' @return A scalar in \code{[0, 1]}, or \code{NA_real_} when the association
#'   cannot be estimated.
#'
#' @keywords internal
.outcome_performance <- function(m, outcome) {

  if (is.null(outcome) || ncol(m) == 0) return(NA_real_)

  k <- 5
  n <- nrow(m)

  if (n < 2 * k) return(NA_real_)

  folds <- .with_preserved_seed(sample(rep(seq_len(k), length.out = n)))

  scores <- numeric(0)

  if (identical(outcome$type, "survival") && requireNamespace("survival", quietly = TRUE)) {

    time <- outcome$time
    status <- outcome$status

    for (f in seq_len(k)) {

      test_idx <- which(folds == f)

      if (length(test_idx) < 3) next

      cidx <- .safe_try({

        cvals <- apply(m[test_idx, , drop = FALSE], 2, function(col) {

          fit <- survival::coxph(
            survival::Surv(time[test_idx], status[test_idx]) ~ col
          )

          as.numeric(fit$concordance["concordance"])

        })

        mean(cvals, na.rm = TRUE)

      }, NA_real_)

      if (!is.na(cidx)) scores <- c(scores, abs(cidx - 0.5) * 2)

    }

  } else if (identical(outcome$type, "survival")) {

    time <- outcome$time
    status <- outcome$status

    for (f in seq_len(k)) {

      test_idx <- which(folds == f)

      cvals <- apply(m[test_idx, , drop = FALSE], 2, function(col) {

        .safe_try(stats::cor(col[status[test_idx] == 1],
                              time[test_idx][status[test_idx] == 1],
                              method = "spearman",
                              use = "pairwise.complete.obs"), NA_real_)

      })

      if (any(!is.na(cvals))) scores <- c(scores, mean(abs(cvals), na.rm = TRUE))

    }

  } else {

    y <- outcome$y

    for (f in seq_len(k)) {

      test_idx <- which(folds == f)

      cvals <- apply(m[test_idx, , drop = FALSE], 2, function(col) {

        .safe_try(stats::cor(col, y[test_idx], use = "pairwise.complete.obs"), NA_real_)

      })

      if (any(!is.na(cvals))) scores <- c(scores, mean(abs(cvals), na.rm = TRUE))

    }

  }

  if (length(scores) == 0) return(NA_real_)

  mean(scores, na.rm = TRUE)

}

# =============================================================================
# .benchmark_transformations()
# =============================================================================

#' Benchmark candidate transformations for one data block
#' @keywords internal

.benchmark_transformations <- function(block_name, block, data_type, outcome = NULL) {

  tr <- TransformationRecommendation()

  tr$block <- block_name
  tr$detected_type <- data_type
  tr$automatic <- TRUE
  tr$objective <- if (is.null(outcome)) "unsupervised" else "supervised"

  candidates <- .candidate_transformations(data_type)
  tr$candidates <- candidates

  before_pooled <- .pool_numeric(block)

  rows <- list()
  msgs <- character(0)
  errs <- character(0)

  for (method in candidates) {

    res <- .safe_try(.apply_transformation(block, method), NULL)

    if (is.null(res)) {

      errs <- c(errs, sprintf("Transformation '%s' failed and was skipped.", method))
      next

    }

    after_pooled <- .pool_numeric(res$block)

    if (length(after_pooled) == 0) {

      msgs <- c(msgs, sprintf("Transformation '%s' produced no numeric output.", method))
      next

    }

    metrics <- .transformation_metrics(before_pooled, after_pooled)

    outcome_perf <- NA_real_

    if (!is.null(outcome)) {

      m <- .numeric_matrix(res$block)

      if (identical(outcome$type, "survival")) {

        ok <- is.finite(outcome$time) & is.finite(outcome$status)

      } else {

        ok <- is.finite(outcome$y)

      }

      m <- m[ok, , drop = FALSE]

      out_sub <- if (identical(outcome$type, "survival")) {

        list(type = "survival", time = outcome$time[ok], status = outcome$status[ok])

      } else {

        list(type = outcome$type, y = outcome$y[ok])

      }

      outcome_perf <- .outcome_performance(m, out_sub)

    }

    score <- .composite_score(metrics, outcome_perf)

    rows[[method]] <- data.frame(
      transformation = method,
      score = score,
      skewness = metrics$skewness,
      kurtosis = metrics$kurtosis,
      variance = metrics$variance,
      outliers_pct = metrics$outliers_pct,
      normality_p = metrics$normality_p,
      stability = metrics$stability,
      order_preservation = metrics$order_preservation,
      outcome_performance = outcome_perf,
      stringsAsFactors = FALSE
    )

  }

  if (length(rows) == 0) {

    tr$recommended <- "identity"
    tr$score <- NA_real_
    tr$justification <- "No candidate transformation could be evaluated; defaulting to identity."
    tr$errors <- errs
    tr$messages <- msgs

    return(tr)

  }

  metrics_df <- do.call(rbind, rows)
  rownames(metrics_df) <- NULL

  ranking_df <- metrics_df[order(-metrics_df$score), , drop = FALSE]
  ranking_df$rank <- seq_len(nrow(ranking_df))
  rownames(ranking_df) <- NULL

  tr$metrics <- metrics_df
  tr$ranking <- ranking_df
  tr$recommended <- ranking_df$transformation[1]
  tr$score <- ranking_df$score[1]

  best <- ranking_df[1, ]

  justification <- character(0)

  justification <- c(
    justification,
    sprintf("Highest composite score (%.1f/100) among %d candidate transformation(s).",
            best$score, nrow(ranking_df))
  )

  if (!is.na(best$normality_p) && best$normality_p > 0.05) {

    justification <- c(justification, "Residual distribution is compatible with normality after transformation.")

  }

  if (!is.na(best$outliers_pct) && best$outliers_pct < 5) {

    justification <- c(justification, sprintf("Low outlier rate after transformation (%.1f%%).", best$outliers_pct))

  }

  if (!is.na(best$outcome_performance)) {

    justification <- c(justification,
                        sprintf(paste("Association with the outcome is retained across folds",
                                      "(mean in-fold score = %.2f; not an out-of-sample estimate)."),
                                best$outcome_performance))

  }

  tr$justification <- justification

  best_res <- .apply_transformation(block, tr$recommended)

  tr$model <- tr$recommended
  tr$parameters <- best_res$info$parameters

  tr$before <- .distribution_profile(before_pooled)
  tr$after <- .distribution_profile(.pool_numeric(best_res$block))

  tr$warnings <- character(0)
  tr$errors <- errs
  tr$messages <- msgs

  tr

}

# =============================================================================
# Quality scoring
# =============================================================================

#' Compute a 0-100 quality score for one block
#' @keywords internal

.compute_quality_score <- function(diagnostic) {

  features <- max(diagnostic$features, 1)
  samples <- max(diagnostic$samples, 1)
  observations <- max(diagnostic$observations, 1)

  missing_pct <- ifelse(is.na(diagnostic$missing_percent), 0, diagnostic$missing_percent)

  frac_constant <- length(diagnostic$constant_features) / features
  frac_near_constant <- length(diagnostic$near_constant_features) / features
  frac_dup_features <- length(diagnostic$duplicated_features) / features
  frac_dup_samples <- length(diagnostic$duplicated_samples) / samples

  outlier_rate <- length(diagnostic$outliers) / observations

  normality_bonus <- switch(
    diagnostic$normality,
    normal = 1,
    "non-normal" = 0.5,
    0.6
  )

  penalty <-
    missing_pct * 0.40 +
    frac_constant * 100 * 0.15 +
    frac_near_constant * 100 * 0.10 +
    frac_dup_features * 100 * 0.10 +
    frac_dup_samples * 100 * 0.05 +
    outlier_rate * 100 * 0.15 +
    (1 - normality_bonus) * 10

  score <- 100 - penalty

  max(0, min(100, score))

}

# =============================================================================
# Recipe coherence
# =============================================================================

#' Intended order of the preprocessing stages for one data type
#'
#' \code{PreprocessingRecipe} stores which method to use at every stage but
#' not the order in which the stages must run. For count data the library
#' size has to be normalized before any variance-stabilizing transformation
#' is applied; for every other data type the transformation comes first.
#' The resolved order is recorded in the recipe comments so that the plan
#' remains self-describing.
#'
#' @keywords internal

.stage_order <- function(data_type) {

  # Library size has to be normalized before any variance-stabilizing
  # transformation for counts; every other type transforms first.

  core <- if (identical(data_type, "count")) {

    c("normalization", "transformation")

  } else {

    c("transformation", "normalization")

  }

  # Structural removal (constant, duplicated, empty) runs first because it is
  # invariant to everything downstream. Statistical filtering ("filter":
  # variance, abundance, prevalence) runs late on purpose: the variance of
  # raw counts carries no meaning until the block has been normalized and
  # transformed onto a comparable scale.

  c(
    "remove_samples",
    "remove_features",
    "imputation",
    core,
    "scaling",
    "batch",
    "outliers",
    "filter",
    "feature_selection"
  )

}

#' Resolve the normalization stage against the chosen transformation
#'
#' Rank-based transformations already impose a common distribution on every
#' feature, so following them with quantile normalization would discard the
#' transformation selected by the benchmark. Compositional and proportion
#' data are closed by construction and need no normalization.
#'
#' @keywords internal

.resolve_normalization <- function(data_type, features, transformation,
                                   modality = "unknown") {

  transformation <- if (is.null(transformation)) "identity" else transformation
  modality <- if (is.null(modality)) "unknown" else modality

  # The modality, when the user declared it, is more informative than the
  # statistical type: RNA-seq counts and any other counts are identical as
  # numbers but call for different normalization families.

  by_modality <- switch(

    modality,

    rnaseq = "tmm",
    proteomics = if (features > 20) "quantile" else "median",
    metabolomics = "pqn",
    microbiome = "total_sum_scaling",
    methylation = "none",

    NULL

  )

  by_type <- switch(

    data_type,

    count = "total_sum_scaling",
    compositional = "none",
    proportion = "none",
    continuous = if (features > 20) "quantile" else "none",
    mixed = if (features > 20) "quantile" else "none",

    "none"

  )

  # Size-factor methods divide a sample by its own total, median or trimmed
  # mean, all of which assume non-negative magnitudes. A declared modality
  # does not make already-centred or negative data eligible for them, so the
  # modality preference is dropped rather than the type constraint.

  magnitude_based <- c("tmm", "rle", "cpm", "total_sum_scaling", "tic", "pqn")

  if (!is.null(by_modality) &&
      by_modality %in% magnitude_based &&
      !(data_type %in% c("count", "compositional", "proportion"))) {

    by_modality <- NULL

  }

  normalization <- if (!is.null(by_modality)) by_modality else by_type

  # For count data the normalization runs BEFORE the transformation (see
  # .stage_order), so no transformation can invalidate it. For every other
  # type the order is reversed, and a transformation that discards the
  # original magnitude scale leaves nothing for a magnitude-based
  # normalization to work on: dividing centred log-ratios by their row sum,
  # or re-normalizing ranks, is arithmetic without meaning.

  if (identical(data_type, "count")) return(normalization)

  scale_free <- c("rank", "quantile", "robust", "clr", "alr", "ilr")

  if (transformation %in% scale_free &&
      normalization %in% c(magnitude_based, "median")) {

    return("none")

  }

  # Rank-based transformations already impose a common distribution on every
  # feature; quantile normalization on top would discard the transformation
  # the benchmark selected.

  if (identical(normalization, "quantile") &&
      transformation %in% c("rank", "quantile")) {

    return("none")

  }

  normalization

}

#' Resolve the scaling stage against the chosen transformation
#'
#' Quantile and robust transformations already return centred, unit-spread
#' features, so an additional scaling step would be redundant.
#'
#' @keywords internal

.resolve_scaling <- function(data_type, transformation, outlier_rate) {

  transformation <- if (is.null(transformation)) "identity" else transformation

  if (transformation %in% c("quantile", "robust")) return("none")

  if (!(data_type %in% c("continuous", "mixed", "count", "compositional", "proportion"))) {

    return("none")

  }

  if (isTRUE(outlier_rate > 0.05)) "robust" else "z-score"

}

# =============================================================================
# .build_recipe()
# =============================================================================

#' Build an automatic PreprocessingRecipe for one block
#' @keywords internal

.build_recipe <- function(block_name, diagnostic, transformation,
                          metadata = NULL, modality = "unknown") {

  modality <- if (is.null(modality)) "unknown" else modality

  r <- PreprocessingRecipe()

  r$block <- block_name
  r$enabled <- TRUE
  r$automatic <- TRUE
  r$data_type <- diagnostic$data_type
  r$modality <- modality
  r$objective <- sprintf("Prepare '%s' (%s data) for downstream integration.",
                          block_name, diagnostic$data_type)
  r$priority <- 1L
  r$stage_order <- .stage_order(diagnostic$data_type)
  r$comments <- c(
    "Recipe generated automatically by check_data().",
    sprintf(
      "Intended stage order: %s",
      paste(r$stage_order, collapse = " -> ")
    )
  )

  # ---------------------------------------------------------------------------
  # Filtering
  # ---------------------------------------------------------------------------

  r$remove_empty_samples <- length(diagnostic$empty_samples) > 0
  r$remove_duplicate_samples <- length(diagnostic$duplicated_samples) > 0
  r$sample_missing_threshold <- 0.50

  r$remove_empty_features <- length(diagnostic$empty_features) > 0
  r$remove_constant_features <- length(diagnostic$constant_features) > 0
  r$remove_near_constant_features <- length(diagnostic$near_constant_features) > 0
  r$remove_duplicate_features <- length(diagnostic$duplicated_features) > 0
  r$feature_missing_threshold <- 0.30

  r <- .set_slot(
    r, "variance_threshold",
    if (diagnostic$data_type %in% c("continuous", "mixed")) 1e-4 else NULL
  )

  r <- .set_slot(
    r, "abundance_threshold",
    if (diagnostic$data_type %in% c("count", "compositional")) 1e-6 else NULL
  )

  # ---------------------------------------------------------------------------
  # Imputation
  # ---------------------------------------------------------------------------

  mp <- ifelse(is.na(diagnostic$missing_percent), 0, diagnostic$missing_percent)

  if (mp == 0) {

    r$imputation <- "none"
    r$imputation_parameters <- list()

  } else if (diagnostic$data_type %in% c("count", "compositional")) {

    r$imputation <- "pseudocount"
    r$imputation_parameters <- list(pseudocount = 1)

  } else if (diagnostic$data_type %in% c("binary", "ordinal")) {

    r$imputation <- "mode"
    r$imputation_parameters <- list()

  } else if (mp < 5) {

    r$imputation <- "median"
    r$imputation_parameters <- list()

  } else {

    # knn also covers heavy missingness. Multiple imputation (mice) used to
    # be recommended here, but it cannot be replayed: it draws several
    # completed datasets rather than fitting a model that transfers to new
    # samples, so a pipeline built on it is not reproducible on an external
    # set. Multiple imputation belongs to the analysis stage, downstream of
    # this one.

    r$imputation <- "knn"
    r$imputation_parameters <- list(k = if (mp < 20) 5 else 10)

  }

  r <- .set_slot(r, "imputation_model", NULL)

  # ---------------------------------------------------------------------------
  # Transformation
  # ---------------------------------------------------------------------------

  r$transformation <- transformation$recommended
  r$transformation_candidates <- transformation$candidates
  r <- .set_slot(r, "transformation_model", transformation$model)
  r$transformation_parameters <- transformation$parameters
  r$transformation_score <- transformation$score
  r$transformation_justification <- transformation$justification

  # ---------------------------------------------------------------------------
  # Normalization
  # ---------------------------------------------------------------------------

  r$normalization <- .resolve_normalization(
    diagnostic$data_type,
    diagnostic$features,
    r$transformation,
    modality
  )

  r$normalization_parameters <- list()
  r <- .set_slot(r, "normalization_model", NULL)

  # ---------------------------------------------------------------------------
  # Scaling
  # ---------------------------------------------------------------------------

  r$scaling <- .resolve_scaling(
    diagnostic$data_type,
    r$transformation,
    length(diagnostic$outliers) / max(diagnostic$observations, 1)
  )

  r$scaling_parameters <- list()
  r <- .set_slot(r, "scaling_model", NULL)

  # ---------------------------------------------------------------------------
  # Batch correction
  # ---------------------------------------------------------------------------

  batch_col <- .find_metadata_column(metadata, c("^batch$", "^plate$", "^batch_id$", "^center$"))

  if (!is.null(batch_col)) {

    r$batch <- "mean_centering"
    r$batch_variable <- batch_col
    r$batch_parameters <- list()

  } else {

    r <- .set_slot(r, "batch", NULL)
    r <- .set_slot(r, "batch_variable", NULL)
    r$batch_parameters <- list()

  }

  r <- .set_slot(r, "batch_model", NULL)

  # ---------------------------------------------------------------------------
  # Feature selection
  # ---------------------------------------------------------------------------

  if (diagnostic$features > 500) {

    r$feature_selection <- "variance_filter"
    r$feature_selection_parameters <- list(top_n = min(500, diagnostic$features))

  } else {

    r <- .set_slot(r, "feature_selection", NULL)
    r$feature_selection_parameters <- list()

  }

  r <- .set_slot(r, "feature_selection_model", NULL)

  # ---------------------------------------------------------------------------
  # Dimensionality reduction
  # ---------------------------------------------------------------------------

  if (diagnostic$features > 20 && diagnostic$data_type %in% c("continuous", "mixed", "count", "compositional")) {

    r$dimensionality_reduction <- "PCA"
    r$dimensionality_parameters <- list(n_components = min(10, diagnostic$features - 1))

  } else {

    r <- .set_slot(r, "dimensionality_reduction", NULL)
    r$dimensionality_parameters <- list()

  }

  r <- .set_slot(r, "dimensionality_model", NULL)

  # ---------------------------------------------------------------------------
  # Expected quality
  # ---------------------------------------------------------------------------

  r$estimated_quality <- .compute_quality_score(diagnostic)
  r$expected_missing <- 0
  r$expected_variance <- if (!is.null(transformation$after$variance)) transformation$after$variance else NA_real_
  r$expected_normality <- if (!is.null(transformation$after$normality)) transformation$after$normality else NA_character_

  r

}

# =============================================================================
# Quality control checks
# =============================================================================

#' Compute passed, failed, and skipped QC checks
#'
#' @keywords internal
#' @noRd
.compute_qc_checks <- function(validation, dataset_score, has_outcome) {

  passed <- character(0)
  failed <- character(0)
  skipped <- character(0)

  if (isTRUE(validation$valid)) {

    passed <- c(passed, "structure_valid")

  } else {

    failed <- c(failed, "structure_valid")

  }

  # Every comparison below goes through isTRUE()/.n_rows() so that a slot that
  # was never populated (a hand-built or early-returned CMOValidation) makes
  # the check fail rather than raising "missing value where TRUE/FALSE needed".

  total_missing_pct <- validation$summary$total_missing_percent

  if (isTRUE(total_missing_pct < 20)) {

    passed <- c(passed, "missing_threshold")

  } else {

    failed <- c(failed, "missing_threshold")

  }

  if (.n_rows(validation$details$duplicated_samples) == 0) {

    passed <- c(passed, "no_duplicated_samples")

  } else {

    failed <- c(failed, "no_duplicated_samples")

  }

  if (.n_rows(validation$details$duplicated_features) == 0) {

    passed <- c(passed, "no_duplicated_features")

  } else {

    failed <- c(failed, "no_duplicated_features")

  }

  if (isTRUE(dataset_score >= 50)) {

    passed <- c(passed, "quality_threshold")

  } else {

    failed <- c(failed, "quality_threshold")

  }

  overlap <- validation$summary$overlap

  if (!is.null(overlap) && nrow(overlap) > 1) {

    off_diag <- overlap[row(overlap) != col(overlap)]

    if (any(off_diag > 0)) {

      passed <- c(passed, "sample_overlap")

    } else {

      failed <- c(failed, "sample_overlap")

    }

  } else {

    skipped <- c(skipped, "sample_overlap")

  }

  if (!has_outcome) {

    skipped <- c(skipped, "outcome_availability")

  } else {

    passed <- c(passed, "outcome_availability")

  }

  list(
    passed = length(failed) == 0,
    score = dataset_score,
    failed_checks = failed,
    passed_checks = passed,
    skipped_checks = skipped
  )

}

# =============================================================================
# Plotting engine (base graphics, captured with recordPlot so nothing is
# printed to the current device)
# =============================================================================

#' Record a plot safely off-screen
#'
#' @keywords internal
#' @noRd
.safe_record <- function(plot_fn) {

  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Off-screen devices do not keep a display list by default, so recordPlot()
  # would return an empty recording that replays as a blank canvas.

  grDevices::dev.control(displaylist = "enable")

  ok <- tryCatch({ plot_fn(); TRUE }, error = function(e) FALSE)

  if (!ok) return(NULL)

  recorded <- grDevices::recordPlot()

  if (length(recorded[[1]]) == 0) return(NULL)

  recorded

}

#' Apply a function over all blocks
#'
#' @keywords internal
#' @noRd
.map_blocks <- function(assays, fn) {

  out <- lapply(names(assays), function(nm) fn(nm, assays[[nm]]))
  names(out) <- names(assays)

  out[!vapply(out, is.null, logical(1))]

}

#' Draw a labeled bar plot
#'
#' @keywords internal
#' @noRd
.plot_bar <- function(values, labels, main, ylab = "") {

  .safe_record(function() {

    graphics::par(mar = c(7, 4, 3, 1))

    graphics::barplot(
      values,
      names.arg = labels,
      main = main,
      ylab = ylab,
      las = 2,
      col = "#4C72B0",
      cex.names = 0.7
    )

  })

}

#' Plot missingness heatmap for a block
#'
#' @keywords internal
#' @noRd
.plot_missing_heatmap_block <- function(block_name, block) {

  if (nrow(block) == 0 || ncol(block) == 0) return(NULL)

  m <- is.na(block)

  .safe_record(function() {

    graphics::image(
      x = seq_len(ncol(m)), y = seq_len(nrow(m)), z = t(m),
      col = c("grey90", "firebrick"),
      axes = FALSE,
      xlab = "Features", ylab = "Samples",
      main = sprintf("Missing pattern: %s", block_name)
    )

  })

}

#' Plot missing percent by sample
#'
#' @keywords internal
#' @noRd
.plot_missing_by_sample_block <- function(block_name, block) {

  if (nrow(block) == 0) return(NULL)

  pct <- 100 * rowSums(is.na(block)) / max(ncol(block), 1)

  if (all(pct == 0)) return(NULL)

  ord <- order(pct, decreasing = TRUE)[seq_len(min(30, length(pct)))]

  .plot_bar(pct[ord], rownames(block)[ord],
            main = sprintf("Missing %% by sample: %s", block_name),
            ylab = "% missing")

}

#' Plot missing percent by feature
#'
#' @keywords internal
#' @noRd
.plot_missing_by_feature_block <- function(block_name, block) {

  if (ncol(block) == 0) return(NULL)

  pct <- 100 * colSums(is.na(block)) / max(nrow(block), 1)

  if (all(pct == 0)) return(NULL)

  ord <- order(pct, decreasing = TRUE)[seq_len(min(30, length(pct)))]

  .plot_bar(pct[ord], colnames(block)[ord],
            main = sprintf("Missing %% by feature: %s", block_name),
            ylab = "% missing")

}

#' Plot top missingness patterns by sample
#'
#' @keywords internal
#' @noRd
.plot_missing_pattern_block <- function(block_name, block) {

  if (nrow(block) == 0 || ncol(block) == 0) return(NULL)

  sig <- apply(is.na(block), 1, function(r) paste(as.integer(r), collapse = ""))
  tab <- sort(table(sig), decreasing = TRUE)

  if (length(tab) == 0) return(NULL)

  tab <- tab[seq_len(min(10, length(tab)))]

  .plot_bar(as.numeric(tab), paste0("P", seq_along(tab)),
            main = sprintf("Top missingness patterns: %s", block_name),
            ylab = "Samples")

}

#' Plot sample overlap heatmap between blocks
#'
#' @keywords internal
#' @noRd
.plot_overlap_heatmap <- function(overlap) {

  if (is.null(overlap) || nrow(overlap) == 0) return(NULL)

  .safe_record(function() {

    graphics::par(mar = c(7, 7, 3, 1))

    graphics::image(
      x = seq_len(ncol(overlap)), y = seq_len(nrow(overlap)), z = t(overlap),
      axes = FALSE, xlab = "", ylab = "",
      main = "Sample overlap between blocks",
      col = grDevices::hcl.colors(20, "Blues", rev = TRUE)
    )

    graphics::axis(1, at = seq_len(ncol(overlap)), labels = colnames(overlap), las = 2, cex.axis = 0.7)
    graphics::axis(2, at = seq_len(nrow(overlap)), labels = rownames(overlap), las = 2, cex.axis = 0.7)

  })

}

#' Plot block overlap as a network
#'
#' @keywords internal
#' @noRd
.plot_overlap_network <- function(overlap) {

  if (is.null(overlap) || nrow(overlap) < 2) return(NULL)

  n <- nrow(overlap)
  theta <- seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
  xy <- cbind(cos(theta), sin(theta))

  .safe_record(function() {

    graphics::par(mar = c(1, 1, 3, 1))

    graphics::plot(
      xy, type = "n", axes = FALSE, xlab = "", ylab = "",
      xlim = c(-1.4, 1.4), ylim = c(-1.4, 1.4),
      main = "Block overlap network", asp = 1
    )

    max_overlap <- max(overlap[row(overlap) != col(overlap)], 1)

    for (i in seq_len(n)) {

      for (j in seq_len(n)) {

        if (j <= i) next

        w <- overlap[i, j]

        if (w <= 0) next

        graphics::segments(
          xy[i, 1], xy[i, 2], xy[j, 1], xy[j, 2],
          lwd = 1 + 4 * w / max_overlap,
          col = grDevices::adjustcolor("#4C72B0", alpha.f = 0.6)
        )

      }

    }

    graphics::points(xy, pch = 21, bg = "#DD8452", col = "black", cex = 3)
    graphics::text(xy * 1.2, labels = rownames(overlap), cex = 0.8)

  })

}

#' Plot correlation heatmap for a block
#'
#' @keywords internal
#' @noRd
.plot_correlation_heatmap_block <- function(block_name, correlation) {

  if (is.null(correlation) || nrow(correlation) < 2) return(NULL)

  .safe_record(function() {

    graphics::par(mar = c(7, 7, 3, 1))

    graphics::image(
      x = seq_len(ncol(correlation)), y = seq_len(nrow(correlation)),
      z = t(correlation)[, rev(seq_len(nrow(correlation)))],
      axes = FALSE, xlab = "", ylab = "", zlim = c(-1, 1),
      main = sprintf("Correlation: %s", block_name),
      col = grDevices::hcl.colors(30, "Blue-Red")
    )

  })

}

#' Plot distribution of pairwise correlations
#'
#' @keywords internal
#' @noRd
.plot_correlation_distribution_block <- function(block_name, correlation) {

  if (is.null(correlation) || nrow(correlation) < 2) return(NULL)

  vals <- correlation[upper.tri(correlation)]

  .safe_record(function() {

    graphics::hist(
      vals, breaks = 20, col = "#55A868", border = "white",
      main = sprintf("Correlation distribution: %s", block_name),
      xlab = "Pearson correlation"
    )

  })

}

#' Plot histogram of pooled values
#'
#' @keywords internal
#' @noRd
.plot_histogram_block <- function(block_name, pooled) {

  if (length(pooled) < 2) return(NULL)

  .safe_record(function() {

    graphics::hist(
      pooled, breaks = 30, col = "#4C72B0", border = "white",
      main = sprintf("Histogram: %s", block_name), xlab = "Value"
    )

  })

}

#' Plot density curve of pooled values
#'
#' @keywords internal
#' @noRd
.plot_density_block <- function(block_name, pooled) {

  if (length(pooled) < 2 || stats::sd(pooled) == 0) return(NULL)

  .safe_record(function() {

    graphics::plot(
      stats::density(pooled), main = sprintf("Density: %s", block_name),
      xlab = "Value", col = "#C44E52", lwd = 2
    )

  })

}

#' Plot Q-Q plot against normal
#'
#' @keywords internal
#' @noRd
.plot_qqplot_block <- function(block_name, pooled) {

  if (length(pooled) < 3) return(NULL)

  .safe_record(function() {

    stats::qqnorm(pooled, main = sprintf("Q-Q plot: %s", block_name))
    stats::qqline(pooled, col = "firebrick", lwd = 2)

  })

}

#' Plot boxplots of block features
#'
#' @keywords internal
#' @noRd
.plot_boxplot_block <- function(block_name, block) {

  nm <- .numeric_columns(block)

  if (length(nm) == 0) return(NULL)

  keep <- nm[seq_len(min(30, length(nm)))]

  .safe_record(function() {

    graphics::par(mar = c(7, 4, 3, 1))

    graphics::boxplot(
      as.list(block[, keep, drop = FALSE]),
      las = 2, main = sprintf("Boxplots: %s", block_name),
      col = "#8172B2", cex.axis = 0.7
    )

  })

}

#' Compute violin plot polygon coordinates
#'
#' @keywords internal
#' @noRd
.violin_polygon <- function(x, at, width = 0.4) {

  x <- x[is.finite(x)]

  if (length(x) < 2 || stats::sd(x) == 0) return(NULL)

  d <- stats::density(x)
  d$y <- d$y / max(d$y) * width

  list(
    x = c(at - d$y, rev(at + d$y)),
    y = c(d$x, rev(d$x))
  )

}

#' Plot violin plots of block features
#'
#' @keywords internal
#' @noRd
.plot_violin_block <- function(block_name, block) {

  nm <- .numeric_columns(block)

  if (length(nm) == 0) return(NULL)

  keep <- nm[seq_len(min(10, length(nm)))]

  polys <- lapply(seq_along(keep), function(i) .violin_polygon(block[[keep[i]]], at = i))

  if (all(vapply(polys, is.null, logical(1)))) return(NULL)

  .safe_record(function() {

    rng <- range(unlist(lapply(polys, function(p) p$y)), na.rm = TRUE)

    graphics::par(mar = c(7, 4, 3, 1))

    graphics::plot(
      NA, xlim = c(0.5, length(keep) + 0.5), ylim = rng,
      axes = FALSE, xlab = "", ylab = "Value",
      main = sprintf("Violin plots: %s", block_name)
    )

    graphics::axis(1, at = seq_along(keep), labels = keep, las = 2, cex.axis = 0.7)
    graphics::axis(2)

    for (p in polys) if (!is.null(p)) graphics::polygon(p$x, p$y, col = "#64B5CD", border = "black")

  })

}

#' Plot PCA scores, PC1 versus PC2
#'
#' @keywords internal
#' @noRd
.plot_pca_block <- function(block_name, pca) {

  if (is.null(pca) || ncol(pca$x) < 2) return(NULL)

  .safe_record(function() {

    graphics::plot(
      pca$x[, 1], pca$x[, 2], pch = 19, col = grDevices::adjustcolor("#4C72B0", 0.7),
      xlab = "PC1", ylab = "PC2", main = sprintf("PCA: %s", block_name)
    )

  })

}

#' Plot PCA scree of variance explained
#'
#' @keywords internal
#' @noRd
.plot_scree_block <- function(block_name, explained_variance) {

  if (is.null(explained_variance) || length(explained_variance) == 0) return(NULL)

  k <- seq_len(min(10, length(explained_variance)))

  .plot_bar(100 * explained_variance[k], paste0("PC", k),
            main = sprintf("Scree plot: %s", block_name),
            ylab = "% variance explained")

}

#' Plot hierarchical clustering dendrogram
#'
#' @keywords internal
#' @noRd
.plot_dendrogram <- function(m, main) {

  if (nrow(m) < 3 || ncol(m) < 2) return(NULL)

  .safe_record(function() {

    d <- stats::dist(m)
    hc <- stats::hclust(d)

    graphics::plot(hc, main = main, xlab = "", sub = "", cex = 0.6)

  })

}

#' Plot dendrogram clustering samples
#'
#' @keywords internal
#' @noRd
.plot_sample_clustering_block <- function(block_name, block) {

  m <- .numeric_matrix(block)
  m <- m[stats::complete.cases(m), , drop = FALSE]

  .plot_dendrogram(m, sprintf("Sample clustering: %s", block_name))

}

#' Plot dendrogram clustering features
#'
#' @keywords internal
#' @noRd
.plot_feature_clustering_block <- function(block_name, block) {

  m <- .numeric_matrix(block)
  m <- m[stats::complete.cases(m), , drop = FALSE]

  .plot_dendrogram(t(m), sprintf("Feature clustering: %s", block_name))

}

#' Plot pooled values highlighting outliers
#'
#' @keywords internal
#' @noRd
.plot_outliers_block <- function(block_name, pooled, outlier_idx) {

  if (length(pooled) < 2) return(NULL)

  .safe_record(function() {

    cols <- rep("#4C72B0", length(pooled))

    if (length(outlier_idx) > 0) cols[outlier_idx] <- "firebrick"

    graphics::plot(
      pooled, col = cols, pch = 19,
      main = sprintf("Outliers: %s", block_name),
      xlab = "Index", ylab = "Value"
    )

  })

}

#' Plot composite scores per transformation
#'
#' @keywords internal
#' @noRd
.plot_transformation_scores_block <- function(block_name, ranking) {

  if (is.null(ranking) || nrow(ranking) == 0) return(NULL)

  .plot_bar(ranking$score, ranking$transformation,
            main = sprintf("Transformation scores: %s", block_name),
            ylab = "Composite score")

}

#' Plot histograms before and after transform
#'
#' @keywords internal
#' @noRd
.plot_transformation_comparison_block <- function(block_name, before, after) {

  if (length(before) < 2 || length(after) < 2) return(NULL)

  .safe_record(function() {

    graphics::par(mfrow = c(1, 2))

    graphics::hist(before, breaks = 20, col = "#C44E52", border = "white",
                   main = sprintf("%s: before", block_name), xlab = "Value")

    graphics::hist(after, breaks = 20, col = "#55A868", border = "white",
                   main = sprintf("%s: after", block_name), xlab = "Value")

    graphics::par(mfrow = c(1, 1))

  })

}

# =============================================================================
# .build_plots()
# =============================================================================

#' Assemble all diagnostic plots for a dataset
#'
#' @keywords internal
#' @noRd
.build_plots <- function(assays, diagnostics, transformations, overlap) {

  plots <- list()

  # check_data() skips a block when it has no samples, no features, or its
  # diagnostics could not be built, so diagnostics/transformations can be
  # shorter than assays. Driving everything from the blocks that actually
  # have diagnostics keeps the per-block vectors aligned; otherwise the
  # labels and the values below silently belong to different blocks.

  block_names <- names(diagnostics)
  assays <- assays[block_names]

  plots$missing_heatmap <- .map_blocks(assays, .plot_missing_heatmap_block)

  missing_pct <- vapply(diagnostics, function(d) d$missing_percent, numeric(1))

  plots$missing_by_block <- .plot_bar(missing_pct, block_names,
                                       main = "Missing (%) by block", ylab = "% missing")

  plots$missing_by_sample <- .map_blocks(assays, .plot_missing_by_sample_block)
  plots$missing_by_feature <- .map_blocks(assays, .plot_missing_by_feature_block)
  plots$missing_pattern <- .map_blocks(assays, .plot_missing_pattern_block)

  plots$overlap_heatmap <- .plot_overlap_heatmap(overlap)
  plots$overlap_network <- .plot_overlap_network(overlap)

  plots$correlation_heatmap <- stats::setNames(
    lapply(block_names, function(nm) .plot_correlation_heatmap_block(nm, diagnostics[[nm]]$correlation)),
    block_names
  )
  plots$correlation_heatmap <- plots$correlation_heatmap[!vapply(plots$correlation_heatmap, is.null, logical(1))]

  plots$correlation_distribution <- stats::setNames(
    lapply(block_names, function(nm) .plot_correlation_distribution_block(nm, diagnostics[[nm]]$correlation)),
    block_names
  )
  plots$correlation_distribution <- plots$correlation_distribution[!vapply(plots$correlation_distribution, is.null, logical(1))]

  plots$histograms <- stats::setNames(
    lapply(block_names, function(nm) .plot_histogram_block(nm, .pool_numeric(assays[[nm]]))),
    block_names
  )
  plots$histograms <- plots$histograms[!vapply(plots$histograms, is.null, logical(1))]

  plots$density <- stats::setNames(
    lapply(block_names, function(nm) .plot_density_block(nm, .pool_numeric(assays[[nm]]))),
    block_names
  )
  plots$density <- plots$density[!vapply(plots$density, is.null, logical(1))]

  plots$qqplots <- stats::setNames(
    lapply(block_names, function(nm) .plot_qqplot_block(nm, .pool_numeric(assays[[nm]]))),
    block_names
  )
  plots$qqplots <- plots$qqplots[!vapply(plots$qqplots, is.null, logical(1))]

  plots$boxplots <- .map_blocks(assays, .plot_boxplot_block)
  plots$violinplots <- .map_blocks(assays, .plot_violin_block)

  plots$pca <- stats::setNames(
    lapply(block_names, function(nm) .plot_pca_block(nm, diagnostics[[nm]]$pca)),
    block_names
  )
  plots$pca <- plots$pca[!vapply(plots$pca, is.null, logical(1))]

  plots$scree <- stats::setNames(
    lapply(block_names, function(nm) .plot_scree_block(nm, diagnostics[[nm]]$explained_variance)),
    block_names
  )
  plots$scree <- plots$scree[!vapply(plots$scree, is.null, logical(1))]

  plots$sample_clustering <- .map_blocks(assays, .plot_sample_clustering_block)
  plots$feature_clustering <- .map_blocks(assays, .plot_feature_clustering_block)

  plots$outliers <- stats::setNames(
    lapply(block_names, function(nm) {
      .plot_outliers_block(nm, .pool_numeric(assays[[nm]]), diagnostics[[nm]]$outliers)
    }),
    block_names
  )
  plots$outliers <- plots$outliers[!vapply(plots$outliers, is.null, logical(1))]

  plots$transformation_scores <- stats::setNames(
    lapply(block_names, function(nm) .plot_transformation_scores_block(nm, transformations[[nm]]$ranking)),
    block_names
  )
  plots$transformation_scores <- plots$transformation_scores[!vapply(plots$transformation_scores, is.null, logical(1))]

  plots$transformation_comparison <- stats::setNames(
    lapply(block_names, function(nm) {

      best <- transformations[[nm]]$recommended
      res <- .safe_try(.apply_transformation(assays[[nm]], best), NULL)

      if (is.null(res)) return(NULL)

      .plot_transformation_comparison_block(
        nm, .pool_numeric(assays[[nm]]), .pool_numeric(res$block)
      )

    }),
    block_names
  )
  plots$transformation_comparison <- plots$transformation_comparison[
    !vapply(plots$transformation_comparison, is.null, logical(1))
  ]

  plots

}

# =============================================================================
# Table builders
# =============================================================================

#' Build summary table of block diagnostics
#'
#' @keywords internal
#' @noRd
.build_diagnostics_table <- function(diagnostics) {

  if (length(diagnostics) == 0) return(data.frame())

  do.call(rbind, lapply(diagnostics, function(d) {

    data.frame(
      block = d$block,
      data_type = d$data_type,
      samples = d$samples,
      features = d$features,
      missing_percent = round(d$missing_percent, 2),
      constant_features = length(d$constant_features),
      near_constant_features = length(d$near_constant_features),
      duplicated_features = length(d$duplicated_features),
      duplicated_samples = length(d$duplicated_samples),
      skewness = round(d$skewness, 3),
      kurtosis = round(d$kurtosis, 3),
      normality = d$normality,
      outliers = length(d$outliers),
      stringsAsFactors = FALSE
    )

  }))

}

#' Build summary table of transformations
#'
#' @keywords internal
#' @noRd
.build_transformations_table <- function(transformations) {

  if (length(transformations) == 0) return(data.frame())

  do.call(rbind, lapply(transformations, function(t) {

    data.frame(
      block = t$block,
      detected_type = t$detected_type,
      objective = t$objective,
      candidates_tested = length(t$candidates),
      recommended = t$recommended,
      score = round(t$score, 2),
      stringsAsFactors = FALSE
    )

  }))

}

#' Build summary table of preprocessing recipes
#'
#' @keywords internal
#' @noRd
.build_recipes_table <- function(recipes) {

  if (length(recipes) == 0) return(data.frame())

  do.call(rbind, lapply(recipes, function(r) {

    data.frame(
      block = r$block,
      imputation = ifelse(is.null(r$imputation), "none", r$imputation),
      transformation = ifelse(is.null(r$transformation), "none", r$transformation),
      normalization = ifelse(is.null(r$normalization), "none", r$normalization),
      scaling = ifelse(is.null(r$scaling), "none", r$scaling),
      batch = ifelse(is.null(r$batch), "none", r$batch),
      feature_selection = ifelse(is.null(r$feature_selection), "none", r$feature_selection),
      dimensionality_reduction = ifelse(is.null(r$dimensionality_reduction), "none", r$dimensionality_reduction),
      estimated_quality = round(r$estimated_quality, 1),
      stringsAsFactors = FALSE
    )

  }))

}

#' Build table of QC check results
#'
#' @keywords internal
#' @noRd
.build_qc_table <- function(qc) {

  data.frame(
    check = c(qc$passed_checks, qc$failed_checks, qc$skipped_checks),
    status = c(
      rep("passed", length(qc$passed_checks)),
      rep("failed", length(qc$failed_checks)),
      rep("skipped", length(qc$skipped_checks))
    ),
    stringsAsFactors = FALSE
  )

}

# =============================================================================
# Recommendations
# =============================================================================

#' Generate text recommendations per block
#'
#' @keywords internal
#' @noRd
.generate_recommendations <- function(diagnostics, recipes, dataset_score) {

  global <- character(0)
  blocks <- list()

  for (nm in names(diagnostics)) {

    d <- diagnostics[[nm]]
    r <- recipes[[nm]]

    msgs <- character(0)

    if (length(d$constant_features) > 0) {

      msgs <- c(msgs, sprintf("Remove %d constant feature(s).", length(d$constant_features)))

    }

    if (length(d$near_constant_features) > 0) {

      msgs <- c(msgs, sprintf("Remove %d near-constant feature(s).", length(d$near_constant_features)))

    }

    if (length(d$duplicated_features) > 0) {

      msgs <- c(msgs, sprintf("Remove %d duplicated feature(s).", length(d$duplicated_features)))

    }

    if (length(d$duplicated_samples) > 0) {

      msgs <- c(msgs, sprintf("Remove %d duplicated sample(s).", length(d$duplicated_samples)))

    }

    if (length(d$empty_samples) > 0) {

      msgs <- c(msgs, sprintf("Remove %d completely empty sample(s).", length(d$empty_samples)))

    }

    if (length(d$empty_features) > 0) {

      msgs <- c(msgs, sprintf("Remove %d completely empty feature(s).", length(d$empty_features)))

    }

    if (!is.na(d$missing_percent) && d$missing_percent > 0) {

      msgs <- c(msgs, sprintf("Impute missing values using '%s' (%.1f%% missing).",
                               r$imputation, d$missing_percent))

    }

    if (!is.null(r$transformation) && r$transformation != "identity") {

      msgs <- c(msgs, sprintf("Apply the '%s' transformation.", r$transformation))

    }

    if (!is.null(r$normalization) && r$normalization != "none") {

      msgs <- c(msgs, sprintf("Apply '%s' normalization.", r$normalization))

    }

    if (!is.null(r$scaling) && r$scaling != "none") {

      msgs <- c(msgs, sprintf("Apply '%s' scaling.", r$scaling))

    }

    if (!is.null(r$batch)) {

      msgs <- c(msgs, sprintf("Correct for batch effects using variable '%s'.", r$batch_variable))

    }

    if (!is.null(r$feature_selection)) {

      msgs <- c(msgs, sprintf("Apply '%s' feature selection.", r$feature_selection))

    }

    if (length(msgs) == 0) msgs <- "No corrective action required."

    blocks[[nm]] <- msgs

    global <- c(global, sprintf("[%s] %s", nm, msgs))

  }

  if (!is.na(dataset_score) && dataset_score < 50) {

    global <- c(global, "Overall dataset quality is low; review flagged blocks before integration.")

  }

  list(global = global, blocks = blocks)

}

# =============================================================================
# Structural validation helpers
# =============================================================================

#' Validate multi-block data structure
#'
#' @keywords internal
#' @noRd
.validate_block_structure <- function(object) {

  errors <- character(0)
  warnings <- character(0)

  details <- list(
    duplicated_samples = data.frame(),
    duplicated_features = data.frame(),
    missing_sample_ids = data.frame(),
    missing_feature_ids = data.frame(),
    missing_values = data.frame(),
    empty_samples = data.frame(),
    empty_features = data.frame(),
    unsupported_variables = data.frame()
  )

  block_summary <- data.frame()

  for (block_name in names(object$assays)) {

    block <- object$assays[[block_name]]

    n_samples <- nrow(block)
    n_features <- ncol(block)

    if (n_samples == 0) {

      errors <- c(errors, sprintf("Block '%s' contains no samples.", block_name))

    }

    if (n_features == 0) {

      errors <- c(errors, sprintf("Block '%s' contains no features.", block_name))

    }

    unsupported <- names(block)[
      !vapply(
        block,
        function(x) is.numeric(x) || is.integer(x) || is.logical(x) ||
          is.factor(x) || is.character(x) || inherits(x, "Surv"),
        logical(1)
      )
    ]

    if (length(unsupported) > 0) {

      errors <- c(errors, sprintf("Block '%s' contains unsupported variable types.", block_name))

      details$unsupported_variables <- rbind(
        details$unsupported_variables,
        data.frame(block = block_name, feature = unsupported, stringsAsFactors = FALSE)
      )

    }

    ids <- rownames(block)

    if (is.null(ids)) {

      errors <- c(errors, sprintf("Block '%s' has no sample IDs.", block_name))

    } else {

      dup <- unique(ids[duplicated(ids)])

      if (length(dup) > 0) {

        errors <- c(errors, sprintf("Duplicated sample IDs in '%s'.", block_name))

        details$duplicated_samples <- rbind(
          details$duplicated_samples,
          data.frame(block = block_name, sample = dup, stringsAsFactors = FALSE)
        )

      }

      miss <- ids[is.na(ids) | ids == ""]

      if (length(miss) > 0) {

        errors <- c(errors, sprintf("Missing sample IDs in '%s'.", block_name))

        details$missing_sample_ids <- rbind(
          details$missing_sample_ids,
          data.frame(block = block_name, sample = miss, stringsAsFactors = FALSE)
        )

      }

    }

    vars <- colnames(block)
    dup <- unique(vars[duplicated(vars)])

    if (length(dup) > 0) {

      errors <- c(errors, sprintf("Duplicated feature names in '%s'.", block_name))

      details$duplicated_features <- rbind(
        details$duplicated_features,
        data.frame(block = block_name, feature = dup, stringsAsFactors = FALSE)
      )

    }

    miss <- vars[is.na(vars) | vars == ""]

    if (length(miss) > 0) {

      errors <- c(errors, sprintf("Missing feature names in '%s'.", block_name))

      details$missing_feature_ids <- rbind(
        details$missing_feature_ids,
        data.frame(block = block_name, feature = miss, stringsAsFactors = FALSE)
      )

    }

    if (n_samples > 0 && n_features > 0) {

      total_na <- sum(is.na(block))
      pct_na <- round(100 * total_na / (n_samples * n_features), 2)

      if (total_na > 0) {

        warnings <- c(warnings, sprintf("%s: %.2f%% missing values.", block_name, pct_na))

        na_pos <- which(is.na(block), arr.ind = TRUE)

        details$missing_values <- rbind(
          details$missing_values,
          data.frame(
            block = block_name,
            sample = rownames(block)[na_pos[, 1]],
            feature = colnames(block)[na_pos[, 2]],
            stringsAsFactors = FALSE
          )
        )

      }

      empty_samples <- rownames(block)[apply(block, 1, function(x) all(is.na(x)))]

      if (length(empty_samples) > 0) {

        warnings <- c(warnings, sprintf("%s: %d empty sample(s).", block_name, length(empty_samples)))

        details$empty_samples <- rbind(
          details$empty_samples,
          data.frame(block = block_name, sample = empty_samples, stringsAsFactors = FALSE)
        )

      }

      empty_features <- colnames(block)[vapply(block, function(x) all(is.na(x)), logical(1))]

      if (length(empty_features) > 0) {

        warnings <- c(warnings, sprintf("%s: %d empty feature(s).", block_name, length(empty_features)))

        details$empty_features <- rbind(
          details$empty_features,
          data.frame(block = block_name, feature = empty_features, stringsAsFactors = FALSE)
        )

      }

      block_summary <- rbind(
        block_summary,
        data.frame(
          block = block_name,
          samples = n_samples,
          features = n_features,
          missing_percent = pct_na,
          constant_features = length(.constant_features(block)),
          near_constant_features = length(.near_constant_features(block)),
          empty_samples = length(empty_samples),
          empty_features = length(empty_features),
          stringsAsFactors = FALSE
        )
      )

    }

  }

  list(errors = errors, warnings = warnings, details = details, block_summary = block_summary)

}

#' Validate metadata against block samples
#'
#' @keywords internal
#' @noRd
.validate_metadata <- function(object) {

  errors <- character(0)
  warnings <- character(0)
  metadata_only <- character(0)
  block_only <- character(0)

  if (is.null(object$metadata)) {

    return(list(errors = errors, warnings = warnings,
                metadata_only = metadata_only, block_only = block_only))

  }

  metadata <- object$metadata

  if (!("sample_id" %in% colnames(metadata))) {

    errors <- c(errors, "Metadata does not contain a 'sample_id' column.")

    return(list(errors = errors, warnings = warnings,
                metadata_only = metadata_only, block_only = block_only))

  }

  metadata_ids <- metadata$sample_id

  if (anyDuplicated(metadata_ids)) {

    errors <- c(errors, "Duplicated sample IDs in metadata.")

  }

  if (any(is.na(metadata_ids) | metadata_ids == "")) {

    errors <- c(errors, "Missing sample IDs in metadata.")

  }

  block_ids <- unique(unlist(lapply(object$assays, rownames), use.names = FALSE))

  block_only <- setdiff(block_ids, metadata_ids)
  metadata_only <- setdiff(metadata_ids, block_ids)

  if (length(block_only) > 0) {

    warnings <- c(warnings, sprintf(
      "%d sample(s) present in data blocks but absent from metadata.", length(block_only)
    ))

  }

  if (length(metadata_only) > 0) {

    warnings <- c(warnings, sprintf(
      "%d sample(s) present in metadata but absent from data blocks.", length(metadata_only)
    ))

  }

  list(errors = errors, warnings = warnings, metadata_only = metadata_only, block_only = block_only)

}

#' How the shared-sample count collapses as blocks are added
#'
#' The pairwise matrix is the wrong number to read and the easiest one to
#' reach for. Every pair of twenty blocks can share all 1800 samples while
#' the twenty of them together share none, because each block can be missing
#' a different ninety. An analysis needs the intersection of all of them, so
#' that is the figure that decides whether it can run.
#'
#' Blocks are added largest first, which puts the collapse next to the block
#' that caused it rather than wherever it happened to fall in the list.
#'
#' @param assays A named list of blocks.
#'
#' @return A data.frame with one row per block: the running intersection
#'   after adding it and how many samples that cost.
#' @keywords internal
#' @noRd
.cumulative_overlap <- function(assays) {

  if (length(assays) == 0) return(data.frame())

  # A person is in a block if the block measured something on them, not if
  # their identifier appears in it. A block assembled against the full
  # sample list carries a row for everyone and fills the gaps with NA, so
  # counting row names reports the whole cohort as present in a module that
  # was administered to ninety people. The collapse then surfaces only after
  # preprocessing drops those rows, which is far too late to be useful.

  id_sets <- lapply(assays, function(x) {

    rows <- rownames(x)

    if (is.null(rows) || nrow(x) == 0) return(character())

    rows[rowSums(!is.na(as.matrix(x))) > 0]

  })

  order_by_size <- order(vapply(id_sets, length, integer(1)), decreasing = TRUE)

  id_sets <- id_sets[order_by_size]

  running <- NULL
  rows <- list()

  for (nm in names(id_sets)) {

    before <- if (is.null(running)) length(id_sets[[nm]]) else length(running)

    running <- if (is.null(running)) id_sets[[nm]] else
      intersect(running, id_sets[[nm]])

    rows[[length(rows) + 1L]] <- data.frame(
      block = nm,
      samples = length(id_sets[[nm]]),
      shared_after = length(running),
      lost = before - length(running),
      stringsAsFactors = FALSE
    )

  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out

}

#' Say which blocks are responsible for an unusable intersection
#'
#' A count of zero tells the user their analysis cannot run and nothing about
#' what to do next. What they need is the name of the block to drop, so the
#' cheapest useful answer is to work out what the intersection would be
#' without each one.
#'
#' @param id_sets Sample identifiers per block.
#' @param min_samples How many are needed.
#' @param original The same blocks before preprocessing, when available.
#'
#' @return A character vector of lines, ready to paste into an error.
#' @keywords internal
#' @noRd
.explain_no_overlap <- function(id_sets, min_samples, original = NULL) {

  lines <- character()

  sizes <- vapply(id_sets, length, integer(1))

  lines <- c(lines, "", "  Samples in each block:")
  lines <- c(lines, sprintf("    %-28s %d", names(sizes), sizes))

  # Established first, because it decides what the rest of the message can
  # honestly claim. If the raw blocks overlapped and the preprocessed ones do
  # not, nothing about the identifiers is wrong and saying so would send the
  # reader to check a file that is fine.

  broken_by_preprocessing <- FALSE
  shared_before <- NA_integer_
  shrunk <- character()

  if (!is.null(original)) {

    common <- intersect(names(original), names(id_sets))

    if (length(common) > 1) {

      shared_before <- length(Reduce(intersect,
                                     lapply(original[common], rownames)))

      broken_by_preprocessing <- shared_before >= min_samples

      shrunk <- common[vapply(common, function(nm)
        length(id_sets[[nm]]) < nrow(original[[nm]]), logical(1))]

    }

  }

  if (broken_by_preprocessing) {

    return(c(
      lines, "",
      sprintf("  Before preprocessing these blocks shared %d sample(s), so the",
              shared_before),
      "  identifiers are fine and preprocessing is what removed the overlap.",
      if (length(shrunk) > 0)
        sprintf("  Samples were dropped from: %s",
                paste(shrunk, collapse = ", ")) else NULL,
      "  Loosening the sample filters in the recipe would keep more."
    ))

  }

  # Blocks that share nothing at all with the biggest one are almost never a
  # cohort problem: they are identifiers written a different way.

  biggest <- names(sizes)[which.max(sizes)]

  disjoint <- names(id_sets)[vapply(
    names(id_sets),
    function(nm) !identical(nm, biggest) &&
      length(intersect(id_sets[[nm]], id_sets[[biggest]])) == 0,
    logical(1))]

  if (length(disjoint) > 0) {

    lines <- c(
      lines, "",
      sprintf("  These share no identifier at all with '%s':", biggest),
      sprintf("    %s", paste(disjoint, collapse = ", ")),
      "  That is usually a naming difference rather than a different cohort.",
      sprintf("    '%s' has: %s", biggest,
              paste(utils::head(id_sets[[biggest]], 3), collapse = ", ")),
      sprintf("    '%s' has: %s", disjoint[1],
              paste(utils::head(id_sets[[disjoint[1]]], 3), collapse = ", "))
    )

    return(lines)

  }

  # Otherwise: which single block, dropped, would make the analysis possible.

  if (length(id_sets) > 1) {

    without <- vapply(seq_along(id_sets), function(i) {
      length(Reduce(intersect, id_sets[-i]))
    }, integer(1))

    names(without) <- names(id_sets)

    helpful <- without[without >= min_samples]

    if (length(helpful) > 0) {

      helpful <- sort(helpful, decreasing = TRUE)

      shown <- utils::head(helpful, 6)

      lines <- c(
        lines, "",
        "  Dropping any one of these would leave enough, best first:",
        sprintf("    without %-24s %d sample(s)", names(shown), shown),
        if (length(helpful) > length(shown))
          sprintf("    ... and %d other block(s) with the same effect",
                  length(helpful) - length(shown)) else NULL
      )

    } else {

      lines <- c(
        lines, "",
        "  No single block is responsible; the shortfall is spread across",
        "  several. Adding them one at a time, largest first:"
      )

      running <- NULL

      by_size <- names(sizes)[order(sizes, decreasing = TRUE)]

      for (nm in by_size) {
        running <- if (is.null(running)) id_sets[[nm]] else
          intersect(running, id_sets[[nm]])
        lines <- c(lines, sprintf("    + %-26s %d left", nm, length(running)))
      }

    }

  }

  lines

}

#' Compute sample overlap matrix between blocks
#'
#' @keywords internal
#' @noRd
.compute_overlap_matrix <- function(object) {

  blocks <- names(object$assays)

  overlap <- matrix(
    0, nrow = length(blocks), ncol = length(blocks),
    dimnames = list(blocks, blocks)
  )

  # Measured on, not merely listed in. Row names alone report a block that
  # was administered to ninety people as covering the whole cohort whenever
  # it was assembled against the full sample list and padded with NA.

  measured <- lapply(object$assays, function(x) {

    rows <- rownames(x)

    if (is.null(rows) || nrow(x) == 0) return(character())

    rows[rowSums(!is.na(as.matrix(x))) > 0]

  })

  for (i in seq_along(blocks)) {

    for (j in seq_along(blocks)) {

      overlap[i, j] <- length(intersect(measured[[blocks[i]]],
                                        measured[[blocks[j]]]))

    }

  }

  overlap

}

# =============================================================================
# check_data()
# =============================================================================

#' Validate a MultiOmicsData object
#'
#' Performs a complete structural, quality and preprocessing-readiness audit
#' of a \code{MultiOmicsData} object. For every block this computes a full
#' \code{BlockDiagnostics} profile, benchmarks a battery of candidate
#' transformations via \code{TransformationRecommendation}, and derives an
#' automatic \code{PreprocessingRecipe}. The dataset is additionally scored
#' for overall quality, screened through a set of quality-control checks,
#' and summarized with diagnostic plots, tables and human-readable
#' recommendations.
#'
#' The function never modifies the input object. Everything it produces is
#' returned inside a single \code{CMOValidation} object.
#'
#' @param object A \code{MultiOmicsData} object.
#'
#' @return
#' A \code{CMOValidation} object.
#'
#' @export

check_data <- function(object) {

  started <- Sys.time()

  if (!inherits(object, "MultiOmicsData")) {

    stop("'object' must be a MultiOmicsData object.")

  }

  validation <- CMOValidation()

  errors <- character(0)
  warnings <- character(0)

  # ===========================================================================
  # Structure
  # ===========================================================================

  if (!is.list(object$assays)) errors <- c(errors, "'assays' must be a list.")

  if (length(object$assays) == 0) errors <- c(errors, "No data blocks found.")

  if (length(errors) > 0) {

    validation$valid <- FALSE
    validation$errors <- errors

    validation$execution$started <- started
    validation$execution$finished <- Sys.time()
    validation$execution$runtime <- as.numeric(
      difftime(validation$execution$finished, started, units = "secs")
    )

    return(validation)

  }

  struct <- .validate_block_structure(object)
  meta <- .validate_metadata(object)

  errors <- c(struct$errors, meta$errors)
  warnings <- c(struct$warnings, meta$warnings)

  details <- struct$details
  details$metadata_only <- meta$metadata_only
  details$block_only <- meta$block_only

  # ===========================================================================
  # Per-block diagnostics, transformation benchmark and recipe
  # ===========================================================================

  diagnostics <- list()
  transformations <- list()
  recipes <- list()

  has_outcome <- FALSE

  for (block_name in names(object$assays)) {

    block <- object$assays[[block_name]]

    if (nrow(block) == 0 || ncol(block) == 0) next

    modality <- .block_modality(object, block_name)

    d <- .safe_try(.build_block_diagnostics(block_name, block, modality), NULL)

    if (is.null(d)) next

    diagnostics[[block_name]] <- d

    outcome <- .safe_try(.discover_outcome(object, block_name), NULL)

    if (!is.null(outcome)) has_outcome <- TRUE

    tr <- .safe_try(
      .benchmark_transformations(block_name, block, d$data_type, outcome),
      NULL
    )

    if (is.null(tr)) tr <- .benchmark_transformations(block_name, block, d$data_type, NULL)

    transformations[[block_name]] <- tr

    recipes[[block_name]] <- .build_recipe(
      block_name, d, tr,
      metadata = object$metadata,
      modality = modality
    )

  }

  # ===========================================================================
  # Quality scoring
  # ===========================================================================

  block_scores <- vapply(diagnostics, .compute_quality_score, numeric(1))

  feature_weights <- vapply(diagnostics, function(d) d$features, numeric(1))

  dataset_score <- if (length(block_scores) > 0) {

    stats::weighted.mean(block_scores, w = pmax(feature_weights, 1))

  } else {

    NA_real_

  }

  # ===========================================================================
  # Overlap matrix
  # ===========================================================================

  overlap <- .compute_overlap_matrix(object)

  # The pairwise matrix cannot answer the question an analysis actually asks,
  # which is how many samples every block has in common at once.

  cumulative <- .cumulative_overlap(object$assays)

  shared_by_all <- if (nrow(cumulative) > 0)
    cumulative$shared_after[nrow(cumulative)] else 0L

  if (length(object$assays) > 1) {

    smallest_pair <- min(overlap[row(overlap) != col(overlap)])

    if (shared_by_all < 10) {

      costly <- cumulative[cumulative$lost > 0, , drop = FALSE]
      costly <- costly[order(-costly$lost), , drop = FALSE]

      # Naming the top three when every block costs the same amount invents a
      # culprit. Blame is only assigned when one block actually stands out.
      blame <- if (nrow(costly) == 0) {
        ""
      } else if (nrow(costly) == 1 ||
                 costly$lost[1] >= 2 * costly$lost[min(2, nrow(costly))]) {
        sprintf(" Most of the loss comes from '%s'.", costly$block[1])
      } else {
        sprintf(paste(" The loss is spread across %d blocks rather than caused",
                      "by one; see summary$cumulative_overlap."), nrow(costly))
      }

      # A warning rather than an error, because this does not stop
      # preprocessing and preprocessing is what check_data() plans. Blocks
      # that share nobody can still be cleaned, and analysing a subset of
      # them afterwards is a perfectly ordinary thing to do. Refusing here
      # would block that on the strength of a decision the user has not made
      # yet. analyze() still refuses outright when the joint analysis is
      # actually attempted.

      warnings <- c(warnings, sprintf(
        paste0("Only %d sample(s) are present in every block, which is too few",
               " to analyse them all together.%s Preprocessing is unaffected;",
               " name a subset of blocks in analyze()."),
        shared_by_all, blame))

    } else if (shared_by_all < smallest_pair &&
               (smallest_pair - shared_by_all) >=
                 max(10, 0.05 * smallest_pair)) {

      # The trap this exists for: every pair looks complete and the whole set
      # is not. Someone reading the matrix alone would never see it coming,
      # because a pairwise table cannot express an intersection of twenty.
      # The comparison is against the smallest pair rather than a fixed
      # fraction, since that is the number the matrix invites them to trust.

      warnings <- c(warnings, sprintf(
        paste("Blocks share %d to %d samples pairwise, but only %d are present",
              "in every block at once. An analysis across all of them will use",
              "those %d."),
        smallest_pair, max(overlap[row(overlap) != col(overlap)]),
        shared_by_all, shared_by_all))

    }

  }

  # ===========================================================================
  # Global summary
  # ===========================================================================

  n_samples <- length(unique(unlist(lapply(object$assays, rownames), use.names = FALSE)))
  n_features <- sum(vapply(object$assays, ncol, integer(1)))
  observations <- sum(vapply(object$assays, function(b) nrow(b) * ncol(b), numeric(1)))

  total_missing <- sum(vapply(object$assays, function(b) sum(is.na(b)), numeric(1)))
  total_missing_percent <- 100 * total_missing / max(observations, 1)

  validation$summary <- list(
    n_blocks = length(object$assays),
    n_samples = n_samples,
    n_features = n_features,
    observations = observations,
    total_missing = total_missing,
    total_missing_percent = total_missing_percent,
    duplicated_samples = nrow(details$duplicated_samples),
    duplicated_features = nrow(details$duplicated_features),
    block_summary = struct$block_summary,
    overlap = overlap,
    cumulative_overlap = cumulative,
    shared_by_all = shared_by_all,
    quality_score = dataset_score,
    n_errors = length(errors),
    n_warnings = length(warnings)
  )

  validation$details <- details

  validation$diagnostics <- diagnostics
  validation$recipes <- recipes
  validation$transformations <- transformations

  # ===========================================================================
  # Quality control
  # ===========================================================================

  validation$valid <- length(errors) == 0
  validation$score <- dataset_score
  validation$errors <- errors
  validation$warnings <- warnings

  validation$qc <- .compute_qc_checks(validation, dataset_score, has_outcome)

  # ===========================================================================
  # Recommendations
  # ===========================================================================

  validation$recommendations <- .generate_recommendations(diagnostics, recipes, dataset_score)

  # ===========================================================================
  # Plots
  # ===========================================================================

  validation$plots <- .safe_try(
    .build_plots(object$assays, diagnostics, transformations, overlap),
    validation$plots
  )

  # ===========================================================================
  # Tables
  # ===========================================================================

  validation$tables <- list(
    block_summary = struct$block_summary,
    diagnostics = .build_diagnostics_table(diagnostics),
    recommendations = data.frame(
      recommendation = validation$recommendations$global,
      stringsAsFactors = FALSE
    ),
    preprocessing = .build_recipes_table(recipes),
    transformations = .build_transformations_table(transformations)
  )

  # ===========================================================================
  # Execution / bookkeeping
  # ===========================================================================

  finished <- Sys.time()

  validation$execution <- list(
    runtime = as.numeric(difftime(finished, started, units = "secs")),
    started = started,
    finished = finished,
    package_version = .safe_try(as.character(utils::packageVersion("CausalMultiOmics")), NA_character_),
    validation_version = "1.0.0",
    arguments = list(object_class = class(object)),
    seed = NULL,
    session = utils::sessionInfo()$R.version$version.string
  )

  validation$history <- c(validation$history, sprintf("check_data() run on %s", format(started)))
  validation$timestamp <- Sys.time()

  validation

}
