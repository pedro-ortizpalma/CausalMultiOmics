# Fold-averaged association between a transformed block and the outcome

The sample is split into `k` folds and the feature-outcome association
is measured inside each fold, then averaged. Splitting keeps the
estimate from being dominated by a handful of influential samples, so
the result is a measure of how *stably* the association survives a given
transformation.

## Usage

``` r
.outcome_performance(m, outcome)
```

## Arguments

- m:

  Numeric matrix of transformed features (rows = samples).

- outcome:

  Outcome descriptor from `.discover_outcome()`.

## Value

A scalar in `[0, 1]`, or `NA_real_` when the association cannot be
estimated.

## Details

This is deliberately NOT cross-validation: no model is fitted on the
training folds and evaluated on a held-out one. Every quantity below is
computed within the fold it is measured on, so it carries the optimistic
bias of an in-sample statistic and must not be read as out-of-sample
predictive performance. It is used only to rank transformations against
each other, where that bias applies equally to every candidate.
