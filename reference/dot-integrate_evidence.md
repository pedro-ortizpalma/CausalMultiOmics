# Merge the edges describing the same relationship and score them

Three scores come out, and they measure different things on purpose:

## Usage

``` r
.integrate_evidence(edges, params, contributions = TRUE)
```

## Arguments

- edges:

  Edges emitted by the generators.

- params:

  Tuning parameters.

- contributions:

  Build the per-method table on each edge. Resampling integrates a whole
  graph per replicate and then reads four fields of each edge, so
  building that table five thousand times and discarding it was most of
  what resampling cost.

## Details

- strength:

  How large the effect is, standardised across methods, combined with
  how stable it is.

- confidence:

  How precisely it was estimated: sample size, interval width,
  multiplicity-adjusted significance.

- consistency:

  How many of the methods that could see the relationship agreed on its
  sign.

Consistency deliberately does NOT feed a causal claim. Methods that
share an unmeasured confounder agree with each other while all being
biased in the same direction, so agreement is evidence of stability and
nothing more. The identification strategy is tracked separately and
reported beside the score.
