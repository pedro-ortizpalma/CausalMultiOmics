# Create an EvidenceEdge object

One directed relationship between two variables, as reported by one
analysis method. Every evidence generator emits these; the integrator
merges the ones that describe the same relationship and scores them.

## Usage

``` r
EvidenceEdge()
```

## Value

An EvidenceEdge object.

## What this object is allowed to claim

An edge records what was estimated AND what would have to be true for
that estimate to carry a causal reading. `identification` says which
strategy, if any, licenses the causal interpretation:

- `"none"`:

  Marginal association. No adjustment was made.

- `"adjustment"`:

  Conditioned on `adjustment_set`. Causal only if that set blocks every
  backdoor path, which the data cannot confirm.

- `"temporal"`:

  The exposure was measured before the outcome, which rules out reverse
  causation but not confounding.

- `"instrument"`:

  An instrumental variable was used.

This matters because agreement between methods is easy to mistake for
validity. If several models share an unmeasured confounder they agree
with each other and are all wrong together: consistency measures
stability, not correctness. Keeping the assumptions attached to the
estimate is what stops a stability score from being read as a causal
one.
