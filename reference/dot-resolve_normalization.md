# Resolve the normalization stage against the chosen transformation

Rank-based transformations already impose a common distribution on every
feature, so following them with quantile normalization would discard the
transformation selected by the benchmark. Compositional and proportion
data are closed by construction and need no normalization.

## Usage

``` r
.resolve_normalization(
  data_type,
  features,
  transformation,
  modality = "unknown"
)
```
