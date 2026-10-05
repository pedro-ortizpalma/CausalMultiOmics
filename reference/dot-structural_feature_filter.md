# Decide which features the structural filters remove

Constant, near-constant, duplicated, empty and mostly-missing features.
These are invariant to everything downstream, which is why they run
first.

## Usage

``` r
.structural_feature_filter(x, recipe, diagnostic = NULL)
```
