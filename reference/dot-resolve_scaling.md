# Resolve the scaling stage against the chosen transformation

Quantile and robust transformations already return centred, unit-spread
features, so an additional scaling step would be redundant.

## Usage

``` r
.resolve_scaling(data_type, transformation, outlier_rate)
```
