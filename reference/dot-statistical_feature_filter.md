# Decide which features the statistical filters remove

Variance, abundance and prevalence only mean something once the block
has been normalized and transformed onto a comparable scale, which is
why this runs late rather than alongside the structural filters.

## Usage

``` r
.statistical_feature_filter(x, recipe)
```
