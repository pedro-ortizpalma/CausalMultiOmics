# Validate a MultiOmicsData object

Performs a complete structural, quality and preprocessing-readiness
audit of a `MultiOmicsData` object. For every block this computes a full
`BlockDiagnostics` profile, benchmarks a battery of candidate
transformations via `TransformationRecommendation`, and derives an
automatic `PreprocessingRecipe`. The dataset is additionally scored for
overall quality, screened through a set of quality-control checks, and
summarized with diagnostic plots, tables and human-readable
recommendations.

## Usage

``` r
check_data(object, plots = TRUE)
```

## Arguments

- object:

  A `MultiOmicsData` object.

- plots:

  Record the diagnostic figures inside the returned object. They cost a
  few percent of the runtime but around 90\\ size, so `FALSE` is worth
  it when the audit is being stored or run over many blocks.
  [`cmo_plots`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/cmo_plots.md)
  lists what was kept and
  [`plot.CMOValidation`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/plot.CMOValidation.md)
  draws it.

## Value

A `CMOValidation` object.

## Details

The function never modifies the input object. Everything it produces is
returned inside a single `CMOValidation` object.

## See also

[`load_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/load_data.md),
[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md),
[`report`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/report.md),
[`as.data.frame.CMOValidation`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOValidation.md)

## Examples

``` r
data <- simulate_data(n = 80, blocks = list(main = 8), seed = 1)

audit <- check_data(data)
audit
#> 
#> CMOValidation
#> =============
#> 
#> Status:                        VALID
#> Quality score:                 99.91
#> Blocks:                        1
#> Samples:                       80
#> Features:                      8
#> Errors:                        0
#> Warnings:                      0
#> Recipes:                       1

# The block-level findings as a table.
as.data.frame(audit)
#>   block samples features missing_percent constant_features
#> 1  main      80        8               0                 0
#>   near_constant_features empty_samples empty_features
#> 1                      0             0              0

# Nothing is raised: read the verdict and act on it.
audit$valid
#> [1] TRUE
audit$errors
#> character(0)
```
