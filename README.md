
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CausalMultiOmics

<!-- badges: start -->

<!-- badges: end -->

CausalMultiOmics audits multi-block datasets before they are integrated.

Data blocks of any modality — omics, imaging, clinical, environmental,
wearable devices — are collected into a single container and profiled
block by block: structural problems, missing-value patterns,
distributional shape, feature quality and multivariate structure. For
every block a battery of candidate transformations is benchmarked and an
automatic preprocessing recipe is derived from the result, together with
a quality score, quality-control checks, diagnostic plots and a
self-contained HTML report.

Nothing is modified in place. `check_data()` never touches its input;
every finding is returned inside a single object.

## Installation

CausalMultiOmics is not on CRAN yet. Install it from a local checkout:

``` r
# install.packages("devtools")
devtools::install("path/to/CausalMultiOmics")
```

## Example

Blocks are supplied as a named list. They do not need to share samples,
but each one must carry its sample identifiers in its row names — that
is what links blocks to each other and to the metadata.

``` r
library(CausalMultiOmics)

set.seed(1)

transcriptomics <- data.frame(
  G1 = rnorm(20),
  G2 = rnorm(20),
  G3 = c(rnorm(19), NA),
  row.names = paste0("S", 1:20)
)

proteomics <- data.frame(
  P1 = rpois(18, 5),
  P2 = rpois(18, 10),
  row.names = paste0("S", 3:20)
)

omics <- load_data(
  assays = list(
    transcriptomics = transcriptomics,
    proteomics = proteomics
  ),
  metadata = data.frame(
    sample_id = paste0("S", 1:20),
    group = rep(c("A", "B"), each = 10),
    stringsAsFactors = FALSE
  )
)

omics
#> 
#> MultiOmicsData
#> ==============
#> 
#> Blocks:                2
#> Samples:               20
#> Features:              5
#> Metadata:              Yes
#> Preprocessing:         0
#> History:               1
```

`check_data()` performs the audit and returns a `CMOValidation` object.

``` r
validation <- check_data(omics)

validation
#> 
#> CMOValidation
#> =============
#> 
#> Status:                        VALID
#> Quality score:                 98.84
#> Blocks:                        2
#> Samples:                       20
#> Features:                      5
#> Errors:                        0
#> Warnings:                      1
#> Recipes:                       2
```

Each block gets its own diagnostics, transformation benchmark and
preprocessing recipe:

``` r
validation$diagnostics$proteomics$data_type
#> [1] "count"

validation$transformations$proteomics$recommended
#> [1] "vst"

validation$recipes$proteomics$imputation
#> [1] "none"
```

## Reading the results

`report()` turns a validation into something readable. The console
format leads with the verdict and flags only what needs attention:

``` r
report(validation, format = "console")
```

The default format writes a self-contained HTML document — styles,
scripts and plots are all inlined, so it opens with no network access
and no CDN dependency:

``` r
report(validation, file = "validation_report.html")
```

## Development

``` r
devtools::document()
devtools::test()
devtools::check()
```

`tests/manual/test_all.R` is a standalone development harness that
exercises the internals and prints what a user would see at the console.
It is not part of the built package.
