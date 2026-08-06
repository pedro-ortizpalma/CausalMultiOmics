
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
#> Quality score:                 99.28
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

`preprocess()` executes that plan and records every step, so the
pipeline can be replayed on a second cohort with
`apply_preprocessing()`.

``` r
clean <- preprocess(omics, validation, plots = FALSE, quiet = TRUE)

clean
#> 
#> PreprocessingResult
#> ===================
#> 
#> Blocks:                        2
#> Recipes:                       2
#> Steps executed:                11
#> Removed samples:               0
#> Removed features:              0
#> Plots:                         0
#> Runtime:                       0.03 s
```

## Building evidence

`analyze()` does not fit a model and hand it back. It runs every
applicable method, collects the relationships each one reports, and
merges them into a single scored graph.

``` r
results <- analyze(clean, outcome = "group", effort = "fast",
                   plots = FALSE, quiet = TRUE)

results
#> 
#> CMOResult
#> =========
#> 
#> Outcome:                       group
#> Design:                        cross-sectional
#> Samples:                       18
#> Features analysed:             5 of 5
#> 
#> Methods run:                   7
#> Relationships found:           7
#> After integration:             7
#> With temporal precedence:      0
#> 
#> Causal paths:                  2
#> Plots:                         0
#> Tables:                        15
#> Runtime:                       0.96 s
```

Every relationship carries the identification strategy that would
license reading it causally, and the assumptions that would have to
hold. An edge identified as `"adjustment"` is an association however
high it scores.

``` r
results$graph$edges[, c("source", "target", "level_label",
                        "identification", "evidence_score")]
#>   source target   level_label identification evidence_score
#> 1     P1     P2 associational     adjustment          29.18
#> 2     G1     G3 associational     adjustment          13.39
#> 3     G2  group associational           none          10.87
#> 4     P1  group associational           none           5.60
#> 5     P2  group associational           none           5.39
#> 6     G3  group associational           none           3.09
#> 7     G1  group associational           none           2.69
```

`explain()` gathers everything known about one variable, including what
each method reported on its own before anything was merged.

``` r
explain(results, "P1")
```

`counterfactual()` states the findings in the units the variable was
measured in. Nothing is included unless you name it as something that
could plausibly be changed — a contrast for a genotype is arithmetic
without meaning.

``` r
counterfactual(results, modifiable = "proteomics")
```

Prior biological knowledge can be attached with `annotate_evidence()`.
It is reported beside the score and never folded into it, so a
relationship nobody has published yet is not penalised for being new. It
takes a local export rather than a live query, which keeps the analysis
offline, deterministic and independent of a database version the result
could not record.

``` r
known <- data.frame(source = "P1", target = "M1",
                    support = 0.9, database = "local STRING export")

annotate_evidence(results, known)
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
