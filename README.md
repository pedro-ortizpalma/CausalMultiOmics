
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
#> Runtime:                       1.29 s
```

Every relationship carries the identification strategy that would
license reading it causally, and the assumptions that would have to
hold. An edge identified as `"adjustment"` is an association however
high it scores.

``` r
results$graph$edges[, c("source", "target", "level_label",
                        "identification", "evidence_score")]
#>   source target   level_label identification evidence_score
#> 1     P1     P2 associational     adjustment          28.23
#> 2     G1     G3 associational     adjustment          12.95
#> 3     G2  group associational           none          10.87
#> 4     P1  group associational           none           5.60
#> 5     P2  group associational           none           5.39
#> 6     G3  group associational           none           2.99
#> 7     G1  group associational           none           2.69
```

`explain()` gathers everything known about one variable, including what
each method reported on its own before anything was merged.

``` r
explain(results, "P1")
```

## From evidence to a claim

A result is a body of evidence. A claim is one thing taken out of it,
stated at the strength the evidence supports, with what would settle the
argument attached.

``` r
h <- hypothesis(results)

h
```

The last section is the one a result never has. It is derived from the
specific weaknesses of the specific relationship — measure a confounder
of at least this strength, measure the exposure first, replicate without
these six people, separate this variable from the four it moves with —
so a reader can act on it, which they cannot do with “residual
confounding cannot be excluded”.

The claim carries its own protocol, so it can be taken to a cohort it
has never seen:

``` r
validation <- apply_preprocessing(new_cohort, clean)

test_hypothesis(h, validation)
```

Replication is judged on direction and size rather than on a p-value.
The usual way one is oversold is a direction that holds with an effect a
fifth as large, which a significance test calls a success.

## What stops a number meaning more than it should

Four checks run over the merged graph. None of them changes an estimate;
they change what may be claimed about one, and what it is ranked by.

**The quantity each method measured.** Only quantities of the same
family are pooled. Averaging a log hazard ratio with a correlation
coefficient produced a headline number in no units at all, and it was
the number the report led with.

**A causal structure, if you have one.** A DAG is a claim about how the
world works, not something recoverable from a correlation matrix, so it
has to come from you. Supplying one turns “we adjusted for age” into a
verdict:

``` r
structure <- data.frame(
  from = c("age", "age", "protein", "inflammation"),
  to   = c("protein", "disease", "inflammation", "disease")
)

# Adjusting for the mediator removes part of the effect being measured.
check_dag(structure, "protein", "disease", adjusted = c("age", "inflammation"))

analyze(clean, outcome = "group", dag = structure)
```

Adjusting for a mediator or a collider makes an estimate worse than
doing nothing. Both look like diligence, and only a declared structure
tells them apart.

**How much of it was measured.** `data_quality` is the share of the data
behind a relationship that was measured rather than reconstructed.
Imputation fills gaps with plausible numbers, and from that moment
nothing downstream can tell a measurement from an estimate: a variable
that arrived a quarter empty reports the same precision as one fully
observed. Only imputed values count against it, since rows dropped for
being incomplete cost sample size and that is already in the precision
score.

**Whether the picture repeats.** `result$consensus` is the graph across
resamples rather than in the one sample collected. Per-edge stability
answers “would this relationship come back?” one at a time; it cannot
answer “would this picture come back?”. It also says what it is not: a
bootstrap resamples the people who were measured, so it reports how much
the result depends on which of them ended up in the study, never whether
a relationship is real.

``` r
results <- analyze(clean, outcome = "group", effort = "standard")

results$consensus
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

`tests/manual/` holds two standalone walkthroughs that drive the package
the way a user would and print everything they would see at the console.
`walkthrough_simple.R` runs on simulated data; `walkthrough_complex.R`
runs on the NHANES 1999-2006 exposome release, which is not
redistributed here — `tests/manual/test_data/SOURCE.md` says where to
get it. Neither is part of the built package.
