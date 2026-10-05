# Attach external biological support to an evidence graph

Kept out of
[`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
on purpose. Pathway and interaction databases are queried over the
network, so folding them into the analysis would make the same data
produce different scores on different days, fail without a connection,
and depend on a database version that is not recorded anywhere in the
result.

## Usage

``` r
annotate_evidence(result, annotations, weight = 0.1)
```

## Arguments

- result:

  A `CMOResult`.

- annotations:

  A data.frame with columns `source`, `target` and optionally `support`
  (0-1) and `database`. Supply this to annotate from a local export,
  which is the reproducible option.

- weight:

  How much the biological support contributes to the recomputed evidence
  score, between 0 and 1.

## Value

The `CMOResult` with biological support attached.

## Details

Called separately, the annotation is an explicit, dated act: the source
and the retrieval time are stored on every edge it touches.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`explain`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/explain.md)

## Examples

``` r
data <- simulate_data(n = 80, blocks = list(main = 8), seed = 1)
ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)
fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
               quiet = TRUE,
               control = analysis_control(methods = c("association", "conditional")))

# A local export keeps the run reproducible: the database version is
# whatever this table came from, and it is stored on the edges.
support <- data.frame(source = "main_1", target = "y",
                      support = 0.8, database = "local export",
                      stringsAsFactors = FALSE)

annotate_evidence(fit, support)
#> 
#> CMOResult
#> =========
#> 
#> Outcome:                       y
#> Design:                        cross-sectional
#> Samples:                       80
#> Features analysed:             8 of 8
#> 
#> Methods run:                   2 of 2
#> Relationships found:           8
#> After integration:             7
#> With temporal precedence:      0
#> 
#> Causal paths:                  0
#> Plots:                         0
#> Tables:                        15
#> Runtime:                       0.07 s
```
