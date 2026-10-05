# Re-run one relationship under every defensible preprocessing choice

A multiverse analysis, restricted to the decisions this package makes on
the user's behalf. The relationship is refitted under each variant
recipe and the spread of the answer is reported.

## Usage

``` r
sensitivity(
  object,
  feature = NULL,
  variants = NULL,
  max_paths = 24L,
  quiet = FALSE
)
```

## Arguments

- object:

  A `CMOResult`.

- feature:

  The variable whose relationship with the outcome to test. Defaults to
  the highest-scoring one.

- variants:

  Recipes to try, as produced by `.sensitivity_variants()`. Left alone
  by default, which builds them from the defensible alternatives for
  each block.

- max_paths:

  Cap on how many recipes to run.

- quiet:

  Suppress progress.

## Value

A `CMOSensitivity`.

## What is varied and what is not

Imputation and transformation: the stages where a competent analyst
could reasonably have chosen otherwise and the choice changes the
numbers.

Scaling is not varied, because it cannot change the answer. Estimates
are reported per standard deviation of the exposure, and any affine
rescaling leaves that untouched, so every scaling choice returns the
identical number. Including them would fill the table with exact
duplicates, and a robustness check that counts the same answer twice
reports a finding as steadier than it is.

Sample filters are held fixed too. Changing them changes who is in the
study, and an estimate on a different population is not a different
answer to the same question — it is an answer to a different one, which
belongs in
[`compare_results`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/compare_results.md)
rather than here.

## Why the estimates are standardised

A rank transformation puts the exposure on a 1 to n scale and a log
compresses it, so the raw coefficients down different paths are in
different units. Putting them in one column would invite exactly the
comparison that cannot be made, and would report a 275-fold spread for a
relationship that never moved. Every path is therefore reported as
change in the outcome per standard deviation of the exposure, however
that exposure was expressed.

## Reading the result

The number to look at is not the median. It is the share of paths that
agree on the direction, and the range. A relationship that is 0.8 down
one path and -0.1 down another was never a finding; it was a
preprocessing choice with a coefficient attached.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`explain`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/explain.md),
[`as.data.frame.CMOSensitivity`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOSensitivity.md)

## Examples

``` r
# \donttest{
sim <- simulate_data(n = 200, blocks = list(a = c("x", "z")),
                     dag = data.frame(from = "x", to = "y", effect = 0.8),
                     outcome = "y", missing = 0.1)
prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)
res <- analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE,
               control = analysis_control(methods = "association"))

sensitivity(res, "x")
#> Refitting x -> y under 15 preprocessing path(s)...
#> 
#> CMOSensitivity
#> ==============
#> 
#>   x  ->  y
#> 
#> The path taken:              0.6890 per SD  (0.6859 per unit, as reported)
#> Paths that ran:              15 of 15 attempted
#> 
#> Across paths, per SD:        0.6376 to 0.7059
#> Median:                      0.6890
#> Agree on direction:          100%
#> Reach p < 0.05:              100%
#> 
#>   This relationship holds down every reasonable path.
#> 
#> Every path
#> ------------------------------------------------------------------
#>   knn       rank        none        0.6376  p = 1.1e-14  
#>   mean      rank        none        0.6417  p = 7.2e-15  
#>   median    rank        none        0.6462  p = 4.3e-15  
#>   knn       log         none        0.6749  p = 1.4e-16  
#>   mean      log         none        0.6810  p = <1e-16   
#>   median    log         none        0.6816  p = <1e-16   
#>   knn       quantile    none        0.6854  p = <1e-16     <- reported
#>   mean      quantile    none        0.6890  p = <1e-16   
#>   knn       yeojohnson  none        0.6915  p = <1e-16   
#>   median    quantile    none        0.6922  p = <1e-16   
#>   knn       identity    none        0.6979  p = <1e-16   
#>   median    yeojohnson  none        0.6994  p = <1e-16   
#>   mean      yeojohnson  none        0.7000  p = <1e-16   
#>   median    identity    none        0.7059  p = <1e-16   
#>   mean      identity    none        0.7059  p = <1e-16   
#> 
#> How to read this
#> ------------------------------------------------------------------
#>   Only the choices this package makes on your behalf are varied:
#>   imputation, transformation and scaling. Sample filters are held
#>   fixed, because changing who is in the study changes the
#>   question rather than the answer.
#>   The number to read is the share of paths agreeing on the
#>   direction, not the median. A median across a set of paths that
#>   disagree is a summary of the disagreement.
#>   Stability here is not evidence that the relationship is real. A
#>   chance correlation in this sample is not created or destroyed
#>   by how the data was transformed, only re-expressed, so noise
#>   passes this check comfortably. It measures dependence on the
#>   analyst. Null calibration and replication in another cohort are
#>   what address the other question.
#> 
# }

```
