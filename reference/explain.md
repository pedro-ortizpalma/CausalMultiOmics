# Explain why a variable ended up where it did

Assembles everything the engine knows about one variable into a single
account: which methods supported it, at what epistemic level, how stable
it was under resampling, what a confounder would have to look like to
explain it away, and which paths it sits on.

## Usage

``` r
explain(object, feature)
```

## Arguments

- object:

  A `CMOResult`.

- feature:

  Name of the variable to explain.

## Value

A list, printed as a readable account.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`hypothesis`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/hypothesis.md),
[`sensitivity`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/sensitivity.md),
[`counterfactual`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/counterfactual.md)

## Examples

``` r
tr <- data.frame(
  G1 = rnorm(40), G2 = rnorm(40),
  row.names = paste0("S", 1:40)
)

meta <- data.frame(sample_id = paste0("S", 1:40), y = rnorm(40))

x <- load_data(assays = list(block = tr), metadata = meta)
prep <- preprocess(x, check_data(x), plots = FALSE, quiet = TRUE)

result <- analyze(prep, outcome = "y", effort = "fast",
                  plots = FALSE, quiet = TRUE,
                  control = analysis_control(methods = c("association", "conditional")))

explain(result, "G1")
#> 
#> Why does G1 appear in this analysis?
#> ============================================================
#> 
#> Role: hub, associated with a higher outcome
#> 
#> Relationship with y
#> ----------------------------------------
#>   Direction                Higher G1 goes together with higher y
#>   Evidence score           4.1 / 100
#>   Highest evidence         associational (level 2)
#>   Identification           none
#>   E-value                  1.49 - a modest unmeasured confounder would explain this away
#> 
#>   What each method found on its own
#>             method               measured pooled estimate CI low CI high     p
#>  linear regression regression coefficient   TRUE    0.125 -0.232   0.483 0.495
#>   n agrees
#>  40   TRUE
#> 
#>   Headline estimate: regression coefficient, pooled from 1 observation(s).
#> 
#>   What those methods are
#> 
#>     association
#>       Fits a straight line between the variable and the outcome,
#>       after subtracting the effect of any factors you asked it to
#>       account for. 
#>       Limitation: It cannot tell which one came first, and it can
#>       only account for factors that were actually measured. 
#> 
#>   Score, opened up
#>                 component     value weight
#>               Effect size 0.1115040   0.34
#>                 Precision 0.3662027   0.33
#>          Method agreement 1.0000000   0.33
#>              Data quality 1.0000000     NA
#>  Spread across the cohort        NA     NA
#>            Evidence level 0.4000000     NA
#>          Temporal support 0.0000000     NA
#>      Direction confidence        NA     NA
#>       Bootstrap stability        NA     NA
#>     Sensitivity (E-value) 0.2978478     NA
#>        Biological support        NA     NA
#>                                                              note
#>                                     how large the relationship is
#>                                      how tightly it was estimated
#>                    weighted by the epistemic level of each method
#>                                          nothing here was imputed
#>                                                       not checked
#>                              highest level reached: associational
#>                                              no measurement order
#>                                                          untested
#>                                                resampling not run
#>  E = 1.49, a modest unmeasured confounder would explain this away
#>                        reported alongside, never inside the score
#> 
#>   What would have to be true
#>     - No adjustment was made; the estimate is a marginal association. 
#>     - Any shared cause of the two variables is fully reflected in the
#>       estimate. 
#> 
#> Paths it sits on
#> ----------------------------------------
#>           path weakest_link
#>  G1 -> G2 -> y         8.41
#> 
#> Ranked by 1 method(s), combined ranking 0.5
#> 

```
