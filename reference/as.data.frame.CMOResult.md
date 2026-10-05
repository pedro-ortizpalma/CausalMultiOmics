# The relationships an analysis found, as a data frame

Returns one row per relationship, ordered by evidence score, carrying
the columns a reader uses. The full table the engine produced is
available with `all = TRUE`.

## Usage

``` r
# S3 method for class 'CMOResult'
as.data.frame(x, ..., all = FALSE, outcome_only = FALSE, min_score = 0)
```

## Arguments

- x:

  A `CMOResult` object.

- ...:

  Ignored.

- all:

  Return every column the engine produced rather than the reading
  subset.

- outcome_only:

  Keep only the relationships involving the outcome.

- min_score:

  Drop relationships scoring below this value.

## Value

A `data.frame`, ordered by `evidence_score`. Empty when the analysis
found nothing.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`explain`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/explain.md),
[`outcome_name`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/outcome_name.md)

## Examples

``` r
data <- simulate_data(n = 80, blocks = list(main = 8), seed = 1)
ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)
fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
               quiet = TRUE,
               control = analysis_control(methods = c("association", "conditional")))

head(as.data.frame(fit))
#>    source target direction    estimate     p_value       fdr  n identification
#> 1 main_01      y  negative -0.26317455 0.008548825 0.0683906 80           none
#> 2 main_04      y  negative -0.18482482 0.067890567 0.2715623 80           none
#> 3 main_03      y  positive  0.14947200 0.141296973 0.2906893 80           none
#> 4 main_02      y  positive  0.14798824 0.145344652 0.2906893 80           none
#> 5 main_08      y  positive  0.13320217 0.190545486 0.3048728 80           none
#> 6 main_05      y  positive  0.06271641 0.539439780 0.7192530 80           none
#>   level   level_label evidence_score e_value n_methods           methods
#> 1     2 associational          13.57   1.857         1 linear regression
#> 2     2 associational           8.66   1.649         1 linear regression
#> 3     2 associational           6.93   1.554         1 linear regression
#> 4     2 associational           6.86   1.550         1 linear regression
#> 5     2 associational           6.12   1.510         1 linear regression
#> 6     2 associational           2.03   1.308         1 linear regression
#>   temporal
#> 1    FALSE
#> 2    FALSE
#> 3    FALSE
#> 4    FALSE
#> 5    FALSE
#> 6    FALSE

# Only what bears on the outcome, and only the better-supported findings.
as.data.frame(fit, outcome_only = TRUE, min_score = 10)
#>    source target direction   estimate     p_value       fdr  n identification
#> 1 main_01      y  negative -0.2631745 0.008548825 0.0683906 80           none
#>   level   level_label evidence_score e_value n_methods           methods
#> 1     2 associational          13.57   1.857         1 linear regression
#>   temporal
#> 1    FALSE

```
