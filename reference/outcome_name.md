# The name of the outcome an analysis was run on

The outcome of a `CMOResult` is stored as a list, because the engine
needs its type, its levels and its values as well as its name. Comparing
that list against a character column succeeds by coercion and quietly
returns the wrong rows, so reach for the name through this accessor.

## Usage

``` r
outcome_name(object)
```

## Arguments

- object:

  A `CMOResult` object.

## Value

A length-one character vector.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`as.data.frame.CMOResult`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOResult.md)

## Examples

``` r
data <- simulate_data(n = 60, blocks = list(main = 6), seed = 1)
ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)
fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
               quiet = TRUE,
               control = analysis_control(methods = c("association", "conditional")))
outcome_name(fit)
#> [1] "y"

```
