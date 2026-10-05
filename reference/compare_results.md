# Compare two analyses of the same question

Answers the four questions a reader has when handed two results: which
relationships appear in both, which reverse, which vanish, and which
survive at a size that no longer means the same thing.

## Usage

``` r
compare_results(a, b, names = c("first", "second"), tolerance = 0.5)
```

## Arguments

- a, b:

  Two `CMOResult` objects.

- names:

  What to call them in the output.

- tolerance:

  How far the second estimate may fall from the first, as a ratio,
  before the relationship is reported as agreeing in direction only. The
  default of 0.5 flags anything that halved.

## Value

A `CMOComparison`.

## Why the last one matters most

A relationship that holds in both cohorts at a fifth of the size is the
usual way a replication is oversold. Both are statistically significant,
both point the same way, and a comparison judged on presence alone calls
it agreement. Size is reported as a ratio for that reason.

## What it will not do

It does not pool. Two estimates can be compared without being averaged,
and averaging them needs them to be on the same scale with the same
adjustment, which the comparability check exists to test rather than
assume.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`sensitivity`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/sensitivity.md)

## Examples

``` r
# \donttest{
one <- simulate_data(n = 200, blocks = list(a = c("x", "z")),
                     dag = data.frame(from = "x", to = "y", effect = 0.8),
                     outcome = "y", seed = 1)
two <- simulate_data(n = 200, blocks = list(a = c("x", "z")),
                     dag = data.frame(from = "x", to = "y", effect = 0.8),
                     outcome = "y", seed = 2)

fit <- function(s) {
  p <- preprocess(s, check_data(s), plots = FALSE, quiet = TRUE)
  analyze(p, "y", effort = "fast", plots = FALSE, quiet = TRUE,
          control = analysis_control(methods = "association"))
}

compare_results(fit(one), fit(two))
#> 
#> CMOComparison
#> =============
#> 
#>   first  vs  second
#> 
#>   Same outcome, same design, same adjustment: comparable.
#> 
#> Found in the first:          1
#> Found in the second:         2
#> In both:                     1
#> Overlap (Jaccard):           0.50
#> 
#> Only in the second
#> ------------------------------------------------------------------
#>   z -> y
#>   Nothing in the first analysis rules these out either.
#> 
#> How to read this
#> ------------------------------------------------------------------
#>   1 relationship(s) in both, 0 only in the first, 1 only in the
#>   second.
#>   A relationship missing from one of two analyses has not been
#>   refuted by it. It may simply not have cleared the threshold
#>   there, and the two lists are not a test of each other.
#> 
#>   Nothing here is pooled. Comparing two estimates does not average
#>   them, and averaging needs them on one scale with one adjustment.
#> 
# }

```
