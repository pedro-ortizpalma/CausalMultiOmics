# Check what a causal structure implies about an adjustment

Answers, before any model is fitted, the question that decides whether
an estimate means anything: given this causal structure, does adjusting
for these variables identify the effect of this exposure on this
outcome?

## Usage

``` r
check_dag(dag, exposure, outcome, adjusted = character())
```

## Arguments

- dag:

  A `dagitty` object, a dagitty specification string, or a data.frame
  with `from` and `to` columns.

- exposure, outcome:

  Variable names.

- adjusted:

  What you intend to condition on.

## Value

A `CMODagCheck` object, printed as a verdict with reasons.

## Why this is separate from the data

A DAG is a claim about how the world works, not something recoverable
from a correlation matrix. Several different structures fit the same
data equally well, so the engine cannot derive one and will not pretend
to. What it can do is take the structure you are willing to defend and
tell you what follows from it.

What follows is often uncomfortable. Adjusting for a mediator removes
part of the effect being measured. Adjusting for a collider opens a path
that was closed and creates an association out of nothing, so the
adjusted estimate is worse than the unadjusted one. Both look like
diligence and both are damage.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`analysis_assumptions`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_assumptions.md)

## Examples

``` r
structure <- data.frame(
  from = c("age", "age", "protein", "inflammation"),
  to   = c("protein", "disease", "inflammation", "disease")
)

# Reading the diagram needs dagitty, which is a suggested dependency.
if (requireNamespace("dagitty", quietly = TRUE)) {

  # Adjusting for age closes the backdoor path.
  check_dag(structure, "protein", "disease", adjusted = "age")

  # Adjusting for the mediator removes the effect being measured.
  check_dag(structure, "protein", "disease",
            adjusted = c("age", "inflammation"))

}
#> 
#> Effect of protein on disease
#> ============================================================
#> 
#> Adjusted for : age, inflammation
#> Verdict      : NOT identified
#> 
#>   Conditioning on inflammation is what breaks this: it
#>   should not be in the adjustment set at all.
#> 
#> Problems with the adjustment
#> ------------------------------------------------------------
#>   - Conditioned on inflammation, which lies on the path from
#>     protein to disease: part of the effect being measured has
#>     been removed.
#> 
#> A sufficient adjustment set
#> ------------------------------------------------------------
#>   age
#> 
```
