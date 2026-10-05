# State one relationship as a claim that could be wrong

A `CMOResult` is a body of evidence. This takes one relationship out of
it and states it as a hypothesis: the claim at the strength the evidence
supports, the case for it, the case against it, and the specific
measurements that would settle the argument.

## Usage

``` r
hypothesis(object, feature = NULL)
```

## Arguments

- object:

  A `CMOResult`.

- feature:

  The variable to state a claim about. Defaults to the highest-scoring
  relationship with the outcome.

## Value

A `Hypothesis` object.

## Why this is not just the top row of a table

The engine can rank relationships. It cannot decide which one is worth
making a claim about, because that depends on what the claim is for.
What it can do is take the relationship you name and say exactly what
could be asserted, what could not, and what would have to be observed
next.

The grade is set by identification alone. Precision, method agreement
and resampling stability describe how well the association was
estimated; they say nothing about what it is evidence of, and letting
them raise the grade would turn a well-measured association into a cause
by arithmetic.

## See also

[`test_hypothesis`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/test_hypothesis.md)
to take it to another cohort.

## Examples

``` r
# \donttest{
set.seed(1)
ids <- paste0("S", 1:60)
obj <- load_data(
  list(main = data.frame(a = rnorm(60), b = rnorm(60), row.names = ids)),
  metadata = data.frame(sample_id = ids, y = rnorm(60))
)
prep <- preprocess(obj, check_data(obj), plots = FALSE, quiet = TRUE)
res <- analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE,
               control = analysis_control(methods = "association"))

hypothesis(res)
#> 
#> Hypothesis
#> ==========================================================================
#> 
#>   Higher b goes with lower y.
#> 
#>   This is an association.
#> 
#>   Effect:                -0.0673 (regression coefficient)
#>   95% CI:                [-0.3302, 0.1956]
#>   Identification:        none
#> 
#>   - Nothing was adjusted for, so any shared cause is inside this number.
#> 
#> What threatens it
#> --------------------------------------------------------------------------
#>   - Only one method found it (linear regression), so nothing corroborates
#>     it.
#>   - It does not survive correction for multiplicity (FDR 0.943).
#>   - A confounder of strength 1.3 would erase it, which is weak enough to
#>     be commonplace.
#> 
#> What would settle it
#> --------------------------------------------------------------------------
#>   > Measure a confounder of b and y. To erase this it would have to be
#>     associated with both at a risk ratio of at least 1.3; anything weaker
#>     leaves the relationship standing.
#>   > Measure b before y in the same people. Nothing in this data
#>     establishes which came first, and the reverse direction fits it
#>     equally well.
#> 
#>   From CausalMultiOmics 0.1.1, 60 sample(s), score 1.5.
#> 
# }
```
