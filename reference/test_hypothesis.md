# Take a stated claim to a different cohort

The claim carries its own protocol: which outcome, which covariates,
which model. This runs exactly that on data it has never seen and
reports whether the relationship is there, at what size, and in which
direction.

## Usage

``` r
test_hypothesis(hypothesis, object)
```

## Arguments

- hypothesis:

  A `Hypothesis` from
  [`hypothesis`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/hypothesis.md).

- object:

  A `PreprocessingResult` for the new cohort, ideally produced by
  [`apply_preprocessing`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/apply_preprocessing.md)
  so the columns are on the same scale as the ones the claim was made
  on.

## Value

The hypothesis, with its `replication` slot filled in.

## Details

Replication is not agreement of p-values. A claim replicates when the
new estimate points the same way and is of a comparable size, and the
most common failure is a direction that holds with an effect a fifth as
large, which a significance test would call a success.

## See also

[`hypothesis`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/hypothesis.md),
[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`explain`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/explain.md)

## Examples

``` r
data <- simulate_data(n = 80, blocks = list(main = 8), seed = 1)
ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)
fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
               quiet = TRUE,
               control = analysis_control(methods = c("association", "conditional")))

claim <- hypothesis(fit)

# A second cohort, put on the same scale as the first rather than
# preprocessed on its own terms.
newdata <- simulate_data(n = 60, blocks = list(main = 8), seed = 2)
newready <- apply_preprocessing(newdata, ready, quiet = TRUE)

test_hypothesis(claim, newready)
#> 
#> Hypothesis
#> ==========================================================================
#> 
#>   Higher main_01 goes with lower y.
#> 
#>   This is an association.
#> 
#>   Effect:                -0.2632 (regression coefficient)
#>   95% CI:                [-0.4544, -0.0720]
#>   Identification:        none
#> 
#>   - Nothing was adjusted for, so any shared cause is inside this number.
#> 
#> What threatens it
#> --------------------------------------------------------------------------
#>   - Only one method found it (linear regression), so nothing corroborates
#>     it.
#>   - It does not survive correction for multiplicity (FDR 0.0684).
#>   - A confounder of strength 1.9 would erase it, which is weak enough to
#>     be commonplace.
#> 
#> What would settle it
#> --------------------------------------------------------------------------
#>   > Measure a confounder of main_01 and y. To erase this it would have to
#>     be associated with both at a risk ratio of at least 1.9; anything
#>     weaker leaves the relationship standing.
#>   > Measure main_01 before y in the same people. Nothing in this data
#>     establishes which came first, and the reverse direction fits it
#>     equally well.
#> 
#> Tested on another cohort
#> --------------------------------------------------------------------------
#>   Verdict:               NOT DETECTED
#>   There:                 -7e-04 [-0.2488, 0.2473], n = 60, p = 1
#>   Here:                  -0.2632
#>   Size:                  0% of the original
#>   ! A replication tests the claim, not the analysis that produced it.
#> 
#>   From CausalMultiOmics 0.1.1, 80 sample(s), score 13.6.
#> 
```
