# How much work to do, and on what

Nothing here changes what the analysis is allowed to conclude. These are
decisions about cost and scope: getting one wrong makes the answer
slower or noisier, never wrong.

## Usage

``` r
analysis_control(
  methods = NULL,
  goal = c("causal", "predictive"),
  resample = NULL,
  resample_scheme = c("bootstrap", "cv"),
  resample_methods = NULL,
  permutations = NULL,
  diagnostics = NULL,
  bootstrap = 200,
  max_features = 150,
  min_per_block = 10,
  min_evidence_score = 1,
  seed = 1L
)
```

## Arguments

- methods:

  Which evidence generators to run. All applicable ones by default.

- goal:

  `"causal"` or `"predictive"`, which changes how features are screened.

- resample:

  How many resamples for stability. Taken from `effort` when left alone.

- resample_scheme:

  `"bootstrap"` or `"cv"`.

- resample_methods:

  Which generators to re-run on each resample.

- permutations:

  How many outcome permutations for null calibration.

- diagnostics:

  Run model diagnostics, effect concentration and the complete-case
  check.

- bootstrap:

  Resamples inside the mediation generator.

- max_features:

  Cap on features carried into the pairwise stage.

- min_per_block:

  Features guaranteed to each block during screening, so a large block
  cannot take every slot.

- min_evidence_score:

  Relationships scoring below this are dropped.

- seed:

  Passed to [`set.seed()`](https://rdrr.io/r/base/Random.html) where the
  engine needs randomness. The caller's generator is restored
  afterwards.

## Value

An `analysis_control` object.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`analysis_assumptions`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_assumptions.md),
[`cmo_setup`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/cmo_setup.md)

## Examples

``` r
analysis_control(methods = c("association", "survival"), max_features = 200)
#> 
#> analysis_control
#> ================
#> 
#>   methods              association, survival
#>   goal                 causal
#>   resample             from effort
#>   resample_scheme      bootstrap
#>   resample_methods     from effort
#>   permutations         from effort
#>   diagnostics          from effort
#>   bootstrap            200
#>   max_features         200
#>   min_per_block        10
#>   min_evidence_score   1
#>   seed                 1
#> 
#>   Nothing here changes what may be concluded.
#> 

```
