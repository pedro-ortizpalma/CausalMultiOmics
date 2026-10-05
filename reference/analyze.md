# Build quantified causal evidence from preprocessed multi-block data

`analyze()` does not return a model. It runs every applicable analysis
method, collects the relationships each one reports as `EvidenceEdge`
objects, integrates them into a single scored directed graph, and
extracts the findings that graph supports.

## Usage

``` r
analyze(
  object,
  outcome,
  time = NULL,
  subject = NULL,
  covariates = NULL,
  blocks = "all",
  effort = c("standard", "fast", "thorough", "exhaustive"),
  assume = NULL,
  control = NULL,
  plots = TRUE,
  quiet = FALSE
)
```

## Arguments

- object:

  A `PreprocessingResult`, as returned by
  [`preprocess()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md).
  Preprocessing decisions are already fixed, which is why analysis
  starts here rather than from a `MultiOmicsData`.

- outcome:

  Name of the outcome column in the metadata. Several may be named, in
  which case each is analysed and multiplicity is corrected across all
  of them together rather than within each.

- time:

  Optional name of a time or follow-up column. Combined with a binary
  outcome this makes the design a survival one.

- subject:

  Optional name of a subject identifier column. Repeated values make the
  design longitudinal.

- covariates:

  Optional character vector of metadata columns to adjust for. These
  form the declared adjustment set.

- blocks:

  Blocks to analyse, or `"all"`.

- effort:

  How much computation to spend. `"fast"` runs the generators once and
  nothing else; `"standard"` adds model diagnostics and 50 resamples;
  `"thorough"` adds null calibration; `"exhaustive"` resamples every
  generator 500 times. Anything set explicitly in `control` wins over
  the preset.

- assume:

  What you are claiming, built with
  [`analysis_assumptions`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_assumptions.md):
  the causal diagram, which variables are modifiable, measurement
  reliability, competing events. Every one is a statement the data
  cannot check, and every one changes what may be concluded.

- control:

  How much work to do, built with
  [`analysis_control`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_control.md):
  which generators, how many resamples, how many features. Nothing here
  changes what may be concluded.

- plots:

  Whether to render diagnostic plots.

- quiet:

  Whether to suppress progress messages.

## Value

A `CMOResult` object, or a `CMOMultiResult` when several outcomes were
named.

## What the scores mean

Three scores are reported per relationship and they measure different
things. **Strength** is how large the effect is. **Confidence** is how
precisely it was estimated. **Consistency** is how many of the methods
that could see the relationship agreed on its direction.

Consistency is not validity. Methods that share an unmeasured confounder
agree with one another while all being biased in the same direction, so
agreement is evidence of stability and nothing more. Every edge
therefore also carries `identification` — the strategy that would
license a causal reading — and `assumptions`, the conditions that would
have to hold. An edge identified as `"none"` or `"adjustment"` is an
association, however high its score.

## The seed

Every random procedure runs under `control$seed`, and the caller's
random number generator is restored afterwards. An analysis is a
read-only act: one that moved your generator would silently change every
simulation you ran next.

## What the returned object contains

The result carries individual-level data, by design: the traceability
the package aims for requires that every number can be traced back to
the rows it came from. A `PreprocessingResult` holds two full copies of
the cohort, the input and the transformed version, so that
[`apply_preprocessing()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/apply_preprocessing.md)
can replay the fitted models on a new cohort. A `CMOResult` holds the
outcome value of every subject. Both are therefore identifiable data:
check with whoever governs your dataset before emailing one of these
objects or committing a `.rds` of it to a repository.

## See also

[`analysis_assumptions`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_assumptions.md),
[`analysis_control`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_control.md),
[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)

## Examples

``` r
data <- simulate_data(n = 80, blocks = list(main = 8), seed = 1)
ready <- preprocess(data, check_data(data), plots = FALSE, quiet = TRUE)

fit <- analyze(ready, outcome = "y", effort = "fast", plots = FALSE,
               quiet = TRUE,
               control = analysis_control(methods = c("association", "conditional")))
fit
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
#> Runtime:                       0.88 s

# The findings as a table, ordered by evidence.
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

# Only the methods that need no extra package, for a quick pass.
analyze(ready, outcome = "y", effort = "fast", plots = FALSE, quiet = TRUE,
        control = analysis_control(methods = c("association",
                                               "conditional")))
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
#> Runtime:                       0.09 s
```
