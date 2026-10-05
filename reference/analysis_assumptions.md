# What you are claiming, as opposed to what you measured

Everything here is a statement the data cannot check. A causal diagram
is a claim about biology. A variable declared modifiable is a claim that
someone could act on it. A reliability figure is a claim about the
assay. Each changes what the analysis is allowed to conclude, and each
is your responsibility rather than the engine's.

## Usage

``` r
analysis_assumptions(
  dag = NULL,
  modifiable = NULL,
  negative_controls = NULL,
  heterogeneity = NULL,
  reliability = NULL,
  competing = NULL,
  forbidden = NULL,
  required = NULL,
  positivity = TRUE,
  min_residual = 0.1
)
```

## Arguments

- dag:

  Causal structure, as a `dagitty` object, a dagitty specification
  string, or a data.frame with `from` and `to`. Supplying one lets the
  engine check whether an adjustment identifies each effect rather than
  only recording that something was adjusted for. It never changes an
  estimate. See
  [`check_dag`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_dag.md).

- modifiable:

  Variables or blocks someone could plausibly act on. Nothing is
  expressed as a contrast in original units unless it appears here: a
  contrast for a genotype is arithmetic without meaning.

- negative_controls:

  Variables that should show no relationship with the outcome. If they
  do, something other than biology is driving the graph.

- heterogeneity:

  Metadata columns to test as effect modifiers. Naming one asks whether
  each relationship differs across the groups it defines.

- reliability:

  Named vector giving the share of each variable's measured variance
  that is real, from duplicates or from the assay. Classical measurement
  error attenuates a coefficient by exactly this factor, and supplying
  it corrects for that. Nothing is guessed: a variable absent from this
  vector is left alone.

- competing:

  Metadata column marking competing events in a survival analysis.
  Without it, dying of another cause is treated as censoring, which
  assumes those people could still have had the outcome.

- forbidden, required:

  data.frames of `from`/`to` pairs that structure learning may not
  propose, or must include. Both are claims about the world in the same
  way a diagram is, and both are recorded as assumptions on every edge
  they shaped.

- positivity:

  Check whether each effect is estimable at all: whether the variables
  adjusted for leave the exposure any independent variation. An estimate
  without it is the model extrapolating.

- min_residual:

  Share of the exposure's variance that must survive adjustment before
  the estimate is treated as measurement rather than extrapolation.

## Value

An `analysis_assumptions` object.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`analysis_control`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_control.md),
[`check_dag`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_dag.md)

## Examples

``` r
analysis_assumptions(
  dag = data.frame(from = "age", to = c("bmi", "death")),
  modifiable = "diet",
  reliability = c(bmi = 0.95)
)
#> 
#> analysis_assumptions
#> ====================
#> 
#>   causal diagram     2 relationship(s)
#>   modifiable         diet
#>   reliability        bmi = 0.95
#>   positivity         checked, at 10% residual variation
#> 
#>   Every line above is a claim the data cannot check.
#> 

```
