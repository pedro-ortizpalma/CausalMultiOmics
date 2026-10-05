# What the results imply about changing a variable

Expresses each relationship as a contrast in the units the variable was
originally measured in: move this variable by this much, and the outcome
differs by this much.

## Usage

``` r
counterfactual(
  object,
  modifiable,
  change = "sd",
  direction = c("increase", "decrease"),
  baseline = "median",
  baseline_risk = NULL,
  min_identification = c("none", "adjustment", "temporal", "instrument")
)
```

## Arguments

- object:

  A `CMOResult`.

- modifiable:

  Variables or block names to consider. Anything the user could
  plausibly act on: diet, a metabolite, a treatable protein. Required.

- change:

  How much to move each variable, in its original units. `"sd"` (the
  default) uses one standard deviation, `"iqr"` the interquartile range,
  a number applies that amount to everything, and a named vector sets it
  per variable.

- direction:

  Either `"increase"` or `"decrease"`.

- baseline:

  Reference point the change starts from, since non-linear preprocessing
  makes the answer depend on it. `"median"` or a number.

- baseline_risk:

  Observed risk of the event, used to turn an odds ratio into a risk
  ratio for a binary outcome. Taken from the data when absent.

- min_identification:

  Lowest identification strength to report: `"none"`, `"adjustment"`,
  `"temporal"` or `"instrument"`.

## Value

A `CMOCounterfactual` object, printed as readable statements.

## What this is and is not

The arithmetic is a contrast implied by a fitted model, not a prediction
of what an intervention would do. Those coincide only when the effect is
identified, which for observational data it usually is not. Every row
therefore carries its identification label and a caveat written to match
it, and the wording never says "would reduce" unless an instrument
supports that reading.

Only variables named in `modifiable` are included. A contrast for a
genotype or an ancestry component is arithmetic without meaning, and
printing one invites exactly the reading it cannot support.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`explain`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/explain.md),
[`as.data.frame.CMOCounterfactual`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOCounterfactual.md)

## Examples

``` r
set.seed(1)
n <- 60
protein <- rnorm(n, 120, 15)
y <- 0.05 * protein + rnorm(n, 0, 1)
ids <- paste0("S", seq_len(n))

x <- load_data(
  assays = list(blood = data.frame(APOA1 = protein, row.names = ids)),
  metadata = data.frame(sample_id = ids, HDL = y)
)

prep <- preprocess(x, check_data(x), plots = FALSE, quiet = TRUE)
res <- analyze(prep, outcome = "HDL", effort = "fast",
               plots = FALSE, quiet = TRUE,
               control = analysis_control(methods = c("association", "conditional")))

counterfactual(res, modifiable = "APOA1")
#> 
#> What the results imply about changing these variables
#> ====================================================================
#> 
#> Outcome  : HDL (continuous)
#> Design   : cross-sectional
#> Change   : increase of one standard deviation, from the median
#> 
#>   An increase of 12.83 in APOA1 from 121.9 is associated with a
#>   change of 0.622 in HDL.
#>     evidence score 26/100, none
#>     This is an unadjusted association. It is not a prediction of
#>     what would happen if the variable were changed.
#> 
#> --------------------------------------------------------------------
#>  variable     from       to                   measure  value percent_change
#>     APOA1 121.9366 134.7641 difference in the outcome 0.6224             NA
#>  identification
#>            none
#> 

```
