# Generate a multi-block study with a known answer

Builds a `MultiOmicsData` from a causal structure you specify, and
records what it did so the result can be checked rather than admired.

## Usage

``` r
simulate_data(
  n = 200,
  blocks = list(main = 10),
  dag = NULL,
  outcome = "y",
  outcome_type = c("continuous", "binary", "survival"),
  modules = NULL,
  module_loading = 0.9,
  missing = 0,
  missing_pattern = c("random", "by_outcome"),
  batch = 0,
  batch_effect = 0.5,
  coverage = 1,
  noise = 1,
  seed = 1
)
```

## Arguments

- n:

  Number of people.

- blocks:

  Named list. Each element is either a character vector of feature names
  to place in that block, or a single number meaning that many
  pure-noise features. Names used in `dag` must appear here to be
  measured; anything else in `dag` becomes metadata.

- dag:

  A data.frame with columns `from`, `to` and optionally `effect`
  (default 0.6). Both structure and magnitude in one object, because
  they are the same claim.

- outcome:

  Name of the outcome variable, which must appear in `dag` unless
  `effects_on_outcome` is used.

- outcome_type:

  `"continuous"`, `"binary"` or `"survival"`. Survival adds a time and a
  status column.

- modules:

  Named list of latent processes: each element is a character vector of
  feature names that will be driven by one unobserved variable. This is
  the shape a real biological process has, and it is not expressible as
  a DAG between measured variables.

- module_loading:

  How strongly each module drives its members.

- missing:

  Fraction of values to blank, either one number for every block or a
  named vector per block.

- missing_pattern:

  `"random"` blanks at random; `"by_outcome"` blanks preferentially
  where the outcome is high, which is the pattern imputation handles
  worst and the one that manufactures findings.

- batch:

  Number of batches, or 0 for none.

- batch_effect:

  Size of the shift between batches.

- coverage:

  Fraction of people each block measured, either one number or a named
  vector. Below 1 the blocks stop sharing a population, which is what
  makes sample alignment a real problem.

- noise:

  Standard deviation of the noise added to every generated variable.

- seed:

  Passed to [`set.seed()`](https://rdrr.io/r/base/Random.html). The
  caller's random number generator is restored afterwards.

## Value

A `MultiOmicsData` with `misc$simulation` describing what was planted.

## How the data is generated

Variables are produced in topological order over the supplied structure.
A variable with no parents is standard normal; a variable with parents
is the sum of its parents times their effects, plus noise. So an effect
of 0.8 means eight tenths of a standard deviation in the child per
standard deviation in the parent, before noise, which is the reading a
regression coefficient will recover.

Anything named in the structure is created. Names listed in `blocks`
become measured features in that block; the outcome becomes the outcome;
anything else becomes a metadata column, which is how covariates and
effect modifiers arrive.

## What the truth records

`result$misc$simulation` holds the structure, the effects, which
features are pure noise, which cells were blanked, module membership and
the seed. A test that asserts a mediator was found can then assert it
against the mediator that was planted, rather than against whichever
variable came out on top.

## See also

[`load_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/load_data.md),
[`check_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md),
[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)

## Examples

``` r
# A confounder, a mediator, and a variable the outcome causes.
structure <- data.frame(
  from   = c("age", "age", "protein", "inflammation", "disease"),
  to     = c("protein", "disease", "inflammation", "disease", "biomarker"),
  effect = c(0.5, 0.4, 0.8, 0.6, 0.9)
)

sim <- simulate_data(
  n = 200,
  blocks = list(blood = c("protein", "inflammation", "biomarker"),
                other = 5),
  dag = structure,
  outcome = "disease"
)

sim$misc$simulation$noise_features
#> [1] "other_01" "other_02" "other_03" "other_04" "other_05"

```
