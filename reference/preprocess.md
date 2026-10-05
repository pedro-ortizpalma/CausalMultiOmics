# Execute a preprocessing plan

Runs the `PreprocessingRecipe` objects produced by
[`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)
against the data they describe. The function decides nothing on its own:
every method, parameter and threshold it applies comes from the recipe,
and the stage order comes from the recipe's `stage_order`.

## Usage

``` r
preprocess(
  object,
  plan,
  blocks = NULL,
  plots = TRUE,
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- object:

  A `MultiOmicsData` object.

- plan:

  A `CMOValidation` (the usual case, as returned by
  [`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)),
  a single `PreprocessingRecipe`, or a named list of recipes.

- blocks:

  Optional character vector restricting execution to some blocks. Blocks
  not covered are carried through untouched.

- plots:

  Whether to render before/after diagnostic plots.

- force:

  Execute even when the plan does not match the object, or when the
  validation reported errors. The mismatch is recorded in the result.

- quiet:

  Whether to suppress progress messages.

## Value

A `PreprocessingResult` object.

## Details

Each stage is fitted and then applied, and the fitted model is kept.
That is what makes the result reproducible:
[`apply_preprocessing()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/apply_preprocessing.md)
can put an external dataset through the identical pipeline without
deriving a single decision from it.

Sample-level decisions (empty, duplicated or outlying samples) are
marked as non-replayable, because an external cohort has its own
samples. Feature-level decisions are replayable and pin the feature set.

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

[`apply_preprocessing`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/apply_preprocessing.md)
to replay the result on new data.

## Examples

``` r
tr <- data.frame(
  Gene1 = rnorm(10),
  Gene2 = rnorm(10),
  Gene3 = rep(1, 10),
  row.names = paste0("S", 1:10)
)

x <- load_data(assays = list(transcriptomics = tr))

validation <- check_data(x)

result <- preprocess(x, validation, plots = FALSE, quiet = TRUE)

result
#> 
#> PreprocessingResult
#> ===================
#> 
#> Blocks:                        1
#> Recipes:                       1
#> Steps executed:                5
#> Removed samples:               0
#> Removed features:              1
#> Plots:                         0
#> Runtime:                       0.01 s
```
