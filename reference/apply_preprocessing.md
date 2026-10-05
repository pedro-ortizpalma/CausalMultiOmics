# Replay a fitted pipeline on new data

Puts a new dataset through exactly the pipeline recorded in a
`PreprocessingResult`, reusing every fitted model rather than deriving
anything from the new data. This is what makes an external validation
cohort comparable with the cohort the pipeline was built on.

## Usage

``` r
apply_preprocessing(object, result, blocks = NULL, quiet = FALSE)
```

## Arguments

- object:

  A `MultiOmicsData` object holding the new data.

- result:

  A `PreprocessingResult` from
  [`preprocess()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md).

- blocks:

  Optional character vector restricting the replay.

- quiet:

  Whether to suppress progress messages.

## Value

A `MultiOmicsData` object carrying the processed blocks.

## Details

Two rules follow from that and are applied deliberately:

- Feature-level decisions are replayed, so the output carries the same
  features in the same order as the training data.

- Sample-level decisions are not replayed. Removing a sample from a new
  cohort because a training sample was removed would be meaningless, and
  silently dropping rows from a validation set is a common way to
  produce optimistic results.

## See also

[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)

## Examples

``` r
train <- simulate_data(n = 80, blocks = list(main = 6), seed = 1)
cleaned <- preprocess(train, check_data(train), plots = FALSE, quiet = TRUE)

# A new cohort is transformed with the models fitted on the first one,
# not refitted on itself.
newdata <- simulate_data(n = 40, blocks = list(main = 6), seed = 2)
apply_preprocessing(newdata, cleaned, quiet = TRUE)
#> 
#> MultiOmicsData
#> ==============
#> 
#> Blocks:                1
#> Samples:               40
#> Features:              6
#> Metadata:              Yes
#> Preprocessing:         0
#> History:               3
```
