# Assemble the full summary of a TransformationRecommendation object

Returns the summary rather than printing it, so that it can be stored,
captured with
[`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html)
or looped over. Printing is the job of
[`print.summary.TransformationRecommendation()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.TransformationRecommendation.md),
which the console calls for you when you type `summary(x)`.

## Usage

``` r
# S3 method for class 'TransformationRecommendation'
summary(object, ...)
```

## Arguments

- object:

  A `TransformationRecommendation` object.

- ...:

  Ignored.

## Value

An object of class `summary.TransformationRecommendation`.
