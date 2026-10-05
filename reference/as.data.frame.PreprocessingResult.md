# What each preprocessing stage did, as a data frame

One row per stage and block, with the dimensions before and after, so
the shape of the data can be traced through the pipeline.

## Usage

``` r
# S3 method for class 'PreprocessingResult'
as.data.frame(x, ...)
```

## Arguments

- x:

  A `PreprocessingResult` object.

- ...:

  Ignored.

## Value

A `data.frame`, one row per recorded step.

## See also

[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)
