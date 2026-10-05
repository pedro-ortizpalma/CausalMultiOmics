# Assemble the full summary of a CMOResult object

Returns the summary rather than printing it, so that it can be stored,
captured with
[`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html)
or looped over. Printing is the job of
[`print.summary.CMOResult()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.CMOResult.md),
which the console calls for you when you type `summary(x)`.

## Usage

``` r
# S3 method for class 'CMOResult'
summary(object, ...)
```

## Arguments

- object:

  A `CMOResult` object.

- ...:

  Ignored.

## Value

An object of class `summary.CMOResult`.
