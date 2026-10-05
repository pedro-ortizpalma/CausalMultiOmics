# Assemble the full summary of a BlockDiagnostics object

Returns the summary rather than printing it, so that it can be stored,
captured with
[`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html)
or looped over. Printing is the job of
[`print.summary.BlockDiagnostics()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/print.summary.BlockDiagnostics.md),
which the console calls for you when you type `summary(x)`.

## Usage

``` r
# S3 method for class 'BlockDiagnostics'
summary(object, ...)
```

## Arguments

- object:

  A `BlockDiagnostics` object.

- ...:

  Ignored.

## Value

An object of class `summary.BlockDiagnostics`.
