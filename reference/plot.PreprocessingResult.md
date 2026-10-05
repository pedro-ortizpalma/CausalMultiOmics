# Draw a figure recorded during preprocessing

Draw a figure recorded during preprocessing

## Usage

``` r
# S3 method for class 'PreprocessingResult'
plot(x, which = NULL, block = NULL, ...)
```

## Arguments

- x:

  A `PreprocessingResult` object.

- which:

  Name of the figure. Use `cmo_plots(x)` to see the names.

- block:

  Block name, for figures drawn once per block.

- ...:

  Ignored.

## Value

The recorded plot, invisibly.

## See also

[`cmo_plots`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/cmo_plots.md),
[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)
