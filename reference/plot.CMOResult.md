# Draw a figure recorded during an analysis

Draw a figure recorded during an analysis

## Usage

``` r
# S3 method for class 'CMOResult'
plot(x, which = NULL, block = NULL, ...)
```

## Arguments

- x:

  A `CMOResult` object.

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
[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
