# The analysis variants a sensitivity run fitted, as a data frame

One row per preprocessing path, so that the spread of the estimate
across defensible choices can be plotted or tabulated directly.

## Usage

``` r
# S3 method for class 'CMOSensitivity'
as.data.frame(x, ...)
```

## Arguments

- x:

  A `CMOSensitivity` object.

- ...:

  Ignored.

## Value

A `data.frame` with the source and target of the relationship added to
every row.

## See also

[`sensitivity`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/sensitivity.md)
