# The edges of an evidence graph, as a data frame

The edges of an evidence graph, as a data frame

## Usage

``` r
# S3 method for class 'EvidenceGraph'
as.data.frame(x, ..., all = FALSE)
```

## Arguments

- x:

  An `EvidenceGraph` object.

- ...:

  Ignored.

- all:

  Return every column rather than the reading subset.

## Value

A `data.frame`, ordered by `evidence_score`.

## See also

[`as.data.frame.CMOResult`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOResult.md)
