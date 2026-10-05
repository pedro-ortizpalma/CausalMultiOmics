# Print a readable validation report

Generic for turning an analysis object into a human-readable report.

## Usage

``` r
report(object, ...)
```

## Arguments

- object:

  An object to report on.

- ...:

  Passed to methods.

## Value

The object, invisibly.

## See also

[`check_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md),
[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md),
[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`export_graph`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/export_graph.md)

## Examples

``` r
data <- simulate_data(n = 60, blocks = list(main = 6), seed = 1)
audit <- check_data(data)

file <- tempfile(fileext = ".html")
report(audit, file = file)
#> Building HTML report...
#> Report saved to: /tmp/RtmpyoO9oA/file1bcf7686c1bb.html
#> Size: 476.0 KB
file.exists(file)
#> [1] TRUE
unlink(file)
```
