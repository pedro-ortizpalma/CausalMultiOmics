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
#> Warning: No usable PNG device on this machine, so the report will have no figures in it. On macOS, installing XQuartz (xquartz.org) gives R a device it can write PNGs with.
#> Report saved to: /tmp/Rtmp7CAPtG/file1ad772de9f1f.html
#> Size: 16.0 KB
file.exists(file)
#> [1] TRUE
unlink(file)
```
