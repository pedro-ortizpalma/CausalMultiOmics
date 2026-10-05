# Detect the statistical nature of a whole data block

Detect the statistical nature of a whole data block

## Usage

``` r
.detect_data_type(block)
```

## Arguments

- block:

  A data.frame representing one data block.

## Value

A character string: one of `"continuous"`, `"count"`, `"binary"`,
`"ordinal"`, `"proportion"`, `"compositional"`, `"censored"`, `"mixed"`.
