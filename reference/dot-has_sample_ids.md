# Does a block carry real sample identifiers?

A matrix without rownames and a data.frame with automatic row names both
come out of
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) labelled
"1", "2", ..., which is indistinguishable from identifiers the user
chose deliberately. That matters: two unrelated blocks would then appear
to share every sample, the overlap matrix would be fabricated, and
metadata matching would join on positions instead of samples. The check
has to run on the original object, before conversion, because afterwards
the information is gone.

## Usage

``` r
.has_sample_ids(x)
```

## Arguments

- x:

  A matrix or data.frame.

## Value

`TRUE` when the block has usable sample identifiers.

## Details

[`.row_names_info()`](https://rdrr.io/r/base/base-internal.html) returns
a negative row count exactly when the row names are the automatic
compact-integer kind.
