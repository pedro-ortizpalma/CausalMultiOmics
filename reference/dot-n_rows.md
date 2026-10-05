# Row count that tolerates NULL and non-rectangular input

[`nrow()`](https://rdrr.io/r/base/nrow.html) returns `NULL` for both,
and `NULL == 0` is `logical(0)`, which blows up the surrounding `if`.

## Usage

``` r
.n_rows(x)
```
