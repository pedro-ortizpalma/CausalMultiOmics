# Square root, floored at zero

`na.rm` is deliberately left at its default: with `na.rm = TRUE`
[`pmax()`](https://rdrr.io/r/base/Extremes.html) does not skip missing
values, it replaces them with the other operand (0), which would
silently impute every `NA` as zero.

## Usage

``` r
.t_sqrt(x)
```
