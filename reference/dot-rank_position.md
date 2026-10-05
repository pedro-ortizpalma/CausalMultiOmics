# Position of each value within a stored reference distribution

Returns a probability in (0, 1). This is what lets rank- and
quantile-based transformations be applied to data the model never saw: a
new value is placed against the training distribution rather than
re-ranked among its own peers.

## Usage

``` r
.rank_position(x, reference)
```
