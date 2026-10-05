# Evaluate an expression without leaking RNG state into the caller's session

Parts of the diagnostic engine need randomness (fold assignment, Shapiro
subsampling) and want it reproducible, but
[`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)
is a read-only audit: it must not move the user's random number
generator, or every simulation run afterwards would silently change. The
current `.Random.seed` is saved, the expression is evaluated under
`seed`, and the original state is put back on exit.

## Usage

``` r
.with_preserved_seed(expr, seed = 1L)
```

## Arguments

- expr:

  Expression to evaluate.

- seed:

  Integer seed used while evaluating `expr`.
