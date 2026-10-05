# Declare a method that is recognised but cannot be replayed

Some well-known methods do not fit the fit/apply contract: they produce
a completed dataset rather than a transferable model. Registering them
with an explanatory error is better than leaving them out, because the
user gets told why instead of "unknown method".

## Usage

``` r
.unsupported_method(label, reason, alternatives)
```
