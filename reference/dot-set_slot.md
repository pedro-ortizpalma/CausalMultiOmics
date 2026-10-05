# Assign a (possibly NULL) value to a list slot without deleting it

Ordinary `$<-` / `[[<-` assignment of `NULL` removes the element from a
list. Every class defined in classes.R has a fixed set of slots that
must always remain present (even when their value is `NULL`), so every
assignment that might carry a `NULL` value goes through this helper
instead.

## Usage

``` r
.set_slot(obj, name, value)
```
