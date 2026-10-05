# Build the preprocessing method registry

Returns a nested list `registry[[stage]][[method]]`, each entry a list
with `fit`, `apply`, an optional `requires` vector of package names, and
a human-readable `label`.

## Usage

``` r
.method_registry()
```

## Value

A nested list of method definitions.

## Details

Adding a method to the engine means adding an entry here; nothing else
in the pipeline needs to change.
