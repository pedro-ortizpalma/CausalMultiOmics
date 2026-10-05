# Create a CMOResult object

Top-level container for a complete CausalMultiOmics analysis. The result
of
[`analyze()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
is not a model but a body of evidence: every method that ran contributes
edges, the integrator merges and scores them, and the graph plus its
interpretation is what the user reads.

## Usage

``` r
CMOResult()
```

## Value

A CMOResult object.

## Details

Each slot is filled by the stage that produces it, so a partially
finished analysis is represented by the slots that are still empty
rather than by a missing element.
