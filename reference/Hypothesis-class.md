# Create a Hypothesis object

A `CMOResult` is a body of evidence. A `Hypothesis` is one claim taken
out of it, carrying everything needed to decide whether to act on it,
argue with it, or go and settle it.

## Usage

``` r
Hypothesis()
```

## Value

A Hypothesis object.

## Details

The distinction matters because a graph of forty scored relationships is
not a scientific statement. It is material from which statements can be
made, and the making is where the judgement lives: which relationship,
at what strength, under which assumptions, and what would have to be
observed for it to be wrong.

The last of those is what separates this from a result. A finding says
what was seen; a hypothesis says what would change its author's mind.
Both are derived here from what the engine already knows about the
relationship, so the answer is specific to it rather than a paragraph of
generic caution.

## Why this topic has an explicit name

The constructor and the
[`hypothesis()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/hypothesis.md)
function that builds one differ only in case, and on a case-insensitive
filesystem their generated `.Rd` files are the same file: whichever
roxygen writes second wins and the other is silently left undocumented.
