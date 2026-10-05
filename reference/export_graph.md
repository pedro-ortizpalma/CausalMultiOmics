# Write the evidence graph in a format another tool can open

The graph is the deliverable for a good many users, and most of them
will want to lay it out somewhere this package has no business trying to
be. Cytoscape and Gephi both read GraphML, which is the default.

## Usage

``` r
export_graph(object, file, format = c("graphml", "dot", "json"), min_score = 0)
```

## Arguments

- object:

  A `CMOResult`.

- file:

  Where to write. The extension is not checked against `format`; the
  format argument decides.

- format:

  One of `"graphml"` (Cytoscape, Gephi), `"dot"` (Graphviz) or `"json"`.
  The first two go through igraph; `"json"` is written directly and
  needs nothing installed.

  GML and Pajek are deliberately absent. Pajek carries no edge
  attributes, so a graph exported to it arrives as arrows with none of
  the evidence that made it worth exporting, and igraph's GML writer
  rejects the vertex table this package produces. A format that silently
  discards the answer or fails outright is worse than one that is not
  offered.

- min_score:

  Relationships scoring below this are left out. The default keeps
  everything.

## Value

The path, invisibly.

## Details

Every edge attribute travels with it, so the evidence survives the
export: the score, the three components it is made of, the
identification, the data quality and the FDR are all readable in the
receiving tool. A graph exported with only its arrows would arrive
stripped of everything that made it worth reading.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`report`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/report.md),
[`as.data.frame.CMOResult`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/as.data.frame.CMOResult.md)

## Examples

``` r
# \donttest{
sim <- simulate_data(n = 100, blocks = list(a = 4))
prep <- preprocess(sim, check_data(sim), plots = FALSE, quiet = TRUE)
res <- analyze(prep, "y", effort = "fast", plots = FALSE, quiet = TRUE,
               control = analysis_control(methods = "association"))

export_graph(res, file.path(tempdir(), "graph.graphml"))
# }
```
