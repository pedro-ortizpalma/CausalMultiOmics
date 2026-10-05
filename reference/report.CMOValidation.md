# Readable report for a CMOValidation object

Summarizes everything
[`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)
extracted: overall verdict and quality score, dataset composition,
per-block diagnostics, detected data issues, sample overlap between
blocks, the automatically derived preprocessing plan, the transformation
benchmark, quality-control checks, the prioritized action list and the
outputs available for inspection.

## Usage

``` r
# S3 method for class 'CMOValidation'
report(
  object,
  file = NULL,
  format = c("html", "console"),
  sections = c("overview", "blocks", "issues", "overlap", "preprocessing",
    "transformations", "qc", "actions", "outputs"),
  max_items = 12,
  width = 78,
  open = interactive(),
  prompt = TRUE,
  plot_width = 900,
  plot_height = 560,
  plot_res = 110,
  quiet = FALSE,
  ...
)
```

## Arguments

- object:

  A `CMOValidation` object.

- file:

  Destination path for the HTML report. May be a file name or a
  directory. When `NULL` (the default) the user is prompted for a
  location in an interactive session, and the working directory is used
  otherwise. Ignored when `format = "console"`.

- format:

  Either `"html"` (the default) to write a self-contained interactive
  document, or `"console"` to print the plain-text report.

- sections:

  Character vector selecting which sections to print. Any of
  `"overview"`, `"blocks"`, `"issues"`, `"overlap"`, `"preprocessing"`,
  `"transformations"`, `"qc"`, `"actions"`, `"outputs"`. Defaults to all
  of them.

- max_items:

  Maximum number of entries listed per itemized block (messages,
  recommendations, failed checks).

- width:

  Target line width.

- open:

  Whether to open the saved report in a browser.

- prompt:

  Whether to ask for a destination when `file` is `NULL`. Set to `FALSE`
  for unattended scripts.

- plot_width, plot_height, plot_res:

  Pixel dimensions and resolution used when rasterizing the stored plots
  into the document.

- quiet:

  Whether to suppress progress messages.

- ...:

  Ignored.

## Value

The validation object, invisibly.

## Details

Unlike [`summary()`](https://rdrr.io/r/base/summary.html), which dumps
every stored component, this method is organized for reading: it leads
with the verdict, flags only what needs attention, and tells the user
where the remaining detail lives.
