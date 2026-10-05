# Readable report for an analysis result

Turns a `CMOResult` into a self-contained HTML document written for a
reader who is not a statistician: findings first, in plain language,
with the technical tables tucked behind `audience = "technical"`.

## Usage

``` r
# S3 method for class 'CMOResult'
report(
  object,
  file = NULL,
  format = c("html", "console"),
  audience = c("general", "technical"),
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

  A `CMOResult` object.

- file:

  Destination path for the HTML report. May be a file name or a
  directory. When `NULL` the user is prompted in an interactive session
  and the working directory is used otherwise.

- format:

  Either `"html"` (the default) or `"console"` for a plain-text summary.

- audience:

  `"general"` (the default) writes for a non-specialist reader.
  `"technical"` adds a section with the complete tables and every
  diagnostic figure.

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

The analysis object, invisibly.

## Details

The report states throughout what the numbers do and do not support. A
reader who does not know what an adjusted association is will otherwise
read a high score as proof of cause, so every finding carries a plain
label for how much can be claimed, and the reasoning behind that label
is explained in its own section rather than buried in a footnote.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md)
