# Resolve the destination path for the HTML report

When `file` is `NULL` the user is prompted. In a non-interactive session
the prompt cannot block, so the default path is used and reported.

## Usage

``` r
.report_destination(
  file = NULL,
  default_name = "CausalMultiOmics_validation_report.html",
  prompt = TRUE
)
```
