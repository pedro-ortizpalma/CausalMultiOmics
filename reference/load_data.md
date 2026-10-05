# Load Multi-Block Data

Creates a MultiOmicsData object from one or more data blocks.

## Usage

``` r
load_data(assays, metadata = NULL, modality = NULL)
```

## Arguments

- assays:

  Named list of matrices or data frames, each with sample identifiers as
  row names.

- metadata:

  Optional sample metadata. A `sample_id` column is used to match rows
  against the identifiers found in `assays`.

- modality:

  Optional named character vector declaring the assay modality of each
  block, for example `c(rna = "rnaseq", prot = "proteomics")`. Names
  must match names in `assays`; blocks left out are recorded as
  `"unknown"`. Recognised values are `"rnaseq"`, `"proteomics"`,
  `"metabolomics"`, `"microbiome"`, `"methylation"`, `"clinical"`,
  `"imaging"` and `"other"`. Any other string is stored but behaves like
  `"unknown"`.

## Value

A MultiOmicsData object.

## Details

Each element of `assays` represents an independent block of variables.
Blocks may correspond to any data modality (omics, imaging, clinical,
environmental, wearable devices, etc.) and are not required to contain
the same samples.

This function only imports and organizes the data. Validation,
harmonization, preprocessing and integration are performed by later
functions in the analysis workflow.

Every block must carry sample identifiers in its row names. They are
what links blocks to each other and to `metadata`, so a block that
relies on automatic row names is rejected rather than silently labelled
by position.

Blocks may optionally declare their assay `modality`. The statistical
type of a block can be detected from the numbers, but the modality
cannot: RNA-seq counts and any other counts look identical, and yet they
call for different normalization families. Declaring it lets
[`check_data()`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)
recommend modality-appropriate methods; leaving it out simply falls back
to type-driven defaults.

## See also

[`check_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md),
[`simulate_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/simulate_data.md),
[`preprocess`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/preprocess.md)

## Examples

``` r
tr <- data.frame(
  Gene1 = rnorm(10),
  Gene2 = rnorm(10),
  row.names = paste0("S",1:10)
)

pr <- data.frame(
  Prot1 = rnorm(8),
  Prot2 = rnorm(8),
  row.names = paste0("S",3:10)
)

x <- load_data(
  assays = list(
    transcriptomics = tr,
    proteomics = pr
  )
)

```
