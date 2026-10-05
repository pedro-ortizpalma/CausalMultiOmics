# Intended order of the preprocessing stages for one data type

`PreprocessingRecipe` stores which method to use at every stage but not
the order in which the stages must run. For count data the library size
has to be normalized before any variance-stabilizing transformation is
applied; for every other data type the transformation comes first. The
resolved order is recorded in the recipe comments so that the plan
remains self-describing.

## Usage

``` r
.stage_order(data_type)
```
