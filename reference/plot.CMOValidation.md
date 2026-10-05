# Draw a figure recorded during the audit

Called without `which`, lists what is available instead of failing.

## Usage

``` r
# S3 method for class 'CMOValidation'
plot(x, which = NULL, block = NULL, ...)
```

## Arguments

- x:

  A `CMOValidation` object.

- which:

  Name of the figure. Use `cmo_plots(x)` to see the names.

- block:

  Block name, for figures drawn once per block.

- ...:

  Ignored.

## Value

The recorded plot, invisibly.

## See also

[`cmo_plots`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/cmo_plots.md),
[`check_data`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/check_data.md)

## Examples

``` r
data <- simulate_data(n = 60, blocks = list(main = 6), seed = 1)
audit <- check_data(data)

plot(audit)                                  # what is available
#> 
#> 18 figure(s) available
#> 
#>   missing_heatmap                per block: main
#>   missing_by_block            
#>   missing_pattern                per block: main
#>   overlap_heatmap             
#>   correlation_heatmap            per block: main
#>   correlation_distribution       per block: main
#>   histograms                     per block: main
#>   density                        per block: main
#>   qqplots                        per block: main
#>   boxplots                       per block: main
#>   violinplots                    per block: main
#>   pca                            per block: main
#>   scree                          per block: main
#>   sample_clustering              per block: main
#>   feature_clustering             per block: main
#>   outliers                       per block: main
#>   transformation_scores          per block: main
#>   transformation_comparison      per block: main
#> 
#> Draw one with:  plot(x, "missing_heatmap")
plot(audit, "missing_by_block")


```
