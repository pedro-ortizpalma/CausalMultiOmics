# Which figures an object carries

Lists the recorded plots stored inside a result object, and for the ones
that were drawn per block, which blocks are available.

## Usage

``` r
cmo_plots(x)
```

## Arguments

- x:

  A `CMOValidation`, `PreprocessingResult` or `CMOResult` object.

## Value

The plot names, invisibly. Called for the listing it prints.

## See also

[`plot.CMOValidation`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/plot.CMOValidation.md)

## Examples

``` r
data <- simulate_data(n = 60, blocks = list(main = 6), seed = 1)
audit <- check_data(data)
cmo_plots(audit)
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

```
