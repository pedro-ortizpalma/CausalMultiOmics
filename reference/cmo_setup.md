# What this installation can run, and what it is missing

Reports which evidence generators and which optional preprocessing
methods are available on this machine, and prints the
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
call that would enable the rest. The generator list is read from the
engine's own registry, so it stays correct as methods are added.

## Usage

``` r
cmo_setup(install = FALSE)
```

## Arguments

- install:

  Install the missing CRAN packages instead of only naming them.

## Value

A `data.frame` of class `cmo_setup`, one row per generator and per
optional preprocessing method, with columns `component`, `label`,
`status`, `requires`, `missing` and `kind`.

## See also

[`analyze`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analyze.md),
[`analysis_control`](https://pedro-ortizpalma.github.io/CausalMultiOmics/reference/analysis_control.md)

## Examples

``` r
cmo_setup()
#> 
#> CausalMultiOmics setup
#> ======================
#> 
#> Evidence generators: 9 of 9 available
#> 
#>   ok  association    Adjusted association           
#>   ok  conditional    Conditional independence       
#>   ok  survival       Cox proportional hazards       
#>   ok  longitudinal   Linear mixed model             
#>   ok  mediation      Bootstrap mediation            
#>   ok  elasticnet     Elastic net selection          
#>   ok  randomforest   Random forest importance       
#>   ok  bayesnet       Bayesian network structure     
#>   ok  sem            Structural equation model      
#> 
#> Optional preprocessing methods unavailable: 8 of 9
#>   --  imputation:knn                 needs VIM
#>   --  imputation:mice                needs mice
#>   --  imputation:missForest          needs missForest
#>   --  batch:combat                   needs sva
#>   --  batch:harmony                  needs harmony
#>   --  batch:ruv                      needs RUVSeq
#>   --  feature_selection:boruta       needs Boruta
#>   --  feature_selection:relief       needs FSelectorRcpp
#> 
#> To enable the rest:
#> 
#>   install.packages(c("VIM", "mice", "missForest", "sva", "harmony", "RUVSeq", "Boruta", "FSelectorRcpp"))
#> 
#>   or:  cmo_setup(install = TRUE)
#> 
#> An analysis runs without them, with fewer methods. That is not the
#> same as a smaller answer: the evidence score weights agreement between
#> methods, so a relationship seen by one generator scores below the same
#> relationship seen by four.

```
