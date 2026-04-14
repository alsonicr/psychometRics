# FA.exploratory

The function provide information factor analysis and if the actual data
are recommended for factoriale analysis. It also used multiple
statisical function to provide an indication regarding the number of
factor present in the dataset. Those criteria containt the parallele
analysis method, the Velicer's MAP, VSS and Kaiser criterion.

## Usage

``` r
EFA.explo(data, items, plot = FALSE, ...)
```

## Arguments

- data:

  a data.frame containing the test or scale response in columns

- items:

  The name of the items

- plot:

  Plot vss and parallel analysis

- ...:

## Value

the result provide a print of the factor detected for each of the method

## See also

`fa.parallel` `vss`

## Examples

``` r
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
eval1 <- EFA.explo(inference, items)
eval1
#> 
#>  
#>   ### EFA analysis and adequacy ### 
#>  
#> Bartlett's K-squared = 235.06 df = 7 p-value < 4.163655e-47 
#> The Bartlett's test of sphericity was significant at an alpha level of .05. 
#> These data are probably suitable for factor analysis 
#>  
#>  
#> KMO Measure of Sampling Adequacy =  0.86 can be considerate for factor analysis has Meritorious – Good 
#>  
#> Parallele analysis factor solution :  2 
#> VSS complexity 1 factor solution   :  1 
#> VSS complexity 2 factor solution   :  4 
#> Velicer MAP factor solution        :  1 
#> kaiser criterion                   :  1 
#> BICfactor solution                 :  1 
#> eBIC factor solution               :  1 
```
