# Factor explorer

the function use parallel analyse, Velicer and VSS method to provide the
possible number of factor.

## Usage

``` r
factor_explorer(data, items, plot = FALSE, ...)
```

## Arguments

- data:

  a data.frame containing the test or scale response in columns

- items:

  The name of the items

- plot:

  Plot vss and parallel

- ...:

## Value

the result provide a print of the factor detected for each of the method

## Examples

``` r
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
factor_explorer(inference, items)
#> Error in factor_explorer(inference, items): could not find function "factor_explorer"
```
