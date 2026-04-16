# Facto print method

formating result for the facto class

## Usage

``` r
# S3 method for class 'facto'
print(x)
```

## Arguments

- x:

  a facto class object

## Examples

``` r
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
f <- factor_explorer(inference, items)
#> Error in factor_explorer(inference, items): could not find function "factor_explorer"
print(t)
#> function (x) 
#> UseMethod("t")
#> <bytecode: 0x6459e5f72dd8>
#> <environment: namespace:base>
```
