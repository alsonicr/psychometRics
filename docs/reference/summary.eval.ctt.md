# Print Method for CTT_summary

Print Method for CTT_summary

## Usage

``` r
summary.eval.ctt(x)
```

## Arguments

- x:

## Value

print the result of the function

## Examples

``` r
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
t <- CTT_summary(inference, items)
#> Error in CTT_summary(inference, items): could not find function "CTT_summary"
print(t)
#> function (x) 
#> UseMethod("t")
#> <bytecode: 0x6459e5f72dd8>
#> <environment: namespace:base>
```
