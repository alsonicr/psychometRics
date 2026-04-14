# Frequency scan

Frequency scan provides the frequency of correct answers for dichotomous
items (0 or 1) in a data frame

## Usage

``` r
scan.freq(data, items, verbose = T)
```

## Arguments

- data:

  A data.frame containing participant score for each item

- items:

  The names of the items (should be \>1)

## Value

data.frame with item names and their frequency of correct responses

## See also

[`eval.ctt`](eval.ctt.md)

## Examples

``` r
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
freq.scan(inference, items)
#> Error in freq.scan(inference, items): could not find function "freq.scan"

inference[1,1] <- 4
scan.freq(inference, items)
#>      item  freq
#> 1 item_03 0.766
#> 2 item_04 0.815
#> 3 item_05 0.713
#> 4 item_06 0.708
#> 5 item_07 0.542
#> 6 item_08 0.596
#> 7 item_09 0.503
#> 8 item_10 0.318
```
