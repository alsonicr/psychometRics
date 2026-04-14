# Difficulty scan

Difficulty scan provides an indicator of item difficulty in classical
test theory. The difficulty is calculated as the item mean score divided
by the difference between the maximum and minimum score of the item.

## Usage

``` r
scan.diff(data, items, verbose = T)
```

## Arguments

- data:

  A data.frame containing participant score for each item

- items:

  The names of the items (should be \>1)

## Value

data.frame with item names and their difficulty

## See also

[`eval.ctt`](eval.ctt.md)

## Examples

``` r
data("inference")
items <- names(inference)
dif.scan(data = inference, items = items)
#> Error in dif.scan(data = inference, items = items): could not find function "dif.scan"

inference[1,1] <- 4
scan.diff(inference, items)
#> Warning: argument is not numeric or logical: returning NA
#> Error in max(data[, item], na.rm = T) - min(data[, item], na.rm = T): non-numeric argument to binary operator
```
