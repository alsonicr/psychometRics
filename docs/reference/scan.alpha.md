# Cronbach alpha scan

The function provides the variation of reliability (Cronbach's alpha)
when an item is dropped. It also computes a z-test comparing alpha with
and without the item. The result is a table with the alpha when the item
is removed, the alpha gain (difference between the alpha with all items
and without the item), the z value and the p value. Positive alpha gain
indicates alpha decreases when the item is removed; negative alpha gain
indicates alpha increases when the item is removed. In general, alpha
gain should be positive; if negative, the item may be unreliable and
considered for removal. The function supports parallel execution when
appropriate.

## Usage

``` r
scan.alpha(data, items, digits = 3, parallel = FALSE, verbose = TRUE, ...)
```

## Arguments

- data:

  A data.frame containing participant scores for each item

- items:

  The names of the items (should be \>1)

- digits:

  Number of digits in the table summary

- parallel:

  Bool: use parallel backend (doSNOW) when there are many items

- verbose:

  Bool for progress visualization

- ...:

  arguments passed to `cronbach.alpha` from the ltm package

## Value

A data.frame with alpha variations and a z-test per item

## Details

This function provides the variation of Cronbach's alpha when an item is
dropped from the data.

## See also

[`eval.ctt`](eval.ctt.md), `cronbach.alpha`

## Examples

``` r
data("inference")
items <- c(paste0("item_0", 3:9), "item_10")
alp1 <- scan.alpha(data = inference, items = items)
#> progres: 1/8
#> progres: 2/8
#> progres: 3/8
#> progres: 4/8
#> progres: 5/8
#> progres: 6/8
#> progres: 7/8
#> progres: 8/8
alp1
#>      item alpha.gain     z drop.alpha alpha0
#> 1 item_03     0.028  1.127      0.713  0.741
#> 2 item_04     0.022  0.897      0.719  0.741
#> 3 item_05     0.037† 1.503      0.704  0.741
#> 4 item_06     0.052* 1.967      0.689  0.741
#> 5 item_07     0.038† 1.400      0.703  0.741
#> 6 item_08    -0.002  0.099      0.743  0.741
#> 7 item_09     0.023  0.910      0.719  0.741
#> 8 item_10     0.018  0.731      0.723  0.741
alp2 <- scan.alpha(inference, items, parallel = TRUE, verbose = FALSE)
alp2
#>      item alpha.gain     z drop.alpha alpha0
#> 1 item_03     0.028  1.144      0.713  0.741
#> 2 item_04     0.022  0.869      0.719  0.741
#> 3 item_05     0.037† 1.489      0.704  0.741
#> 4 item_06     0.052* 2.001      0.689  0.741
#> 5 item_07     0.038† 1.481      0.703  0.741
#> 6 item_08    -0.002  0.098      0.743  0.741
#> 7 item_09     0.023  0.898      0.719  0.741
#> 8 item_10     0.018  0.732      0.723  0.741
```
