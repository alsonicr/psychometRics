# Discrimination correlation

The function provides item-rest (RIR) or item-total (RIT) correlations
for all items in the test. Multiple types of item correlation are
provided such as Somers' D, point-biserial correlation, and polyserial
correlation for non-dichotomous items. Results can be interpreted as the
item's ability to discriminate between lower- and higher-performing
respondents with respect to the overall raw score.

## Usage

``` r
scan.disc(
  data,
  items,
  type = "rir",
  method = c("cor", "somer", "polyserial"),
  conf.level = 0.95,
  args.cor = list(),
  args.somer = list(),
  args.polyserial = list()
)
```

## Arguments

- data:

  A data.frame containing participant score for each item

- items:

  The names of the items (should be \>1)

- type:

  Type of correlation: 'rir' for rest-item correlation or 'rit' for
  total-item correlation (can be both)

- method:

  The type(s) of correlation to use: c("cor","somer","polyserial")

- conf.level:

  The confidence level for the correlation

- ...:

  Argument for the cor.test() function

## Value

a data.frame with the result and confidence interval

## See also

[`eval.ctt`](eval.ctt.md)

## Examples

``` r
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
scan.disc(data = inference, item = items, type ="rir")
#>   type    item     r r.lwr.ci r.upr.ci somers.d somers.lwr.ci somers.upr.ci
#> 1  rir item_03 0.445    0.412    0.477    0.566         0.502         0.630
#> 2  rir item_04 0.413    0.378    0.446    0.564         0.482         0.645
#> 3  rir item_05 0.490    0.458    0.520    0.588         0.535         0.641
#> 4  rir item_06 0.563    0.535    0.591    0.684         0.633         0.735
#> 5  rir item_07 0.489    0.458    0.520    0.560         0.523         0.598
#> 6  rir item_08 0.291    0.253    0.328    0.314         0.268         0.360
#> 7  rir item_09 0.417    0.383    0.450    0.472         0.433         0.512
#> 8  rir item_10 0.393    0.358    0.427    0.497         0.452         0.543
#>   polyserial
#> 1      0.615
#> 2      0.600
#> 3      0.650
#> 4      0.746
#> 5      0.615
#> 6      0.369
#> 7      0.523
#> 8      0.513
scan.disc(data = inference, item = items, type ="rit")
#>   type    item     r r.lwr.ci r.upr.ci somers.d somers.lwr.ci somers.upr.ci
#> 1  rit item_03 0.593    0.566    0.619    0.742         0.672         0.812
#> 2  rit item_04 0.554    0.525    0.582    0.735         0.641         0.828
#> 3  rit item_05 0.638    0.613    0.662    0.767         0.713         0.820
#> 4  rit item_06 0.696    0.675    0.717    0.840         0.788         0.893
#> 5  rit item_07 0.652    0.628    0.675    0.765         0.738         0.792
#> 6  rit item_08 0.489    0.457    0.519    0.554         0.515         0.593
#> 7  rit item_09 0.596    0.569    0.622    0.699         0.669         0.729
#> 8  rit item_10 0.565    0.536    0.592    0.741         0.695         0.787
#>   polyserial
#> 1      0.819
#> 2      0.806
#> 3      0.848
#> 4      0.922
#> 5      0.819
#> 6      0.619
#> 7      0.747
#> 8      0.737
```
