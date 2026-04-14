# The number needed to treat (NNT)

The number needed to treat (NNT)

## Usage

``` r
NNT(d = NULL, CER = NULL, EER = NULL, type = "classic")
```

## Arguments

- d:

  a cohen's d effect size or equivalent

- CER:

  the experimental event rate or a vector of value (between 0 and 1)

- EER:

  the control event rate between (0 and 1)

- type:

  the type of NNT could be "classic", "Furukawa" or "Kraemer"

## Value

an NNT score or a table of NNT score

## Examples

``` r
NNT(d=0.20,CER=0.20,type="Furukawa")
#>        nnt CER
#> 1 3.637894 0.2
NNT(d=0.20,type="Kraemer")
#> Warning: According tp Furukawa et Al (2011) the Kraemer method can't be unrelaibale and cannot take account of CER variation
#> [1] 8.89182
NNT(CER=.20,EER=.40)
#> [1] 5
```
