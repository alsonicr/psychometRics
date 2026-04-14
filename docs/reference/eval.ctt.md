# Classical Test Theory Summary

the fonction provide a warpping of the [`scan.freq`](scan.freq.md),
[`scan.diff`](scan.diff.md),
[`scan.disc`](scan.disc.md)[`scan.alpha`](scan.alpha.md) and
[`scan.rar`](scan.rar.md) functions

## Usage

``` r
eval.ctt(
  data,
  items,
  data.response = NULL,
  items.rar = NULL,
  keys = NULL,
  digits = 3,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  a data.frame containing the test or scale response in columns

- items:

  The name of the items (should be \>1 )

- digits:

  number of digits for result

- ...:

## Value

a summary of the result and a list with the complet result of each
function

## See also

[`scan.freq`](scan.freq.md), [`scan.diff`](scan.diff.md),
[`scan.disc`](scan.disc.md)[`scan.alpha`](scan.alpha.md) and
[`scan.rar`](scan.rar.md)

## Examples

``` r
library(psychometRics)
library(dplyr)

data("inference")
items <- c(paste0("item_0",3:9),"item_10")
ev1 <- eval.ctt(data = inference, items = items)
#> frequence computation 
#> item difficulty computation
#> item discrimination (rir,rit) computation
#> alpha computation 
#> progres: 1/8
#> progres: 2/8
#> progres: 3/8
#> progres: 4/8
#> progres: 5/8
#> progres: 6/8
#> progres: 7/8
#> progres: 8/8
ev1
#>  Results summary : 
#> 
#> item        freq   difficulty       r   somers.d   polyserial  alpha.gain        z   drop.alpha   alpha0
#> --------  ------  -----------  ------  ---------  -----------  -----------  ------  -----------  -------
#> item_03    0.766        0.766   0.445      0.566        0.615  0.028         1.110        0.713    0.741
#> item_04    0.815        0.815   0.413      0.564        0.600  0.022         0.871        0.719    0.741
#> item_05    0.713        0.713   0.490      0.588        0.650  0.037†        1.525        0.704    0.741
#> item_06    0.708        0.708   0.563      0.684        0.746  0.052*        1.947        0.689    0.741
#> item_07    0.542        0.542   0.489      0.560        0.615  0.038†        1.509        0.703    0.741
#> item_08    0.596        0.596   0.291      0.314        0.369  -0.002        0.100        0.743    0.741
#> item_09    0.503        0.503   0.417      0.472        0.523  0.023         0.909        0.719    0.741
#> item_10    0.318        0.318   0.393      0.497        0.513  0.018         0.712        0.723    0.741
#> 


item.1 <- c(rep("A", 15), rep("B", 10), rep("C", 15))
item.2 <- c(rep("A", 10), rep("B", 15), rep("C", 15))
item.3 <- c(rep("A", 5), rep("B", 15), rep("C", 20))
item.4 <- c(rep("A", 3), rep("B", 19), rep("C", 18))
item.5 <- c(rep("A", 11), rep("B", 13), rep("C", 16))

dat.response <- data.frame(
  item.1 = item.1,
  item.2 = item.2,
  item.3 = item.3,
  item.4 = item.4,
  item.5 = item.5
)

key <- c("A","B","C","A","B")

dat.score <- data.frame()
for (i in 1:nrow(dat.response)){
  tmp <- dat.response[i,] == key
  dat.score <- rbind(dat.score, as.data.frame(tmp))
}

dat.score <- as.data.frame(sapply(dat.score,as.numeric))
ev2 <- eval.ctt(dat.score,items = paste0("item.",1:5),data.response = dat.response,items.rar = paste0("item.",1:5),keys = key)
#> frequence computation 
#> item difficulty computation
#> item discrimination (rir,rit) computation
#> alpha computation 
#> progres: 1/5
#> progres: 2/5
#> progres: 3/5
#> progres: 4/5
#> progres: 5/5
#> RAR computation 
ev2
#>  Results summary : 
#> 
#> item       freq   difficulty        r   somers.d   polyserial  alpha.gain        z   drop.alpha   alpha0
#> -------  ------  -----------  -------  ---------  -----------  -----------  ------  -----------  -------
#> item.1    0.375        0.375   -0.425     -0.456       -0.543  -0.715        0.853       -0.042   -0.757
#> item.2    0.375        0.375    0.448      0.413        0.572  3.080*        1.675       -3.837   -0.757
#> item.3    0.500        0.500   -0.674     -0.752       -0.845  -1.153*       1.674        0.396   -0.757
#> item.4    0.075        0.075   -0.204     -0.405       -0.380  -0.149        0.154       -0.608   -0.757
#> item.5    0.325        0.325    0.430      0.430        0.560  2.700*        2.139       -3.457   -0.757
#> 
#> RAR table : 
#> 
#> Error in summary.eval.ctt(x): object 'eval2' not found
```
