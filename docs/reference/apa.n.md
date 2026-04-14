# APA formating number

convert a numerical result to an apa norme restul

## Usage

``` r
apa.n(x, d = 2, oto = F, p = F)
```

## Arguments

- x:

  a numric

- d:

  the number of digits needed

- oto:

  a true of false statement (false by default) to indicate if the value
  x of the nurmric value provided would matematicly constrict between
  -one to +one a numric

- p:

  a true of false statement (false by default) to indicate if the value
  x is a p-value

## Value

a formated number for apa normed document

## Examples

``` r
apa.n(34.036, d=2) ;
#> [1] "34.04"
apa.n(34.036, d=3)
#> [1] "34.036"
apa.n(34.036, d=4)
#> [1] "34.0360"
apa.n(0.001, d=1)
#> [1] "0.0"
apa.n(0.001, d=2)
#> [1] "0.00"
apa.n(0.001, d=3)
#> [1] "0.001"
apa.n(0.001, d=2,p=T)
#> [1] ".00"
apa.n(0.001, d=3,p=T)
#> [1] ".001"
apa.n(-0.10, d=2,oto=T)
#> [1] "-.10"
apa.n(-0.10, d=3,oto=T)
#> [1] "-.100"
```
