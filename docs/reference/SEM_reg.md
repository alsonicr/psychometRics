# SEM regression model

SEM regression model

## Usage

``` r
SEM_reg(y, x, latent = FALSE)
```

## Arguments

- y:

  A vector of string who contain the name of the predicted variable

- x:

  A vector of string who contain the name of the predictor variable

## Value

A string formula for a Lavaan model

## Examples

``` r
SEM_reg("Y","x")
#> Y  ~  x 
SEM_reg("Y",c("x1","x2","x3"))
#> Y  ~  x1 + x2 + x3 
SEM_reg(c("Y1","Y2","Y3"),c("x1","x2","x3"))
#> Y1 + Y2 + Y3  ~  x1 + x2 + x3 
SEM_reg("Y",c("x1","x2","x3"), latent=T)
#> Y  =~  x1 + x2 + x3 

```
