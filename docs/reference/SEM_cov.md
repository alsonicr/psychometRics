# Title

Title

## Usage

``` r
SEM_cov(variables, variance = T)
```

## Arguments

- variables:

  A vector of names variables

- variance:

  If TRUE variance of variable will be writeen

## Value

return a string of model format for lavaan

## Examples

``` r
SEM_cov(c("A","B","C","D"))
#> A ~~ B
#> A ~~ C
#> A ~~ D
#> B ~~ C
#> B ~~ D
#> C ~~ D
#> A ~~ A
#> B ~~ B
#> C ~~ C
#> D ~~ D
SEM_cov(c("A","B","C","D"), variance = FALSE)
#> A ~~ B
#> A ~~ C
#> A ~~ D
#> B ~~ C
#> B ~~ D
#> C ~~ D
```
