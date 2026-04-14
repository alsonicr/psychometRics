# Crosslag model formulat

Create a Lavaan modem formulat for a classic crosslag model

## Usage

``` r
crosslag_model(
  variables,
  times,
  order = "times/variables",
  join = "_",
  prefix = NULL
)
```

## Arguments

- variables:

  A vector of names variables

- times:

  names of the time indicator for the model

- order:

  a string defining the order of the variable and the time indicator by
  default "times/variables" or "variables/times"

- join:

  a string defining by what carractere are join variable and time (by
  default "\_")

## Value

return a lavaan formulat for a crosslag model

## Examples

``` r
vars = c("x","y")
t = 1:4
crosslag_model(variables = vars, times = t, order = "times/variables", join="_" )
#> ### Regression ###
#> 2_x + 2_y  ~  1_x + 1_y 
#> 3_x + 3_y  ~  2_x + 2_y 
#> 4_x + 4_y  ~  3_x + 3_y 
#> 
#> ### Covariable ###
#> 1_x ~~ 1_y
#> 1_x ~~ 1_x
#> 1_y ~~ 1_y
#> 2_x ~~ 2_y
#> 2_x ~~ 2_x
#> 2_y ~~ 2_y
#> 3_x ~~ 3_y
#> 3_x ~~ 3_x
#> 3_y ~~ 3_y
#> 4_x ~~ 4_y
#> 4_x ~~ 4_x
#> 4_y ~~ 4_y
crosslag_model(variables = vars, times = t, order = "variables/times", join="" )
#> ### Regression ###
#> x2 + y2  ~  x1 + y1 
#> x3 + y3  ~  x2 + y2 
#> x4 + y4  ~  x3 + y3 
#> 
#> ### Covariable ###
#> x1 ~~ y1
#> x1 ~~ x1
#> y1 ~~ y1
#> x2 ~~ y2
#> x2 ~~ x2
#> y2 ~~ y2
#> x3 ~~ y3
#> x3 ~~ x3
#> y3 ~~ y3
#> x4 ~~ y4
#> x4 ~~ x4
#> y4 ~~ y4
```
