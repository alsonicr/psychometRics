# EFA comparaison model

Function that provide model fit for difference EFA model

## Usage

``` r
EFA.comp(data, items, nfactor, parallel = TRUE, fit = TRUE, ...)
```

## Arguments

- data:

  e

- items:

  e

- nfactor:

  e

- parallel:

  e

- fit:

  e

## Value

test

## Examples

``` r
data("inference")
items <- c(paste0("item_0",3:9),"item_10")
eval1 = EFA.comp(data = inference, items = items, nfactor = 1:3)
#> Models are process in parallel, if you want error and warning run with 'parallel = FALSE'
#> Models computation 
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
#> Fits computation 
#>   |                                                                              |                                                                      |   0%  |                                                                              |=======================                                               |  33%  |                                                                              |===============================================                       |  67%  |                                                                              |======================================================================| 100%
summary(eval1)
#> ##########################
#> ###       model 1      ###
#> ##########################
#> 
#> Standardized loadings: (* = significant at 1% level)
#> 
#>             F1       unique.var   communalities
#> item_03  0.538*           0.710           0.290
#> item_04  0.483*           0.767           0.233
#> item_05  0.583*           0.660           0.340
#> item_06  0.678*           0.540           0.460
#> item_07  0.573*           0.672           0.328
#> item_08  0.337*           0.887           0.113
#> item_09  0.484*           0.766           0.234
#> item_10  0.464*           0.785           0.215
#> 
#>                            F1
#> Sum of squared loadings 2.213
#> Proportion of total     1.000
#> Proportion var          0.277
#> Cumulative var          0.277
#> 
#> ##########################
#> ###       model 2      ###
#> ##########################
#> 
#> Standardized loadings: (* = significant at 1% level)
#> 
#>             F1      F2       unique.var   communalities
#> item_03  0.629*                   0.626           0.374
#> item_04  0.422       .            0.752           0.248
#> item_05  0.399       .            0.659           0.341
#> item_06  0.388   0.350            0.546           0.454
#> item_07      .   0.413            0.664           0.336
#> item_08      .       .            0.888           0.112
#> item_09      .   0.430            0.743           0.257
#> item_10          0.564*           0.708           0.292
#> 
#>                               F1    F2 total
#> Sum of sq (obliq) loadings 1.256 1.157 2.413
#> Proportion of total        0.521 0.479 1.000
#> Proportion var             0.157 0.145 0.302
#> Cumulative var             0.157 0.302 0.302
#> 
#> Factor correlations: (* = significant at 1% level)
#> 
#>        F1      F2 
#> F1  1.000         
#> F2  0.665*  1.000 
#> 
#> ##########################
#> ###       model 3      ###
#> ##########################
#> 
#> Standardized loadings: (* = significant at 1% level)
#> 
#>             F1      F2      F3       unique.var   communalities
#> item_03      .*  0.533*                   0.612           0.388
#> item_04      .   0.468*                   0.758           0.242
#> item_05          0.577*                   0.658           0.342
#> item_06          0.674*                   0.546           0.454
#> item_07          0.576*                   0.670           0.330
#> item_08                 14.976*        -223.289         224.289
#> item_09      .   0.499*                   0.744           0.256
#> item_10      .*  0.505*                   0.703           0.297
#> 
#>                                 F3     F2     F1   total
#> Sum of sq (obliq) loadings 224.289  2.125  0.184 226.598
#> Proportion of total          0.990  0.009  0.001   1.000
#> Proportion var              28.036  0.266  0.023  28.325
#> Cumulative var              28.036 28.302 28.325  28.325
#> 
#> Factor correlations: (* = significant at 1% level)
#> 
#>        F1      F2      F3 
#> F1  1.000                 
#> F2 -0.095   1.000         
#> F3 -0.003   0.022*  1.000 
#> 
#> ######################################
#> ###   Models Fit and comparaisons  ###
#> ######################################
#> 
#>               cfi   tli rmsea  srmr d.CFI d.TLI d.RMSEA d.SRMR Df      AIC
#> nfactor = 1 0.988 0.983 0.027 0.019    NA    NA      NA     NA  7 20624.48
#> nfactor = 2 1.000 1.000 0.003 0.010 0.012 0.016  -0.023 -0.010 13 20622.59
#> nfactor = 3 1.000 1.005 0.000 0.004 0.000 0.006  -0.003 -0.005 20 20648.35
#>                  BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
#> nfactor = 1 20790.89  3.246         NA    NA      NA         NA
#> nfactor = 2 20754.58 13.359     10.113 0.017       6       0.12
#> nfactor = 3 20740.17 53.123     39.764 0.045       7       0.00

```
