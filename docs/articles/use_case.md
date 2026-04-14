# Example and use case

## A classic scale

For this use case we will use the data from Rochat et al. (2025). The
scale is an inference measure scale for children between 6 and 10 years
old and is considered as a factor of comprehension.

We start by importing the data and defining the items for the analysis.

``` r
library(psychometRics)
library(dplyr)
library(kableExtra)

data("inference")
items <- c(paste0("item_0",3:9),"item_10")
head(inference)
```

    ##   ID session       date item_01 item_02 item_03 item_04 item_05 item_06 item_07
    ## 1  1      T5 2017-06-02       1       1       1       1       1       1       1
    ## 2  1      T6 2018-05-14       1       1       1       1       1       1       1
    ## 3  2      T5 2017-06-02       1       1       1       0       0       0       1
    ## 4  2      T6 2018-05-15       1       1       1       1       1       1       1
    ## 5  3      T5 2017-06-02       1       1       1       1       1       1       1
    ## 6  3      T6 2018-05-16       1       1       1       1       1       1       1
    ##   item_08 item_09 item_10 item_11 item_12 item_13 item_14
    ## 1       1       1       1      NA      NA      NA      NA
    ## 2       1       1       1      NA      NA      NA      NA
    ## 3       1       0       0      NA      NA      NA      NA
    ## 4       1       0       0      NA      NA      NA      NA
    ## 5       1       1       1      NA      NA      NA      NA
    ## 6       1       1       0      NA      NA      NA      NA

Now we can run the functions to evaluate classical test theory:
frequency of answers, item difficulty, item discrimination, and
Cronbach’s alpha.

``` r
scan.freq(data = inference, items = items)
```

    ##      item  freq
    ## 1 item_03 0.766
    ## 2 item_04 0.815
    ## 3 item_05 0.713
    ## 4 item_06 0.708
    ## 5 item_07 0.542
    ## 6 item_08 0.596
    ## 7 item_09 0.503
    ## 8 item_10 0.318

``` r
scan.diff(data = inference, items = items)
```

    ##      item difficulty
    ## 1 item_03      0.766
    ## 2 item_04      0.815
    ## 3 item_05      0.713
    ## 4 item_06      0.708
    ## 5 item_07      0.542
    ## 6 item_08      0.596
    ## 7 item_09      0.503
    ## 8 item_10      0.318

``` r
scan.disc(data = inference, items = items)
```

    ##   type    item     r r.lwr.ci r.upr.ci somers.d somers.lwr.ci somers.upr.ci
    ## 1  rir item_03 0.445    0.412    0.477    0.566         0.502         0.630
    ## 2  rir item_04 0.413    0.378    0.446    0.564         0.482         0.645
    ## 3  rir item_05 0.490    0.458    0.520    0.588         0.535         0.641
    ## 4  rir item_06 0.563    0.535    0.591    0.684         0.633         0.735
    ## 5  rir item_07 0.489    0.458    0.520    0.560         0.523         0.598
    ## 6  rir item_08 0.291    0.253    0.328    0.314         0.268         0.360
    ## 7  rir item_09 0.417    0.383    0.450    0.472         0.433         0.512
    ## 8  rir item_10 0.393    0.358    0.427    0.497         0.452         0.543
    ##   polyserial
    ## 1      0.615
    ## 2      0.600
    ## 3      0.650
    ## 4      0.746
    ## 5      0.615
    ## 6      0.369
    ## 7      0.523
    ## 8      0.513

``` r
scan.alpha(data = inference, items = items)
```

    ## progres: 1/8
    ## progres: 2/8
    ## progres: 3/8
    ## progres: 4/8
    ## progres: 5/8
    ## progres: 6/8
    ## progres: 7/8
    ## progres: 8/8

    ##      item alpha.gain     z drop.alpha alpha0
    ## 1 item_03     0.028  1.103      0.713  0.741
    ## 2 item_04     0.022  0.880      0.719  0.741
    ## 3 item_05     0.037† 1.525      0.704  0.741
    ## 4 item_06     0.052* 1.975      0.689  0.741
    ## 5 item_07     0.038† 1.493      0.703  0.741
    ## 6 item_08    -0.002  0.100      0.743  0.741
    ## 7 item_09     0.023  0.910      0.719  0.741
    ## 8 item_10     0.018  0.707      0.723  0.741

All those functions can be executed in a single command.

``` r
eval1 = eval.ctt(inference, items)
```

    ## frequence computation 
    ## item difficulty computation
    ## item discrimination (rir,rit) computation
    ## alpha computation 
    ## progres: 1/8
    ## progres: 2/8
    ## progres: 3/8
    ## progres: 4/8
    ## progres: 5/8
    ## progres: 6/8
    ## progres: 7/8
    ## progres: 8/8

``` r
eval1
```

    ##  Results summary : 
    ## 
    ## item        freq   difficulty       r   somers.d   polyserial  alpha.gain        z   drop.alpha   alpha0
    ## --------  ------  -----------  ------  ---------  -----------  -----------  ------  -----------  -------
    ## item_03    0.766        0.766   0.445      0.566        0.615  0.028         1.153        0.713    0.741
    ## item_04    0.815        0.815   0.413      0.564        0.600  0.022         0.902        0.719    0.741
    ## item_05    0.713        0.713   0.490      0.588        0.650  0.037†        1.465        0.704    0.741
    ## item_06    0.708        0.708   0.563      0.684        0.746  0.052*        1.975        0.689    0.741
    ## item_07    0.542        0.542   0.489      0.560        0.615  0.038†        1.437        0.703    0.741
    ## item_08    0.596        0.596   0.291      0.314        0.369  -0.002        0.099        0.743    0.741
    ## item_09    0.503        0.503   0.417      0.472        0.523  0.023         0.915        0.719    0.741
    ## item_10    0.318        0.318   0.393      0.497        0.513  0.018         0.741        0.723    0.741

A detection function to identify problematic items can also be run. The
objective of the function is to provide assistance in detecting weak
items. It returns a list of items and the reason for each flag. When
using the `summary` method, a kable table highlighting the flagged
values is produced.

``` r
d.eval1 <- detect.eval.ctt.warn(eval1)
d.eval1
```

    ## Warning detection CTT
    ## 
    ## List of items flag for frequency :   
    ## List of items flag for difficulty :   
    ## List of items flag for discrimination :   
    ## List of items flag for alpha :  item_03 item_04 item_05 item_06 item_07 item_08 item_09 item_10 
    ## 

By default, the detection method uses standard values, but you can
increase the requirements to ensure higher quality.

``` r
d.eval1.alt <- detect.eval.ctt.warn(eval1,threshold.disc = 0.35)
```

``` r
summary(d.eval1.alt)
```

    ## Warning detection CTT
    ## 
    ## List of items flag for frequency :   
    ## List of items flag for difficulty :   
    ## List of items flag for discrimination :  item_08 
    ## List of items flag for alpha :  item_03 item_04 item_05 item_06 item_07 item_08 item_09 item_10 
    ## 

| item    |  freq | difficulty | r     | somers.d | polyserial | alpha.gain | alpha0 | Total.warn |
|:--------|------:|-----------:|:------|:---------|-----------:|-----------:|:-------|-----------:|
| item_03 | 0.766 |      0.766 | 0.445 | 0.566    |      0.615 |      0.028 | 0.741  |          1 |
| item_04 | 0.815 |      0.815 | 0.413 | 0.564    |      0.600 |      0.022 | 0.741  |          1 |
| item_05 | 0.713 |      0.713 | 0.49  | 0.588    |      0.650 |      0.037 | 0.741  |          1 |
| item_06 | 0.708 |      0.708 | 0.563 | 0.684    |      0.746 |      0.052 | 0.741  |          1 |
| item_07 | 0.542 |      0.542 | 0.489 | 0.56     |      0.615 |      0.038 | 0.741  |          1 |
| item_08 | 0.596 |      0.596 | 0.291 | 0.314    |      0.369 |     -0.002 | 0.741  |          3 |
| item_09 | 0.503 |      0.503 | 0.417 | 0.472    |      0.523 |      0.023 | 0.741  |          1 |
| item_10 | 0.318 |      0.318 | 0.393 | 0.497    |      0.513 |      0.018 | 0.741  |          1 |

## The case of Multiple Choice Questions

Some test evaluations often include MCQs. MCQs are generally constituted
by a correct answer (called the key) and incorrect answers
(distractors). Those alternatives can be inspected to detect issues.

``` r
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
```

``` r
rar <- scan.rar(dat.score,items = paste0("item.",1:5),data.response = dat.response,items.rar = paste0("item.",1:5))
rar
```

    ##     item      A     B      C
    ## 1 item.1 -0.425 0.750 -0.246
    ## 2 item.2  0.032 0.448 -0.477
    ## 3 item.3  0.164 0.585 -0.674
    ## 4 item.4 -0.204 0.441 -0.335
    ## 5 item.5  0.051 0.430 -0.458

``` r
d.rar <- detect.rar.warning(rar, keys = key)
d.rar
```

    ## Warning detection for RAR
    ## 
    ## List of items with ambigus distractors :  item.1 item.2 item.3 item.4 item.5 
    ## List of items with weak key :  item.1 item.3 item.4 
    ## List of items with uncredible distractors :

``` r
summary(d.eval1.alt)
```

    ## Warning detection for RAR
    ## 
    ## List of items with ambigus distractors :  item.1 item.2 item.3 item.4 item.5 
    ## List of items with weak key :  item.1 item.3 item.4 
    ## List of items with uncredible distractors :

| item   | A      | B     | C      | ambigus.distractor | weak.key | uncredible.distractor |
|:-------|:-------|:------|:-------|:-------------------|:---------|:----------------------|
| item.1 | -0.425 | 0.75  | -0.246 | 1                  | 1        | NA                    |
| item.2 | 0.032  | 0.448 | -0.477 | 1                  | 0        | NA                    |
| item.3 | 0.164  | 0.585 | -0.674 | 2                  | 1        | NA                    |
| item.4 | -0.204 | 0.441 | -0.335 | 1                  | 1        | NA                    |
| item.5 | 0.051  | 0.43  | -0.458 | 1                  | 0        | NA                    |

## References

Rochat, N., Lima, L., & Bressoux, P. (2025). *The Riddle Knowledge
Inference Test (R-Kit)*. Journal of Psychoeducational Assessment, 43(3),
328-343.
