# Correlation Answer Rest (RAR)

The function computes correlations for Multiple Choice Questions (MCQs)
and provides a correlation between an answer and the score-rest.
Normally the result for the correct answer (Key) is similar to the RIR
results. The result of the RAR can identify problems such as weak
distractors, ambiguous distractors (look-alike correct answers), or key
errors.

## Usage

``` r
scan.rar(data.score, data.response, items.rar, items = NULL)
```

## Arguments

- data.score:

  A data.frame containing the items scale response in columns

- data.response:

  A data.frame containing the answer response in columns

- items.rar:

  a Vector of all the RAR items names

- items:

  a Vector of all the test items names

## Value

the result provide a print of the factor detected for each of the method

## Details

In RAR the correct answer is also called the "key". Normally the result
for the key is equal to the RIR score of the item. Other items, called
distractors, should normally have a negative correlation with the total
score. Ambiguous distractors are distractors whose RAR could be
positive. A wrong answer (i.e., selection of a distractor) should not be
positively correlated with participant score; if it is,
higher-performing participants tend to select this distractor. This
could result from an incorrect key or ambiguous item wording.

## References

reference

## See also

`detec.rar.warning`, [`eval.ctt`](eval.ctt.md)

## Examples

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

rez <- scan.rar(dat.score, dat.response, items.rar = paste0("item.",1:5))
rez
#>     item      A     B      C
#> 1 item.1 -0.425 0.750 -0.246
#> 2 item.2  0.032 0.448 -0.477
#> 3 item.3  0.164 0.585 -0.674
#> 4 item.4 -0.204 0.441 -0.335
#> 5 item.5  0.051 0.430 -0.458
```
