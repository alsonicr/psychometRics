
library(psychometRics)
data("inference")
items <- c(paste0("item_0",3:9),"item_10")



freq.scan(inference, items)
discrimination_cor(inference, items)
alpha.scan(inference, items)
CTT_summary(inference, items)


f <- factor_explorer(inference, items)
f
t <- factor_comparison(inference, items, f)
t


