

library(psychometRics)
data("inference")
items <- c(paste0("item_0",3:9),"item_10")

psychometRics::


freq_scan(inference, items)
discrimination_cor(inference, items)

alpa.scan(inference, items)
CTT_summary(inference, items)
f <- factor_explorer(inference, items)
t <- factor_comparison(inference, items, f)



