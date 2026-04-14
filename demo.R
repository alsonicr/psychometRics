

library(psychometRics)
library(dplyr)

## Inference Data

data("inference")
items <- c(paste0("item_0",3:9),"item_10")

scan.freq(inference, items)
scan.diff(inference, items)
scan.disc(inference, items)
scan.disc(data = inference, items = items)
scan.alpha(data = inference, items = items,B = 1000)
scan.alpha(inference, items, parallel = T,B = 2000)







eval1 = eval.ctt(inference, items)
eval1


d.eval1 <- detect.eval.ctt.warn(eval1)
summary(d.eval1)

efa <- EFA.explo(inference, items)
efa
efa.c <- EFA.comp(inference, items,nfactor = 1:3)
summary(efa.c)


## Rar Data


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

rar <- scan.rar(data.score = dat.score,items = paste0("item.",1:5),data.response = dat.response,items.rar = paste0("item.",1:5))
rar

d.rar <- detect.rar.warning(data = rar, keys = key, response.columns = names(rar)[-1])
d.rar
summary(d.rar)


eval2 <- eval.ctt(dat.score,items = paste0("item.",1:5),data.response = dat.response,items.rar = paste0("item.",1:5),keys = key)
eval2


d.eval2 <- detect.eval.ctt.warn(dat=eval2)
d.eval2
summary(d.eval2)

