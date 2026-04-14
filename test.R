

library(psychometRics)
library(dplyr)
library(doSNOW)
library(parallel)


## Inference Data

data("inference")
items <- c(paste0("item_0",3:9),"item_10")

a = scan.freq(inference, items)
a
x = x %>% as.data.frame() |> mutate_if(is.numeric, round, digits=3)
print(x)


scan.diff(inference, items)
scan.disc(inference, items)

scan.disc(inference, items, args.cor = list(alternative = "less"))


scan.alpha(inference, items, B=100)
s = scan.alpha(inference, items, parallel = T, probs = c(0.20, 0.80))

call <- quote(f(a, b))


eval1 = eval.ctt(inference, items,)
eval1



warp.func.args <- unique(c(formalArgs(scan.freq), formalArgs(scan.diff), formalArgs(scan.disc), formalArgs(scan.alpha)))

warp.func.args <- warp.func.args[ !warp.func.args %in% c("data","items")]


args <- list(a= 1, b= 2 )


arg.warp.func <- c(formalArgs(scan.disc),formalArgs(cor.test))
arg.warp.func <- arg.warp.func[ !arg.warp.func %in% c("data","items","...")]
tmp.args <- args[names(args) %in% arg.warp.func]


do.call(cor.test,
        c(list(b, a, conf.level = conf.level), args.cor))



d.eval1 <- detect.eval.ctt.warn(eval1)
summary(d.eval1)

efa <- EFA.explo(inference, items)
efa
efa.c <- EFA.comp(inference, items,nfactor = 1:3)
summary(efa.c)

quote

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

keys <- c("A","B","C","A","B")

dat.score <- data.frame()
for (i in 1:nrow(dat.response)){
  tmp <- dat.response[i,] == key
  dat.score <- rbind(dat.score, as.data.frame(tmp))
}

dat.score <- as.data.frame(sapply(dat.score,as.numeric))

rar <- scan.rar(dat.score,items = paste0("item.",1:5),data.response = dat.response,items.rar = paste0("item.",1:5))
rar


detect.rar.warning <- function(rar, keys, nbr.option=NULL, threshold.k = 0.20, threshold.d = -0.05){

  ### Testing warning
  ambigus.distractor  <- c()
  weak.key <- c()
  alternative <- unique(unlist(rar$param$data.response))


  for (resp  in alternative){

    t1 <- (keys!=resp & rar$rar[,resp]>= threshold.d)
    ambigus.distractor <- c(ambigus.distractor, t1)

    t2 <- ( (keys==resp & rar$rar[,resp]< threshold.k) | (keys==resp & is.na(rar$rar[,resp])) )
    weak.key <- c(weak.key, t2)
  }

  ## Ambigus distractor matrix
  ambigus.distractor <- matrix(ambigus.distractor,ncol=length(alternative))
  ambigus.distractor[is.na(ambigus.distractor)] <- F
  ambigus.distractor.col <- rowSums(ambigus.distractor) != 0

  ## Weak key matrix
  weak.key <- matrix(weak.key,ncol=length(alternative))
  weak.key[is.na(weak.key)] <- F
  weak.key.col <- rowSums(weak.key) != 0

  ## uncredible.distractor matrix
  if(!is.null(nbr.option)){
    uncredible.distractor <- rowSums(!is.na(data[,alternative])) != nbr.option
  }

  ## Summary table warning bool
  warning <- data.frame("Ambigus.Distractor" = ambigus.distractor.col, "Weak.Key" = weak.key.col)
  if(!is.null(nbr.option)){
    warning <- warning %>% mutate("Uncredible.Distractor" = uncredible.distractor)
  }

  ## Summary table warning bool
  warning.count <- data.frame(
    ambigus.distractor	= rowSums(ambigus.distractor),
    weak.key 	= rowSums(weak.key)
  )
  if(!is.null(nbr.option)){
    warning.count <- warning.count %>% cbind(rowSums(uncredible.distractor))
  }

  ### Results
  rez <- list()
  rez[["data_rar"]] <- rar$rar %>% select(all_of(alternative))
  rez[["data_info"]] <- rar$rar %>% select(!all_of(alternative))
  rez[["keys"]] <- keys
  rez[["ambigus.distractor"]] <- ambigus.distractor
  rez[["weak.key"]] <- weak.key
  rez[["aggregate.warning"]] <- ambigus.distractor | weak.key
  if(!is.null(nbr.option)){
    rez[["uncredible.distractor"]] <- uncredible.distractor
  }
  rez[["summary.warning"]] <- warning
  rez[["summary.count"]] <- warning.count







  class(rez) <- c("psychometRics","detecRar",class(rez))
  class(rez) <- "detecRar"
  return(rez)
}


#' a print function for deteRar class
#' @export

print.detecRar <- function(object){
  ### List of items production
  item.AD <- object$data_info$item[rowSums(object$ambigus.distractor)>0 ]
  item.WK <- object$data_info$item[rowSums(object$weak.key)>0 ]
  if(!is.null(object$uncredible.distractor)) {
    item.UD <- object$data_info$item[rowSums(object$uncredible.distractor)>0]
  } else {
    item.UD <- c()
  }

  cat("Warning detection for RAR\n\n")
  cat("List of items with ambigus distractors : ", item.AD,"\n")
  cat("List of items with weak key : ", item.WK,"\n")
  cat("List of items with uncredible distractors : ", item.UD,"\n\n")
}






aa = detect.rar.warning(rar,keys)
summary(aa)


























d.rar <- detect.rar.warning(data = rar, keys = key, response.columns = names(rar)[-1])
d.rar
summary(d.rar)


eval2 <- eval.ctt(dat.score,items = paste0("item.",1:5),data.response = dat.response,items.rar = paste0("item.",1:5),keys = key)
eval2


d.eval2 <- detect.eval.ctt.warn(dat=eval2)
d.eval2
summary(d.eval2)

