
#' Correlation Answer Rest (RAR) warning table
#'
#' The function provide assistance to detect red flag item in the  \code{\link{scan.RAR}} result.
#' It provide detection for ambigus distractor and weak key selection.
#'
#'
#' @param data A psychometric rar class data containing the results for rar
#' @param keys A vector of correct answered for each items
#' @param response.columns Avector containing the name of each colomn containing possible answer
#' @param threshold.k A minimun threshold for the key to be considered acceptable (weak key detection)
#' @param threshold.D A maximun threshold for a distractor to be considered acceptable (ambigus distractor detection)
#'
#' @import kableExtra
#' @importFrom kableExtra cell_spec
#' @import dplyr
#' @export
#' @return A class detecRar list with table of item detection and kable table to visualise the problematics items
#'
#'
#' @seealso \code{\link{scan.RAR}}, \code{\link{eval.ctt}}
#'
#' @examples
#'
#' library(psychometRics)
#' library(dplyr)
#'
#'  item.1 <- c(rep("A", 15), rep("B", 10), rep("C", 15))
#'  item.2 <- c(rep("A", 10), rep("B", 15), rep("C", 15))
#'  item.3 <- c(rep("A", 5), rep("B", 15), rep("C", 20))
#'  item.4 <- c(rep("A", 3), rep("B", 19), rep("C", 18))
#'  item.5 <- c(rep("A", 11), rep("B", 13), rep("C", 16))
#'
#'  dat.response <- data.frame(
#'    item.1 = item.1,
#'    item.2 = item.2,
#'    item.3 = item.3,
#'    item.4 = item.4,
#'    item.5 = item.5
#'  )
#'
#'  key <- c("A","B","C","A","B")
#'
#'  dat.score <- data.frame()
#'  for (i in 1:nrow(dat.response)){
#'    tmp <- dat.response[i,] == key
#'    dat.score <- rbind(dat.score, as.data.frame(tmp))
#'  }
#'
#'  dat.score <- as.data.frame(sapply(dat.score,as.numeric))
#'
#' rar <- scan.rar(dat.score, dat.response, items.rar = paste0("item.",1:5))
#' rez = detect.rar.warning(data = rar, keys = key, response.columns = names(rar)[-1])
#' rez
#'
#'



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



#' a summary function for detecRar class
#'
#' @importFrom kableExtra cell_spec kbl kable_paper
#' @import dplyr
#' @export

setClass("detecRar")
setMethod(f="summary", signature(object="detecRar"),
function(object) {

  ### table
  tbl.summa <- cbind(object$data_info,object$data_rar,object$summary.warning) |> mutate_if(is.numeric, round, digits=3)

  ### Kable table

  tmp <- object$data_rar %>%  mutate_if(is.numeric, round, digits = 3 )
  tmp[object$aggregate.warning] <- cell_spec(tmp[object$aggregate.warning],  background = "red")

  ambigus.distractor <- rowSums(object$ambigus.distractor)
  weak.key <-  rowSums(object$weak.key)
   if(!is.null(object$uncredible.distractor)) {
    uncredible.distractor <- rowSums(object$uncredible.distractor)
  } else {
    uncredible.distractor <- NA
  }

  tmp2 <- data.frame(ambigus.distractor,weak.key,uncredible.distractor)
  tmp2[tmp2>0 & !is.na(tmp2)] <- cell_spec(tmp2[tmp2>0 & !is.na(tmp2)],  background = "red")

  rez <- object$data_info |> cbind(tmp,tmp2) %>%
    kbl( booktabs = T, digits = 3, longtable = T,escape = F) %>%
    kable_paper("striped", full_width = F)


  ### Print result

  print(object)
  print(tbl.summa)
  print(rez)

})




