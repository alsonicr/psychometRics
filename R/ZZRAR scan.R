

#' Correlation Answer Rest (RAR)
#'
#' the function provide a correlation for Multiple Question Choice (MQC) and
#' provide a correlation between an answer and the score rest. Normaly the
#' result for the correct answer (Key) is similare at the RIR results. The result
#' of the RAR can identify problème sucha as weak distractor, ambiguis
#' distractor (look alike good answer) or key error /
#'
#'
#' @param data.score A data.frame containing the items scale response in columns
#' @param data.response A data.frame contaning the answer response in columns
#' @param items The name of the MQC items
#' @noRd
#' @return the result provide a print of the factor detected for each of the method
#' @import dplyr
#' @examples
#'
#' item.1 <- c(rep("A", 15), rep("B", 10), rep("C", 15))
#' item.2 <- c(rep("A", 10), rep("B", 15), rep("C", 15))
#' item.3 <- c(rep("A", 5), rep("B", 15), rep("C", 20))
#' item.4 <- c(rep("A", 3), rep("B", 19), rep("C", 18))
#' item.5 <- c(rep("A", 11), rep("B", 13), rep("C", 16))
#'
#' dat.response <- data.frame(
#'   item.1 = item.1,
#'   item.2 = item.2,
#'   item.3 = item.3,
#'   item.4 = item.4,
#'   item.5 = item.5
#' )
#'
#' key <- c("A","B","C","A","B")
#'
#' dat.score <- data.frame()
#' for (i in 1:nrow(dat.response)){
#'   tmp <- dat.response[i,] == key
#'   dat.score <- rbind(dat.score, as.data.frame(tmp))
#' }
#'
#' dat.score <- as.data.frame(sapply(dat.score,as.numeric))
#'
#' rez <- rar.scan(dat.score, dat.response, items = paste0("item.",1:5))
#' rez






rar.scan <- function(data.score, data.response, items){
  message("deprecated use scan.rar() instead")
  rar <- data.frame()

  for(item in items){
    rez = c()

    ### Response options
    response_option <- sort(unique(data.response[,item]))

    ### Rar computation
    for(alt in  response_option){
      comp <- ifelse(data.response[,item] == alt, 1, 0)
      score <- rowSums(data.score %>% select(items) %>% select(-item),na.rm = T)
      tmp <- cor(comp, score, use="complete.obs")
      rez <- c(rez, tmp)
    }

    ### Data format
    tmp <- list()
    tmp[["item_id"]] <- item
    for (i in 1:length(response_option)){
      tmp[response_option[i]] <- rez[i]
    }

    rar <- plyr::rbind.fill(rar, as.data.frame(tmp))

  }

  class(rar) <- c("psychometRics", "rar" , class(rar))

  return(rar)
}
