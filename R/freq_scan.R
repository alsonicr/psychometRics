#' Frequency scam
#'
#' Frequency scan provide the frequency of correct answer for dichotomic item (0 or 1) in a data frame
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#'
#' @return data.frame with the item name and their frequency of correct response
#' @export
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' freq.scan(inference, items)
#'
#' inference[1,1] <- 4
#' freq.scan(inference, items)

freq.scan <- function(data, items, verbose = T) {
  freq <- data.frame()
  for (item in items) {

    if (sum(!(data[,item] %in% c(0,1,NA)))>0){
      if(verbose) cat("Item : ", item," is not a dichotomus item frequency will return NA \n")
      freq <- rbind(freq, data.frame(item = item, freq = NA))
    } else {
      tmp <- mean(data[, item], na.rm = T)
      freq <- rbind(freq, data.frame(item = item, freq = tmp))
    }
  }
  freq
}

