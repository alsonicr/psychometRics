#' Frequency scan
#'
#' Frequency scan provides the frequency of correct answers for dichotomous items (0 or 1) in a data frame
#'
#' @param data A data.frame containing participant score for each item
#' @param items The names of the items (should be >1)
#'
#' @return data.frame with item names and their frequency of correct responses
#' @seealso \code{\link{eval.ctt}}
#'
#' @export
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' freq.scan(inference, items)
#'
#' inference[1,1] <- 4
#' scan.freq(inference, items)

scan.freq <- function(data, items, verbose = T) {
  freq <- data.frame()
  for (item in items) {

    if (length(unique(data[,item])[!is.na(unique(data[,item]))])>2){
      if(verbose) cat("Item:", item, "is not a dichotomous item; frequency will return NA\n")
      freq <- rbind(freq, data.frame(item = item, freq = NA))
    } else {
      tmp <- mean(data[, item], na.rm = T)
      freq <- rbind(freq, data.frame(item = item, freq = tmp))
    }
  }

  class(freq) <- c("psychometRic", "frequency" , class(freq))
  # print(freq %>% mutate_if(is.numeric, round, digits=3))
  return(freq)
}

#' Print method for the frequency class
#'
#' @param x An alpha class object
#'
#' @return print the result of the function
#' @export
#' @importFrom dplyr

print.frequency <- function(x){
  x <- x  %>% as.data.frame() %>% mutate_if(is.numeric, round, digits=3)
  print(x)
}

