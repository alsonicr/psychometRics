#' Difficulty scan
#'
#' Difficulty scan provides an indicator of item difficulty in classical test theory.
#' The difficulty is calculated as the item mean score divided by
#' the difference between the maximum and minimum score of the item.
#'
#'
#' @param data A data.frame containing participant score for each item
#' @param items The names of the items (should be >1)
#' @return data.frame with item names and their difficulty
#' @seealso \code{\link{eval.ctt}}
#'
#' @export
#'
#' @examples
#' data("inference")
#' items <- names(inference)
#' dif.scan(data = inference, items = items)
#'
#' inference[1,1] <- 4
#' scan.diff(inference, items)

scan.diff <- function(data, items, verbose = T ) {
  dif <- data.frame()
  for (item in items) {
      tmp <- mean(data[, item], na.rm = T)/ (max(data[, item], na.rm = T) - min(data[, item], na.rm = T))
      dif <- rbind(dif, data.frame(item = item, difficulty = tmp))
  }

  class(dif) <- c("psychometRics", "difficulty" , class(dif))
  # if(verbose) print(dif %>% mutate_if(is.numeric, round, digits=3))
  return(dif)
}


#' Print method for the difficulty class
#'
#' @param x An alpha class object
#'
#' @return print the result of the function
#' @export
#' @importFrom dplyr

print.difficulty <- function(x){
  x <- x %>% as.data.frame() %>% mutate_if(is.numeric, round, digits=3)
  print(x)
}



