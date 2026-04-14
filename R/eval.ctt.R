
#' Classical Test Theory Summary
#'
#' the fonction provide a warpping of the \code{\link{scan.freq}}, \code{\link{scan.diff}}, \code{\link{scan.disc}}\code{\link{scan.alpha}} and \code{\link{scan.rar}} functions
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#' @param digits number of digits for result
#' @param ...
#'
#' @import kableExtra
#' @return a summary of the result and a list with the complet result of each function
#' @export
#'
#'
#' @seealso  \code{\link{scan.freq}}, \code{\link{scan.diff}}, \code{\link{scan.disc}}\code{\link{scan.alpha}} and \code{\link{scan.rar}}
#'
#' @examples
#' library(psychometRics)
#' library(dplyr)
#'
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' ev1 <- eval.ctt(data = inference, items = items)
#' ev1
#'
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
#' ev2 <- eval.ctt(dat.score,items = paste0("item.",1:5),data.response = dat.response,items.rar = paste0("item.",1:5),keys = key)
#' ev2



eval.ctt <- function(data,items, data.response=NULL, items.rar=NULL, keys = NULL,digits=3, verbose = TRUE, ...){

  args <- list(...)
  invisible({
    if(verbose) cat("frequence computation \n" )
    freq <- do.call(scan.freq, list(data, items, verbose))
    if(verbose) cat("item difficulty computation\n" )
    diff <- do.call(scan.diff, list(data, items))
    if(verbose) cat("item discrimination (rir,rit) computation\n" )
    disc <- do.call(scan.disc,
                    c(list(data, items),
                      args[names(args) %in% c("type", "method", "conf.level")]))

    if(verbose) cat("alpha computation \n" )
    alpha <- do.call(
      scan.alpha, list(data, items, verbose=verbose)
    )

    if(!is.null(data.response) & !is.null(items.rar) & !is.null(keys)){
      if(verbose)cat("RAR computation \n")
      rar <- scan.rar(data.score=data, data.response=data.response, items.rar=items.rar, items = items)

    }
  })


  value <- list()
  ## parameter
  value[["param"]][["items.rar"]] <- items.rar
  value[["param"]][["items"]] <- items
  value[["param"]][["keys"]] <- keys
  value[["param"]][["digits"]] <- digits

  ## result
  value[["frequency"]] <- freq
  value[["difficulty"]] <- diff
  value[["discrimination"]] <- disc
  value[["alpha"]] <- alpha
  if(!is.null(data.response) & !is.null(items.rar)) value[["rar"]] <- rar
  value[["digits"]] <- digits


  attr(value,"class") <- c("psychometric", "eval.ctt", class(value))
  invisible(value)
}


#' Print Method for CTT_summary
#'
#' @param x
#'
#' @return print the result of the function
#' @export
#' @importFrom knitr kable
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' t <- CTT_summary(inference, items)
#' print(t)
#'
print.eval.ctt <- function(x){

  summary(x)
  # freq <- x$frequency
  # disc <- x$discrimination[,names(x$discrimination) %in% c("item","r","somers.d","polyserial")]
  # alpha <- x$alpha[,names(x$alpha) %in% c("item","alpha.gain","drop.alpha","p.value")]
  # summa <- merge(freq,disc ,by="item")
  # summa <- merge(summa,alpha ,by="item")
  # cat(" Results summary : ")
  # print( knitr::kable(summa,'simple',digits = x$digits))
}



#' Print Method for CTT_summary
#'
#' @param x
#'
#' @return print the result of the function
#' @export
#' @importFrom knitr kable
#' @import dplyr
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' t <- CTT_summary(inference, items)
#' print(t)
#'

summary.eval.ctt <- function(x){
  frequency <- x$frequency
  disc <- x$discrimination[,names(x$discrimination) %in% c("item","r","somers.d","polyserial")]
  alpha <- x$alpha %>%
    as.data.frame() %>%
    mutate(
      alpha.gain = apa.p(alpha.gain, p.value)
    ) %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    select(item, alpha.gain, z, drop.alpha, alpha0)
  summa <- frequency %>%
    left_join(x$difficulty, by="item") %>%
    left_join(disc,"item") %>%
    left_join(alpha, "item")

  cat(" Results summary : ")
  print(knitr::kable(summa,'simple',digits = x$param$digits))

  cat("\n")
  if(!is.null(x$rar)){
    cat("RAR table : \n\n")
    rar <- eval2$rar %>% mutate_if(is.numeric, round, digits=2)
   print(rar)

  }
}

