
#' Classical Test Theory Summary
#'
#' the fonction provide a warpping of the freq_scan, discrimination_cor, alpha.scan functions  # nolint: line_length_linter.
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#' @param digits number of digits for result
#' @param ...
#'
#' @return a summary of the result and a list with the complet result of each function
#' @export
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' CTT_summary(inference, items)


CTT_summary <- function(data,items, digits=3,...){

  args <- list(...)

  cat("frequence computation \n" )
  freq <- do.call(freq_scan, list(data, items))
  cat("item discrimination (rir,rit) computation\n" )
  discri <- do.call(discrimination_cor,
                    c(list(data, items),
                      args[names(args) %in% c("type", "method", "conf.level")]))

  cat("alpha computation \n" )
  alpha <- do.call(alpha.scan,
                   c(list(data, items),
                     args[names(args) %in% c("gain.format", 'digits', 'parallel', 'verbose' )]))

  value <- list(frequence= freq,
                discrimination=discri,
                alpha=alpha,
                digits=digits)

  attr(value,"class") <- "CTT_summary"
  # summary.CTT_summary(value)
  return(value)
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
print.CTT_summary <- function(x){
  freq <- x$frequence
  disc <- x$discrimination[,names(x$discrimination) %in% c("item","r","somers.d","polyserial")]
  alpha <- x$alpha[,names(x$alpha) %in% c("item","alpha.gain","drop.alpha","p.value")]
  summa <- merge(freq,disc ,by="item")
  summa <- merge(summa,alpha ,by="item")
  cat(" Results summary : ")
  print( knitr::kable(summa,'simple',digits = x$digits))
}



