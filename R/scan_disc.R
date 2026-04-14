
#' Discrimination correlation
#'
#' The function provides item-rest (RIR) or item-total (RIT) correlations for all items in the test.
#' Multiple types of item correlation are provided such as Somers' D, point-biserial correlation, and polyserial correlation for non-dichotomous items.
#' Results can be interpreted as the item's ability to discriminate between lower- and higher-performing respondents with respect to the overall raw score.
#'
#' @param data A data.frame containing participant score for each item
#' @param items The names of the items (should be >1)
#' @param type Type of correlation: 'rir' for rest-item correlation or 'rit' for total-item correlation (can be both)
#' @param method The type(s) of correlation to use: c("cor","somer","polyserial")
#' @param conf.level The confidence level for the correlation
#' @param ... Argument for the cor.test() function
#' @return a data.frame with the result and confidence interval
#' @seealso \code{\link{eval.ctt}}
#'
#' @export
#' @importFrom  DescTools SomersDelta
#' @importFrom  DescTools SomersDelta
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' scan.disc(data = inference, item = items, type ="rir")
#' scan.disc(data = inference, item = items, type ="rit")
#'

scan.disc <- function(data, items, type = "rir", method = c("cor","somer","polyserial"), conf.level=0.95, args.cor = list(), args.somer = list(), args.polyserial = list()){
  # args <- list(...)
  # print(args)
  res <- data.frame()

  for(t in type){
    for (item in items){
      if(t == "rir"){
        a <- data[, item]
        b <- rowSums(data[, setdiff(items, item)], na.rm = T)
      }
      if(t == "rit"){
        a <- data[, item]
        b <- rowSums(data[, items], na.rm = T)
      }

      ROW <- data.frame( type = t,
                         item = item)
      if("cor" %in% method){
        Cor <- do.call(cor.test,
                        c(list(b, a, conf.level = conf.level), args.cor))
        ROW <- cbind(ROW, data.frame(r = Cor$estimate, r.lwr.ci = Cor$conf.int[1], r.upr.ci = Cor$conf.int[2]))
      }
      if("somer" %in% method){
        somer.d <- do.call(SomersDelta,
                           c(list(b, a, conf.level = conf.level), args.somer))
        ROW <- cbind(ROW, data.frame(somers.d = somer.d[1], somers.lwr.ci = somer.d[2], somers.upr.ci = somer.d[3]))
      }

      if("polyserial" %in% method){
        polyc <- do.call(polycor::polyserial, c(list(b, a), args.polyserial))
        ROW <- cbind(ROW, data.frame(polyserial =polyc))
      }

      res <- rbind(res , ROW)
    }
  }
  rownames(res)<-c()

  class(res) <- c("psychometRic", "discrimination" , class(res))
  # print(res %>% mutate_if(is.numeric, round, digits=3))
  return(res)
}


#' Print method for the difficulty class
#'
#' @param x An alpha class object
#'
#' @return print the result of the function
#' @export
#' @importFrom dplyr

print.discrimination <- function(x){
  x = x %>% as.data.frame() |> mutate_if(is.numeric, round, digits=3)
  print(x)
}




