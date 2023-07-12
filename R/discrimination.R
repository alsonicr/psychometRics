
#' Discrimination correlation
#'
#' The function provide correlation item rest (rir) or correlation item test (rit) for a all of the item in the test.
#' Multiple type of item corrleation are provided as somer's d, point biserial correlation and polyserial correlation for non dichotomic item
#' Result could be interpretend as the item ability to discriminate between a lower and higher difficulty for this item in regard to the overall raw score.
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#' @param type type correlation could be rir for Rest Item-correlation or rit for Total Item-correlation (can be both)
#' @param method the type of correlation to use c("cor","somer","polyserial")
#' @param conf.level the confidence interval for the correlation needed
#'
#' @return a data.frame with the result and confidence interval
#' @export
#' @importFrom  DescTools SomersDelta
#' @importFrom  DescTools SomersDelta
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' discrimination_cor(inference, items)

discrimination_cor <- function(data, items, type = "rir", method = c("cor","somer","polyserial"),conf.level=0.95,...){
  args <- list(...)
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
                       c(list(a, b, conf.level = conf.level ), args))
        ROW <- cbind(ROW, data.frame(r = Cor$estimate, r.lwr.ci = Cor$conf.int[1], r.upr.ci = Cor$conf.int[2]))

      }
      if("somer" %in% method){
        somer.d <- do.call(DescTools::SomersDelta,
                           c(list(b, a, conf.level = conf.level), args))
        ROW <- cbind(ROW, data.frame(somers.d = somer.d[1], somers.lwr.ci = somer.d[2], somers.upr.ci = somer.d[3]))
      }

      if("polyserial" %in% method){
        polyc <- do.call(polycor::polyserial, list(b, a))
        ROW <- cbind(ROW, data.frame(polyserial =polyc))
      }

      res <- rbind(res , ROW)
    }
  }
  rownames(res)<-c()
  return (res)
}
