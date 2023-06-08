
#' Frequency scam
#'
#' Frequency scan provide the frequency of correct answer for dichotomic item in a data frame
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#'
#' @return data table with the item name and their frequency of correct response
#' @export
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' freq_scan(inference, items)

freq_scan <- function(data, items) {
  freq <- data.frame()
  for (item in items) {

    if (sum(!(data[,item] %in% c(0,1)))>0){
      cat("Item : ", item," is not a dichotomus item frequency will return NA \n")
      freq <- rbind(freq, data.frame(item = item, freq = NA))
    } else {
      tmp <- mean(data[, item], na.rm = T)
      freq <- rbind(freq, data.frame(item = item, freq = tmp))
    }
  }
  freq
}





#' Discrimination correlation
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#' @param type type correlation could be rir for Rest Item-correlation or rit for Total Item-correlation
#'
#' @return a data.frame with the result and confidence interval of pearson correlation and somer delta
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




#' Alpha Cronback scan
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#' @param digits Number of digits in the table summary
#' @param parallel Bool use doSNOW and parallel when there is a great number of items
#' @param verbose Bool for progression visualisation

#'
#' @return return a table with the variation of the alpha when the item is remove from the pool
#' @export
#' @importFrom  foreach foreach
#' @import  doParallel
#' @importFrom  ltm cronbach.alpha
#' @importFrom  tibble add_column
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' alpha.scan(inference, items)
#' alpha.scan(inference, items, parallel = TRUE)

alpha.scan <- function(data, items, digits=3, parallel=FALSE, verbose = TRUE) {
  all <- cronbach.alpha(data[, items], na.rm = T, CI = T)

  alpha <- data.frame()
  n=1


  alpha_drop <- function(data,items,item.drop){
    tmp <- cronbach.alpha(data[, setdiff(items, item.drop)], na.rm = T, CI = T)

    data.frame(
      item = item.drop,
      drop.alpha = tmp$alpha,
      drop.alpha.lwr.ci = tmp$ci[1],
      drop.alpha.upr.ci = tmp$ci[2]
    )
  }


  if(parallel){

    library(doSNOW, verbose = F,quietly=T )
    no_cores <- parallel::detectCores()-2
    cl <- parallel::makeCluster(no_cores)
    # registerDoParallel(cl)
    doSNOW::registerDoSNOW(cl)

    if(verbose){
      iterations <- 100
      pb <- txtProgressBar(max = iterations, style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)

      alpha <- foreach(item.drop = items ,
                       .combine="rbind",
                       .packages='ltm',
                       .options.snow = opts) %dopar% {
                         alpha_drop(data, items, item.drop)
                       }
    } else {
      alpha <- foreach(item.drop = items ,
                       .combine="rbind",
                       .packages='ltm') %dopar% {
                         alpha_drop(data, items, item.drop)
                       }
    }

    stopCluster(cl)
  } else {
    for (item in items) {
      if(verbose){
        cat(paste0('progres: ', n,"/",length(items), "\n"))
      }
      n = n + 1
      alpha <- rbind(alpha, alpha_drop(data,items,item.drop=item))
    }
  }

  alpha$alpha0 <- all$alpha
  alpha$alpha0.lwr.ci <- all$ci[1]
  alpha$alpha0.upr.ci <- all$ci[2]
  alpha <- add_column(alpha, alpha.gain = alpha$alpha0 - alpha$drop.alpha, .after = 1)

  SE0 <- (alpha$alpha0.upr.ci - alpha$alpha0.lwr.ci)/qnorm(0.025,lower.tail = F)
  SEd <- (alpha$drop.alpha.upr.ci - alpha$drop.alpha.lwr.ci)/qnorm(0.025,lower.tail = F)

  alpha = add_column(alpha, z = abs(alpha$alpha0 - alpha$drop.alpha)/sqrt(SE0^2+SEd^2), .after = 2)
  alpha = add_column(alpha, p.value = pnorm(alpha$z,lower.tail = F), .after = 3)

  rownames(alpha) <- NULL
  alpha
}




#' Classical Test Theory Summary
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items (should be >1 )
#' @param digits
#' @param ...
#'
#' @return
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
                    c(list(inference, items),
                      args[names(args) %in% c("type", "method", "conf.level")]))

  cat("alpha computation \n" )
  alpha <- do.call(alpha.scan,
                   c(list(inference, items),
                     args[names(args) %in% c("gain.format", 'digits', 'parallel', 'verbose' )]))

  value <- list(frequence= freq,
                discrimination=discri,
                alpha=alpha,
                digits=digits)

  attr(value,"class") <- "CTT_summary"
  # summary.CTT_summary(value)
  return(value)
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#' @importFrom knitr kable
#' @examples
print.CTT_summary <- function(x){
  freq <- x$frequence
  disc <- x$discrimination[,names(x$discrimination) %in% c("item","r","somers.d","polyserial")]
  alpha <- x$alpha[,names(x$alpha) %in% c("item","alpha.gain","drop.alpha")]
  summa <- merge(freq,disc ,by="item")
  summa <- merge(summa,alpha ,by="item")
  cat(" Results summary : ")
  print( knitr::kable(summa,'simple',digits = x$digits))
}






