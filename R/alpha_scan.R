

#' Alpha Cronback scan
#'
#' the function provide the variation of relaibility of alpha when an item is drop from the data.
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
      cat("\n")
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
