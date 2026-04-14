#' Cronbach alpha scan
#'
#' This function provides the variation of Cronbach's alpha when an item
#' is dropped from the data.
#'
#' @param data A data.frame containing participant scores for each item
#' @param items The names of the items (should be >1)
#' @param digits Number of digits in the table summary
#' @param parallel Bool: use parallel backend (doSNOW) when there are many items
#' @param verbose Bool for progress visualization
#' @param ... arguments passed to \code{\link{cronbach.alpha}} from the ltm package
#'
#' @description The function provides the variation of reliability (Cronbach's alpha)
#' when an item is dropped. It also computes a z-test comparing alpha with and without
#' the item. The result is a table with the alpha when the item is removed, the
#' alpha gain (difference between the alpha with all items and without the item),
#' the z value and the p value. Positive alpha gain indicates alpha decreases when
#' the item is removed; negative alpha gain indicates alpha increases when the
#' item is removed. In general, alpha gain should be positive; if negative, the
#' item may be unreliable and considered for removal. The function supports
#' parallel execution when appropriate.
#' @return A data.frame with alpha variations and a z-test per item
#' @seealso \code{\link{eval.ctt}}, \code{\link{cronbach.alpha}}
#'
#' @export
#' @importFrom  foreach foreach
#' @import  doParallel
#' @importFrom  ltm cronbach.alpha
#' @importFrom  tibble add_column
#' @import parallel
#' @import doSNOW
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0", 3:9), "item_10")
#' alp1 <- scan.alpha(data = inference, items = items)
#' alp1
#' alp2 <- scan.alpha(inference, items, parallel = TRUE, verbose = FALSE)
#' alp2
#'

scan.alpha <- function(data, items, digits = 3, parallel = FALSE, verbose = TRUE, ...) {

  # all <- cronbach.alpha(data[, items], na.rm = T, CI = T)
  all <- do.call(cronbach.alpha, list(data[, items], na.rm = T, CI = T, ...))
  alpha <- data.frame()
  n <- 1


  alpha_drop <- function(data, items, item.drop,...) {
    # tmp <- cronbach.alpha(data[, setdiff(items, item.drop)], na.rm = T, CI = T)
    tmp <- do.call(cronbach.alpha, list(data[, setdiff(items, item.drop)],na.rm = T, CI = T, ...))

    data.frame(
      item = item.drop,
      drop.alpha = tmp$alpha,
      drop.alpha.lwr.ci = tmp$ci[1],
      drop.alpha.upr.ci = tmp$ci[2]
    )
  }


  if (parallel) {
    # library(doSNOW, verbose = F, quietly = T)
    # no_cores <- parallel::detectCores() - 2
    # cl <- parallel::makeCluster(no_cores)
    # doSNOW::registerDoSNOW(cl)

    ## Parallel setup
    no_cores <- parallel::detectCores()-1
    cl <- makeCluster(no_cores)
    registerDoSNOW(cl)

    if (verbose) {

      ### Progress bar
      iterations <- length(items)
      pb <- txtProgressBar(max = iterations, style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)

      alpha <- foreach(
        item.drop = items,
        .combine = "rbind",
        .packages = "ltm",
        .options.snow = opts
      ) %dopar% {
        alpha_drop(data, items, item.drop,...)
      }
      cat("\n")
    } else {
      alpha <- foreach(
        item.drop = items,
        .combine = "rbind",
        .packages = "ltm"
      ) %dopar% {
        alpha_drop(data, items, item.drop,...)
      }
    }

    stopCluster(cl)
  } else {
    for (item in items) {
      if (verbose) {
          cat(paste0("progress: ", n, "/", length(items), "\n"))
        }
      n <- n + 1
      alpha <- rbind(alpha, alpha_drop(data, items, item.drop = item, ...))
    }
  }

  alpha$alpha0 <- all$alpha
  alpha$alpha0.lwr.ci <- all$ci[1]
  alpha$alpha0.upr.ci <- all$ci[2]
  alpha <- add_column(alpha, alpha.gain = alpha$alpha0 - alpha$drop.alpha, .after = 1)

  ## Z test
  SE0 <- (alpha$alpha0.upr.ci - alpha$alpha0.lwr.ci) / qnorm(0.025, lower.tail = F)
  SEd <- (alpha$drop.alpha.upr.ci - alpha$drop.alpha.lwr.ci) / qnorm(0.025, lower.tail = F)

  alpha <- add_column(alpha, z = abs(alpha$alpha0 - alpha$drop.alpha) / sqrt(SE0^2 + SEd^2), .after = 2)
  alpha <- add_column(alpha, p.value = pnorm(alpha$z, lower.tail = F), .after = 3)
  rownames(alpha) <- NULL


  class(alpha) <- c("psychometRics", "alpha", class(alpha))
  return(alpha)
}

#' Print method for the alpha class
#'
#' @param x An alpha class object
#'
#' @return print the result of the function
#' @export
#' @importFrom dplyr

print.alpha <- function(x) {
  x <- x %>%
    as.data.frame() %>%
    mutate(
      alpha.gain = apa.p(alpha.gain, p.value)
    ) %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    select(item, alpha.gain, z, drop.alpha, alpha0)
  print(x)
}



