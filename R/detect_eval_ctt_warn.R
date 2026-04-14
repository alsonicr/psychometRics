

#' Classical test theory evaluation warning table
#'
#' The function provide assistance to detect red flag item in the  \code{\link{scan.RAR}} result.
#' It provide detection for ambigus distractor and weak key selection.
#'
#'
#' @param data A psychometric rar class data containing the results for rar
#' @param keys A vector of correct answered for each items
#' @param response.columns Avector containing the name of each colomn containing possible answer
#' @param threshold.k A minimun threshold for the key to be considered acceptable (weak key detection)
#' @param threshold.D A maximun threshold for a distractor to be considered acceptable (ambigus distractor detection)
#'
#' @import kableExtra
#' @importFrom kableExtra cell_spec
#' @import dplyr
#' @export
#' @return A class detecRar list with table of item detection and kable table to visualise the problematics items
#'
#'
#' @seealso \code{\link{scan.RAR}}, \code{\link{eval.ctt}}
#'
#' @examples
#'
#' library(psychometRics)
#' library(dplyr)
#'
#'  item.1 <- c(rep("A", 15), rep("B", 10), rep("C", 15))
#'  item.2 <- c(rep("A", 10), rep("B", 15), rep("C", 15))
#'  item.3 <- c(rep("A", 5), rep("B", 15), rep("C", 20))
#'  item.4 <- c(rep("A", 3), rep("B", 19), rep("C", 18))
#'  item.5 <- c(rep("A", 11), rep("B", 13), rep("C", 16))
#'
#'  dat.response <- data.frame(
#'    item.1 = item.1,
#'    item.2 = item.2,
#'    item.3 = item.3,
#'    item.4 = item.4,
#'    item.5 = item.5
#'  )
#'
#'  key <- c("A","B","C","A","B")
#'
#'  dat.score <- data.frame()
#'  for (i in 1:nrow(dat.response)){
#'    tmp <- dat.response[i,] == key
#'    dat.score <- rbind(dat.score, as.data.frame(tmp))
#'  }
#'
#'  dat.score <- as.data.frame(sapply(dat.score,as.numeric))
#'
#' rar <- scan.rar(dat.score, dat.response, items.rar = paste0("item.",1:5))
#' rez = detect.rar.warning(data = rar, keys = key, response.columns = names(rar)[-1])
#' rez
#'
#'


detect.eval.ctt.warn <- function(
    dat,
    threshold.freq = 0.05,
    threshold.diff = 0.05,
    threshold.disc = 0.20,
    threshold.alpha = 0.02,
    threshold.k = 0.20,
    threshold.d = -0.05
) {

  ## Warning detection

  ### Frequency
  frequency.warn <- dat$frequency$freq < threshold.freq | dat$frequency$freq > (1 - threshold.freq)
  ### Difficulty
  difficulty.warn <- dat$difficulty$difficulty < threshold.diff |
    dat$difficulty$difficulty > (1 - threshold.diff)
  ### Discrimination (rir)
  discrimination.warn <- dat$discrimination |>
    select(any_of(c("r", "somers.d", "polyserial"))) |>
    mutate_all(.funs = function(x) {
      return(x < threshold.disc)
    })
  ### Alpha
  alpha.warn <- dat$alpha |>
    as.data.frame() |>
    select(alpha.gain, p.value, alpha0) |>
    mutate(
      alpha.gain = ifelse(alpha.gain < -threshold.alpha & p.value < 0.05, TRUE, FALSE),
      alpha0 = ifelse(alpha0 < .80, TRUE, FALSE)
    ) |>
    select(-p.value)
  ### RAR evaluation of distractor
  if(!is.null(dat$rar)){
    rar.warn <- detect.rar.warning(data = dat$rar, keys = key, threshold.k = threshold.k, threshold.d = threshold.d)
  }

  ## Table

  ### warning Kable table
  tmp1 <- dat$frequency %>%  mutate_if(is.numeric, round, digits = 3 )
  tmp1[frequency.warn,2] <- cell_spec(tmp1[frequency.warn,2],  background = "red")

  tmp2 <- dat$difficulty %>%  mutate_if(is.numeric, round, digits = 3 )
  tmp2[difficulty.warn] <- cell_spec(tmp2[difficulty.warn],  background = "red")

  tmp3 <- dat$discrimination %>%  mutate_if(is.numeric, round, digits = 3 ) |>
    select(any_of(c("r", "somers.d", "polyserial")))
  tmp3[as.matrix(discrimination.warn)] <- cell_spec(tmp3[as.matrix(discrimination.warn)],  background = "red")

  tmp4 <- dat$alpha |>
    as.data.frame() |>
    select(alpha.gain, alpha0) %>%
    mutate_if(is.numeric, round, digits = 3 )
  tmp4[as.matrix(alpha.warn)] <- cell_spec(tmp4[as.matrix(alpha.warn)],  background = "red")

  if(!is.null(dat$rar)){
    tmp5 <- rar.warn$data_rar %>%  mutate_if(is.numeric, round, digits = 3 )
    tmp5[rar.warn$aggregate.warning] <- cell_spec(tmp5[rar.warn$aggregate.warning],  background = "red")
    tmp5 <- cbind(rar.warn$data_info,tmp5)
  }

  kbl.table <- tmp1 |> left_join(tmp2, "item") %>%  cbind(tmp3,tmp4) |> as.data.frame()
  if(!is.null(dat$rar)) kbl.table <- kbl.table %>% left_join(tmp5, "item")

  ### Count warning table

  kbl.table2 <- data.frame(
    item = dat$param$items,
    warn.freq= as.numeric(frequency.warn),
    warn.diff = as.numeric(difficulty.warn),
    warn.disc = discrimination.warn %>% mutate( warn.disc = r + somers.d + polyserial) %>%  pull(warn.disc),
    warn.alpha = alpha.warn %>% mutate(warn.alpha = alpha.gain + alpha0) %>%  pull(warn.alpha))

  if(!is.null(dat$rar)){
    kbl.table2 <- kbl.table2 %>%  left_join(cbind(rar.warn$data_info,rar.warn$summary.count), "item")
  }

  kbl.table2 <- kbl.table2 %>%
    mutate(Total.warn = rowSums(across(where(is.numeric))))

  kbl.table <- kbl.table %>% left_join(kbl.table2 %>% select(item,Total.warn),"item")
  ## Output

  rez <- list()

  rez[["param"]]<- list(
    threshold.freq,threshold.diff,
    threshold.disc,threshold.alpha,
    threshold.k,threshold.d,
    items = dat$param$items,
    item.rar = dat$param$items.rar,
    eval.ctt = dat
  )
  rez[["frequency"]] <- frequency.warn
  rez[["difficulty"]] <- difficulty.warn
  rez[["discrimination"]] <- discrimination.warn
  rez[["alpha"]] <- alpha.warn
  if(!is.null(dat$rar)) rez[["rar"]] <- rar.warn
  rez[["warning.count"]] <- kbl.table2
  rez[["table"]] <- kbl.table


  class(rez)  <- "detecCTT"
  invisible(rez)
}



#' a print function for detecCTT class
#' @export

print.detecCTT<- function(x){

  cat("Warning detection CTT\n\n")

  cat("List of items flag for frequency : ",x$param$items[x$frequency],"\n" )
  cat("List of items flag for difficulty : ",x$param$items[x$difficulty],"\n" )
  cat("List of items flag for discrimination : ",x$param$items[rowSums(x$discrimination)>0],"\n" )
  cat("List of items flag for alpha : ",x$param$items[rowSums(x$alpha)>0],"\n","\n")
  if(!is.null(x$rar)) print(x$rar)


}



#' a summary function for detecCTT class
#'
#' @importFrom kableExtra cell_spec kbl kable_paper
#' @import dplyr
#' @export



setClass("detecCTT")
setMethod(f="summary", signature(object="detecCTT"),
          function(object) {
            print(object)
            print(object$table %>%
                    kbl(booktabs = T, digits = 3, longtable = T,escape = F) %>%
                    kable_paper("striped", full_width = F)
            )

          }

)
