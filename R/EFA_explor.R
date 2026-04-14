#' FA.exploratory
#'
#' The function provide information factor analysis and if the actual data are recommended for factoriale analysis. 
#' It also used multiple statisical function to provide an indication regarding the number of factor present in the dataset.
#' Those criteria containt the parallele analysis method, the Velicer's MAP, VSS and Kaiser criterion. 
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items
#' @param plot Plot vss and parallel analysis
#' @param ...
#'
#' @return the result provide a print of the factor detected for each of the method
#' @seealso  \code{\link{fa.parallel}}  \code{\link{vss}}
#'
#' @export
#' @importFrom  psych fa.parallel
#' @importFrom  psych vss
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' eval1 <- EFA.explo(inference, items)
#' eval1

EFA.explo <- function(data, items, plot = FALSE, ...){
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  kmo <- psych::KMO(data[,items])
  bartlett <- bartlett.test(data[,items])

  an1 <-quiet( psych::fa.parallel(data[,items],fa = "fa",plot = plot))
  Kaiser.criterion <- sum(an1$fa.values >= 1)
  an2 <- psych::vss(data[,items], plot = plot)

  factor <- list("parallele"= an1$nfact,
                 "VSS complexity 1"=which.max(an2$cfit.1),
                 "VSS complexity 2"=which.max(an2$cfit.2),
                 "Velicer MAP"= which.min(an2$map),
                 "kaiser criterion" = Kaiser.criterion,
                 "BIC" = which.min(an2[["vss.stats"]]$BIC),
                 "eBIC" = which.min(an2[["vss.stats"]]$eBIC)
  )




  value <- list(
    "kmo" = kmo,
    "Bartlett.test" = bartlett,
    "paralle analysis" = an1,
    "vss analysis" = an2,
    "factor"= factor
  )

  class(value) <- 'EFA.explo'
  invisible(value)
}




#' Facto print method
#'
#' formating result for the facto class
#'
#' @param x a facto class object
#' @export


print.EFA.explo <- function(x){

  cat(
    "\n \n",
    " ### EFA analysis and adequacy ###",
    "\n \n"
  )

  if (x$Bartlett.test$p.value< 0.05) {
    bartlett.interpretation <- "The Bartlett's test of sphericity was significant at an alpha level of .05. \nThese data are probably suitable for factor analysis \n"
  } else {
    "The Bartlett's test of sphericity was NOT significant at an alpha level of .05. \n These data are probably NOT suitable for factor analysis \n"
  }
  cat("Bartlett's K-squared =", round(x$Bartlett.test$statistic,2), "df =",x$Bartlett.test$parameter , "p-value <",x$Bartlett.test$p.value, "\n")
  cat(bartlett.interpretation,"\n \n")


  if (x$kmo$MSA < 0.5) kmo.interpretation <- "Unacceptable – Bad"
  if (x$kmo$MSA >= 0.5 & x$kmo$MSA < 0.6) kmo.interpretation <- "Miserable – Bad"
  if (x$kmo$MSA >= 0.6 & x$kmo$MSA < 0.7) kmo.interpretation <- "Mediocre – Okay"
  if (x$kmo$MSA >= 0.7 & x$kmo$MSA < 0.8) kmo.interpretation <- "Middling – Okay"
  if (x$kmo$MSA >= 0.8 & x$kmo$MSA < 0.9) kmo.interpretation <- "Meritorious – Good"
  if (x$kmo$MSA >= 0.9 ) kmo.interpretation <- "Marvelous – Great"
  cat("KMO Measure of Sampling Adequacy = ", round(x$kmo$MSA,3), "can be considerate for factor analysis has", kmo.interpretation ,"\n \n" )


  cat("Parallele analysis factor solution : ", x$factor$parallele[1], "\n")
  cat("VSS complexity 1 factor solution   : ", x$factor$`VSS complexity 1`[1], "\n")
  cat("VSS complexity 2 factor solution   : ", x$factor$`VSS complexity 2`[1], "\n")
  cat("Velicer MAP factor solution        : ", x$factor$`Velicer MAP`[1], "\n")
  cat("kaiser criterion                   : ", x$factor$`kaiser criterion`[1], "\n")
  cat("BICfactor solution                 : ", x$factor$BIC[1], "\n")
  cat("eBIC factor solution               : ", x$factor$eBIC[1], "\n")
}



