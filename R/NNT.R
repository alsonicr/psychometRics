

#' The number needed to treat (NNT)
#'
#' @param d a cohen's d effect size or equivalent
#' @param CER the experimental event rate  or a vector of value (between 0 and 1)
#' @param EER the control event rate between (0 and 1)
#' @param type the type of NNT could be "classic", "Furukawa" or "Kraemer"
#'
#' @return an NNT score or a table of NNT score
#' @export
#'
#' @examples
#'
#' NNT(d=0.20,CER=0.20,type="Furukawa")
#' NNT(d=0.20,type="Kraemer")
#' NNT(CER=.20,EER=.40)
#'
NNT <- function(d=NULL,CER=NULL,EER=NULL, type="classic"){
  if(type =="classic"){
    nnt <- 1/(EER - CER)
  }

  if(type == "Furukawa" ){
    if(is.null(CER)) CER <- seq(0,1,0.1)
    if(is.null(d)) {
      print("A cohen's d value must be provided")
      break
    }
    nnt = 1 / pnorm( d + qnorm(CER)) - CER
    nnt <- as.data.frame(cbind(nnt,CER))
    rownames(nnt) <- NULL
  }

  if(type=="Kraemer"){
    if(is.null(d)){
      print("A cohen's d value must be provided")
      break
    }
    warning("According tp Furukawa et Al (2011) the Kraemer method can't be unrelaibale and cannot take account of CER variation")
    AUC <-  pnorm(d/sqrt(2))
    nnt <-  1 / ((2 * AUC) -1)
    nnt
  }

  return(nnt)

}
