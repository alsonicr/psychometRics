#' CFI indicator
#'
#' extract a CFI fit index from a psych:fa class result
#'
#' @param x a  psych:fa class
#' @noRd
#' @return the CFI index of the tested model
#' @export
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' FA <- psych::fa(inference[,items], nfactors = 2)
#' fa.CFI(FA)

fa.CFI <- function(x) {
  nombre <- paste(x, "CFI", sep = ".")
  nombre <-
    ((x$null.chisq - x$null.dof) - (x$STATISTIC - x$dof)) / (x$null.chisq - x$null.dof)
  return(nombre)
}
