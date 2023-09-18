
#' SEM regression model
#'
#' @param y A vector of string who contain the name of the predicted variable
#' @param x A vector of string who contain the name of the predictor variable
#'
#' @return A string formula for a Lavaan model
#' @export
#'
#' @examples
#'
#' SEM_reg("Y","x")
#' SEM_reg("Y",c("x1","x2","x3"))
#' #' SEM_reg(c("Y1","Y2","Y3"),c("x1","x2","x3"))
#'
SEM_reg <- function(y, x){
  rez <- paste(
    paste(y, collapse = " + "),
    " ~ ",
    paste(x, collapse = " + "),
    "\n"
  )
  attr(rez,"class") <- "SEM"
  return(rez)
}


#' Title
#'
#' @param variables A vector of names variables
#'
#' @return
#' @export
#'
#' @examples
#' SEM_cov(c("A","B","C","D"))
#'
SEM_cov<- function(variables){
  tmp <- combn(variables,m = 2,simplify = F)
  rez <-""
  for(i in tmp){
    rez <- paste(rez, paste(i,collapse = " ~~ "), "\n",sep = "")
  }
  attr(rez,"class") <- "SEM"
  return(rez)
}




#' Title
#'
#' @param variables
#' @param times
#' @param order
#' @param join
#'
#' @return
#' @export
#'
#' @examples
crosslag_model <- function(variables, times, order = "times/variables", join="_"){

  combi <- expand.grid(variables, times)

  if (order == "times/variables"){
    crosslag.var = paste0(combi[,2], join, combi[,1])
  } else {
    crosslag.var = paste0(combi[,1], join, combi[,2])
  }

  max <- length(variables)
  x <- seq_along(crosslag.var)
  crosslag.var <- split(crosslag.var, ceiling(x/max))


  ### Regression part
  regression <-""
  for(time in 1:length(crosslag.var)){
    if (time == length(crosslag.var)) { break }
    regression <- paste(regression, SEM_reg(crosslag.var[[time]], crosslag.var[[time+1]]),sep="")
  }
  ### Covariate part
  covariable  <- ""
  for(time in crosslag.var){

    covariable <- paste(covariable, SEM_cov(time),sep = "" )
  }

  rez <- paste(
    "### Regression ###\n",regression,
    "\n",
    "### Covariable ###\n",covariable,
    sep="")

  attr(rez,"class") <- "SEM"
  # cat(rez)
  return(rez)
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.SEM <- function(x){
  cat(x)
}




