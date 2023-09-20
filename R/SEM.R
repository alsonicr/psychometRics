
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
#' SEM_reg(c("Y1","Y2","Y3"),c("x1","x2","x3"))
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
#' @param variance If TRUE variance of variable will be writeen
#'
#' @return return a string of model format for lavaan
#' @export
#'
#' @examples
#' SEM_cov(c("A","B","C","D"))
#' SEM_cov(c("A","B","C","D"), variance = FALSE)
#'
SEM_cov<-  function(variables, variance = T){
  tmp <- combn(variables,m = 2,simplify = F,)
  rez <-""
  for(i in tmp){
    rez <- paste(rez, paste(i,collapse = " ~~ "), "\n",sep = "")
  }

  if (variance){
    rez <- paste(rez , paste0(variables, " ~~ ", variables ,collapse = '\n',sep=""),sep="")

  }

  attr(rez,"class") <- "SEM"
  return(rez)
}



#' Crosslag model formulat
#'
#' Create a Lavaan modem formulat for a classic crosslag model
#'
#' @param variables A vector of names variables
#' @param times names of the time indicator for the model
#' @param order a string defining the order of the variable and the time indicator by default "times/variables" or "variables/times"
#' @param join a string defining by what carractere are join variable and time (by default "_")
#'
#' @return return a lavaan formulat for a crosslag model
#' @export
#'
#' @examples
#' vars = c("x","y")
#' t = 1:4
#' crosslag_model(variables = vars, times = t, order = "times/variables", join="_" )
#' crosslag_model(variables = vars, times = t, order = "variables/times", join="" )
#'
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

    covariable <- paste(covariable, SEM_cov(time), "\n",sep = "" )
  }

  rez <- paste(
    "### Regression ###\n",regression,
    "\n",
    "### Covariable ###\n",covariable,
    sep="")

  attr(rez,"class") <- "SEM"
  return(rez)
}




#' Print method for SEM class
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




