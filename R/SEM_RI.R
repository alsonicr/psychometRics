


#' Regression Random Intercept
#'
#' @param y a variable name for the random intercept
#' @param x the variables to be constrained to the random intercept
#'
#' @return
#' @export
#'
#' @examples
SEM_reg_RI <- function(y,x){
  rez <- paste(
    paste(paste("RI_",y,sep = ""), collapse = " + "),
    " =~ ",
    paste(paste("1*",x,sep=""), collapse = " + "),
    "\n"
  )
  attr(rez,"class") <- "SEM"
  return(rez)
}


#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
SEM_wth_var <- function(variables){
  rez <- ""
  for (var in variables){
    tmp <- paste("w_",var," =~ ", "1*",var,"\n",sep="")
    rez <- paste(rez, tmp, sep = "")
  }
  attr(rez,"class") <- "SEM"
  return (rez)
}


#' The random intercept crosslag panel model
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
RI_crosslag_model <- function(variables,times, order = "times/variables", join="_"){

  ## Time variable
  RI <- list()
  for(var in variables){
    if(order == "times/variables"){
      tmp <- list(paste0(times, join, var))
      names(tmp) <- var
      RI <- append(RI,tmp)
    }
    if(order == "variables/times"){
      tmp <- list(paste0(var, join, times))
      names(tmp) <- var
      RI <- append(RI,tmp)
    }

  }

  ## Create between components (random intercepts)
  RI_reg <- ""
  for (var in names(RI)){
    RI_reg <- paste0( RI_reg, SEM_reg_RI(var, RI[[var]]))
  }
  ## Variance RI

  RI_cov <- SEM_cov(paste("RI_",variables,sep=""))


  # Create within-person centered variables
  wth_var <-""
  for (var in names(RI)){
    wth_var <- paste0( wth_var, SEM_wth_var( RI[[var]]))
  }

  ## Crosslag
  crosslag <- crosslag_model(paste("w_",variables,sep=""),times = 1:3, order=order, join=join)


  value <- paste0(
    "## Random intercept \n",RI_reg,"\n",
    "## Randow Intercept cov \n", RI_cov,"\n",
    "## within-person centered variables \n",wth_var,"\n",
    "## Crosslag \n", crosslag,"\n"
  )

  class(value)<- c("SEM","RI-CLPM")

  return(value)
}
