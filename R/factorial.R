

#' Factor explorer
#'
#' the function use parallel analyse, Velicer and VSS method to provide the possible number of factor.
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items
#' @param plot Plot vss and parallel
#' @param ...
#'
#' @return the result provide a print of the factor detected for each of the method
#' @export
#' @importFrom  psych fa.parallel
#' @importFrom  psych vss
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' factor_explorer(inference, items)

factor_explorer <- function(data, items, plot = FALSE, ...){
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }
  an1 <-quiet( psych::fa.parallel(data[,items],fa = "fa",plot = plot))
  an2 <- psych::vss(data[,items], plot = plot)

  factor <- list("parallele"= an1$nfact,
                 "VSS complexity 1"=which.max(an2$cfit.1),
                 "VSS complexity 2"=which.max(an2$cfit.2),
                 "Velicer MAP"= which.min(an2$map),
                 "BIC" = which.min(an2[["vss.stats"]]$BIC),
                 "eBIC" = which.min(an2[["vss.stats"]]$eBIC)
  )




  value <- list(
    "paralle analysis" = an1,
    "vss analysis" = an2,
    "factor"= factor
  )

  class(value) <- 'facto'
  return(value)
}


#' Facto print method
#'
#' formating result for the facto class
#'
#' @param x a facto class object
#'
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' f <- factor_explorer(inference, items)
#' print(t)

print.facto <- function(x){
  # print(x["factor"])
  cat("Parallele analysis factor solution : ", x$factor$parallele[1], "\n")
  cat("VSS complexity 1 factor solution   : ", x$factor$`VSS complexity 1`[1], "\n")
  cat("VSS complexity 2 factor solution   : ", x$factor$`VSS complexity 2`[1], "\n")
  cat("Velicer MAP factor solution        : ", x$factor$`Velicer MAP`[1], "\n")
  cat("BICfactor solution                 : ", x$factor$BIC[1], "\n")
  cat("eBIC factor solution               : ", x$factor$eBIC[1], "\n")
}




#' CFI indicator
#'
#' extract a CFI fit index from a psych:fa class result
#'
#' @param x a  psych:fa class
#'
#' @return the CFI index of the tested model
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' FA <- psych::fa(inference[,items], nfactors = factor)
#' fa.CFI(FA)

fa.CFI <- function(x) {
  nombre <- paste(x, "CFI", sep = ".")
  nombre <-
    ((x$null.chisq - x$null.dof) - (x$STATISTIC - x$dof)) / (x$null.chisq - x$null.dof)
  return(nombre)
}






#' Factor comparaison
#'
#' Compute anova and fit index to compare multiple factor analyse fit for multiple factors solution
#'
#' @param data a data.frame containing the test or scale response in columns
#' @param items The name of the items
#' @param factors a facto class object from factor_explorer() or a vector with the number of factor
#'
#' @return a table with a CHI² comparison and CFI, TLI, RMSEA and SRMR differences
#' @export
#' @import dplyr
#' @importFrom psych fa
#' @importFrom psych anova.psych
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' f <- factor_explorer(inference, items)
#' factor_comparaison(inference, items, f)
#' factor_comparaison(inference, items, c(1,2,3))

factor_comparaison <- function(data, items, factors){

  if(class(factors)=="facto"){
    factors <-unique(as.numeric(unlist(factors[["factor"]])))
  }
  factors <- sort(factors)

  factor.model <- vector("list")

  for (factor in factors){
    tmp <- vector("list")
    FA <- psych::fa(data[,items], nfactors = factor)
    tmp[["model"]]  <- FA
    tmp[["fit"]] <- data.frame(
      "CFI"= fa.CFI(FA),
      "TLI"= FA$TLI,
      "RMSEA" = FA$RMSEA[1],
      "SRMR" = FA$rms
    )
    factor.model[[as.character(factor)]]<- tmp
  }

  fit.comp <- data.frame()
  for(i in 1:length(factor.model)){
    fit.comp <-rbind(fit.comp,factor.model[[i]]$fit)
  }

  fit.comp <- fit.comp %>%
    dplyr::mutate(d.CFI = CFI - lag(CFI),
           d.TLI = TLI - lag(TLI),
           d.RMSEA = RMSEA - lag(RMSEA),
           d.SRMR = SRMR - lag(SRMR))
  row.names(fit.comp) <- NULL

  evaluation <- "psych::anova.psych("
  f <- paste0("factor.model$`",names(factor.model),"`$model",collapse = ", ")
  evaluation <- paste(evaluation,f,")")
  model.comp <- eval(parse(text=evaluation))
  model.comp <- cbind(model.comp,fit.comp)

  factor.model[["model comparaison"]] <- model.comp
  class(factor.model) <- 'factor.comparaison'
  return(factor.model)
}


#' Print method for factor_comparaison
#'
#' @param x a factor.comparaison class
#'
#' @return
#' @export
#'
#' @examples
print.factor.comparaison <- function(x){
  print( knitr::kable(x[["model comparaison"]],'simple',digits = 3))
}




