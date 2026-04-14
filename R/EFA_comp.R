


#' EFA comparaison model
#'
#' Function that provide model fit for difference EFA model
#'
#'
#' @param data e
#' @param items e
#' @param nfactor e
#' @param parallel e
#' @param fit e
#'
#'
#' @return test
#' @export
#'
#'
#' @import lavaan
#' @import foreach
#' @import doSNOW
#' @import parallel
#'
#' @examples
#' data("inference")
#' items <- c(paste0("item_0",3:9),"item_10")
#' eval1 = EFA.comp(data = inference, items = items, nfactor = 1:3)
#' summary(eval1)
#'
#'





EFA.comp <- function(data, items, nfactor, parallel = TRUE, fit=TRUE ,...){

  if(parallel){
    cat(
      "Models are process in parallel, if you want error and warning run with 'parallel = FALSE'\n"
    )

    ## Parallel setup
    no_cores <- parallel::detectCores()-1
    cl <- makeCluster(no_cores)
    registerDoSNOW(cl)

    ## Models Compute
    cat( "Models computation \n")

    ### Progress bar
    iterations <- length(nfactor)
    pb <- txtProgressBar(max = iterations, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    ### Parallel loop
    try({
      models <- foreach(f = nfactor, .packages=c("psychometRics","lavaan"),.options.snow = opts) %dopar%{
        formul <-""
        for(i in 1:f){
          Fact = paste0('efa("efa1")*F',i)
          formul <- paste0(formul, SEM_reg(Fact,items, latent = T))
        }
        sem.tmp <- sem(formul,data, ... ) ## Don't forget to remove
      }
    })

    close(pb)

    if(fit){
      ## Fits Compute
      cat( "Fits computation \n")

      ### Progress bar
      iterations <- length(models)
      pb <- txtProgressBar(max = iterations, style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)

      ### Parallel loop
      try({
        fits <- foreach(f = models, .packages=c("psychometRics","lavaan"),.options.snow = opts) %dopar%{
          fit.tmp = fitmeasures(f, fit.measures = c('cfi','tli','rmsea','srmr'),
                                fm.args = list(standard.test     = "default",
                                               scaled.test       = "default",
                                               rmsea.ci.level    = 0.90,
                                               rmsea.close.h0    = 0.05,
                                               rmsea.notclose.h0 = 0.08,
                                               robust            = F,
                                               cat.check.pd      = T) )
        }
      })
    }


    close(pb)
    stopCluster(cl)
  } else {

    ## No parallel compute
    models <- list()
    fits <-list()
    formul <-""

    ## Compute loop for models and fits
    for(f in nfactor){
      print(paste0("### Factor ",f, " ###"))

      # Formulat
      Fact = paste0('efa("efa1")*F',f)
      formul <- paste0(formul, SEM_reg(Fact,items, latent = T))

      # Model
      sem.tmp <- sem(formul,data,...)

      if(fit){
        fit.tmp = fitMeasures(sem.tmp,
                              fit.measures = c('cfi','tli','rmsea','srmr'),
                              fm.args = list(standard.test     = "default",
                                             scaled.test       = "default",
                                             rmsea.ci.level    = 0.90,
                                             rmsea.close.h0    = 0.05,
                                             rmsea.notclose.h0 = 0.08,
                                             robust            = F,
                                             cat.check.pd      = T))
        fits <- append(fits,list(fit.tmp))
      }
      # Fit



      # Merge data
      models <- append(models,sem.tmp)

    }

  }

  ## Fits and comparison return
  evaluation <- "lavaan::lavTestLRT("
  efas <- paste("models[[",1:length(nfactor),"]]")
  efas <- paste0(efas, collapse = ", ")
  evaluation <- paste0(evaluation, efas, ")")
  model.comp <- eval(parse(text=evaluation))
  model.comp = as.data.frame(model.comp)
  model.comp$models <- paste0("nfactor = ",if(length(nfactor)>1)nfactor else (nfactor-1):nfactor)


  if(fit){
    fits <- data.frame(do.call("rbind",fits))
    fits <- fits %>%
      dplyr::mutate(
        d.CFI = cfi - lag(cfi),
        d.TLI = tli - lag(tli),
        d.RMSEA = rmsea - lag(rmsea),
        d.SRMR = srmr - lag(srmr)
      )
    fits$models <- paste0("nfactor = ",nfactor)
    fits <- model.comp %>% left_join(fits,by = "models") %>%
      relocate(models,cfi,tli,rmsea,srmr, d.CFI, d.TLI, d.RMSEA ,d.SRMR)
  } else {
    fits <- model.comp  %>% data.frame() %>%
      mutate(models = paste0("nfactor = ",nfactor)) %>%
      relocate(models)
  }




  ## Return build
  rez <- list(models = models, fits = fits)
  class(rez) <-  "EFA.comp"
  return(rez)
}



#' Print method for the rar class
#'
#' @param x An alpha class object
#'
#' @return print the result of the function
#' @export
#' @importFrom dplyr

print.EFA.comp <- function(x){
  x <- x$fits  %>% as.data.frame() %>% mutate_if(is.numeric, round, digits=3)
  print(x)
}





#' summary method for EFA.comp
#'
#'
#' @export
#' @import dplyr
#' @importFrom tibble  column_to_rownames
#' @importFrom tibble  remove_rownames
#' @import lavaan




# setClass("EFA.comp")
setMethod("summary", signature(object="EFA.comp"),
          function(object) {

            f = 1

            for(i in object[["models"]]){
              cat(       "##########################\n")
              cat(paste0("###       model ",f,"      ###\n"))
              cat(       "##########################\n")
              f=f+1

              print(lavaan:::lav_object_summary(i, header = FALSE, estimates = FALSE,
                                                efa = T))
            }
            cat("######################################\n")
            cat("###   Models Fit and comparaisons  ###\n")
            cat("######################################\n\n")
            print(round(object$fits %>% tibble::remove_rownames() %>% tibble::column_to_rownames("models"),3))
          }
)

