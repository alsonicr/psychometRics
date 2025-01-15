
library(psychometRics)
data("inference")
items <- c(paste0("item_0",3:9),"item_10")



freq.scan(inference, items, verbose = F)
inference[1:4,4:7] <- NA
freq.scan(inference, items, verbose = T)
inference[1:4,4:7] <- 5
freq.scan(inference, items, verbose = T)



discrimination_cor(inference, items)
alpha.scan(inference, items, verbose = F )
alpha.scan(inference, items, verbose = T, parallel = T )


CTT_summary(inference, items, verbose = T)
a = CTT_summary(inference, items, verbose = F)


SEM_reg("y","x")
SEM_reg("y",c("a","b","c"))
SEM_reg(c("y1","y2"),c("a","b","c"))
SEM_cov(c("a","b","c"))

crosslag_model(variables=c("a","b","c"),times = 1:3, order="times/variables" , prefix = "w")
a = crosslag_model(variables=c("a","b","c"),times = 1:3)



SEM_reg_RI("x",c("x1","x2","x3","x4"))
SEM_wth_var(paste("x",1:4,sep=""))

RI_crosslag_model(c("x","y"),1:5,order="variables/times",join="")

RI_crosslag_model(c("x","y"),1:5,order="times/variables",join="")



# Load the required packages.
require(lavaan)

# Load in the data
## Traditional RI-CLPM
dat <- read.table("RICLPM.dat",
                  col.names = c(
                    "x1", "x2", "x3", "x4", "x5",
                    "y1", "y2", "y3", "y4", "y5")
)


model = RI_crosslag_model(c("x","y"),1:5,order="variables/times",join="")



RICLPM.fit <- lavaan(model,
                     data = dat,
                     missing = "ML",
                     meanstructure = T,
                     int.ov.free = T
)
summary(RICLPM.fit, standardized = T)





cross_lag_parrameters <- function(variables, times, order="variables/times",join=""){

  model.componant <- list()
  model.componant$model$time <- times

  time_var <- list()
  for(var in variables){
    if(order == "times/variables"){
      tmp <- list(paste0(model.componant$model$time, join, var))
      names(tmp) <- "parameter"
      tmp <- list(tmp)
      names(tmp) <- var
      time_var <- append(time_var,tmp)
    }
    if(order == "variables/times"){
      tmp <- list(paste0(var, join, model.componant$model$time))
      names(tmp) <- "parameter"
      tmp <- list(tmp)
      names(tmp) <- var
      time_var <- append(time_var,tmp)
    }
  }

  model.componant <- append(model.componant,list("time variables"=time_var))


  latent_time_var <- list()
  for(var in 1:length(model.componant[["time variables"]])){
    n <- names(model.componant[["time variables"]])[var]

    lv = c(
      paste0("w_",
             model.componant[["time variables"]][[var]]$parameter)
      )
    tmp = list(lv)
    names(tmp) <- "parameter"
    tmp <- list(tmp)
    names(tmp) <- n
    latent_time_var <- append(latent_time_var,tmp)
  }


  model.componant <- append(model.componant,list("latent time variables"=latent_time_var))
  class(model.componant) <- c("SEM_parameters")

  return(model.componant)
}





cross_lag_RI <- function(latent_variable,SEM_parameters){
  RI <- list()
  param <- SEM_parameters$`time variables`[[latent_variable]][[1]]
  tmp <- list(
    "parameter"= param,
    "constrain"= rep(1,length(param))
  )

  if (sum(names(SEM_parameters)=="random intercept") == 0 ){
    RI$`random intercept`[latent_variable] <- list(tmp)
    SEM_parameters <- append(SEM_parameters,RI)
  } else {
    RI$`random intercept`[latent_variable] <- list(tmp)
    SEM_parameters["random intercept"] <-mapply("c",
                            SEM_parameters["random intercept"],
                            RI,
                            SIMPLIFY=FALSE)
  }


  return(SEM_parameters)

}








cross_lag_RS <- function(latent_variable,SEM_parameters){
  RS <- list()
  param <- SEM_parameters$`time variables`[[latent_variable]][[1]]

  tmp <- list(
    "parameter"= param,
    "constrain"= 0:(length(param)-1)
  )

  if (sum(names(SEM_parameters)=="random slope") == 0 ){
    RS$`random slope`[latent_variable] <- list(tmp)
    SEM_parameters <- append(SEM_parameters,RS)
  } else {
    RS$`random slope`[latent_variable] <- list(tmp)
    SEM_parameters["random slope"] <-mapply("c",
                            SEM_parameters["random slope"],
                            RS,
                            SIMPLIFY=FALSE)
  }


  return(SEM_parameters)


}







reg <- function(Ys,Xs,constrain=NULL){

  if(is.null(constrain)){
    rez <- paste0(
      paste0(Ys ,collapse =" + "),
      " ~~ ",
      paste0(Xs,collapse =" + "),
      "\n"
    )
  } else {
    rez <- paste0(
      paste0(Ys ,collapse =" + "),
      " ~~ ",
      paste0(constrain,"*",Xs,collapse =" + "),
      "\n"
    )
  }

  return(rez)
}

variables <- c("y","x")
times = paste0(1:4)
order="variables/times"
join=""
mod <- cross_lag_parrameters(variables, times, order="variables/times",join="")
mod = cross_lag_RI("y",mod)
mod = cross_lag_RI("x",mod)
mod = cross_lag_RS("y",mod)
mod = cross_lag_RS("x",mod)
SEM_parameters = mod

sem.model.build <- function(SEM_parameters){

  ## Create within-person centered variables
  within_person <-"## within-person centered variables\n"
  for (var in names(SEM_parameters[["latent time variables"]])){

    tmp <- paste( SEM_parameters[["latent time variables"]][[var]]$parameter,
            " =~ ",
            "1*",
            SEM_parameters[["time variables"]][[var]]$parameter,
            sep="",
            collapse = " \n")

    within_person <- paste0(within_person,tmp," \n")
  }

  ## Cross lag panel
  cross.lag <- "## Crosslag Panel \n"
  for (t in 1:(length(SEM_parameters$model$time)-1)){
    variables <- names(SEM_parameters$`latent time variables`)
    Ys <- c()
    Xs <- c()
    for (var in variables){

      Xs <- c(Xs,SEM_parameters$`latent time variables`[[var]]$parameter[t])
      Ys <- c(Ys,SEM_parameters$`latent time variables`[[var]]$parameter[t+1])
    }
    cross.lag<- paste0(cross.lag,SEM_reg(Ys,Xs))
  }

  ## Covariance
  covar <- "## Covariance \n"

  for (t in 1:(length(SEM_parameters$model$time))){
    variables <- names(SEM_parameters$`latent time variables`)
    v <- c()
    for (var in variables){
      v <- c(v, SEM_parameters$`latent time variables`[[var]]$parameter[t])
    }
    covar <- paste0(covar, SEM_cov(v),"\n")

  }


  ## Random Intercep
  RI_cov<-c()
  RI <- "## Random Intercept\n"
  for (var in names(SEM_parameters[["random intercept"]])){
    y <-paste0("RI_",var)
    RI_cov<-c(RI_cov,y)
    tmp <- reg(y,
               SEM_parameters[["random intercept"]][[var]]$parameter,
               SEM_parameters[["random intercept"]][[var]]$constrain)

    RI <- paste0(RI, tmp)
  }
  RI <- paste0(RI, SEM_cov(RI_cov),"\n")

  ## Random Slope
  RS_cov<-c()
  RS <- "## Random Slope\n"
  for (var in names(SEM_parameters[["random slope"]])){
    y <-paste0("RS_",var)
    RS_cov<- c(RS_cov,y)
    tmp <- reg(y,
               SEM_parameters[["random slope"]][[var]]$parameter,
               SEM_parameters[["random slope"]][[var]]$constrain)
    RS <- paste0(RS, tmp)
  }
  RS <- paste0(RS, SEM_cov(RS_cov))


  rez <- paste0(
    within_person,
    cross.lag,
    covar,
    RI,
    RS
  )
  class(rez) <- "SEM"

  return(rez)
}



cat(sem.model.build(mod))








HS.model <- ' visual  =~ x1 + v2*x2 + v2*x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit.HS.ortho <- cfa(HS.model,
                    data = HolzingerSwineford1939,
                    orthogonal = TRUE)


summary(fit.HS.ortho, standardized = TRUE)













