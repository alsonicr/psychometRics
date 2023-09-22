
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
