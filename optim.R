setwd("\\Users\\Admin\\Documents\\workspace\\Veero Optim")

source("optimDataPrep.R")
source("optimObj.R")

sol <- runOptim(0,15,rmpart,sfgreq)
params <- getParams(0,15,rmpart,sfgreq)
