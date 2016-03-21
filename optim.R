setwd("\\Users\\Admin\\Documents\\workspace\\Veero Optim")

require(lpSolve)
require(reshape)
require(plyr)
require(data.table)

source("Fns.R")
source("optimDataPrep.R")
source("optimObj.R")

requirements <- read.csv("input1Test.csv")
StkInp <- read.csv("input2Test.csv")
StkInp <- consolStk(StkInp)
conv <- read.csv("conversion.csv")
output <- data.frame()

for(i in 0:0){

  rmpart <- getrmpart(requirements,StkInp,conv)
  if(max(rmpart$month) >= i){
    params <- getParams(i,70,rmpart)
    solobj <- runOptim(params)
    sol <- prepSol(solobj,rmpart,params)
    output <- rbind.fill(output,sol)
    StkInp <- updateStk(sol,StkInp)
    print(sum(solobj$solution*params$Cost1))
  }
  
  output[is.na(as.matrix(output))] <- 0
  totalRM <- sum(output$fromStock) + ifelse("Purchase" %in% names(output),sum(output$Purchase), 0)
  print(totalRM)
  print(paste(c("Purchse",ifelse("Purchase" %in% names(output),sum(output$Purchase), 0))))
  write2Disk(output,params,paste(c(i,"all")))
}
