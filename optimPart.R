setwd("\\Users\\Admin\\Documents\\workspace\\Veero Optim")

require(lpSolve)
require(reshape)
require(plyr)
require(data.table)

source("Fns.R")
source("optimDataPrep.R")
source("optimObj.R")

requirements <- read.csv("Requirement - Copy.csv")
StkInp <- read.csv("Stock - Copy.csv")
StkInp <- consolStk(StkInp)
conv <- read.csv("conversion.csv")
conv$Wastage <- getconWst(conv)
output <- data.frame()

for(i in unique(requirements$Order.no)){
  rmpart <- getrmpart(requirements[requirements$Order.no == i,],StkInp,conv)
  for(j in unique(rmpart[rmpart$Req > 0 ,]$SFG)){
    if(T){
      params <- getParams(15,rmpart[rmpart$SFG == j ,])
      solobj <- runOptim(params)
      sol <- prepSol(solobj,rmpart,params)
      sol$Order.no <- i
      output <- rbind.fill(output,sol)
      
      StkInp <- updateStk(sol,StkInp)
      # print(sum(solobj$solution*params$Cost1))
      # print(j)
    }
  }
  output[is.na(as.matrix(output))] <- 0
}


write2Disk(output,params,"part")
