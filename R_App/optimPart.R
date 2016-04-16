args = commandArgs(trailingOnly=TRUE)
if(length(args) ==0 ){
  print("No Args")
  setwd("/Users/Admin/Documents/workspace/Veero Optim/")
}else {
  print(args[1])
  setwd(args[1])
}

require(lpSolve)
require(reshape)
require(plyr)
require(data.table)

source("R_App/Fns.R")
source("R_App/optimDataPrep.R")
source("R_App/optimObj.R")

requirements <- read.csv(list.files(path = "R_Requirement/",pattern =".*",full.names = T))
StkInp <- read.csv(list.files(path = "R_Stock/",pattern ="Stoc.*",full.names = T))
StkInp <- consolStk(StkInp)
conv <- read.csv("R_Conversion Cost & Wastage/conversion.csv")
wst <- as.vector(read.csv("R_Conversion Cost & Wastage/Wastage.csv"))
conv$Wastage <- getconWst(conv)
output <- data.frame()

for(i in unique(requirements[,c("Order.No")]))
{
  for(j in unique(requirements[,c("Item")]))
  {
    rmpart <- getrmpart(requirements[(requirements$Order.No == i & requirements$Item == j),],StkInp,conv)
    for(k in unique(rmpart[rmpart$Req > 0 ,]$SFG)){
      params <- getParams(wst[[1]],rmpart[rmpart$SFG == k ,])
      solobj <- runOptim(params)
      sol <- prepSol(solobj,rmpart,params)
      sol$Order.No <- i
      sol$Item <- j
      output <- rbind.fill(output,sol)
      
      StkInp <- updateStk(sol,StkInp)
      # print(sum(solobj$solution*params$Cost1))
      # print(k)
    }
    output[is.na(as.matrix(output))] <- 0
  }
}

write2Disk(output,params,"part")
