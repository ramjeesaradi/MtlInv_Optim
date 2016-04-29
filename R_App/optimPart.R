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

requirements <- read.csv(list.files(path = "R_Requirement/",pattern ="R_.*",full.names = T),sep = " ")
StkInp <- read.csv(list.files(path = "R_Stock/",pattern ="R_.*",full.names = T),sep = " ")
################################################
#to be removed when PR and PO are included
StkInp$PO.Number <- NULL
StkInp$PR.Number <- NULL
StkInp$Open.PO.s <- NULL
StkInp$Open.PO.s <- NULL
StkInp <- StkInp[!is.na(StkInp$RM.Breadth),]
################################################

StkInp <- consolStk(StkInp)
conv <- read.csv("R_Conversion Cost & Wastage/conversion.csv")
wst <- as.vector(read.csv("R_Conversion Cost & Wastage/Wastage.csv"))
conv$Wastage <- getconWst(conv)
output <- data.frame()

for(i in unique(requirements[,c("Order.No")]))
{
  for(j in unique(requirements[(requirements$Order.No == i),c("Item")]))
  {
    for(k in unique(requirements[(requirements$Order.No == i & requirements$Item == j),"SFG.Material"])){
      rmpart <- getrmpart(requirements[(requirements$Order.No == i & requirements$Item == j & requirements$SFG.Material == k),],StkInp,conv)
      params <- getParams(wst[[1]],rmpart)
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

write2Disk(output,params,unique(requirements$Plant)[1])
