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

for(i in 0:0){
  for(j in unique(requirements[requirements$CIR.Sales.order.No..Rejection.Production.order.requirement > 0 ,]$Semi.Finished.Material.Number))
    
    {
    days2delivery <- max(difftime(as.Date(requirements$Forecast.Month,"%d-%m-%Y"),Sys.Date()))
    rmpart <- getrmpart(requirements[requirements$Semi.Finished.Material.Number == j,],StkInp,conv)
    if(max(rmpart$month) >= i){
      params <- getParams(days2delivery,i,15,rmpart)
      solobj <- runOptim(params)
      sol <- prepSol(solobj,rmpart,params)
      output <- rbind.fill(output,sol)
      StkInp <- updateStk(sol,StkInp)
      # print(sum(solobj$solution*params$Cost1))
      # print(j)
    }
  }
  output[is.na(as.matrix(output))] <- 0
  totalRM <- sum(output$fromStock) + ifelse("Purchase" %in% names(output),sum(output$Purchase), 0)
  # print(totalRM)
  print(paste(c("Purchase",ifelse("Purchase" %in% names(output),sum(output$Purchase), 0))))
  write2Disk(output,params,paste(c(i,"part")))
}
