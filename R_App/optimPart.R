args = commandArgs(trailingOnly=TRUE)
if(length(args) ==0 ){
  # print("No Args")
  setwd("/Users/Admin/Documents/workspace/Veero Optim/")
  requirementsFile <- list.files(path = "R_Requirement/",pattern =".*",full.names = T)
  stockFile <- list.files(path = "R_Stock/",pattern =".*",full.names = T)
  tstamp <- format(Sys.time(), format = "%Y%m%d_%H%M%S")
}else {
  setwd(args[1])
  requirementsFile <- args[2]
  tstamp <- requirementsFile
  tstamp <- gsub(".*/R_Req_","",tstamp)
  stockFile <- args[3]
  connection <- file(paste(c("R_App/logs/log_",format(Sys.time(), format = "%Y%m%d_%H%M%S"),".txt"),collapse = ""))
  sink(connection, append=T)
  sink(connection, append=T, type = "message")
}


require(lpSolve)
require(reshape)
require(plyr)
require(data.table)

source("R_App/Fns.R")
source("R_App/optimDataPrep.R")
source("R_App/optimObj.R")


requirements <- read.csv(requirementsFile
                         ,sep = "|"
                         ,colClasses = c(Order.No = "factor"))
requirements$Order.No <- as.factor(requirements$Order.No)
requirements$SFG.Material <- sapply(requirements$SFG.Material,function (x) gsub(" ","",x))
#### FG from SFG to copy
requirements$SFG.Net.Req[requirements$SFG.Material == "" | is.na(requirements$SFG.Material)] <- (requirements$FG.Net.Req[requirements$SFG.Material == "" | is.na(requirements$SFG.Material)])
requirements$SFG.Material[requirements$SFG.Material == "" | is.na(requirements$SFG.Material)] <- as.character(requirements$FG.Material[requirements$SFG.Material == "" | is.na(requirements$SFG.Material)])
NAreq <- requirements[(is.na(requirements$Raw.Material)),]
requirements <- requirements[!(is.na(requirements$Raw.Material)),]

requirements$SFG.Material <- sapply(requirements$SFG.Material,function (x) gsub(" ","",x))

# requirements <- requirements[requirements$SFG.Material == "870-24042-PART3",]

StkInp <- read.csv(stockFile
                   ,sep = "|"
                   ,colClasses = c(Batch = "factor"))

################################################
#to be removed when PR and PO are included
if(nrow(StkInp)>=1){
  StkInp$PO.Number <- NA
  StkInp$PR.Number <- NA
  StkInp$Open.PO.s <- NA
  StkInp$Open.PR.s <- NA
}
# StkInp$RM.Breadth[StkInp$Type == "COIL"] <- 1250
StkInp <- StkInp[!is.na(StkInp$RM.Breadth),]
################################################

StkInp <- consolStk(StkInp)
conv <- read.csv("R_Conversion Cost & Wastage/conversion.csv")
wst <- as.vector(read.csv("R_Conversion Cost & Wastage/Wastage.csv"))
conv$Wastage <- getconWst(conv)

##Populating Coil Widths
requirements <- populateCoilwidth(requirements,conv)
output <- data.frame()

for(i in unique(requirements[,c("Order.No")]))
{
  for(j in unique(requirements[(requirements$Order.No == i),c("Item")]))
  {
    for(k in unique(requirements[(requirements$Order.No == i & requirements$Item == j),"SFG.Material"])){
      
      rmpart <- getrmpart(requirements[(requirements$Order.No == i & requirements$Item == j & requirements$SFG.Material == k),],StkInp,conv)
      
      if(nrow(rmpart)>=1){
        params <- list()
        rmpart1 <- getFeasibleOpts(rmpart,wst[[1]])
        params <- getParams(wst[[1]],rmpart1, params)
        solobj <- runOptim(params)
        if(solobj$status == 2){
          params <- list()
          params <- getParams(wst[[1]],rmpart, params)
          params$message <- "No other feasible options"
          solobj <- runOptim(params)
        }
        sol <- prepSol(solobj,rmpart,params)
        sol$Order.No <- i
        sol$Item <- j
        output <- rbind.fill(output,sol)
        
        StkInp <- updateStk(sol,StkInp)
        }
    }
    output[is.na(as.matrix(output))] <- 0
    if(is.null(output$fromStock)){output$fromStock <- 0}
  }
}
output <- output[!(is.na(output$SFG)),]
output$wastage <- round(output$wastage,digits = 4)
output$Purchase <- round(output$Purchase,digits = 4)
output$fromStock <- round(output$fromStock,digits = 4)
requirements <- unique(rbind.fill(requirements,NAreq))
write2Disk(output,params,tstamp)
sink()
sink(type="message")
close(connection)
