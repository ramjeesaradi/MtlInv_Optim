setwd("\\Users\\Admin\\Documents\\workspace\\Veero Optim")
library(lpSolve)
library(reshape)
library(plyr)
library(data.table)

source("Fns.R")



inp1 <- read.csv("input1.csv")

inp1$sfg <- inp1$Semi.Finished.Material.Number
inp1$sfg <- sapply(inp1$sfg, function (x) gsub("\t", "", x))
inp1$Priority.List.Mentioned.in.BOM[!is.na(inp1$Priority.List.Mentioned.in.BOM)]<- 0
inp1$Priority.List.Mentioned.in.BOM[is.na(inp1$Priority.List.Mentioned.in.BOM)]<- 1
inp1 <- inp1[!is.na(inp1$Raw.Material.Sizes..Breadth.),]
inp1[is.na(inp1$Raw.Material.Sizes..Length..),] <- 0

foreCastdt <-  as.Date(inp1$Forecast.Month)

inp1$month <- round(difftime(foreCastdt,min(foreCastdt),units = "days")/30)



# inp1$nsfg <- goodsPerRawMet(rmL = Raw.Material.Sizes..Length.,rmW = Raw.Material.Sizes..Breadth., sfL = Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.,sfW = Breadth)

sfgreq <- aggregate(CIR.Sales.order.No..Rejection.Production.order.requirement~sfg+month,inp1, mean)


######################################################input 2############################

inp2 <- read.csv("input2.csv")
inp2 <- inp2[!is.na(inp2$Ware.house.Stock),]
inp2$Moving.Average.Price <- as.numeric(inp2$Moving.Average.Price)
inp2$RM.Stock.with.Sizes..Length...Breadth.[is.na(inp2$RM.Stock.with.Sizes..Length...Breadth.)] <- 0
inp2$Ware.house.Stock[is.na(inp2$Ware.house.Stock)] <- 0
inp2$Moving.Average.Price[is.na(inp2$Moving.Average.Price)] <- 0
inp2$Lead.Times[is.na(inp2$Lead.Times)] <- 0
inp2$Batch[is.na(inp2$Batch)] <- ""

#Aggregate Stock from both ware houses max(Moving.Average.Price)+max(Lead.Times)
inp2 <- aggregate(cbind(Ware.house.Stock,Moving.Average.Price,Lead.Times)  ~ Material.Code+Breadth+RM.Stock.with.Sizes..Length...Breadth.+Thickness+Density+Batch,
                  inp2,
                  max)

######################################################  Conv############################
conv <- read.csv("conversion.csv", skip = 10)
#######################Table of Purchasable and convertable RawMaterial###########################################
rm1 <- inp1[,c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.","Thickness","Density","Moving.Average.Price","Lead.Times")]
rm2 <- inp2[,c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.","Thickness","Density")]
rm3 <- conv[,c("To.Material.Number","B.1","L.1","W.1","D","From.Material.Number","Conversion.Cost","Wastage" )]
names(rm1) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density", "SheetCost","Lead.Times")
# names(rm2) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density")
####Remove coil from the sheet stocks
rmS <- rm1[(rm1$RM.Length != 0),]

rmC <- merge(rm3,unique(rm1[(rm1$RM.Length == 0),c("RM","SheetCost","Lead.Times")])
             ,by.x = "From.Material.Number", by.y = "RM",
             all.x = T)
names(rmC) <- c( "con","RM", "RM.Breadth","RM.Length","RM.Width","RM.Density","conCost","conWst","CoilCost","Lead.Times")
rmS[,c("conCost","conWst","CoilCost")] <- 0
rmS$con <- ""
rmC[,"SheetCost"] <- 0
rm <- rbind(rmS,rmC[,names(rmS)])
rm <- unique(rm)
#Remove raw meterials with no Length or Breadth
# rm <- rm[which(!(is.na(rm$RM.Breadth) | is.na(rm$RM.Length))),]

####################################SFG(parts) with their compatible RM########################################
# rmpart <- merge(rm,inp1,
#                 by.x = c("RM","Breadth","Length"), 
#                 by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length."),
#                 all.x = T)

rmpart0 <- unique(merge(rm,
                unique(inp1[inp1$Priority.List.Mentioned.in.BOM ==0,c("Raw.Material..Number","Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth","Manufacturing.Lead.time..Repleshinshment.Lead.time.")]),
                by.x = c("RM"), 
                by.y =c("Raw.Material..Number")
                ))
rmpart1 <- unique(merge(rm,
                        unique(inp1[inp1$Priority.List.Mentioned.in.BOM ==1,c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.","Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth","Manufacturing.Lead.time..Repleshinshment.Lead.time.")]),
                        by.x = c("RM","RM.Breadth","RM.Length"),
                        by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.")
                        ))
rmpart <- rbind(rmpart0,rmpart1)
nms <- names(rmpart) <- c(names(rm),"SFG","SFG.Length","SFG.Breadth","Manf.Lead")

##Get Raw material Required quantity and Total Order Requirement


rmpart <- merge(rmpart,
                sfgreq,
                by.x = c("SFG"),
                by.y = c( "sfg"),
                all.x = T)[,c(names(rmpart),"CIR.Sales.order.No..Rejection.Production.order.requirement")]
rmpart <- renameCol(rmpart,"CIR.Sales.order.No..Rejection.Production.order.requirement", "Req")
#################################################################################################################
#Populating sheet And Coil Stocks and Batch
rmpartSheet <- merge(rmpart[rmpart$con=="",],
                inp2[,!(names(inp2) %in% "Lead.Times")],
                by.x = c("RM","RM.Breadth","RM.Length"),
                by.y = c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.")
                ,all.x = T)[,c(names(rmpart),"Ware.house.Stock","Batch")]
rmpartSheet <- renameCol(rmpartSheet,"Ware.house.Stock", "SheetStock")
# rmpartSheet <- renameCol(rmpartSheet, "Moving.Average.Price", "SheetCost")
rmpartCoil <- merge(rmpart[rmpart$con!="",],
                inp2[inp2$RM.Stock.with.Sizes..Length...Breadth.==0,!(names(inp2) %in% "Lead.Times")],
                by.x = c("con")
                ,by.y = c("Material.Code")
                ,all.x = T)[,c(names(rmpart),"Ware.house.Stock","Batch")]
rmpartCoil <- renameCol(rmpartCoil,"Ware.house.Stock", "CoilStock")
# rmpartCoil <- renameCol(rmpartCoil, "Moving.Average.Price", "CoilCost")
rmpart <- rbind.fill(rmpartSheet,rmpartCoil)
rmpart$CoilStock[is.na(rmpart$CoilStock)] <- 0
rmpart$SheetStock[is.na(rmpart$SheetStock)] <- 0
rmpart <- rmpart[!is.na(rmpart$RM),]
rmpart$WstCost <- rmpart$CoilCost + rmpart$SheetCost +rmpart$conCost

########################### Separate entries for each RM with Stock and with out stock ###############################
rmpart$inStock <- NA
rmpartShinStock <- rmpart[rmpart$SheetStock > 0,]
rmpartCoinStock <- rmpart[rmpart$CoilStock > 0,]

rmpart$inStock <- "noStock"
rmpart$Batch <- ""
rmpart$WstCost <- 0

#Change cost of "inStock" rm to 0 except coils to sheet convesion Cost
if(nrow(rmpartShinStock) != 0){
  rmpartShinStock$inStock <- "inStock"
  rmpartShinStock$SheetCost <- 0
  rmpartShinStock$CoilCost <- 0
  rmpartShinStock$CoilStock <- 0
  rmpartShinStock$Lead.Times <- 0
  }

if(nrow(rmpartCoinStock) != 0){
  rmpartCoinStock$inStock <- "inStock"
  rmpartCoinStock$CoilCost <- 0
  rmpartCoinStock$SheetCost <- 0
  rmpartCoinStock$Lead.Times <- 0
  }
#
rmpartShinStock$CoilStock <- 0
rmpart$CoilStock <- 0
rmpart$SheetStock <- 0
rmpart <- rbind(rmpart,rmpartShinStock,rmpartCoinStock)



###########################Generate Costs for each material ##############################################
rmpart$SheetCost[rmpart$inStock == "inStock"] <- 0
rmpart$CoilCost[rmpart$inStock == "inStock"] <- 0

##########################################################################################################
rmpart <- rmpart[!is.na(rmpart$SFG),]
rmpart[is.na(as.matrix(rmpart))] <- 0
#Parts produced per KG Raw material)
rmpart$Qnty <- goodsPerRawMet(rmpart)
#Assign wastage for RM,SFG pairs
rmpart$wastage <- wastage(rmpart)
rmpart$tsfgwt <- sfgwt(rmpart)*rmpart$Req

###########################Generate Total Costs for each material #########################################
rmpart$totalCost <- rmpart$conCost + rmpart$SheetCost + rmpart$CoilCost + rmpart$WstCost*(rmpart$conWst+rmpart$wastage)/100
###########################Generate Total Lead for each SFG #########################################
rmpart$totalLead <- rmpart$Manf.Lead + rmpart$Lead.Times
######################## Apply Filters to Clean master table#########################
#Create duplicate coulumns for casting/pivoting to matrix
rmpart$SFG_ <- rmpart$SFG
rmpart$RM_ <- rmpart$RM
rmpart$RM.Length_ <- rmpart$RM.Length
rmpart$RM.Breadth_ <- rmpart$RM.Breadth
rmpart$con_ <- rmpart$con
rmpart$Batch_ <- rmpart$Batch
rmpart <- rmpart[rmpart$Qnty != 0,]
rmpart <- rmpart[!is.na(rmpart$RM),]

##########################################################################################################

# sfg0 <- rbind(sfg0,sfg0wst)

# params$sfg0 <- sfg0
