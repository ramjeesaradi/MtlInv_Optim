setwd("\\Users\\Admin\\Documents\\workspace\\Veero Optim")
library(lpSolve)
library(reshape)
library(plyr)
library(data.table)

source("Fns.R")

params <- list()

wsthrs <- 15

inp1 <- read.csv("input1.csv",skip = 1)

inp1$sfg <- inp1$Semi.Finished.Material.Number
inp1$sfg <- sapply(inp1$sfg, function (x) gsub("\t", "", x))
inp1$Priority.List.Mentioned.in.BOM[!is.na(inp1$Priority.List.Mentioned.in.BOM)]<- 0
inp1$Priority.List.Mentioned.in.BOM[is.na(inp1$Priority.List.Mentioned.in.BOM)]<- 1

months <-  as.Date(inp1$Forecast.Month)

inp1$month <- round(difftime(months,min(months),units = "days")/30)



# inp1$nsfg <- goodsPerRawMet(rmL = Raw.Material.Sizes..Length.,rmW = Raw.Material.Sizes..Breadth., sfL = Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.,sfW = Breadth)

sfgreq <- aggregate(CIR.Sales.order.No..Rejection.Production.order.requirement~sfg+month,inp1, mean)


# wastage <- cast(inp1, sfg~Semi.Finished.Material.Number+Raw.Material..Number,
#                 fun.aggregate = max,
#                 add.missing = 0,
#                 fill = 0,
#                 value = "pwastage")



######################################################input 2############################

inp2 <- read.csv("input2.csv")

inp2$RM <- sapply(
  data.frame(t(inp2[,c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.")])),
  function (x) paste(x,collapse = "-"))
######################################################  Conv############################
conv <- read.csv("conversion.csv", skip = 10)
#######################Table Purchasable and convertable RawMaterial###########################################
rm1 <- inp1[,c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.")]
rm2 <- inp2[,c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.")]
rm3 <- conv[,c("To.Material.Number","B.1","L.1","From.Material.Number","Conversion.Cost" )]
names(rm1) <- c("RM","RM.Breadth","RM.Length")
names(rm2) <- c("RM","RM.Breadth","RM.Length")
####Remove coil from the sheet stocks
rm2 <- rm2[!is.na(rm2$RM.Length),]
names(rm3) <- c("RM","RM.Breadth","RM.Length", "con","conCost")
############################Get raw materials that are from available coil#####################################
# rm3 <- merge(conv, inp2
#              ,by.x = c("From.Material.Number")
#              ,by.y = c("Material.Code")
#              )[,c("To.Material.Number","B.1","L.1")]
# names(rm3) <- c("RM","RM.Breadth","RM.Length")
################################################################################################################
rm1$con <- ""
rm2$con <- ""
rm2$conCost <- 0
# rm3$con <- "_Conv"
rm <- rbind(rm2,rm3)
rm <- unique(rm)
#Remove raw meterials with no Length or Breadth
# rm <- rm[which(!(is.na(rm$RM.Breadth) | is.na(rm$RM.Length))),]

####################################SFG(parts) with their compatible RM########################################
# rmpart <- merge(rm,inp1,
#                 by.x = c("RM","Breadth","Length"), 
#                 by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length."),
#                 all.x = T)

rmpart0 <- unique(merge(rm,
                unique(inp1[inp1$Priority.List.Mentioned.in.BOM ==0,]),
                by.x = c("RM"), 
                by.y =c("Raw.Material..Number"),
                all.x = T)[,c(names(rm),"Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth")])
rmpart1 <- unique(merge(rm,
                        unique(inp1[inp1$Priority.List.Mentioned.in.BOM ==1,]),
                        by.x = c("RM","RM.Breadth","RM.Length"), 
                        by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length."),
                        all = T)[,c(names(rm),"Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth")])
rmpart <- rbind(rmpart0,rmpart1)
nms <- names(rmpart) <- c(names(rm),"SFG","SFG.Length","SFG.Breadth")

##Assign priority For RM, SFG Combination
rmpart <- merge(rmpart,
                         inp1,
                         by.x = c("RM","RM.Breadth","RM.Length","SFG"),
                         by.y = c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.", "Semi.Finished.Material.Number"),
                         all.x = T)[,c(names(rmpart),"Raw.Material.required.quantity")]
rmpart <- renameCol(rmpart,"Raw.Material.required.quantity", "Qnty")

rmpart <- merge(rmpart,
                inp1,
                by.x = c("SFG"),
                by.y = c( "Semi.Finished.Material.Number"),
                all.x = T)[,c(names(rmpart),"CIR.Sales.order.No..Rejection.Production.order.requirement")]
rmpart <- renameCol(rmpart,"CIR.Sales.order.No..Rejection.Production.order.requirement", "Req")


# rmpart[is.na(as.matrix(rmpart))] <- 0
rmpart$RM.Thickness <- rep(1.2,nrow(rmpart))
rmpart$RM.Density <- rep(0.0056,nrow(rmpart))

#Parts produced per KG Raw material
rmpart$Qnty <- goodsPerRawMet(rmpart)

rmpartSheet <- merge(rmpart[rmpart$con=="",],
                inp2,
                by.x = c("RM","RM.Breadth","RM.Length"),
                by.y = c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth."),
                all.x = T)[,c(names(rmpart),"Ware.house.Stock", "Moving.Average.Price")]
rmpartSheet <- renameCol(rmpartSheet,"Ware.house.Stock", "SheetStock")
rmpartSheet <- renameCol(rmpartSheet, "Moving.Average.Price", "SheetCost")
rmpartCoil <- merge(rmpart[rmpart$con!="",],
                inp2[is.na(inp2$RM.Stock.with.Sizes..Length...Breadth.),c("Material.Code", "Ware.house.Stock", "Moving.Average.Price")],
                by.x = c("con")
                ,by.y = c("Material.Code")
                ,all.x = T)[,c(names(rmpart),"Ware.house.Stock", "Moving.Average.Price")]
rmpartCoil <- renameCol(rmpartCoil,"Ware.house.Stock", "CoilStock")
rmpartCoil <- renameCol(rmpartCoil, "Moving.Average.Price", "CoilCost")
rmpart <- rbind.fill(rmpartSheet,rmpartCoil)
rmpart <- rmpart[!is.na(rmpart$RM),]

###########################Separate entries for with Stock and with out stock###############################

rmpartShInstk <- rmpart[!is.na(rmpart$SheetStock),]
rmpartCoInstk <- rmpart[!is.na(rmpart$CoilStock),]
rmpartShInstk$SheetStock <- NA
if(!is.null(nrow(rmpartCoInstk$CoilStock))){rmpartCoInstk$CoilStock <- NA} 
rmpart <- rbind(rmpart,rmpartShInstk,rmpartCoInstk)
rmpart$inStk[!is.na(rmpart$SheetStock)] <- "inStk"
rmpart$inStk[!is.na(rmpart$CoilStock)] <- "inStk"
rmpart$inStk[is.na(rmpart$inStk)] <- "noStk"

###########################Generate Costs for each material ##############################################
rmpart$SheetCost[rmpart$inStk == "inStk"] <- 0
rmpart$CoilCost[rmpart$inStk == "inStk"] <- 0

##########################################################################################################
rmpart <- rmpart[!is.na(rmpart$SFG),]
rmpart[is.na(as.matrix(rmpart))] <- 0
#Assign wastage for RM,SFG pairs
rmpart$wastage <- wastage(rmpart)
rmpart$tsfgwt <- sfgwt(rmpart)*rmpart$Req

###########################Generate Conv Costs for each material #########################################
rmpart$totalCost <- rmpart$conCost + rmpart$SheetCost + rmpart$CoilCost


######################## Apply Filters to Clean master table#########################
# rmpart <- rmpart[!(rmpart$SheetCost==0 &  rmpart$CoilCost == 0 & rmpart$inStk == "noStk"),]
#Create duplicate coulumns for casting/pivoting to matrix
rmpart$SFG_ <- rmpart$SFG
rmpart$RM_ <- rmpart$RM
rmpart$RM.Length_ <- rmpart$RM.Length
rmpart$RM.Breadth_ <- rmpart$RM.Breadth
rmpart$con_ <- rmpart$con
##########################################################################################################
#############################################Generate Matrices#######################
#Matrix for Calculating the parts produced
sfg <- cast(rmpart,SFG ~ SFG_+RM_+con_+RM.Length+RM.Breadth+inStk,
            value = "Qnty",
            fun.aggregate = max,
            fill=0)

pairnms <- names(sfg)[-1]
# pairs <- lapply(strsplit(pairnms, "_"), function (x) x)
# # ifelse(length(x)==4,c(x,""),x[c(1,2,4,5,3)])
# 
#Matrix for Calculating the percentage wastage
wstprct <- cast(rmpart,SFG ~ SFG_+RM_+con_+RM.Length+RM.Breadth+inStk,
            value = "tsfgwt",
            fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/max(x)),
            fill=0)
params$wstprct <- wstprct[,pairnms]
#Stock Matrix after combinig all whare houses
Sheetstk <- cast(rmpart,RM+RM.Length_+RM.Breadth_ ~ SFG_+RM_+con_+RM.Length+RM.Breadth+inStk,
            value = "SheetStock",
            fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/max(x)),
            fill=0)

params$Sheetstk <- Sheetstk[,c(pairnms)]
Coilstk <- cast(rmpart,con ~ SFG_+RM_+con_+RM.Length+RM.Breadth+inStk,
                 value = "CoilStock",
                 fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/max(x)),
                 fill=0)

params$Coilstk <- Coilstk[pairnms]
Cost <- cast(rmpart,. ~ SFG_+RM_+con_+RM.Length+RM.Breadth+inStk,
             value = "totalCost",
             fun.aggregate = max,
             fill=0)

params$Cost <- Cost[pairnms]
# params$rmstk <- rmstk[,pairnms]
# params$rmstkln <- colMeans(rmstk[,pairnms])
# 
# sfgwst <- cast(rmpart,SFG ~ SFG_+RM_+RM.Length+RM.Breadth,
#             value = "wastage",
#             fun.aggregate = max,
#             fill=0)
# params$sfgwst <- sfgwst[,pairnms]
# 
# sfgpr <- sfgwst <- cast(rmpart,. ~ SFG_+RM_+RM.Length+RM.Breadth,
#             value = "Priority",
#             fun.aggregate = max,
#             fill=0)
# params$sfgpr <- sfgpr[,pairnms]

sfg0 <- merge(sfg,sfgreq[sfgreq$month==0,],by.y = "sfg", by.x = "SFG",all.y = T)
sfg0 <- as.matrix(sfg0[,c(pairnms,"CIR.Sales.order.No..Rejection.Production.order.requirement")])
lhs <- rbind(sfg0[,pairnms],wstprct[,pairnms], Sheetstk[,pairnms],Coilstk[,pairnms])
rhs <- c(sfg0[,ncol(sfg0)],rep(1.6,nrow(wstprct)),rep(1,nrow(Sheetstk)),rep(1,nrow(Coilstk)))
cond <- c(rep("==",nrow(sfg0)),rep("<=",nrow(wstprct)),rep("<=",nrow(Sheetstk)),rep("<=",nrow(Coilstk)))
# sfg0 <- rbind(sfg0,sfg0wst)

# params$sfg0 <- sfg0
