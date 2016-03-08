setwd("\\Users\\Admin\\Documents\\workspace\\Veero Optim")
library(lpSolve)
library(reshape)

source("Fns.R")

params <- list()

inp1 <- read.csv("input1.csv",skip = 1)

inp1$sfg <- inp1$Semi.Finished.Material.Number
inp1$sfg <- sapply(inp1$sfg, function (x) gsub("\t", "", x))
inp1$Priority.List.Mentioned.in.BOM[is.na(inp1$Priority.List.Mentioned.in.BOM)]<- 1

months <-  as.Date(inp1$Forecast.Month)

inp1$month <- round(difftime(months,min(months),units = "days")/30)
inp1$nsfg <- 1/Raw.Material.required.quantity


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
rm3 <- conv[,c("To.Material.Number","B.1","L.1")]
names(rm1) <- c("RM","RM.Breadth","RM.Length")
names(rm2) <- c("RM","RM.Breadth","RM.Length")
names(rm3) <- c("RM","RM.Breadth","RM.Length")
rm1$con <- ""
rm2$con <- ""
rm3$con <- "_Conv"
rm <- rbind(rm2,rm3)
rm <- unique(rm)
#Remove raw meterials with no Length or Breadth
rm <- rm[which(!(is.na(rm$RM.Breadth) | is.na(rm$RM.Length))),]

####################################SFG(parts) with their compatible RM########################################
# rmpart <- merge(rm,inp1,
#                 by.x = c("RM","Breadth","Length"), 
#                 by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length."),
#                 all.x = T)

rmpart <- unique(merge(rm,
                unique(inp1[,c("Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth","Raw.Material..Number")]),
                by.x = c("RM"), 
                by.y =c("Raw.Material..Number"),
                all.x = T))
nms <- names(rmpart) <- c(names(rm),"SFG","SFG.Length","SFG.Breadth")

##Assign priority For RM, SFG Combination
rmpart <- merge(rmpart,
                         inp1,
                         by.x = c("RM","RM.Breadth","RM.Length","SFG"),
                         by.y = c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.", "Semi.Finished.Material.Number"),
                         all.x = T)[,c(names(rmpart),"Priority.List.Mentioned.in.BOM","Raw.Material.required.quantity")]

names(rmpart) <- c(nms,"Priority","Qnty")
rmpart$Priority[is.na(rmpart$Priority)] <- 1+ max(rmpart$Priority[!is.na(rmpart$Priority)])
rmpart[is.na(as.matrix(rmpart))] <- 0
rmpart$RM.Thickness <- rep(1.2,nrow(rmpart))
rmpart$RM.Density <- rep(0.0056,nrow(rmpart))

rmpart$Qnty <- goodsPerRawMet(rmpart)

rmpart <- merge(rmpart,
                inp2,
                by.x = c("RM","RM.Breadth","RM.Length"),
                by.y = c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth."),
                all.x = T)[,c(names(rmpart),"Ware.house.Stock")]
rmpart$SFG_ <- rmpart$SFG
rmpart$RM_ <- apply(rmpart[,c("RM", "con")],1,FUN =  function (x) paste(x,collapse = ""))
rmpart$RM.Length_ <- rmpart$RM.Length
rmpart$RM.Breadth_ <- rmpart$RM.Breadth

#Assign wastage for RM,SFG pairs
rmpart$wastage <- wastage(rmpart)

rmpart <- rmpart[!is.na(rmpart$SFG),]
rmpart[is.na(as.matrix(rmpart))] <- 0


#############################################Generate Matrices#######################
sfg <- cast(rmpart,SFG ~ SFG_+RM_+RM.Length+RM.Breadth,
            value = "Qnty",
            fun.aggregate = max,
            fill=0)

pairnms <- names(sfg)[-1]
#Stock Matrix after combinig all whare houses
rmstk <- cast(rmpart,RM+RM.Length_+RM.Breadth_ ~ SFG_+RM_+RM.Length+RM.Breadth,
            value = "Ware.house.Stock",
            fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/sum(x)),
            fill=0)

params$rmstk <- rmstk[,pairnms]

sfgwst <- cast(rmpart,SFG ~ SFG_+RM_+RM.Length+RM.Breadth,
            value = "wastage",
            fun.aggregate = max,
            fill=0)
params$sfgwst <- sfgwst[,pairnms]

sfgpr <- sfgwst <- cast(rmpart,. ~ SFG_+RM_+RM.Length+RM.Breadth,
            value = "Priority",
            fun.aggregate = max,
            fill=0)
params$sfgpr <- sfgpr[,pairnms]

sfg0 <- merge(sfg,sfgreq[sfgreq$month==0,],by.y = "sfg", by.x = "SFG",all.y = T)
sfg0 <- as.matrix(sfg0[,c(pairnms,"CIR.Sales.order.No..Rejection.Production.order.requirement")])
params$sfg0 <- sfg0
