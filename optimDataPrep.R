setwd("/Users/ramjeesaradi/Documents/workspace/MtlInv_Optim")
library(lpSolve)
library(reshape)

source("Fns.R")

params <- list()

inp1 <- read.csv("input1.csv",skip = 1)
attach(inp1)
inp1$RM <- sapply(
  data.frame(rbind(Raw.Material..Number,Raw.Material.Sizes..Breadth.,Raw.Material.Sizes..Length.)),
    function (x) paste(x,collapse = "-"))
inp1$sfg <- inp1$Semi.Finished.Material.Number
inp1$Priority.List.Mentioned.in.BOM[is.na(inp1$Priority.List.Mentioned.in.BOM)]<- 1

months <-  as.Date(inp1$Forecast.Month)

inp1$month <- round(difftime(months,min(months),units = "days")/30)
inp1$nsfg <- 1/Raw.Material.required.quantity
attach(inp1)

# inp1$nsfg <- goodsPerRawMet(rmL = Raw.Material.Sizes..Length.,rmW = Raw.Material.Sizes..Breadth., sfL = Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.,sfW = Breadth)

sfgreq <- aggregate(CIR.Sales.order.No..Rejection.Production.order.requirement~sfg+month,inp1, mean)


# wastage <- cast(inp1, sfg~Semi.Finished.Material.Number+Raw.Material..Number,
#                 fun.aggregate = max,
#                 add.missing = 0,
#                 fill = 0,
#                 value = "pwastage")

priority <- cast(inp1, .~Semi.Finished.Material.Number+Raw.Material..Number,
                 fun.aggregate = max,
                 add.missing = 0,
                 fill = 0,
                 value = "Priority.List.Mentioned.in.BOM")
priority <- as.vector(t(priority[,-1]))
priority[is.na(as.matrix(priority))]<-1
priority <- priority-1

######################################################input 2############################

inp2 <- read.csv("input2.csv")
attach(inp2)
inp2$RM <- sapply(
  data.frame(t(inp2[,c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.")])),
  function (x) paste(x,collapse = "-"))
params$inventory <- inp2
######################################################  Conv############################
conv <- read.csv("conversion.csv", skip = 10)
###########################################################################################
rm1 <- inp1[,c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.")]
rm2 <- inp2[,c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.")]
rm3 <- conv[,c("To.Material.Number","B.1","L.1")]
names(rm1) <- c("RM","RM.Breadth","RM.Length")
names(rm2) <- c("RM","RM.Breadth","RM.Length")
names(rm3) <- c("RM","RM.Breadth","RM.Length")

rm <- rbind(rm1,rm2,rm3)
rm <- unique(rm)
rm <- rm[which(!(is.na(rm$RM.Breadth) & is.na(rm$RM.Length))),]
# rmpart <- merge(rm,inp1,
#                 by.x = c("RM","Breadth","Length"), 
#                 by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length."),
#                 all.x = T)
rmpart <- unique(merge(rm,
                unique(inp1[,c("Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth","Raw.Material..Number")]),
                by.x = c("RM"), 
                by.y =c("Raw.Material..Number"),
                all.x = T))
names(rmpart) <- c("RM","RM.Breadth","RM.Length","SFG","SFG.Length","SFG.Breadth")
rmpart <- merge(rmpart,
                         inp1,
                         by.x = c("RM","RM.Breadth","RM.Length","SFG"),
                         by.y = c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.", "Semi.Finished.Material.Number"),
                         all.x = T)[,c("RM","RM.Breadth","RM.Length","SFG","SFG.Length","SFG.Breadth","Priority.List.Mentioned.in.BOM","Raw.Material.required.quantity")]

names(rmpart) <- c("RM","RM.Breadth","RM.Length","SFG","SFG.Length","SFG.Breadth","Priority","Qnty")
rmpart$Qnty <- 1/rmpart$Qnty
rmpart$Priority[is.na(rmpart$Priority)] <- 1+ max(rmpart$Priority[!is.na(rmpart$Priority)])
attach(rmpart)
rmpart$Qnty <- goodsPerRawMet(RM.Length,RM.Breadth,sfL = SFG.Length,sfW = SFG.Breadth)
rmpart$SFG_ <- rmpart$SFG
sfg <- cast(rmpart,SFG ~ SFG_+RM+RM.Length+RM.Breadth,
            value = "Qnty",
            fun.aggregate = max,
            fill=0)