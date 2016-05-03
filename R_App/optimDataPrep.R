

getrmpart <- function(requirements,StkInp,conv) {
  requirements$sfg <- requirements$SFG.Material
  requirements$sfg <- sapply(requirements$sfg, function (x) gsub("\t", "", x))
  requirements <- requirements[!is.na(requirements$Plant),]
  foreCastdt <-  as.Date(as.factor(requirements$Req.Delivery.Date), "%d.%m.%Y")
  
  
  requirements$duein <- difftime(as.Date(as.factor(requirements$Req.Delivery.Date),"%d.%m.%Y"),Sys.Date())
  
  requirementsCoil <- merge(requirements[!(requirements$RM.Length==0) & !(requirements$RM.Breadth==0),]
                            ,requirements[(requirements$RM.Length==0) & !(requirements$RM.Breadth==0),c("Raw.Material","SFG.Material")]
                            ,by = "SFG.Material"
                            )
  requirementsCoil <- renameCol(requirementsCoil,"Raw.Material.x","Raw.Material")
  requirementsCoil <- renameCol(requirementsCoil,"Raw.Material.y","con")
  
  requirements <- rbind.fill(requirements,requirementsCoil)
  if(nrow(requirements[is.na(requirements$con), ])>=1){
  requirements[is.na(requirements$con), "con"] <- ""}
  
  # requirements$nsfg <- goodsPerRawMet(rmL = RM.Length,rmW = RM.Breadth, sfL = FG.SFG.Length,sfW = Breadth)
  
  sfgreq <- aggregate(SFG.Net.Req~sfg+duein,requirements, max)
  #######
  StkInp <- StkInp[StkInp$Warehouse.Stock > 0, ]
  StkInp$Type <- sapply(StkInp$Type, function (x) ifelse(x=="END BAND", 1,0))
  ######################################################input 2############################
  
  
  StkInp$RM.Length[is.na(StkInp$RM.Length)] <- 0
  StkInp$RM.Breadth[is.na(StkInp$RM.Breadth)] <- 0
  StkInp$Warehouse.Stock[is.na(StkInp$Warehouse.Stock)] <- 0
  ############################## Assign Priority ##############
  
  requirements$Priority[requirements$Priority == 0]<- 1
  
  requirements$Priority[requirements$RM.Breadth == 0]<- 0
  
  ###################################################### Conv ############################
  #######################Table of Purchasable and convertable RawMaterial###########################################
  rm1 <- requirements[,c("Raw.Material","RM.Breadth","RM.Length","RM.Thickness","RM.Density","Moving.Average.Price","RM.Lead.Time")]
  # rm2 <- StkInp[StkInp$Type ==1 ,c("Raw.Material","RM.Breadth","RM.Length","RM.Thickness","RM.Density")]
  rm2 <- StkInp[,c("Raw.Material","RM.Breadth","RM.Length","RM.Thickness","RM.Density","Moving.Average.Price","RM.Lead.Time")]
  rm3 <- conv[,c("To.Material.Number","Sheet.Breadth","Sheet.Length","Sheet.Thickness","Density","From.Material.Number","Conversion.Cost","Wastage" )]
  names(rm1) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density", "SheetCost","RM.Lead.Time")
  
  names(rm2) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density", "SheetCost","RM.Lead.Time")
  ####Remove coil from the sheet stocks
  # rmS <- rm1[(rm1$RM.Length != 0 | rm1$RM.Breadth != 0),]
  rmS <- rm1[(rm1$RM.Breadth != 0),]
  rmS2 <- rm2[(rm2$RM.Length != 0 | rm2$RM.Breadth != 0),]
  rmC0 <- merge(rm3,unique(rbind(rm1[(rm1$RM.Length == 0),c("RM","SheetCost","RM.Lead.Time")],
                                rm2[(rm2$RM.Length == 0),c("RM","SheetCost","RM.Lead.Time")]))
               ,by.x = "From.Material.Number", by.y = "RM"
               )
  rmC1 <- merge(rm3,unique(rm1[(rm1$RM.Length == 0),c("RM","SheetCost","RM.Lead.Time")])
                ,by.x = "From.Material.Number", by.y = "RM"
  )
  names(rmC0) <- c( "con","RM", "RM.Breadth","RM.Length","RM.Width","RM.Density","conCost","conWst","CoilCost","RM.Lead.Time")
  names(rmC1) <- c( "con","RM", "RM.Breadth","RM.Length","RM.Width","RM.Density","conCost","conWst","CoilCost","RM.Lead.Time")
 
   if(nrow(rmS) >= 1){
    rmS[,c("conCost","conWst","CoilCost")] <- 0
    rmS$con <- ""}
  if(nrow(rmS2) >= 1){
    rmS2[,c("conCost","conWst","CoilCost")] <- 0
    rmS2$con <- ""}
  if(nrow(rmC0) >= 1){rmC0[,"SheetCost"] <- 0}
  if(nrow(rmC1) >= 1){rmC1[,"SheetCost"] <- 0}
  
  #with coil only specified in req & stock
  rm0 <- rbind.fill(rmS,rmC0,rmS2)
  rm0 <- unique(rm0)
  rm0 <- rm0[rm0$RM.Length > 0,]
  #with coil only specified in req
  rm1 <- rbind.fill(rmS,rmC1,rmS2)
  rm1 <- unique(rm1)
  rm1 <- rm1[rm1$RM.Length > 0,]
  #Remove raw meterials with no Length or Breadth
  # rm <- rm[which(!(is.na(rm$RM.Breadth) | is.na(rm$RM.Length))),]
  
  ####################################SFG(parts) with their compatible RM########################################
  # rmpart <- merge(rm,requirements,
  #                 by.x = c("RM","RM.Breadth","Length"), 
  #                 by.y =c("Raw.Material","RM.Breadth","RM.Length"),
  #                 all.x = T)
  
  rmpart0 <- unique(merge(rm0,
                  unique(requirements[requirements$Priority ==0,c("Raw.Material","SFG.Material", "FG.SFG.Length","FG.SFG.Breadth","Mfg.Lead.time","RM.Qty.Set","Priority")]),
                  by.x = c("RM"), 
                  by.y =c("Raw.Material")
                  ,all.y = T))
  names(rmpart0) <- c(names(rm0),"SFG","SFG.Length","SFG.Breadth","Manf.Lead","Qnty","Priority")
  if(nrow(rmpart0) >= 1){
  rmpart0$Qnty <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),rmpart0$Qnty[x],NA))
  rmpart0$con <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),"",rmpart0$con[x]))
  rmpart0$RM.Length <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),rmpart0$SFG.Length[x],rmpart0$RM.Length[x]))
  rmpart0$RM.Breadth <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Breadth[x]),rmpart0$SFG.Breadth[x],rmpart0$RM.Breadth[x]))
  }
  rmpart1 <- unique(merge(rm1,
                          unique(requirements[requirements$Priority >=1,c("Raw.Material","RM.Breadth","RM.Length","SFG.Material", "FG.SFG.Length","FG.SFG.Breadth","Mfg.Lead.time","Priority","RM.Qty.Set","con")]),
                          by.x = c("RM","RM.Breadth","RM.Length","con"),
                          by.y =c("Raw.Material","RM.Breadth","RM.Length","con")
                          ))
  # names(rmpart1) <- c(names(rm1),"SFG","SFG.Length","SFG.Breadth","Manf.Lead","Priority", "Qnty")
  rmpart1 <- renameCol(rmpart1,"SFG.Material","SFG")
  rmpart1 <- renameCol(rmpart1,"FG.SFG.Length","SFG.Length")
  rmpart1 <- renameCol(rmpart1,"FG.SFG.Breadth","SFG.Breadth")
  rmpart1 <- renameCol(rmpart1,"Mfg.Lead.time","Manf.Lead")
  rmpart1 <- renameCol(rmpart1,"Priority","Priority")
  rmpart1 <- renameCol(rmpart1,"RM.Qty.Set","Qnty")
  
  
  rmpart <- rbind.fill(rmpart0,rmpart1)
  rmpart$Qnty <- 1/rmpart$Qnty
  # nms <- names(rmpart) <- c(names(rm),"SFG","SFG.Length","SFG.Breadth","Manf.Lead")
  
  #################################################################################################################
  #Populating sheet And Coil Stocks and Batch
  rmpartSheet <- merge(rmpart[rmpart$con=="",],
                  StkInp[,!(names(StkInp) %in% c("RM.Lead.Time","RM.Density"))],
                  by.x = c("RM","RM.Breadth","RM.Length"),
                  by.y = c("Raw.Material","RM.Breadth","RM.Length")
                  ,all.x = T)[,c(names(rmpart),"Warehouse.Stock","Batch","Type")]
  rmpartSheet <- renameCol(rmpartSheet,"Warehouse.Stock", "SheetStock")
  rmpartSheet$SheetCost[rmpartSheet$Type == 1 ] <- 0
  rmpartSheet$RM.Lead.Time[rmpartSheet$Type == 1 ] <- 0
  # rmpartSheet <- renameCol(rmpartSheet, "Moving.Average.Price", "SheetCost")
  rmpartCoil <- merge(rmpart[rmpart$con!="",],
                  StkInp[StkInp$RM.Length==0,!(names(StkInp) %in% c("RM.Lead.Time","RM.Density","RM.Length","RM.Breadth"))],
                  by.x = c("con")
                  ,by.y = c("Raw.Material")
                  ,all.x = T)[,c(names(rmpart),"Warehouse.Stock","Batch")]
  rmpartCoil <- renameCol(rmpartCoil,"Warehouse.Stock", "CoilStock")
  # rmpartCoil <- renameCol(rmpartCoil, "Moving.Average.Price", "CoilCost")
  rmpart <- rbind.fill(rmpartSheet,rmpartCoil)
  rmpart$CoilStock[is.na(rmpart$CoilStock)] <- 0
  rmpart$SheetStock[is.na(rmpart$SheetStock)] <- 0
  rmpart$Type[is.na(rmpart$Type)] <- 0
  
  rmpart <- rmpart[!is.na(rmpart$RM),]
  rmpart$WstCost <- rmpart$CoilCost + rmpart$SheetCost +rmpart$conCost
  
  ########################### Separate entries for each RM with Stock and with out stock ###############################

  rmpart[,"inStock"] <- NA
  rmpartShinStock <- rmpart[rmpart$SheetStock > 0 & rmpart$Type != 1,]
  rmpartCoinStock <- rmpart[rmpart$CoilStock > 0,]
  #Eliminating noStock entry for EndBands
  rmpart$inStock[rmpart$Type != 1] <- "noStock"
  rmpart$inStock[rmpart$Type == 1] <- "inStock"
  rmpart$Batch[rmpart$Type != 1] <- ""
  rmpart$WstCost[rmpart$Type == 1] <- 0
  
  #Change cost of "inStock" rm to 0 except coils to sheet convesion Cost
  if(nrow(rmpartShinStock) != 0){
    rmpartShinStock$inStock <- "inStock"
    rmpartShinStock$SheetCost <- 0
    rmpartShinStock$CoilCost <- 0
    rmpartShinStock$CoilStock <- 0
    rmpartShinStock$RM.Lead.Time <- 0
    }
  
  if(nrow(rmpartCoinStock) != 0){
    rmpartCoinStock$inStock <- "inStock"
    rmpartCoinStock$CoilCost <- 0
    rmpartCoinStock$SheetCost <- 0
    rmpartCoinStock$RM.Lead.Time <- 0
    }
  #
  rmpart$CoilStock <- 0
  rmpart$SheetStock <- 0
  rmpart <- rbind(rmpart,rmpartShinStock,rmpartCoinStock)
  rmpart$inStock[is.na(rmpart$inStock)] <- "noStock"
  
  
  ###########################Generate Costs for each material ##############################################

  rmpart[rmpart$inStock == "inStock","SheetCost"] <- 0
  rmpart[rmpart$inStock == "inStock","CoilCost"] <- 0

  ##Get Raw material Required quantity and Total Order Requirement
  rmpart <- merge(rmpart,
                  sfgreq,
                  by.x = c("SFG"),
                  by.y = c( "sfg"),
                  all.x = T)[,c(names(rmpart),"SFG.Net.Req","duein")]
  rmpart <- renameCol(rmpart,"SFG.Net.Req", "Req")
  
  ##########################################################################################################
  rmpart <- rmpart[!is.na(rmpart$SFG),]
  rmpart$Batch[is.na(rmpart$Batch)] <- ""
  rmpart$Priority[is.na(rmpart$Priority)] <- 0
  #Parts produced per KG Raw material)
  rmpart[,"Qnty"] <- goodsPerRawMet(rmpart)
  #Assign wastage for RM,SFG pairs
  rmpart$wastage <- wastage(rmpart) + rmpart$conWst
  rmpart$tsfgwt <- sfgwt(rmpart)*rmpart$Req
  
  ###########################Generate Total Costs for each material #########################################
  rmpart$totalCost <- rmpart$conCost + rmpart$SheetCost + rmpart$CoilCost + rmpart$WstCost*(rmpart$wastage)/100
  # rmpart$totalCost <- sapply(c(1:nrow(rmpart)),
  #                       function (x) ifelse(rmpart[x,"Priority"]>=1,rmpart[x,"Priority"],rmpart[x,"totalCost"]))
  # 
  rmpart$cost1 <- rmpart$conCost + rmpart$SheetCost + rmpart$CoilCost
  ###########################Generate Total Lead for each SFG #########################################
  rmpart$totalLead <- rmpart$Manf.Lead + rmpart$RM.Lead.Time
  ######################## Apply Filters to Clean master table#########################
  #Create duplicate coulumns for casting/pivoting to matrix
  rmpart[is.na(as.matrix(rmpart))] <- 0
  rmpart$SFG_ <- rmpart$SFG
  rmpart$RM_ <- rmpart$RM
  rmpart$RM.Length_ <- rmpart$RM.Length
  rmpart$RM.Breadth_ <- rmpart$RM.Breadth
  rmpart$con_ <- rmpart$con
  rmpart$Batch_ <- rmpart$Batch
  rmpart <- rmpart[rmpart$Qnty != 0,]
  rmpart <- rmpart[!is.na(rmpart$RM),]
  return(rmpart)
}

##########################################################################################################

# sfg0 <- rbind(sfg0,sfg0wst)

# params$sfg0 <- sfg0
