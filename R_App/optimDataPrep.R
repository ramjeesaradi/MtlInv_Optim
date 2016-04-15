

getrmpart <- function(requirements,StkInp,conv) {
  requirements$sfg <- requirements$SFG.Material.Number
  requirements$sfg <- sapply(requirements$sfg, function (x) gsub("\t", "", x))
  requirements <- requirements[!is.na(requirements$Plant),]
  foreCastdt <-  as.Date(requirements$Req.Delivery.Date,"%d-%m-%Y")
  
  
  requirements$month <- difftime(as.Date(requirements$Req.Delivery.Date,"%d-%m-%Y"),Sys.Date())
  
  requirementsCoil <- merge(requirements[!is.na(requirements$RM.Length) & !is.na(requirements$RM.Breadth),],requirements[is.na(requirements$RM.Length) & !is.na(requirements$RM.Breadth),c("RM.Number","SFG.Material.Number")]
                            ,by = "SFG.Material.Number"
                            )
  requirementsCoil <- renameCol(requirementsCoil,"RM.Number.x","RM.Number")
  requirementsCoil <- renameCol(requirementsCoil,"RM.Number.y","con")
  
  requirements <- rbind.fill(requirements,requirementsCoil)
  if(nrow(requirements[is.na(requirements$con), ])>=1){
  requirements[is.na(requirements$con), "con"] <- ""}
  
  # requirements$nsfg <- goodsPerRawMet(rmL = RM.Length,rmW = RM.Breadth, sfL = FG.SFG.Material.Length,sfW = Breadth)
  
  sfgreq <- aggregate(SFG.Net.Req~sfg+month,requirements, max)
  #######
  StkInp$Type <- sapply(StkInp$Type, function (x) ifelse(x=="END BAND", 1,0))
  ######################################################input 2############################
  
  
  StkInp$RM.Stock.Length[is.na(StkInp$RM.Stock.Length)] <- 0
  StkInp$RM.Stock.Breadth[is.na(StkInp$RM.Stock.Breadth)] <- 0
  StkInp$Warehouse.Stock[is.na(StkInp$Warehouse.Stock)] <- 0
  ############################## Assign Priority & 
  
  requirements$BOM.Priority.List[is.na(requirements$RM.Length) & is.na(requirements$RM.Breadth)]<- 0
  requirements$BOM.Priority.List[is.na(requirements$BOM.Priority.List)]<- 1
  requirements$RM.Length[is.na(requirements$RM.Length)] <- 0
  requirements$RM.Breadth[is.na(requirements$RM.Breadth)] <- 0
  
  ###################################################### Conv ############################
  #######################Table of Purchasable and convertable RawMaterial###########################################
  rm1 <- requirements[,c("RM.Number","RM.Breadth","RM.Length","RM.Thickness","RM.Density","Moving.Average.Price","Lead.Time")]
  # rm2 <- StkInp[StkInp$Type ==1 ,c("RM.Number","RM.Stock.Breadth","RM.Stock.Length","RM.Stock.Thickness","RM.Stock.Density")]
  rm2 <- StkInp[,c("RM.Number","RM.Stock.Breadth","RM.Stock.Length","RM.Stock.Thickness","RM.Stock.Density","Moving.Average.Price","Lead.Time")]
  rm3 <- conv[,c("To.Material.Number","Sheet.Breadth","Sheet.Length","Sheet.Thickness","Density","From.Material.Number","Conversion.Cost","Wastage" )]
  names(rm1) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density", "SheetCost","Lead.Time")
  
  names(rm2) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density", "SheetCost","Lead.Time")
  ####Remove coil from the sheet stocks
  # rmS <- rm1[(rm1$RM.Length != 0 | rm1$RM.Breadth != 0),]
  rmS <- rm1[(rm1$RM.Breadth != 0),]
  rmS2 <- rm2[(rm2$RM.Length != 0 | rm2$RM.Breadth != 0),]
  rmC0 <- merge(rm3,unique(rbind(rm1[(rm1$RM.Length == 0),c("RM","SheetCost","Lead.Time")],
                                rm2[(rm2$RM.Length == 0),c("RM","SheetCost","Lead.Time")]))
               ,by.x = "From.Material.Number", by.y = "RM"
               )
  rmC1 <- merge(rm3,unique(rm1[(rm1$RM.Length == 0),c("RM","SheetCost","Lead.Time")])
                ,by.x = "From.Material.Number", by.y = "RM"
  )
  names(rmC0) <- c( "con","RM", "RM.Breadth","RM.Length","RM.Width","RM.Density","conCost","conWst","CoilCost","Lead.Time")
  names(rmC1) <- c( "con","RM", "RM.Breadth","RM.Length","RM.Width","RM.Density","conCost","conWst","CoilCost","Lead.Time")
 
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
  #                 by.x = c("RM","RM.Stock.Breadth","Length"), 
  #                 by.y =c("RM.Number","RM.Breadth","RM.Length"),
  #                 all.x = T)
  
  rmpart0 <- unique(merge(rm0,
                  unique(requirements[requirements$BOM.Priority.List ==0,c("RM.Number","SFG.Material.Number", "FG.SFG.Material.Length","FG.SFG.Material.Breadth","Mfg.Lead.time","RM.required.quantity")]),
                  by.x = c("RM"), 
                  by.y =c("RM.Number")
                  ,all.y = T))
  names(rmpart0) <- c(names(rm0),"SFG","SFG.Length","SFG.Breadth","Manf.Lead","Qnty")
  if(nrow(rmpart0) >= 1){
  rmpart0$Qnty <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),rmpart0$Qnty[x],NA))
  rmpart0$con <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),"",rmpart0$con[x]))
  rmpart0$RM.Length <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),rmpart0$SFG.Length[x],rmpart0$RM.Length[x]))
  rmpart0$RM.Breadth <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Breadth[x]),rmpart0$SFG.Breadth[x],rmpart0$RM.Breadth[x]))
  }
  rmpart1 <- unique(merge(rm1,
                          unique(requirements[requirements$BOM.Priority.List >=1,c("RM.Number","RM.Breadth","RM.Length","SFG.Material.Number", "FG.SFG.Material.Length","FG.SFG.Material.Breadth","Mfg.Lead.time","BOM.Priority.List","RM.required.quantity","con")]),
                          by.x = c("RM","RM.Breadth","RM.Length","con"),
                          by.y =c("RM.Number","RM.Breadth","RM.Length","con")
                          ))
  # names(rmpart1) <- c(names(rm1),"SFG","SFG.Length","SFG.Breadth","Manf.Lead","Priority", "Qnty")
  rmpart1 <- renameCol(rmpart1,"SFG.Material.Number","SFG")
  rmpart1 <- renameCol(rmpart1,"FG.SFG.Material.Length","SFG.Length")
  rmpart1 <- renameCol(rmpart1,"FG.SFG.Material.Breadth","SFG.Breadth")
  rmpart1 <- renameCol(rmpart1,"Mfg.Lead.time","Manf.Lead")
  rmpart1 <- renameCol(rmpart1,"BOM.Priority.List","Priority")
  rmpart1 <- renameCol(rmpart1,"RM.required.quantity","Qnty")
  
  
  rmpart <- rbind.fill(rmpart0,rmpart1)
  rmpart$Qnty <- 1/rmpart$Qnty
  # nms <- names(rmpart) <- c(names(rm),"SFG","SFG.Length","SFG.Breadth","Manf.Lead")
  
  #################################################################################################################
  #Populating sheet And Coil Stocks and Batch
  rmpartSheet <- merge(rmpart[rmpart$con=="",],
                  StkInp[,!(names(StkInp) %in% "Lead.Time")],
                  by.x = c("RM","RM.Breadth","RM.Length"),
                  by.y = c("RM.Number","RM.Stock.Breadth","RM.Stock.Length")
                  ,all.x = T)[,c(names(rmpart),"Warehouse.Stock","Batch","Type")]
  rmpartSheet <- renameCol(rmpartSheet,"Warehouse.Stock", "SheetStock")
  rmpartSheet$SheetCost[rmpartSheet$Type == 1 ] <- 0
  rmpartSheet$Lead.Time[rmpartSheet$Type == 1 ] <- 0
  # rmpartSheet <- renameCol(rmpartSheet, "Moving.Average.Price", "SheetCost")
  rmpartCoil <- merge(rmpart[rmpart$con!="",],
                  StkInp[StkInp$RM.Stock.Length==0,!(names(StkInp) %in% "Lead.Time")],
                  by.x = c("con")
                  ,by.y = c("RM.Number")
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
    rmpartShinStock$Lead.Time <- 0
    }
  
  if(nrow(rmpartCoinStock) != 0){
    rmpartCoinStock$inStock <- "inStock"
    rmpartCoinStock$CoilCost <- 0
    rmpartCoinStock$SheetCost <- 0
    rmpartCoinStock$Lead.Time <- 0
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
                  all.x = T)[,c(names(rmpart),"SFG.Net.Req","month")]
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
  rmpart$totalCost <- sapply(c(1:nrow(rmpart)),
                        function (x) ifelse(rmpart[x,"Priority"]>=1,rmpart[x,"Priority"],rmpart[x,"totalCost"]))
  
  rmpart$cost1 <- rmpart$conCost + rmpart$SheetCost + rmpart$CoilCost
  ###########################Generate Total Lead for each SFG #########################################
  rmpart$totalLead <- rmpart$Manf.Lead + rmpart$Lead.Time
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
