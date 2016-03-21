

getrmpart <- function(requirements,StkInpk,conv) {
  requirements$sfg <- requirements$Semi.Finished.Material.Number
  requirements$sfg <- sapply(requirements$sfg, function (x) gsub("\t", "", x))
  requirements <- requirements[!is.na(requirements$Plant),]
  foreCastdt <-  as.Date(requirements$Forecast.Month,"%d-%m-%Y")
  
  requirements$CIR.Sales.order.No..Rejection.Production.order.requirement <- requirements$CIR.Sales.order.No..Rejection.Production.order.requirement*requirements$Qty..Set
  
  requirements$month <- round(difftime(foreCastdt,min(foreCastdt),units = "days")/30)
  
  # requirements$nsfg <- goodsPerRawMet(rmL = Raw.Material.Sizes..Length.,rmW = Raw.Material.Sizes..Breadth., sfL = Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.,sfW = Breadth)
  
  sfgreq <- aggregate(CIR.Sales.order.No..Rejection.Production.order.requirement~sfg+month,requirements, max)
  #######
 
  ######################################################input 2############################
  
  
  StkInp$RM.Stock.with.Sizes..Length...Breadth.[is.na(StkInp$RM.Stock.with.Sizes..Length...Breadth.)] <- 0
  StkInp$Breadth[is.na(StkInp$Breadth)] <- 0
  StkInp$Ware.house.Stock[is.na(StkInp$Ware.house.Stock)] <- 0
  ############################## Assign Priority & 
  
  requirements$Priority.List.Mentioned.in.BOM[is.na(requirements$Raw.Material.Sizes..Length.) & is.na(requirements$Raw.Material.Sizes..Breadth.)]<- 0
  requirements$Priority.List.Mentioned.in.BOM[is.na(requirements$Priority.List.Mentioned.in.BOM)]<- 1
  requirements$Raw.Material.Sizes..Length.[is.na(requirements$Raw.Material.Sizes..Length.)] <- 0
  requirements$Raw.Material.Sizes..Breadth.[is.na(requirements$Raw.Material.Sizes..Breadth.)] <- 0
  
  ###################################################### Conv ############################
  #######################Table of Purchasable and convertable RawMaterial###########################################
  rm1 <- requirements[,c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.","Thickness","Density","Moving.Average.Price","Lead.Times")]
  # rm2 <- StkInp[StkInp$End.Band ==1 ,c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.","Thickness","Density")]
  rm2 <- StkInp[,c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.","Thickness","Density","Moving.Average.Price","Lead.Times")]
  rm3 <- conv[,c("To.Material.Number","B","L","W.1","D","From.Material.Number","Conversion.Cost","Wastage" )]
  names(rm1) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density", "SheetCost","Lead.Times")
  
  names(rm2) <- c("RM","RM.Breadth","RM.Length","RM.Width","RM.Density", "SheetCost","Lead.Times")
  ####Remove coil from the sheet stocks
  # rmS <- rm1[(rm1$RM.Length != 0 | rm1$RM.Breadth != 0),]
  rmS <- rm1[(rm1$RM.Breadth != 0),]
  rmS2 <- rm2[(rm2$RM.Length != 0 | rm2$RM.Breadth != 0),]
  rmC0 <- merge(rm3,unique(rbind(rm1[(rm1$RM.Length == 0),c("RM","SheetCost","Lead.Times")],
                                rm2[(rm2$RM.Length == 0),c("RM","SheetCost","Lead.Times")]))
               ,by.x = "From.Material.Number", by.y = "RM"
               )
  rmC1 <- merge(rm3,unique(rm1[(rm1$RM.Length == 0),c("RM","SheetCost","Lead.Times")])
                ,by.x = "From.Material.Number", by.y = "RM"
  )
  names(rmC0) <- c( "con","RM", "RM.Breadth","RM.Length","RM.Width","RM.Density","conCost","conWst","CoilCost","Lead.Times")
  names(rmC1) <- c( "con","RM", "RM.Breadth","RM.Length","RM.Width","RM.Density","conCost","conWst","CoilCost","Lead.Times")
 
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
  rm <- unique(rm0)
  rm <- rm[rm$RM.Length > 0,]
  #with coil only specified in req
  rm1 <- rbind.fill(rmS,rmC1,rmS2)
  rm1 <- unique(rm1)
  rm1 <- rm1[rm1$RM.Length > 0,]
  #Remove raw meterials with no Length or Breadth
  # rm <- rm[which(!(is.na(rm$RM.Breadth) | is.na(rm$RM.Length))),]
  
  ####################################SFG(parts) with their compatible RM########################################
  # rmpart <- merge(rm,requirements,
  #                 by.x = c("RM","Breadth","Length"), 
  #                 by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length."),
  #                 all.x = T)
  
  rmpart0 <- unique(merge(rm0,
                  unique(requirements[requirements$Priority.List.Mentioned.in.BOM ==0,c("Raw.Material..Number","Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth","Manufacturing.Lead.time..Repleshinshment.Lead.time.","Raw.Material.required.quantity")]),
                  by.x = c("RM"), 
                  by.y =c("Raw.Material..Number")
                  ,all.y = T))
  names(rmpart0) <- c(names(rm0),"SFG","SFG.Length","SFG.Breadth","Manf.Lead","Qnty")
  if(nrow(rmpart0) >= 1){
  rmpart0$Qnty <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),rmpart0$Qnty[x],NA))
  rmpart0$con <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),"",rmpart0$con[x]))
  rmpart0$RM.Length <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Length[x]),rmpart0$SFG.Length[x],rmpart0$RM.Length[x]))
  rmpart0$RM.Breadth <- sapply(c(1:nrow(rmpart0)), function (x) ifelse(is.na(rmpart0$RM.Breadth[x]),rmpart0$SFG.Breadth[x],rmpart0$RM.Breadth[x]))
  }
  rmpart1 <- unique(merge(rm1,
                          unique(requirements[requirements$Priority.List.Mentioned.in.BOM >=1,c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.","Semi.Finished.Material.Number", "Finished.Semi.FinishedMaterial.Sizes..Length...Breadth.","Breadth","Manufacturing.Lead.time..Repleshinshment.Lead.time.","Priority.List.Mentioned.in.BOM","Raw.Material.required.quantity")]),
                          by.x = c("RM","RM.Breadth","RM.Length"),
                          by.y =c("Raw.Material..Number","Raw.Material.Sizes..Breadth.","Raw.Material.Sizes..Length.")
                          ))
  names(rmpart1) <- c(names(rm1),"SFG","SFG.Length","SFG.Breadth","Manf.Lead","Priority", "Qnty")
  rmpart <- rbind.fill(rmpart0,rmpart1)
  rmpart$Qnty <- 1/rmpart$Qnty
  # nms <- names(rmpart) <- c(names(rm),"SFG","SFG.Length","SFG.Breadth","Manf.Lead")
  
  #################################################################################################################
  #Populating sheet And Coil Stocks and Batch
  rmpartSheet <- merge(rmpart[rmpart$con=="",],
                  StkInp[,!(names(StkInp) %in% "Lead.Times")],
                  by.x = c("RM","RM.Breadth","RM.Length"),
                  by.y = c("Material.Code","Breadth","RM.Stock.with.Sizes..Length...Breadth.")
                  ,all.x = T)[,c(names(rmpart),"Ware.house.Stock","Batch","End.Band")]
  rmpartSheet <- renameCol(rmpartSheet,"Ware.house.Stock", "SheetStock")
  rmpartSheet$SheetCost[rmpartSheet$End.Band == 1 ] <- 0
  rmpartSheet$Lead.Times[rmpartSheet$End.Band == 1 ] <- 0
  # rmpartSheet <- renameCol(rmpartSheet, "Moving.Average.Price", "SheetCost")
  rmpartCoil <- merge(rmpart[rmpart$con!="",],
                  StkInp[StkInp$RM.Stock.with.Sizes..Length...Breadth.==0,!(names(StkInp) %in% "Lead.Times")],
                  by.x = c("con")
                  ,by.y = c("Material.Code")
                  ,all.x = T)[,c(names(rmpart),"Ware.house.Stock","Batch")]
  rmpartCoil <- renameCol(rmpartCoil,"Ware.house.Stock", "CoilStock")
  # rmpartCoil <- renameCol(rmpartCoil, "Moving.Average.Price", "CoilCost")
  rmpart <- rbind.fill(rmpartSheet,rmpartCoil)
  rmpart$CoilStock[is.na(rmpart$CoilStock)] <- 0
  rmpart$SheetStock[is.na(rmpart$SheetStock)] <- 0
  rmpart$End.Band[is.na(rmpart$End.Band)] <- 0
  
  rmpart <- rmpart[!is.na(rmpart$RM),]
  rmpart$WstCost <- rmpart$CoilCost + rmpart$SheetCost +rmpart$conCost
  
  ########################### Separate entries for each RM with Stock and with out stock ###############################

  rmpart[,"inStock"] <- NA
  rmpartShinStock <- rmpart[rmpart$SheetStock > 0 & rmpart$End.Band != 1,]
  rmpartCoinStock <- rmpart[rmpart$CoilStock > 0,]
  #Eliminating noStock entry for EndBands
  rmpart$inStock[rmpart$End.Band != 1] <- "noStock"
  rmpart$inStock[rmpart$End.Band == 1] <- "inStock"
  rmpart$Batch[rmpart$End.Band != 1] <- ""
  rmpart$WstCost[rmpart$End.Band != 1] <- 0
  
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
                  all.x = T)[,c(names(rmpart),"CIR.Sales.order.No..Rejection.Production.order.requirement","month")]
  rmpart <- renameCol(rmpart,"CIR.Sales.order.No..Rejection.Production.order.requirement", "Req")
  
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
  rmpart$PrWs <- sapply(c(1:nrow(rmpart)),
                        function (x) ifelse(rmpart[x,"Priority"]>=1,rmpart[x,"Priority"]/100,rmpart[x,"wastage"]))
  rmpart$totalCost <- rmpart$conCost + rmpart$SheetCost + rmpart$CoilCost + rmpart$WstCost*(rmpart$conWst+rmpart$PrWs)/100
  rmpart$cost1 <- rmpart$conCost + rmpart$SheetCost + rmpart$CoilCost
  ###########################Generate Total Lead for each SFG #########################################
  rmpart$totalLead <- rmpart$Manf.Lead + rmpart$Lead.Times
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
