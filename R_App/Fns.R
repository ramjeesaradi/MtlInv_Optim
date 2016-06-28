require(dummies)

prepSol <- function(sol,rmpart,params,requirements,StkInp){
  solution <- data.frame(Name = params$pairnms, qnty = sol$solution )
  if(length(params$pairnms)==1){
  solution <- cbind(solution, Splitclmn(solution$Name,"_"),as.vector((params$wstprct)), as.vector((params$Cost1)), rep(params$message,nrow(solution)))
  }else{solution <- cbind(solution, Splitclmn(solution$Name,"_"),as.vector(t(params$wstprct)), as.vector(t(params$Cost1)),rep(params$message,nrow(solution)))}
  solution <- solution[solution$qnty !=0,]
  names(solution) <- c(names(solution)[1:2],"SFG", "Sheet","Coil","Length", "Breadth","Batch", "Stock", "wastage", "Cost", "Remarks")
  solution.out <- cast(solution, SFG+ Sheet+Coil+Length+ Breadth+ Batch + wastage + Cost + Remarks ~ Stock, value = "qnty",fun.aggregate = sum,fill = 0)
  
  solution.out <- renameCol(solution.out,"inStock", "fromStock")
  solution.out <- renameCol(solution.out,"noStock", "Purchase")
  # solution.out <- solution.out[,c("SFG","Coil","Sheet","Length", "RM.Breadth","wastage", "Batch", "fromStock", "Purchase","RM.Lead.Time")]
  
  return(solution.out)
}
write2Disk <- function(tabl,params,run){
  tabl <- data.frame(lapply(tabl, as.character), stringsAsFactors=FALSE)
  if(! ("Purchase" %in% names(tabl))){
    tabl$Purchase <- 0
  }
  if(!("fromStock" %in% names(tabl))){tabl$fromStock <- 0}
  tabl <- tabl[,c("Order.No","Item","SFG","Coil","Sheet","Length","Breadth","Batch","wastage","fromStock","Purchase", "Cost","Remarks")]
  output0 <- merge(requirements[(requirements$RM.Length==0) & (requirements$RM.Breadth==0),!(names(requirements) %in% c("RM.Length","RM.Breadth"))], tabl
                  ,by.x = c("Order.No","Item","SFG.Material","Raw.Material")
                  ,by.y = c("Order.No","Item","SFG","Sheet")
                  ,all.x = T)
  output0 <- renameCol(output0,"Length","RM.Length" )
  output0 <- renameCol(output0,"Breadth","RM.Breadth" )
  
  #fix for duplication of records produced by left outer join in output0
  output0$RM.Breadth[is.na(output0$RM.Breadth)] <- 0
  output0$RM.Length[is.na(output0$RM.Length)] <- 0
  
  output1 <- merge(requirements[!is.na(requirements$RM.Breadth),], tabl
                   ,by.x = c("Order.No","Item","SFG.Material","Raw.Material","RM.Length","RM.Breadth")
                   ,by.y = c("Order.No","Item","SFG","Sheet","Length","Breadth")
                   ,all.x = T)
  
  output <- rbind.fill(output0,output1)
  StkInp <- renameCol(StkInp,oldname = "Plant", "Stock.Plant")
  output <- merge( output, unique(StkInp[,c("Batch","Storage.Location")])
                   #todo add ,"Location"
                  ,by = c("Batch")
                  # ,by.y = c("Batch")
                  ,all.x = T)
  output <- cbind(output,Splitclmn(output$Batch,"-",outcols = 5))
  output <- rmclmn(output,"Batch")
  output <- renameCol(output,"1","Batch")
  output <- renameCol(output,"2","Plant.Considered")
  output <- renameCol(output,"3","Storage.Location")
  output <- renameCol(output,"4","PO.Number")
  output <- renameCol(output,"5","PR.Number")
  output$fromPR <- sapply(1:nrow(output)
                          , function (x) ifelse(output$PR.Number[x] != " ",output$fromStock[x],0))
  output$fromPO <- sapply(1:nrow(output)
                          , function (x) ifelse(output$PO.Number[x] != " ",output$fromStock[x],0))
  
  output <- output[,c("Order.No","Item","Plant","Order.Req","Forecast.Date","Req.Delivery.Date","Mfg.Lead.time","KIT.Material","FG.Material","FG.Net.Req","SFG.Material","SFG.Net.Req","FG.SFG.Length","FG.SFG.Breadth","Coil","Raw.Material","RM.Length","RM.Breadth","RM.Qty.Set","Priority","Plant.Considered","Batch","Storage.Location","fromStock","Purchase","wastage","Remarks", "PO.Number", "PR.Number", "fromPO","fromPR")]
  output <- output[order(output$Order.No,output$Forecast.Date),]
  output[(output$FG.Material==output$SFG.Material),c("SFG.Material","SFG.Net.Req")] <- NA
  output <- unique(output)
  ## to add when time sstamp is needed ,"_", format(Sys.time(), format = "%Y%m%d_%H%M%S")
  filename <- paste(c("R_Output/R_Sol_",run,".csv"),collapse = "")
  filename1 <- paste(c("R_Output/R_Sol_",unique(requirements$Plant)[1],".csv"),collapse = "")
  write.table(output,filename,row.names = F,quote = F,na = "",sep = "|")
  write.table(output,filename1,row.names = F,quote = F,na = "",sep = "|")
  #TO DO paste(c("Solution_",params$wastage.threshold,".csv"),collapse = "")
}

updateStk <- function(sol,StkInp){
  if(nrow(StkInp) >= 1 & "fromStock" %in% names(sol)){
    sol <- sol[!is.na(sol$Batch)]
   usedStk <- aggregate(fromStock ~ Batch,sol, sum,na.action = na.omit)
  # View(usedStk)
  for(i in usedStk$Batch){
    # print(i)
    # print(usedStk$fromStock[usedStk$Batch == i])
    StkInp$Warehouse.Stock[StkInp$Batch == i]<- round(StkInp$Warehouse.Stock[StkInp$Batch == i] - usedStk$fromStock[usedStk$Batch == i],6)
    # StkInp$Warehouse.Stock[StkInp$Batch == i]<- StkInp$Warehouse.Stock[StkInp$Batch == i] - usedStk$fromStock[usedStk$Batch == i]
    # StkInp$Warehouse.Stock[StkInp$Warehouse.Stock < 1e-4] <- 0
  }
  return(StkInp)} else(return(StkInp))
}

consolStk <- function(StkInp){
  StkInpPR <- rmclmn(StkInp,c("PO.Number", "Open.PO.s"))
  StkInpPO <- rmclmn(StkInp,c("PR.Number", "Open.PR.s"))
  StkInp <- unique(rbind.fill(StkInpPO,StkInpPR))
  StkInp <- StkInp[!((is.na(StkInp$Batch) | StkInp$Batch == "" ) & is.na(StkInp$PO.Number) & is.na(StkInp$PR.Number)),]
  
  StkInp$Warehouse.Stock[is.na(StkInp$Warehouse.Stock)] <- 0
  StkInp$Open.PR.s[is.na(StkInp$Open.PR.s)] <- 0
  StkInp$Open.PO.s[is.na(StkInp$Open.PO.s)] <- 0
  # StkInp$Subcon.Stock[is.na(StkInp$Subcon.Stock)] <- 0
  StkInp$Warehouse.Stock <- StkInp$Warehouse.Stock + StkInp$Open.PO.s + StkInp$Open.PR.s
  
  # StkInp$Warehouse.Stock <- StkInp$Warehouse.Stock + StkInp$Subcon.Stock
  # StkInp$Subcon.Stock <- 0
  
  StkInp$PO.Number[is.na(StkInp$PO.Number)] <- " "
  StkInp$PR.Number[is.na(StkInp$PR.Number)] <- " "
  
  StkInp$Batch <- do.call(paste,c(StkInp[,c("Batch","Plant","Storage.Location", "PO.Number", "PR.Number")],sep = "-"))
  return(StkInp)
}

renameCol <- function(df,oldname, newname){
  names(df)[names(df) %in% oldname] <- newname
  # names(df) <- newnames
  return(df)
}

goodsPerRawMet <- function(df){
  rmL <- df$RM.Length
  rmW <- df$RM.Breadth
  sfL <- df$SFG.Length
  sfW <- df$SFG.Breadth
  rmT <- df$RM.Width
  rmD <- df$RM.Density
  rmWst <- df$conWst
  
  rmWt <- rmL*rmW*rmT*rmD*(1+(rmWst/100))
  
  nll <- floor(rmL/sfL)*floor(rmW/sfW)
  nlw <- floor(rmW/sfL)*floor(rmL/sfW)
  
  Qnty <- df$Qnty
  priority <- df$Priority
  Qnty <- sapply(c(1:length(Qnty))
               , function (x){
                 ifelse(priority[x]==0,
                    (max(c(nll[x],nlw[x]))*1000000/rmWt[x])
                 ,Qnty[x])})
  
  return(Qnty)
}

sfgwt <-  function(df){
  sfL <- df$SFG.Length
  sfW <- df$SFG.Breadth
  rmT <- df$RM.Width
  rmD <- df$RM.Density
  
  wt <- sfL*sfW*rmT*rmD
  return(wt)
}
wastage <- function(df){
  rmL <- df$RM.Length
  rmW <- df$RM.Breadth
  sfL <- df$SFG.Length
  sfW <- df$SFG.Breadth
  #Sheet.Length to Sheet.Length
  nll <- floor(rmL/sfL)
  nww <- floor(rmW/sfW)
  #Sheet.Length to Coil.Width
  nlw <- floor(rmL/sfW)
  nwl <- floor(rmW/sfL)
  
  ll <- nll*sfL*nww*sfW
  lw <- nlw*sfL*nwl*sfW
  usedArea <-  sapply(1:nrow(df), function (x) max(ll[x],lw[x]))
  totalArea <- rmL*rmW
  wastage <- 1-(usedArea/totalArea)
  return(wastage*100)
}
# wastage(rmpart)

toBool <- function(df,skips = NULL){
  if(is.null(skips)){
    dfout <- data.frame(id = 1:nrow(df))
  }else {
    dfout <- data.frame(df[,skips])
    names(dfout) <- skips}
  
  for(name in names(df)[!(names(df) %in% skips)]){
    if (is.factor(df[, name])){
      dfout <- data.frame(dfout,dummy(name,df))
    }else {
      dfout[,name] <- df[, name]
    }
  }
  return(dfout)
}

rmclmn<- function(df, clmns){
  return(df[,!(names(df) %in% clmns)])
}

Splitclmn <- function(col,Split,outcols = 2){
  out.df <- t(as.data.frame(sapply(col, function (x) ifelse(x=="",list(rep("",outcols)),strsplit(as.character(x), Split)))))
  row.names(out.df) <- NULL
  return(out.df)
}

reqOff <- function(sol,params){
  req <- t(params$sfg0[,ncol(sfg0)])
  produce <- sol %*% t(params$sfg0[,-ncol(sfg0)])
  satis <- list(prd = produce,req = req, dif = req-produce)
  return(satis)
}

getconWst <- function(df){
  cW <- df$Coil.Width
  sL <- df$Sheet.Length
  sW <- df$Sheet.Breadth
  dW <- sapply(c(1:length(cW)), function (x) ifelse(cW[x]/sW[x] >= 1,(cW[x] - floor(cW[x]/sW[x])*sW[x]),cW[x]))
  dL <- sapply(c(1:length(cW)), function (x) ifelse(cW[x]/sL[x] >= 1,(cW[x] - floor(cW[x]/sL[x])*sL[x]),cW[x]))
  wst <- sapply(c(1:length(cW)), function (x) 100*min(dL[x], dW[x])/cW[x])
  return(wst)
}

diffPlant <- function(plant,batch){
  stkPlant <- as.numeric(Splitclmn(batch,"-",5)[,2])
  stkPlant[is.na(stkPlant)]<-0
  bool <- (stkPlant !=plant & stkPlant !=0)
  # & stkPlant !=""
  bool[bool == T] <- 1
  return(bool)
}
populateCoilwidth <- function(requirements,conv){
  reqCoil <- merge(requirements,unique(conv[,c("From.Material.Number","Coil.Width")]),by.x = "Raw.Material",by.y = "From.Material.Number",all.x = T)
  reqCoil[!is.na(reqCoil$Coil.Width), "RM.Breadth"] <- reqCoil[!is.na(reqCoil$Coil.Width), "Coil.Width"]
  return(reqCoil[,!(names(reqCoil) %in% "Coil.Width")])
}

getFeasibleOpts <- function(rmpart,wastage.threshold){
  rmpart2 <- rmpart[rmpart$totalLead <= rmpart$duein, ]
  if(!(nrow(rmpart2) >=1)){
    rmpart2 <- rmpart
    params$message <<- "Lead over due Date"
  }
  rmpart1 <- rmpart2[rmpart2$wastage <= wastage.threshold | rmpart2$Priority > 0]
  if(!(nrow(rmpart1) >=1)){
    rmpart1 <- rmpart2
    params$message <<- "No Feasible Wastage"
  }
  return(rmpart1)
}