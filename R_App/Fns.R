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
  output0 <- merge(requirements[is.na(requirements$RM.Length) & is.na(requirements$RM.Breadth),!(names(requirements) %in% c("RM.Length","RM.Breadth"))], tabl
                  ,by.x = c("Order.No","Item","SFG.Material","Raw.Material")
                  ,by.y = c("Order.No","Item","SFG","Sheet")
                  ,all.x = T)
  output0 <- renameCol(output0,"Length","RM.Length" )
  output0 <- renameCol(output0,"Breadth","RM.Breadth" )
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
  
  filename <- paste(c("Solution_W",params$wastage.threshold,"_", format(Sys.time(), format = "%Y%m%d_%H%M"),".csv"),collapse = "")
  write.csv(output,filename,row.names = F,quote = T,na = "")
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
    StkInp$Warehouse.Stock[StkInp$Batch == i]<- round(StkInp$Warehouse.Stock[StkInp$Batch == i] - usedStk$fromStock[usedStk$Batch == i])
  }
  return(StkInp)} else(return(StkInp))
}

consolStk <- function(StkInp){
  StkInp$Warehouse.Stock[is.na(StkInp$Warehouse.Stock)] <- 0
  StkInp$Subcon.Stock[is.na(StkInp$Subcon.Stock)] <- 0
  StkInp$Warehouse.Stock <- StkInp$Warehouse.Stock + StkInp$Subcon.Stock
  StkInp$Subcon.Stock <- 0
  StkInp$Batch <- do.call(paste,c(StkInp[,c("Batch","Plant")],sep = "-"))
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
  Qnty <- df$Qnty
  
  for(i in 1:nrow(df)){if(is.na(Qnty[i])){
    nl <- floor(rmL[i]/sfL[i])
    nw <- floor((rmW[i])/sfW[i])
    gds <- nl*nw
    wt <- rmL[i]*rmW[i]*rmT[i]*rmD[i]*(1+rmWst[i]/100)
    Qnty2 <- gds/wt
    Qnty[i] <- Qnty2
  }}
  
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

Splitclmn <- function(col,Split){
  out.df <- t(as.data.frame(sapply(col, function (x) strsplit(as.character(x), "_"))))
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