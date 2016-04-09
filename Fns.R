require(dummies)

prepSol <- function(sol,rmpart,params,requirements,StkInp){
  solution <- data.frame(Name = params$pairnms, qnty = sol$solution )
  if(length(params$pairnms)==1){
  solution <- cbind(solution, Splitclmn(solution$Name,"_"),as.vector((params$wstprct)), as.vector((params$Cost1)), rep(params$message,nrow(solution)))
  }else{solution <- cbind(solution, Splitclmn(solution$Name,"_"),as.vector(t(params$wstprct)), as.vector(t(params$Cost1)),rep(params$message,nrow(solution)))}
  solution <- solution[solution$qnty !=0,]
  names(solution) <- c(names(solution)[1:2],"SFG", "Sheet","Coil","Length", "Breadth","Batch", "Stock", "wastage", "Cost", "Remarks")
  solution.out <- cast(solution, SFG+ Sheet+Coil+Length+ Breadth+ Batch + wastage + Cost + Remarks ~ Stock, value = "qnty",fun.aggregate = sum,fill = 0)
  solution.out <- merge(solution.out,unique(rmpart[rmpart$inStock == "noStock",c("RM", "RM.Length", "RM.Breadth","Lead.Time")]),
                        by.x = c("Sheet", "Length", "Breadth"),
                        by.y = c("RM", "RM.Length", "RM.Breadth"),
                        all.x = T)
  solution.out <- renameCol(solution.out,"inStock", "fromStock")
  solution.out <- renameCol(solution.out,"noStock", "Purchase")
  # solution.out <- solution.out[,c("SFG","Coil","Sheet","Length", "Breadth","wastage", "Batch", "fromStock", "Purchase","Lead.Time")]
  
  return(solution.out)
}
write2Disk <- function(tabl,params,run){
  tabl <- data.frame(lapply(tabl, as.character), stringsAsFactors=FALSE)
  if(! ("Purchase" %in% names(tabl))){
    tabl$Purchase <- 0
  }
  if(!("fromStock" %in% names(tabl))){tabl$fromStock <- 0}
  tabl <- tabl[,c("Order.no","SFG","Coil","Sheet","Length","Breadth","Batch","wastage","fromStock","Purchase","Lead.Time", "Cost","Remarks")]
  output <- merge(requirements, tabl
                  ,by.x = c("Order.no","SFG.Material.Number","RM.Number","RM.Length","RM.Breadth")
                  ,by.y = c("Order.no","SFG","Sheet","Length","Breadth")
                  ,all.x = T)
  output <- merge( tabl, unique(StkInp[,"Batch","Plant","Location"])
                  ,by.x = c("Batch")
                  ,by.y = c("Batch")
                  ,all.x = T)
  write.csv(tabl,"output.csv",row.names = F)
  #TO DO paste(c("Solution_",params$month,"_" ,params$wastage.threshold,"_", run,".csv"),collapse = "")
}

updateStk <- function(sol,StkInp){
  if(nrow(StkInp) >= 1 & "fromStock" %in% names(sol)){
    sol <- sol[!is.na(sol$Batch)]
   usedStk <- aggregate(fromStock ~ Batch,sol, sum,na.action = na.omit)
  # View(usedStk)
  for(i in usedStk$Batch){
    # print(i)
    # print(usedStk$fromStock[usedStk$Batch == i])
    StkInp$Ware.house.Stock[StkInp$Batch == i]<- round(StkInp$Ware.house.Stock[StkInp$Batch == i] - usedStk$fromStock[usedStk$Batch == i])
  }
  return(StkInp)} else(return(StkInp))
}

consolStk <- function(StkInp){
  StkInp$Ware.house.Stock[is.na(StkInp$Ware.house.Stock)] <- 0
  StkInp$Sub.contractor.Stock[is.na(StkInp$Sub.contractor.Stock)] <- 0
  StkInp$Ware.house.Stock <- StkInp$Ware.house.Stock + StkInp$Sub.contractor.Stock
  StkInp$Sub.contractor.Stock <- 0
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
  #L to L
  nll <- floor(rmL/sfL)
  nww <- floor(rmW/sfW)
  #L to W
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
  cW <- df$W
  sL <- df$L
  sW <- df$B
  dW <- sapply(c(1:length(cW)), function (x) ifelse(cW[x]/sW[x] >= 1,(cW[x] - floor(cW[x]/sW[x])*sW[x]),cW[x]))
  dL <- sapply(c(1:length(cW)), function (x) ifelse(cW[x]/sL[x] >= 1,(cW[x] - floor(cW[x]/sL[x])*sL[x]),cW[x]))
  wst <- sapply(c(1:length(cW)), function (x) 100*min(dL[x], dW[x])/cW[x])
  return(wst)
}