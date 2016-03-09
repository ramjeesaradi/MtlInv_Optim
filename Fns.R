require(dummies)

goodsPerRawMet <- function(df){
  rmL <- df$RM.Length
  rmW <- df$RM.Breadth
  sfL <- df$SFG.Length
  sfW <- df$RM.Breadth
  rmT <- df$RM.Thickness
  rmD <- df$RM.Density
  
  nl <- floor(rmL/sfL)
  nw <- floor(rmW/sfW)
  gds <- nl*nw
  wt <- rmL*rmW*rmT*rmD
  return(gds/wt)
}

wastage <- function(df){
  rmL <- df$RM.Length
  rmW <- df$RM.Breadth
  sfL <- df$SFG.Length
  sfW <- df$RM.Breadth
  nl <- floor(rmL/sfL)
  nw <- floor(rmW/sfW)
  usedArea <-  nl*sfL*nw*sfW
  totalArea <- rmL*rmW
  wastage <- 1-(usedArea/totalArea)
  return(wastage*100)
}

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

reqOff <- function(sol,params){
  req <- t(params$sfg0[,ncol(sfg0)])
  produce <- sol %*% t(params$sfg0[,-ncol(sfg0)])
  satis <- list(prd = produce,req = req, dif = req-produce)
  return(satis)
}