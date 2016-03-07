require(dummies)

goodsPerRawMet <- function(rmL,rmW,rmT = 1.3,rmD = 0.0056, sfL,sfW){
  nl <- floor(rmL/sfL)
  nw <- floor(rmW/sfW)
  gds <- nl*nw
  wt <- rmL*rmW*rmT*rmD
  return(gds/wt)
}

wastage <- function(rmL,rmW,sfL,sfW){
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
