constrobjfn <- function(sol,params){
  constr <- reqMet(sol,params)
  obj1 <- invUtilSheet(sol,params)
  obj2 <- invUtilCoil(sol,params)
  
  obj <- c(obj1,constr)
  fitness <- sqrt(sum(obj^2))
  # fitness <- constr*fitness
  return(fitness)
}

objfn <- function(sol,params){
  obj1 <- invUtil(sol,params)
  # obj2 <- wastemin(sol,params)
  # obj3 <- prmax(sol,params)
  # obj4 <- expensmin(sol,params)
  obj <- c(obj1)
  fitness <- sqrt(sum(obj^2))
  return(fitness)
}
reqMet <- function(sol,params){
  req <- t(params$sfg0[,ncol(sfg0)])
  produce <- sol %*% t(params$sfg0[,-ncol(sfg0)])
  satis <- sum((produce-req)^2)
  return(satis)
}

invUtilSheet <- function(sol,params){
  cost <- sol %*% t(params$Sheetstk)
  cost <- sum(cost)/length(sol)
  return(cost)
  
}

invUtilCoil <- function(sol,params){
  cost <- sol %*% t(params$Coilstk)
  cost <- sum(cost)/length(sol)
  return(cost)
  
}

wastemin <- function(sol,params){
  cost <- sol %*%  t(params$sfgwst)
  return(sum(cost^2))
}

prmax <- function(sol,params){
  cost <- sol %*%  t(params$sfgpr)
  return(sum(cost^2))
}

getParams <- function(days,month,wastage.threshold,rmpart) {
  params <- list()
  params$month <- month
  params$wastage.threshold <- wastage.threshold
  #Take the req for the month in question
  params$message <- ""
  rmpart1 <- rmpart[rmpart$totalLead <= days2delivery & rmpart$month == month, ]
  if(! nrow(rmpart1 >=1)){
    rmpart1 <- rmpart[rmpart$month == month, ]
    params$message <- "Lead over due Date"
    }
  #Generate Matrices
  #Matrix for Calculating the parts produced
  sfg <- cast(rmpart1,SFG + Req ~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
              value = "Qnty",
              fun.aggregate = max,
              fill=0)
  
  pairnms <- names(sfg)[-c(1,2)]
  params$pairnms <- pairnms
  # pairs <- lapply(strsplit(pairnms, "_"), function (x) x)
  # # ifelse(length(x)==4,c(x,""),x[c(1,2,4,5,3)])
  # 
  #Matrix for Calculating the percentage wastage
  WtForwst <- cast(rmpart1,.~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
                  value = "tsfgwt",
                  fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/(max(x)*nrow(sfg))),
                  fill=0)
  params$WtForwst <- WtForwst[,pairnms]
  
  #Wastage percentage
  wstprct <- cast(rmpart1,SFG ~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
                  value = "wastage",
                  fun.aggregate = max,
                  fill=0)
  params$wstprct <- wstprct[,pairnms]
  #Stock Matrix after combinig all whare houses
  Sheetstk <- cast(rmpart1,RM+RM.Length_+RM.Breadth_+Batch ~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
                   value = "SheetStock",
                   fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/max(x)),
                   fill=0)
  
  params$Sheetstk <- Sheetstk[,c(pairnms)]
  Coilstk <- cast(rmpart1,con+Batch ~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
                  value = "CoilStock",
                  fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/max(x)),
                  fill=0)
  
  params$Coilstk <- Coilstk[pairnms]
  
  Cost <- cast(rmpart1,. ~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
               value = "totalCost",
               fun.aggregate = max,
               fill=0)
  
  params$Cost <- Cost[pairnms]
  
  Cost1 <- cast(rmpart1,. ~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
               value = "cost1",
               fun.aggregate = max,
               fill=0)
  params$Cost1 <- Cost1[pairnms]
  
  #prempting wastage for one to one mapped parts
  wastage.threshold <- ifelse(max(rmpart1$Priority)>0,99.9,wastage.threshold)
  
  lhs <- rbind(sfg[,pairnms],WtForwst[,pairnms], Sheetstk[,pairnms],Coilstk[,pairnms])
  # lhs <- cbind(lhs,rep(0,nrow(lhs)))
  lhs[is.na(lhs)]<- 0
  params$lhs <- lhs
  rhs <- c(sfg[,2],rep(100/(100-wastage.threshold),nrow(WtForwst)),rep(1,nrow(Sheetstk)),rep(1,nrow(Coilstk)))
  rhs[is.na(rhs)]<- 0
  params$rhs <-rhs
  cond <- c(rep("==",nrow(sfg)),rep("<=",nrow(WtForwst)),rep("<=",nrow(Sheetstk)),rep("<=",nrow(Coilstk)))
  params$cond <- cond
  return(params)
}

runOptim <- function(params) {
  if(length(params$pairnms)==1){Cost <- as.vector((params$Cost))}else{Cost <-as.vector(t(params$Cost))}
  params$Cost$N <- 0
  solObj <- lp(direction = "min", 
               objective.in = Cost,
               const.mat = params$lhs
               ,const.dir = params$cond
               ,const.rhs = params$rhs
               )
  
  return(solObj)
}