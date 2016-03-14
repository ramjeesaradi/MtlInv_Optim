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

getParams <- function(month,wastage.threshold,rmpart,sfgreq) {
  params <- list()
  #Take the req for the month in question
  rmpart1 <- merge(rmpart[rmpart$totalLead <= (month+1)*30,],sfgreq[sfgreq$month==month,],by.y = "sfg", by.x = "SFG")[,names(rmpart)]
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
  wstprct <- cast(rmpart1,.~ SFG_+RM_+con_+RM.Length+RM.Breadth+Batch_+inStock,
                  value = "tsfgwt",
                  fun.aggregate = function (x) ifelse(sum(x)==0,yes = 0,no = 1/(max(x)*nrow(sfg))),
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
  
  lhs <- rbind(sfg[,pairnms],wstprct[,pairnms], Sheetstk[,pairnms],Coilstk[,pairnms])
  lhs[is.na(lhs)]<- 0
  params$lhs <- lhs
  rhs <- c(sfg[,2],rep(100/(100-wastage.threshold),nrow(wstprct)),rep(1,nrow(Sheetstk)),rep(1,nrow(Coilstk)))
  rhs[is.na(rhs)]<- 0
  params$rhs <-rhs
  cond <- c(rep("==",nrow(sfg)),rep("<=",nrow(wstprct)),rep("<=",nrow(Sheetstk)),rep("<=",nrow(Coilstk)))
  params$cond <- cond
  return(params)
}

runOptim <- function(month,wastage.threshold,rmpart,sfgreq) {
  params <- getParams(month,wastage.threshold,rmpart,sfgreq)
  solObj <- lp(direction = "min", 
               objective.in = as.vector(t(params$Cost)),
               const.mat = params$lhs
               ,const.dir = params$cond
               ,const.rhs = params$rhs
               )
  solution <- data.frame(Name = params$pairnms, qnty = solObj$solution) 
  solution <- cbind(solution, Splitclmn(solution$Name,"_"))
  solution <- solution[solution$qnty !=0,]
  names(solution) <- c(names(solution)[1:2],"SFG", "Sheet","Coil","Length", "Breadth","Batch", "Stock")
  solution.out <- cast(solution, SFG+ Sheet+Coil+Length+ Breadth+ Batch ~ Stock, value = "qnty",fun.aggregate = max,fill = 0)
  write.csv(solution.out,paste(c("Solution",month,wastage.threshold,".csv"),collapse = "_"))
  return(solution)
}