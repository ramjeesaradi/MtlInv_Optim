constrobjfn <- function(sol,params){
  constr <- reqMet(sol,params)
  obj1 <- invUtil(sol,params)
  obj2 <- wastemin(sol,params)
  # obj3 <- prmax(sol,params)
  # obj4 <- expensmin(sol,params)
  obj <- c(obj1,obj2,constr)
  fitness <- sqrt(sum(obj^2))
  # fitness <- constr*fitness
  return(fitness)
}

objfn <- function(sol,params){
  obj1 <- invUtil(sol,params)
  obj2 <- wastemin(sol,params)
  # obj3 <- prmax(sol,params)
  # obj4 <- expensmin(sol,params)
  obj <- c(obj1,obj2)
  fitness <- sqrt(sum(obj^2))
  return(fitness)
}
reqMet <- function(sol,params){
  req <- t(params$sfg0[,ncol(sfg0)])
  produce <- sol %*% t(params$sfg0[,-ncol(sfg0)])
  satis <- sum((produce-req)^2)
  return(satis)
}

invUtil <- function(sol,params){
  cost <- sol %*% t(params$rmstk)
  cost <- sum(1000*((1-cost)^2))
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

gsub(".*?_","","S1A89220D_2021364-98.5-1250")
