scrapGen <- function(sol,params){
  sol1 <- data.frame(RM=sapply(params$PartRMComb0, function (x) gsub(".*?_","",x) ),Qnty= sol)
  solAggr <- aggregate(Qnty~RM,sol1,sum)
  inventory <- merge(params$inventory,solAggr, by.x = "Raw.Material..Number",by.y = "RM", all = T)
  invUtil <- inventory$Ware.house.Stock - inventory$Qnty
  
}
gsub(".*?_","","S1A89220D_2021364-98.5-1250")
