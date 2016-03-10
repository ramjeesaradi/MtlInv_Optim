source("optimDataPrep.R")
source("optimObj.R")
library(optimx)
# library(hydroPSO)

solObj <- lp(direction = "min", 
             objective.in = as.vector(t(Cost[pairnms])),
             const.mat = lhs
             ,const.dir = cond
             ,const.rhs = rhs
             # ,transpose.constraints =FALSE
             # all.int = T
)
solution <- data.frame(Name = pairnms, qnty = solObj$solution) 
# sol <- solObj$solution
# print(reqOff(sol,params))
# sol <- constrOptim(theta = solObj$solution
#             ,mu = 1e-04
#             ,grad = NULL
#             ,ui = sfg0[,-ncol(sfg0)]
#             ,ci = sfg0[,ncol(sfg0)]
#             # ,lower = 0, upper = Inf
#             ,outer.iterations = 100
#             ,outer.eps = 1e-08
#             ,control = list(fnscale = 1 ,maxit = 10000,ndeps = rep(1e-03,length(pairnms)), abstol= 100)
#             ,f = function (x) objfn(x,params)
#             )
# 
# OPTsol <- optim(solObj$solution
#              ,fn = function (x) constrobjfn(x,params)
#              ,gr = NULL
#              # ,all.methods = TRUE
#              # ,method =
#              ,lower = 0, upper = Inf
#              ,control = list(fnscale = 1 ,maxit = 110000,ndeps = rep(1e-3,length(pairnms)), pgtol= 1e-8)
# )
# print(reqOff(OPTsol$par,params))
# #print(sol$par)
