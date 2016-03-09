source("optimDataPrep.R")
source("optimObj.R")
library(optimx)
library(hydroPSO)

solObj <- lp(direction = "max", 
             objective.in = params$rmstkln,
             const.mat = sfg0[,-ncol(sfg0)],
             const.dir = rep("==",nrow(sfg0)),
             const.rhs = (sfg0[,ncol(sfg0)]+1)
             # ,transpose.constraints =FALSE
             # all.int = T
)
sol <- solObj$solution
print(reqOff(sol,params))
# sol <- constrOptim(theta = solObj$solution
#             ,grad = NULL
#             ,ui = sfg0[,-ncol(sfg0)]
#             ,ci = sfg0[,ncol(sfg0)]
#             ,outer.iterations = 10000
#             ,outer.eps = 1e-80
#             ,f = function (x) objfn(x,params)
#             )

OPTsol <- optimx(rep(0,length(pairnms))
             ,fn = function (x) constrobjfn(x,params)
             ,gr = NULL
             # ,all.methods = TRUE
             # ,method =
             # ,lower = 0, upper = Inf
             # ,control = list(fnscale = 1 ,maxit = 100000,ndeps = rep(10,length(pairnms)), abstol= 100)
)
print(reqOff(OPTsol$par,params))
sol <- hydroPSO(rep(0,length(pairnms))
                ,fn = function (x) constrobjfn(x,params)
                ,lower = rep(0,length(pairnms)), upper = rep(10000,length(pairnms))
                ,control = list(MinMax = "max", npart =500)
                )
print(reqMet(sol$par,params))
#print(sol$par)
