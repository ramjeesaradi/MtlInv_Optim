source("optimDataPrep.R")
source("optimObj.R")


solObj <- lp(direction = "max", 
             objective.in = rep(0,length(pairnms)),
             const.mat = sfg0[,-ncol(sfg0)],
             const.dir = rep("==",nrow(sfg0)),
             const.rhs = sfg0[,ncol(sfg0)]
             # ,transpose.constraints =FALSE
             # all.int = T
)
sol <- solObj$solution
# constrOptim(theta = solObj$solution
#             ,ui = sfg0[,-ncol(sfg0)]
#             ,ci = sfg0[,ncol(sfg0)]
#             ,f = function (x) objfn(x,params)
#             )

sol <- optim(rep(0,length(pairnms))
             ,fn = function (x) constrobjfn(x,params)
             ,method =
             ,lower = -Inf, upper = Inf
             ,control = list(fnscale = -1 ,parscale = rep(1,length(pairnms)))
)
sol <- hydroPSO(rep(0,length(pairnms))
                ,fn = function (x) constrobjfn(x,params)
                ,lower = rep(0,length(pairnms)), upper = rep(100000,length(pairnms))
                ,control = list(MinMax = "max", npart =5000)
                )

print(reqMet(sol$par,params))
#print(sol$par)
