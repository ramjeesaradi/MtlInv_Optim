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
print(reqMet(sol,params))
print(length(solObj$solution))
constrOptim(theta = solObj$solution
            ,ui = sfg0[,-ncol(sfg0)]
            ,ci = sfg0[,ncol(sfg0)]
            ,f = function (x) objfn(x,params)
            )

sol <- optim(rep(0,length(pairnms)),
             fn = function (x) constrobjfn(x,params)
             ,control = list(fnscale = 1,maxit = 100000000 ,parscale = rep(100,length(pairnms)),abstol = 1)
)

