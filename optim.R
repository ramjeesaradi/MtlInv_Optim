source("optimDataPrep.R")
source("optimObj.R")


solObj <- lp(direction = "min", 
             objective.in = priority,
             const.mat = sfg0[,-ncol(sfg0)],
             const.dir = rep("==",nrow(sfg0)),
             const.rhs = sfg0[,ncol(sfg0)]
             # transpose.constraints =FALSE,
             # all.int = T
)
sol <- solObj$solution
# sol <- constrOptim()