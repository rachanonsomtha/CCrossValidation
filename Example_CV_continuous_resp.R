# Name: Example_CV_continuous_rep.R
# Auth: Umar Niazi u.niazi@imperial.ac.uk
# Date: 11/02/16
# Desc: some testing for class usage


source('CCrossValidation.R')

## load some test data
library(ISLR)
data("Hitters")

str(Hitters)
Hitters = na.omit(Hitters)
str(Hitters)

g = Hitters$Salary
d = Hitters[,-19]
str(d)

o = CVariableSelection.ReduceModel(d, g, 100)
plot.var.selection(o)

CVariableSelection.ReduceModel.getMinModel(o, 11)



