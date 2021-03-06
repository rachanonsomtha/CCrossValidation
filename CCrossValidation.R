# Copyright (C) 2015  Umar Niazi
#   
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>


# Name: CCrossValidation.R
# Auth: Umar Niazi u.niazi@imperial.ac.uk
# Date: 5/6/2015
# Desc: Class to hold test and training data and perform cross validation


library(methods)
if (!require(MASS) || !require(ROCR) || !require(tree) || !require(randomForest)) {
  stop('Libraries MASS, tree, randomForest and ROCR required to use this class')
}

#############################################
########### Class CCrossValidation
# Name: CCrossValidation
# Decs: Parent class to hold information for test and training data in a dataframe


# declaration
setClass('CCrossValidation', slots=list(dfTest='data.frame', dfTrain='data.frame', fGroups.test='factor',
                                        fGroups.train='factor', cPred='character'))


# constructor
CCrossValidation = function(test.dat, train.dat, test.groups, train.groups, level.predict){
  # the constructor only works on a 2 class problem, if 
  # number of levels greater than 2 then stop
  if (length(levels(test.groups)) != 2 || length(levels(train.groups != 2))){
    stop('Number of levels should be only 2 for a 2 class comparison')
  }
  # check if the level to predict is present in the factor
  if (!(level.predict %in% levels(test.groups)) || !(level.predict %in% levels(train.groups)) ){
    stop('Level to predict is not present in factors provided')
  }
  ## create the object
  ob = new('CCrossValidation', dfTest=test.dat, dfTrain=train.dat, fGroups.test=test.groups,
           fGroups.train=train.groups, cPred=level.predict)
  return(ob)
}



##########################################
######### sub class CCrossValidation.LDA
# Name: CCrossValidation.LDA
# Desc: Child class to perform cross validation, based on data in class cross validation

setClass('CCrossValidation.LDA', slots=list(iBoot='numeric', oPred.cv='ANY', oPerf.cv='ANY', oAuc.cv='ANY',
                                            oPred.val='ANY', oPerf.val='ANY', oAuc.val='ANY', iFolds='numeric', 
                                            ivCV.error.rate='ANY', iTest.error='ANY'), 
         contains='CCrossValidation')


## constructor
CCrossValidation.LDA = function(test.dat, train.dat, test.groups, train.groups, level.predict, boot.num=50, k.fold=10){
  # create the cross validation object first
  oCv = CCrossValidation(test.dat, train.dat, test.groups, train.groups, level.predict)
  # if object created correctly then move to next step and perform cross validation
  # create object 
  oCv.lda = new('CCrossValidation.LDA', iBoot=boot.num, oPred.cv=NULL, oPerf.cv=NULL, oAuc.cv=NULL,
           oPred.val=NULL, oPerf.val=NULL, oAuc.val=NULL, iFolds=k.fold, ivCV.error.rate=NULL, iTest.error=NULL, oCv)
  
  # Name: f_kfoldcv
  # Desc: internal function to perform k fold cross validation
  f_kfoldcv = function(ob){
    ## set the data variables
    dfData = ob@dfTrain
    dfData$fGroups = ob@fGroups.train
    cLevels = levels(ob@fGroups.train)
    ## perform CV
    dfData.full = dfData
    lPred = vector(mode = 'list', length = ob@iBoot)
    lLab = vector(mode = 'list', length=ob@iBoot)
    iCv.error = NULL
    for (oo in 1:ob@iBoot){
      t.lPred = NULL
      t.lLab = NULL
      # select a subset of equal numbers for the 2 levels
      ind.o = which(dfData.full$fGroups == cLevels[1])
      ind.p = which(dfData.full$fGroups == cLevels[2])
      # take the sample of equal size from both groups
      # where size = minimum size from the 2 groups
      iSample.size = min(c(length(ind.p), length(ind.o)))
      ind.o.s = sample(ind.o, size = iSample.size, replace = F)
      ind.p.s = sample(ind.p, size=iSample.size, replace=F)
      # these steps are performed as we want to select equal sizes from each class/group
      # randomize the index labels 
      ind = sample(c(ind.o.s, ind.p.s), replace=F)
      dfData = dfData.full[ind,]
      for (o in 1:1){
        # perform k fold cross validation
        k = ob@iFolds
        # create the folds from the data
        folds = sample(1:k, nrow(dfData), replace = T, prob = rep(1/k, times=k))
        # choose the fold to fit and test the model
        for (i in 1:k){
          # check if selected fold leads to 0 for a class
          # this can happen if a class is very small number of data i.e. n is small
          if ((length(unique(dfData$fGroups[folds != i])) < 2) || (length(unique(dfData$fGroups[folds == i])) < 2)) next
          # check if fold too small to fit model
          if (nrow(dfData[folds != i,]) < 3) next
          # fit model on data not in fold
          fit = lda(fGroups ~ ., data=dfData[folds != i,])
          # predict on data in fold
          pred = predict(fit, newdata = dfData[folds == i,])$posterior[,ob@cPred]
          name = paste('pred',oo, o, i,sep='.' )
          t.lPred[[name]] = pred
          name = paste('label',oo,o, i,sep='.' )
          t.lLab[[name]] = dfData$fGroups[folds == i] == ob@cPred
          pred = predict(fit, newdata = dfData[folds == i,])$class
          iCv.error = append(iCv.error, mean(pred != dfData$fGroups[folds == i]))
        }
      }
      t.lPred = unlist(t.lPred)
      t.lLab = unlist(t.lLab)
      lPred[[oo]] = t.lPred
      lLab[[oo]] = t.lLab
    }
    
    pred = prediction(lPred, lLab)
    perf = performance(pred, 'tpr', 'fpr')
    auc = performance(pred, 'auc')
    ob@oPred.cv = pred
    ob@oPerf.cv = perf
    ob@oAuc.cv = auc
    ob@ivCV.error.rate = iCv.error
    return(ob)
  }
  
  # Name: f_validation
  # Desc: internal function to do training vs test set validation
  f_validation = function(ob){
    ## set the variables first
    dfData.train = ob@dfTrain
    dfData.train$fGroups = ob@fGroups.train
    
    dfData.test = ob@dfTest
    dfData.test$fGroups = ob@fGroups.test
    # fit the model on the training data set
    fit = lda(fGroups ~ ., data=dfData.train)
    # predict on data in fold
    pred = predict(fit, newdata = dfData.test)$posterior[,ob@cPred]
    ivPred = pred
    ivLab = dfData.test$fGroups == ob@cPred
    pred = predict(fit, newdata = dfData.test)$class
    ob@iTest.error = mean(pred != dfData.test$fGroups)
    # calculate rocr objects
    pred = prediction(ivPred, ivLab)
    perf = performance(pred, 'tpr', 'fpr')
    auc = performance(pred, 'auc')
    ob@oPred.val = pred
    ob@oPerf.val = perf
    ob@oAuc.val = auc
    return(ob)
  }
  
  # fill the object with the information
  oCv.lda = f_kfoldcv(oCv.lda)
  oCv.lda = f_validation(oCv.lda)
  return(oCv.lda)
}


#### functions
setGeneric('plot.cv.performance', def = function(ob, legend.pos='bottomright', ...) standardGeneric('plot.cv.performance'))
setMethod('plot.cv.performance', signature='CCrossValidation.LDA', definition = function(ob, legend.pos='bottomright', ...){
  # plot the ROC curves for cross validation and validation set 
  # cv error
  pred = ob@oPred.cv
  perf = ob@oPerf.cv
  auc = ob@oAuc.cv
  plot(perf, main=paste('ROC Prediction of for', ob@cPred),
       spread.estimate='stddev', avg='vertical', spread.scale=2)
  auc.cv = paste('CV AUC=', round(mean(as.numeric(auc@y.values)), digits = 2))
  cv.err = paste('CV Error=', round(mean(ob@ivCV.error.rate), 2))
  abline(0, 1, lty=2)
  
  # validation error
  pred = ob@oPred.val
  perf = ob@oPerf.val
  auc = ob@oAuc.val
  plot(perf, add=T, lty=3, lwd=2, col=2)
  auc.t = paste('Val AUC=', round(mean(as.numeric(auc@y.values)), digits = 2))
  err.t = paste('Val Error=', round(ob@iTest.error, 2))
  legend(legend.pos, legend = c(auc.cv, cv.err, auc.t, err.t))
})

#### Data accessor functions
setGeneric('getAUCVector', def = function(ob, ...) standardGeneric('getAUCVector'))
setMethod('getAUCVector', signature='CCrossValidation.LDA', definition = function(ob, ...){
  # cross validation AUC vector
  auc = performance(ob@oPred.cv, 'auc')
  return(unlist(auc@y.values))
})


setGeneric('getCutoffTprFprCrossValidation', def = function(ob, ...) standardGeneric('getCutoffTprFprCrossValidation'))
setMethod('getCutoffTprFprCrossValidation', signature='CCrossValidation.LDA', definition = function(ob, ...){
  # cross validation AUC vector
  perf = ob@oPerf.cv
  cutoffs = do.call(cbind, perf@alpha.values)
  tpr = do.call(cbind, perf@y.values)
  fpr = do.call(cbind, perf@x.values)
  c = apply(cutoffs, 1, median)
  t = apply(tpr, 1, median)
  f = apply(fpr, 1, median)
  mRet = data.frame(cutoff=c, tpr=t, fpr=f, rate=t/f)
  return(mRet)
})


##########################################
######### sub class CCrossValidation.Tree
# Name: CCrossValidation.Tree
# Desc: Child class to perform cross validation, based on data in class cross validation

setClass('CCrossValidation.Tree', slots=list(iBoot='numeric', oPred.cv='ANY', oPerf.cv='ANY', oAuc.cv='ANY',
                                            oPred.val='ANY', oPerf.val='ANY', oAuc.val='ANY', iFolds='numeric', 
                                            ivCV.error.rate='ANY', iTest.error='ANY'), 
         contains='CCrossValidation')


## constructor
CCrossValidation.Tree = function(test.dat, train.dat, test.groups, train.groups, level.predict, boot.num=50, k.fold=10){
  # create the cross validation object first
  oCv = CCrossValidation(test.dat, train.dat, test.groups, train.groups, level.predict)
  # if object created correctly then move to next step and perform cross validation
  # create object 
  oCv.tree = new('CCrossValidation.Tree', iBoot=boot.num, oPred.cv=NULL, oPerf.cv=NULL, oAuc.cv=NULL,
                oPred.val=NULL, oPerf.val=NULL, oAuc.val=NULL, iFolds=k.fold, ivCV.error.rate=NULL, iTest.error=NULL, oCv)
  

  # Name: f_kfoldcv
  # Desc: internal function to perform k fold cross validation
  f_kfoldcv = function(ob){
    ## set the data variables
    dfData = ob@dfTrain
    dfData$fGroups = ob@fGroups.train
    cLevels = levels(ob@fGroups.train)
    ## perform CV
    dfData.full = dfData
    lPred = vector(mode = 'list', length = ob@iBoot)
    lLab = vector(mode = 'list', length=ob@iBoot)
    iCv.error = NULL
    for (oo in 1:ob@iBoot){
      t.lPred = NULL
      t.lLab = NULL
      # select a subset of equal numbers for the 2 levels
      ind.o = which(dfData.full$fGroups == cLevels[1])
      ind.p = which(dfData.full$fGroups == cLevels[2])
      # take the sample of equal size from both groups
      # where size = minimum size from the 2 groups
      iSample.size = min(c(length(ind.p), length(ind.o)))
      ind.o.s = sample(ind.o, size = iSample.size, replace = F)
      ind.p.s = sample(ind.p, size=iSample.size, replace=F)
      # these steps are performed as we want to select equal sizes from each class/group
      # randomize the index labels 
      ind = sample(c(ind.o.s, ind.p.s), replace=F)
      dfData = dfData.full[ind,]
      for (o in 1:1){
        # perform k fold cross validation
        k = ob@iFolds
        # create the folds from the data
        folds = sample(1:k, nrow(dfData), replace = T, prob = rep(1/k, times=k))
        # choose the fold to fit and test the model
        for (i in 1:k){
          # check if selected fold leads to 0 for a class
          # this can happen if a class is very small number of data i.e. n is small
          if ((length(unique(dfData$fGroups[folds != i])) < 2) || (length(unique(dfData$fGroups[folds == i])) < 2)) next
          # check if fold too small to fit model
          if (nrow(dfData[folds != i,]) < 3) next
          # fit model on data not in fold
          # first select optimal tree size by pruning tree
          df.prune.niazi = dfData[folds != i,]
          # assign this variable to global environment as cv.tree function
          # can not find it
          assign('df.prune.niazi', df.prune.niazi, envir = .GlobalEnv)
          fit = tree(fGroups ~ ., data=df.prune.niazi)
          cv.fit.tree = cv.tree(fit, FUN = prune.misclass)
          min.tree = which.min(cv.fit.tree$dev)
          min.tree = cv.fit.tree$size[min.tree]
          # get the minimum sized tree
          fit = prune.misclass(fit, best = min.tree)
          # remove from global environment
          remove('df.prune.niazi', envir = .GlobalEnv)
          # predict on data in fold
          pred = predict(fit, newdata = dfData[folds == i,], type='vector')[,ob@cPred]
          name = paste('pred',oo, o, i,sep='.' )
          t.lPred[[name]] = pred
          name = paste('label',oo,o, i,sep='.' )
          # create a true/false vector to be used by ROC curve
          # this means we do not provide the actual factor level
          t.lLab[[name]] = dfData$fGroups[folds == i] == ob@cPred
          pred = predict(fit, newdata = dfData[folds == i,], type='class')
          iCv.error = append(iCv.error, mean(pred != dfData$fGroups[folds == i]))
        }
      }
      t.lPred = unlist(t.lPred)
      t.lLab = unlist(t.lLab)
      lPred[[oo]] = t.lPred
      lLab[[oo]] = t.lLab
    }
    
    pred = prediction(lPred, lLab)
    perf = performance(pred, 'tpr', 'fpr')
    auc = performance(pred, 'auc')
    ob@oPred.cv = pred
    ob@oPerf.cv = perf
    ob@oAuc.cv = auc
    ob@ivCV.error.rate = iCv.error
    return(ob)
  }
  
  # Name: f_validation
  # Desc: internal function to do training vs test set validation
  f_validation = function(ob){
    ## set the variables first
    dfData.train = ob@dfTrain
    dfData.train$fGroups = ob@fGroups.train
    
    dfData.test = ob@dfTest
    dfData.test$fGroups = ob@fGroups.test
    ## note: this hack is because of how R performs scoping
    ## read here for details: http://cran.r-project.org/web/packages/car/vignettes/embedding.pdf
    df.prune.niazi = dfData.train
    # fit the model on the training data set
    # assign this variable to global environment as cv.tree function
    # can not find it
    assign('df.prune.niazi', df.prune.niazi, envir = .GlobalEnv)
    fit = tree(fGroups ~ ., data=df.prune.niazi)
    # prune the tree 
    cv.fit.tree = cv.tree(fit, FUN = prune.misclass)
    min.tree = which.min(cv.fit.tree$dev)
    min.tree = cv.fit.tree$size[min.tree]
    # get the minimum sized tree
    fit = prune.misclass(fit, best = min.tree)
    # remove from global environment
    remove('df.prune.niazi', envir = .GlobalEnv)
    # predict on data in fold
    pred = predict(fit, newdata = dfData.test, type='vector')[,ob@cPred]
    ivPred = pred
    # create true/false labels
    ivLab = dfData.test$fGroups == ob@cPred
    pred = predict(fit, newdata = dfData.test, type='class')
    ob@iTest.error = mean(pred != dfData.test$fGroups)
    # calculate rocr objects
    pred = prediction(ivPred, ivLab)
    perf = performance(pred, 'tpr', 'fpr')
    auc = performance(pred, 'auc')
    ob@oPred.val = pred
    ob@oPerf.val = perf
    ob@oAuc.val = auc
    return(ob)
  }
  
  # fill the object with the information
  oCv.tree = f_kfoldcv(oCv.tree)
  oCv.tree = f_validation(oCv.tree)
  return(oCv.tree)
}


#### functions
setMethod('plot.cv.performance', signature='CCrossValidation.Tree', definition = function(ob, legend.pos='bottomright', ...){
  # plot the ROC curves for cross validation and validation set 
  # cv error
  pred = ob@oPred.cv
  perf = ob@oPerf.cv
  auc = ob@oAuc.cv
  plot(perf, main=paste('ROC Prediction of for', ob@cPred),
       spread.estimate='stddev', avg='vertical', spread.scale=2)
  auc.cv = paste('CV AUC=', round(mean(as.numeric(auc@y.values)), digits = 2))
  cv.err = paste('CV Error=', round(mean(ob@ivCV.error.rate), 2))
  abline(0, 1, lty=2)
  
  # validation error
  pred = ob@oPred.val
  perf = ob@oPerf.val
  auc = ob@oAuc.val
  plot(perf, add=T, lty=3, lwd=2, col=2)
  auc.t = paste('Val AUC=', round(mean(as.numeric(auc@y.values)), digits = 2))
  err.t = paste('Val Error=', round(ob@iTest.error, 2))
  legend(legend.pos, legend = c(auc.cv, cv.err, auc.t, err.t))
})


## data acessor functions
# setGeneric('getAUCVector', def = function(ob, ...) standardGeneric('getAUCVector'))
setMethod('getAUCVector', signature='CCrossValidation.Tree', definition = function(ob, ...){
  # cross validation AUC vector
  auc = performance(ob@oPred.cv, 'auc')
  return(unlist(auc@y.values))
})


######################################################################################
## CVariableSelection
## classes to perform variable selection using random forest or step-wise selection

#Name: CVariableSelection
#Desc: Data holder for variable selection class, 
#NOTE: works on binary classification problems mainly, and some numeric response problems

# declaration
setClass('CVariableSelection', slots=list(dfData='data.frame', fGroups='ANY'))

# constructor
CVariableSelection = function(data, groups){
  # the constructor only works on a 2 class problem or continuous variables, if 
  # number of levels greater than 2 then stop
  if (!is.numeric(groups) && !is.factor(groups)) {
    stop('Response variable should be numeric or factor with 2 levels')
  }
  if (is.factor(groups) && length(levels(groups)) != 2){
    stop('Number of levels should be only 2 for a 2 class comparison')
  }
  ## create the object
  ob = new('CVariableSelection', dfData=data, fGroups=groups)
  return(ob)
}


#### Create subclass for variable selection
#Name: CVariableSelection.RandomForest
#Desc: The class takes a set of varialbes and performs a nested cross validation on samples of the data
#      each time selecting equal proportions from both classes, fitting a random forest and saving the 
#      variable importance measure. The variable importance is summarized by its overall mean and sd

setClass('CVariableSelection.RandomForest', slots=list(dfImportance='data.frame', iBoot='numeric'),
         contains='CVariableSelection')


# constructor
CVariableSelection.RandomForest = function(data, groups, boot.num=100, big.warn=T){
  # creat the CVariableSelection object and perform error checks
  oCV = CVariableSelection(data, groups)
  
  # check number of dimensions for the data, as performing a bootstrap on
  # larger than 200 variables may not be feasible
  if (big.warn && ncol(data) > 200) {
    stop('Number of varialbes is larger than 200, it may take long to finish calculations, reduce dimensions or set big.warn=F')
  }
  
  ###### Nested random forest
  dfData.full = data.frame(data, fGroups=groups)
  iBoot = boot.num
  ## check if variable continuous or categoriacal
  cLevels = NULL;
  if (is.numeric(groups)) cLevels = NA else cLevels = levels(groups)
  
  
  #### if variable is 2 class categorical
  ## as the 2 class proportions are not equal, fit random forest multiple times on random samples
  ## containing equal proportions of both classes and check variable importance measures
  # fit random forests multiple times
  # store results 
  lVarImp = vector('list', iBoot)
  ind = NULL
  for (i in 1:iBoot) {
    ## if response variable is categorical or numeric
    if (is.factor(dfData.full$fGroups)){
      # get indices of the particular factors in data table
      ind.o = which(dfData.full$fGroups == cLevels[1])
      ind.p = which(dfData.full$fGroups == cLevels[2])
      # take the sample of equal size from both groups
      # where size = minimum size from the 2 groups
      iSample.size = min(c(length(ind.p), length(ind.o)))
      ind.o.s = sample(ind.o, size = iSample.size, replace = F)
      ind.p.s = sample(ind.p, size=iSample.size, replace=F)
      # join the sample indices together
      ind = sample(c(ind.o.s, ind.p.s), replace=F)
    } else {ind = 1:nrow(dfData.full)}
    # take sample from the full dataset
    dfData = dfData.full[ind,]
    # fit model
    fit.rf = randomForest(fGroups ~., data=dfData, importance = TRUE, ntree = 500)
    # get variables importance
    df = importance(fit.rf)
    ## get the appropriate variable based on if 
    ## it is a classification or regression problem
    if (is.factor(dfData$fGroups)) {
      df = df[order(df[,'MeanDecreaseAccuracy'], decreasing = T),]
    } else df = df[order(df[,'%IncMSE'], decreasing = T),]
    # put in list
    lVarImp[[i]] = df
  } # for
  
  ## put data for each boot of each variable together in a dataframe
  # df = NULL
  # for (i in 1:iBoot) df = rbind(df, lVarImp[[i]])
  df = do.call(rbind, lVarImp)
  # convert rownames i.e. variable names to factors
  f = as.factor(rownames(df))
  # calculate mean and sd for each variable
  ivMean = NULL; ivSD = NULL;
  if (is.factor(dfData.full$fGroups)) {
    ivMean = tapply(df[,'MeanDecreaseAccuracy'], f, mean)
    ivSD = tapply(df[,'MeanDecreaseAccuracy'], f, sd)
  } else {
    ivMean = tapply(df[,'%IncMSE'], f, mean)
    ivSD = tapply(df[,'%IncMSE'], f, sd)
  }
  # df = as.data.frame(df)
  # df$Symbol = rownames(df)
  # dfRF.boot = df
  # calculate coefficient of variation
  cv = ivSD/abs(ivMean)
  # split data into groups based on cv
  ## NOTE: Implement an error trycatch here as this is the type of 
  ## error we get here very rarely
  # Error in quantile.default(cv, 0:10/10) : 
  #   missing values and NaN's not allowed if 'na.rm' is FALSE
  getg = function(cv) cut(cv, breaks = quantile(cv, 0:10/10), include.lowest = T)
  getgl = function(cv) cut(cv, breaks = quantile(cv, 0:10/10), include.lowest = T, labels = 0:9)
  g = tryCatch(getg(cv), error=function(e) NULL)
  gl = tryCatch(getgl(cv), error=function(e) NULL)
  # g = cut(cv, breaks = quantile(cv, 0:10/10), include.lowest = T)
  # gl = cut(cv, breaks = quantile(cv, 0:10/10), include.lowest = T, labels = 0:9)
  dfRF.boot.stats = data.frame(ivMean, ivSD, cv)
  dfRF.boot.stats$groups= g
  dfRF.boot.stats$group.lab=gl
  
  # create the new object
  ob = new('CVariableSelection.RandomForest', dfImportance=dfRF.boot.stats, iBoot=iBoot, oCV)
  return(ob)
}

############ functions

setGeneric('plot.var.selection', def = function(ob, ...) standardGeneric('plot.var.selection'))
setMethod('plot.var.selection', signature='CVariableSelection.RandomForest', definition = function(ob, ...){
  # plot the variable importance as bar plots with 
  # confidence intervals
  dfDat = ob@dfImportance
  dfDat = dfDat[order(dfDat$ivMean, decreasing = T),]
  i = nrow(dfDat)
  if (i > 20) i = 1:20 else i = 1:nrow(dfDat)
  dfDat = dfDat[i,]
  mBar = matrix(dfDat$ivMean, nrow=1)
  colnames(mBar) = rownames(dfDat)
  p.old = par(mar=c(6,3,4,2)+0.1)
  y = c(0, max(ceiling(mBar)))
  l2 = barplot(mBar, las=2, xaxt='n', col='grey', main='Top Variables', ylim=y, ...)
  axis(1, at = l2, labels = colnames(mBar), tick = F, las=2, cex.axis=0.7 )
  # calculate standard errors
  ## function to simulate  
  f_sim_ci = function(m, s){
    m = rnorm(n = 1000, m, s)
    return(quantile(m, c(0.025, 0.975)))
  }
  # draw error bars
  for (i in 1:length(l2)){
    l = l2[i]
    ## make error bars
    # get standard error and simulate ci
    se = dfDat[i,'ivSD']/sqrt(ob@iBoot)
    m = f_sim_ci(mBar[,i], se)
    segments(l, y0 = m[1], l, y1 = m[2], lwd=2)
    segments(l-0.1, y0 = m[1], l+0.1, y1 = m[1], lwd=2)
    segments(l-0.1, y0 = m[2], l+0.1, y1 = m[2], lwd=2)
  }  
  par(p.old)
})


## data accessor functions
CVariableSelection.RandomForest.getVariables = function(ob){
  dfDat = ob@dfImportance
  dfDat = dfDat[order(dfDat$ivMean, decreasing = T),]
  return(dfDat)
}


#Name: CVariableSelection.ReduceModel
#Desc: The class takes a set of varialbes and performs a variable subset selection with test and training set

setClass('CVariableSelection.ReduceModel', slots=list(mTest='matrix', mTrain='matrix', iBoot='numeric'),
         contains='CVariableSelection')


CVariableSelection.ReduceModel = function(data, response, boot.num=1000, cvMethod='exhaustive'){
  # require package
  if (!require(leaps) || !require(MASS)) stop('Package leaps and MASS required')
  groups = response
  # creat the CVariableSelection object and perform error checks
  oCV = CVariableSelection(data, groups)
  
  # split the data into test and training sets
  # and perform variable selection
  iBoot = boot.num
  # matrix to hold results for each boot cycle
  mTrain = matrix(NA, nrow = ncol(data), ncol = iBoot)
  mTest = matrix(NA, nrow = ncol(data), ncol = iBoot)
  for(o in 1:iBoot){
    dfData.train = data
    dfData.train$fGroups = groups
    # create a test set on a percentage the data
    test = sample(1:nrow(dfData.train), size =nrow(dfData.train) * 0.30, replace = F)
    dfData.test = dfData.train[test,]
    dfData.train = dfData.train[-test,]
    # fit model
    reg = regsubsets(fGroups ~ ., data=dfData.train, nvmax = ncol(data), method=cvMethod)
    # test for validation errors in the test set
    ivCV.train = rep(NA, length=ncol(data))
    ivCV.test = rep(NA, length=ncol(data))
    for (i in 1:ncol(data)){
      # get the variables in each subset
      n = names(coef(reg, i))[-1]
      n = c(n, 'fGroups')
      cn = colnames(dfData.train)
      # as dummy codings for categorical variables adds a postfix level
      # to the original variable name, e.g. MaleY where Y is the postfix added to Male
      # we want to extract the original variable name, hence th convulated way of getting
      # the variable names
      f = sapply(cn, function(x) any(startsWith(n, x)))
      dfDat.train = dfData.train[,cn[f]]
      dfDat.test = dfData.test[,cn[f]]
      # if response variable is a 2 class categorical variable 
      # then use lda else use the function
      if (is.factor(oCV@fGroups)){
        # fit the lda model on training dataset
        fit.lda = lda(fGroups ~ ., data=dfDat.train)
        # test error rate on test dataset
        p = predict(fit.lda, newdata=dfDat.test)
        # calculate test error 
        ivCV.test[i] = mean(p$class != dfDat.test$fGroups)  
        # calculate training error
        p = predict(fit.lda, newdata=dfDat.train)
        # calculate error
        ivCV.train[i] = mean(p$class != dfDat.train$fGroups)  
      } else if (is.numeric(oCV@fGroups)) {
        # test error rate on test dataset
        coefi = coef(reg, i)
        # model matrix for test data
        test.mat = model.matrix(fGroups ~ ., data=dfData.test)
        # calculate test error 
        p = test.mat[,names(coefi)]%*%coefi
        ivCV.test[i] = mean((dfData.test$fGroups-p)^2)
        # calculate training error
        # model matrix for training data
        train.mat = model.matrix(fGroups ~ ., data=dfData.train)
        # calculate train error 
        p = train.mat[,names(coefi)]%*%coefi
        ivCV.train[i] = mean((dfData.train$fGroups-p)^2)
      }
    }
    mTrain[,o] = ivCV.train
    mTest[,o] = ivCV.test
    print(paste('boot cycle:', o))
  }
  # save data
  mTrain = t(mTrain)
  mTest = t(mTest)
  ob = new('CVariableSelection.ReduceModel', mTest=mTest, mTrain=mTrain, iBoot=iBoot, oCV)
}

#### functions 
setMethod('plot.var.selection', signature='CVariableSelection.ReduceModel', definition = function(ob, ...){
  # plot the test and training error agasint number of variables
  tr = colMeans(ob@mTrain)
  te = colMeans(ob@mTest)  
  m = cbind(tr, te)
  matplot(1:nrow(m), m, type='b', pch=20, lty = 1, lwd=2, col=1:2, xaxt='n', xlab='No. of Variables', ylab='Error Rate')
  legend('topright', legend = c('Train', 'Test'), fill=1:2)
  axis(1, at = 1:nrow(m), las=2)
})


# accessor functions
CVariableSelection.ReduceModel.getMinModel = function(ob, size, cvMethod='exhaustive'){
  # return the model of the required size
  i = size
  # refit subset using i number of variables on all data
  dfData = ob@dfData
  dfData$fGroups = ob@fGroups
  reg = regsubsets(fGroups ~ ., data=dfData, nvmax = ncol(ob@dfData), method=cvMethod)
  #return(names(coef(reg, i))[-1])
  n = names(coef(reg, i))[-1]
  cn = colnames(dfData)
  # as dummy codings for categorical variables adds a postfix level
  # to the original variable name, e.g. MaleY where Y is the postfix added to Male
  # we want to extract the original variable name, hence th convulated way of getting
  # the variable names
  f = sapply(cn, function(x) any(startsWith(n, x)))
  return(cn[f])
}






















