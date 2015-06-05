# Name: CCrossValidation.R
# Auth: Umar Niazi u.niazi@imperial.ac.uk
# Date: 5/6/2015
# Desc: Class to hold test and training data and perform cross validation


library(methods)
if (!require(MASS) || !require(ROCR)) {
  stop('Libraries MASS and ROCR required to use this class')
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




