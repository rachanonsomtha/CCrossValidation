# CCrossValidation
Class to perform cross validation and draw ROC curves for Test and Training data

# Class CCrossValidation
Base/Parent class that is a container for the training and test data plus some related information. Do not call this class
directly.
NOTE: Currently the class only works on 2 class problems as it is generating ROC curves.

# Class CCrossValidation.LDA
Uses the LDA model from MASS package to perform a nested k fold cross validation on the training data, and a test vs validation
error rate using the test and training data. The results are summarized in a ROCR object, which can be plotted along with the
performance curves. 

# Constructor CCrossValidation.LDA
The arguments to the function include:  
1- test.dat = data.frame of test object, where the samples or n are rows and columns are variables or components of the vector.  
2- train.dat = data.frame of training data (similar format).  
3- test.groups = 2 level factor with length equal to nrow of test.dat data.frame.  
4- train.group = similar.  
5- level.predict = A character string identifying the level that will be predicted.  
6- boot.num = Number of bootstraps or nested cross validation to perform. - default = 50  
7- k.fold = number of folds - default = 10  
RETS: The function (constructor) returns the object of class CCrossValidation.LDA  
DESC: Most of the work happens in the 2 internal functions, f_kfoldcv and f_validation.  
f_kfoldcv: The function starts by setting the training set and variables. It also creates 2 list variables to hold the predicted 
probability and the label (TRUE/FALSE - the actual value for the class) for the data in the current fold. For each boot cycle first
equal sizes data is selected from each class - to keep proportions of the classes at 50:50. Using this data, the data is divided into
k folds and cross validation using lda is performed. The results for each boot stored in the list variables. Finally the Prediction,
performance and Area under curve objects are calculated using ROCR library and a cross validation error rate is calculated.  
  
f_validation: This is a simpler function and fits the LDA model to the training data and makes a prediction on the test data. The
prediction, performance, AUC and error rates are calculated.

# plot.cv.performance
A simple function that plots the data using the ROCR library and its plot function. It plots the true positive rate, false positive 
rate for both the cross validation and validation set results. The cross validation error bounds are based on 2 standard deviations.  

