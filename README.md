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
1- test
