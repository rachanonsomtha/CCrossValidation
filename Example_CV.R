# Name: Example_CV.R
# Auth: Umar Niazi u.niazi@imperial.ac.uk
# Date: 11/02/16
# Desc: some testing for class usage


source('CCrossValidation.R')

# generate test data
theta = seq(1, to = 100)

# means for the two populations
grp.1 = 20
grp.2 = 60

g1.prior = function(theta) dgamma(theta, grp.1, 1 )
g2.prior = function(theta) dgamma(theta, grp.2, 1)

curve(g1.prior, 1, 100)
plot(theta, g1.prior(theta), type='l')
plot(theta, g2.prior(theta), type='l')

# mixing probability for the two populations
g.prior.mix = function(theta, mix=0.5){
  m = mix * g1.prior(theta) + (1-mix) * g2.prior(theta)
  return(m)
}

# generate datasets with various mixing probabilities
# not so good predictor
curve(g.prior.mix, 1, 100)
# good predictor
plot(theta, g.prior.mix(theta, 0.1), type='l')
# good the other way
plot(theta, g.prior.mix(theta, 0.9), type='l')

set.seed(3)
# generate some data
pred.1 = sample(theta, 100, replace=T, prob=g.prior.mix(theta))
pred.2 = sample(theta, 50, replace=T, prob=g.prior.mix(theta, 0.1))
pred.3 = sample(theta, 50, replace=T, prob=g.prior.mix(theta, 0.9))

# generate 2 classes
fGroups = gl(2, 50, labels = c('A', 'Pred'))

dfData = data.frame(var.1 = pred.1, var.2 = c(pred.2, pred.3), fGroups)
test = sample(1:nrow(dfData), size = nrow(dfData) * 0.20, replace = F)

# create the cross validation object
oCV = CCrossValidation.LDA(dfData[test,-3], dfData[-test, -3], fGroups[test], fGroups[-test], level.predict = 'Pred',
                           boot.num = 100, k.fold = 5) 

plot.cv.performance(oCV)

# get the cross validation summaries
# these functions can give a warning, if the cutoffs generated at each bootcycle by ROCR package
# are not of the same length. 
auc = getAUCVector(oCV)
dfPerformance = getCutoffTprFprCrossValidation(oCV)

## repeat analysis with tree model
oCV = CCrossValidation.Tree(dfData[test,-3], dfData[-test, -3], fGroups[test], fGroups[-test], level.predict = 'Pred',
                           boot.num = 100, k.fold = 5) 

plot.cv.performance(oCV)

# get the cross validation summaries
auc = getAUCVector(oCV)









# generate some data
# ivDat = rpois(100, (grp.1+grp.2)/2)
# ivDat = rnbinom(100, size=mean(c(grp.1,grp.2)), mu = mean(c(grp.1,grp.2)))
# hist(ivDat)
# 
# # likelihood function
# lik = function(data, theta) dpois(data, theta)
# 
# g1.post.grid = function(data, theta){
#   post = sapply(theta, function(x) prod(lik(data, x)) * g1.prior(x))
#   post = post/sum(post)
#   return(post)
# }
# 
# g1.post = function(data, theta) dgamma(theta, grp.1+ sum(data), length(data)+1)
# 
# plot(theta, g1.post(ivDat, theta), type='l')
# lines(theta, g1.post.grid(ivDat, theta), col=2)
# 
# 
# g2.post.grid = function(data, theta){
#   post = sapply(theta, function(x) prod(lik(data, x)) * g2.prior(x))
#   post = post/sum(post)
#   return(post)
# }
# 
# g2.post = function(data, theta) dgamma(theta, grp.2+ sum(data), length(data)+1)
# 
# plot(theta, g2.post(ivDat, theta), type='l')
# lines(theta, g2.post.grid(ivDat, theta), col=2)
# 
# plot(theta, g2.post(ivDat, theta), type='l')
# lines(theta, g2.post.grid(ivDat, theta), col=2)
# lines(theta, g1.post(ivDat, theta), lty=2)
# lines(theta, g1.post.grid(ivDat, theta), lty=2, col=2)


