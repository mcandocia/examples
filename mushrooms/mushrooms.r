library(plyr)
library(rpart)
library(rpart.plot)

setwd('/hddl/workspace/mushrooms')

#dictionary made in emacs using replace-regexp and M+shift+5
source('vardict.r')

var_dict[['edibility']] = list('p' = 'poisonous', 'e'='edible')


mushrooms = read.csv('mushrooms.csv')
names(mushrooms)[1] = 'edibility'

remap_variable <- function(dfc, dict)
  return(plyr::mapvalues(dfc, from=names(dict), to=unlist(dict)))

for (name in names(var_dict)){
  mushrooms[,name] = remap_variable(mushrooms[,name], var_dict[[name]])
}

#check dimensionality of columns
plyr::colwise(function(x)length(levels(x)))(mushrooms)
#check columns for missing values
plyr::colwise(function(x) sum(is.nan(x)))(mushrooms)

#veil.type only has one value (partial), so remove
mushrooms$veil.type = NULL

library(randomForest)


###OLD
if (FALSE){
  rf = randomForest(edibility ~ ., data=mushrooms)
  
  logistic_model = glm(edibility ~ ., data=mushrooms, family='binomial')
  step_logistic = step(logistic_model)
  
  #cntrl_general gives a good, general model
  cntrl_general = rpart.control(minsplit=15,cp=0.005)
  #cntrl_specific gives a model that is 100% accurate, but might be overfitting
  cntrl_specific = rpart.control(minsplit=2, cp=0.001)
  
  tree = rpart(edibility ~ ., data=mushrooms, control=cntrl_general)
  
  rpart.plot(tree,type=4)
  preds = predict(tree, mushrooms[,-1], type='class')
  table(mushrooms$edibility, preds)
  
  tree_specific = rpart(edibility ~ ., data=mushrooms, control=cntrl_specific)
  
  rpart.plot(tree_specific,type=4)
  preds_specific= predict(tree_specific, mushrooms[,-1], type='class')
  table(mushrooms$edibility, preds_specific)
}

##COMPARE ALGORITHMS
library(caret)
n = 10

#n = number of folds
nfold_validation <- function(data, model_func, n, seed = 151, 
                              ptype='class', ...){
  set.seed(seed)
  N = nrow(data)
  folds = createFolds(1:N, n)
  training = list()
  testing = list()
  for (i in 1:n){
    model = model_func(edibility ~ ., data = data[unlist(folds[-i]),], ...)
    #training error
    preds = predict(model, data[unlist(folds[-i]),-1], type=ptype)
    if (!is.null(dim(preds)))
      preds = preds[,2]
    training[[i]] = cbind(as.numeric(data[unlist(folds[-i]),1])-1, preds)
    #testing error
    preds = predict(model, data[folds[[i]],-1], type=ptype)
    if (!is.null(dim(preds)))
        preds = preds[,2]
    testing[[i]] = cbind(as.numeric(data[folds[[i]],1])-1, preds)
  }
  return(list(training=do.call(rbind,training), testing=do.call(rbind,testing)))
}

#true positive rate, TP/(TP + FN)
tpr <- function(data, cutoff){
  if (max(data[,2]) == 2)
    data[,2] = data[,2] -1 
  val = data[,2] >= cutoff
  TP = sum(data[,1]==1 & val)
  FN = sum(data[,1]==1 & !val)
  return(TP/(TP+FN))
}

#false positive rate, FP/(FP + TN)
fpr <- function(data, cutoff){
  if (max(data[,2]) == 2)
    data[,2] = data[,2] -1 
  val = data[,2] >= cutoff
  FP = sum(data[,1]==0 & val)
  TN = sum(data[,1]==0 & !val)
  return(FP/(FP+TN))
}

evaluate_model <- function(results, cutoffs = seq(0, 1, 0.01)){
  trainmat <- data.frame(cutoff=cutoffs, TPR = NA, FPR = NA)
  testmat <- data.frame(cutoff=cutoffs, TPR = NA, FPR = NA)
  N = length(cutoffs)
  for (i in 1:N){
    trainmat[i,'TPR'] = tpr(results[['training']], cutoffs[i])
    trainmat[i,'FPR'] = fpr(results[['training']], cutoffs[i])
    testmat[i,'TPR'] = tpr(results[['testing']], cutoffs[i])
    testmat[i,'FPR'] = fpr(results[['testing']], cutoffs[i])
  }
  return(list(train=trainmat, test=testmat))
}

logistic_results <- nfold_validation(mushrooms, glm, n, ptype='response',
                                     family=binomial)
logistic_eval <- evaluate_model(logistic_results)

#cntrl_general gives a good, general model
cntrl_general = rpart.control(minsplit=15,cp=0.005)
#cntrl_specific gives a model that is 100% accurate, but might be overfitting
cntrl_specific = rpart.control(minsplit=2, cp=0.001)

rpart_results = nfold_validation(mushrooms, rpart, n, ptype='vector', control = cntrl_general)
rpart_eval = evaluate_model(rpart_results)

rpart_specific_results = nfold_validation(mushrooms, rpart, n, ptype='vector', control = cntrl_specific)
rpart_specific_eval = evaluate_model(rpart_results)

#random forest
rf_results = nfold_validation(mushrooms, randomForest, n, ptype='prob')
rf_eval = evaluate_model(rf_results)



par(mfrow=c(2,2))

plot(rf_eval$test$FPR, rf_eval$test$TPR, main="Random Forest Test Classification ROC", 
     xlab='False Positive Rate', ylab='True Positive Rate', type='l')
plot(logistic_eval$test$FPR, logistic_eval$test$TPR, main="Logistic Regression Test Classification ROC", 
     xlab='False Positive Rate', ylab='True Positive Rate', type='l')

plot(rpart_eval$test$FPR, rpart_eval$test$TPR, main="CART Tree (general) Test Classification ROC", 
     xlab='False Positive Rate', ylab='True Positive Rate', type='l', ylim = c(0,1))
plot(rpart_specific_eval$test$FPR, rpart_specific_eval$test$TPR, main="CART Tree (specific) Test Classification ROC", 
     xlab='False Positive Rate', ylab='True Positive Rate', type='l', ylim=c(0,1))
