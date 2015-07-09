setwd('C:/users/max/dropbox/stat 542/HW3')
load('spamHW3.Rdata')
N = dim(spam.train)[1]
library(MASS)
library(lars)
library(glmnet)
library(klaR)
library(caret)
library(e1071)
library(kernlab)
library(randomForest)
library(gbm)
test = spam.test[,!names(spam.test) %in% 'Y']
train = spam.train[,!names(spam.train) %in% 'Y']
Ytr = as.factor(spam.train$Y)
Yte = as.factor(spam.test$Y)
lN = log(N)

rawnum <-function(x) as.numeric(as.character(x))

#find RDA from code in lecture notes

##PROBLEM 1: LOGISTIC REGRESSION (FULL, AIC, BIC, LASSO)##
mnull = glm(Y~1,data=spam.train,family=binomial(link=logit))
full = glm(Y~.,data=spam.train,family=binomial(link=logit))
aic = step(mnull,direction='forward',scope=formula(full))
bic = step(mnull,k=lN,direction='forward',scope=formula(full))

model.design = as.matrix(spam.train[,!names(spam.train) %in% 'Y'])

lambda = cv.glmnet(model.design,spam.train$Y,family='binomial',alpha=1)
lambda.min = lambda$lambda.min
lasso=glmnet(model.design,spam.train$Y,family='binomial',alpha=1,lambda=lambda.min)

lasso.preds = predict(lasso,as.matrix(test),type='response')
aic.preds = predict(aic,test,type='response')
bic.preds = predict(bic,test,type='response')
full.preds = predict(full,test,type='response')


aic_names = names(coef(aic))[-1]
bic_names = names(coef(bic))[-1]
lasso_names = rownames(coef(lasso))[(as.numeric(coef(lasso))!=0)][-1]

#union of variables
#currently excluding LASSO
selected = union(aic_names,union(bic_names,lasso_names))

test.s = test[,selected]
train.s = train[,selected]



##PROBLEM 2: LDA, RDA, and Naive Bayes
ltp <-function(x){ exp(x)/(1+exp(x))}

data.tr = cbind(Y=Ytr,train)
data.tr.s = cbind(Y=Ytr,train.s)
data.te = cbind(Y=Yte,test)
data.te.s = cbind(Y=Yte,test.s)

lda.1 = lda(Y~.,data.tr)
lda.2 = lda(Y~.,data.tr.s)
lda.1.preds = predict(lda.1,data.te,method='plug-in')$posterior[,2]
lda.2.preds = predict(lda.2,data.te.s,method='plug-in')$posterior[,2]

#annealing to reduce occurence of NA values
rda.1 = rda(Y~.,data.tr,simAnn=T)
rda.2 = rda(Y~.,data.tr.s,simAnn=T)
rda.1.preds = predict(rda.1,data.te,method='plug-in')
rda.2.preds = predict(rda.2,data.te.s,method='plug-in')
#fix NA values
rda.1.preds$posterior = ifelse(is.na(rda.1.preds$posterior[,2]),
 rawnum(rda.1.preds$class),rda.1.preds$posterior[,2])
rda.2.preds$posterior = ifelse(is.na(rda.2.preds$posterior[,2]),
 rawnum(rda.2.preds$class),rda.2.preds$posterior[,2])

rda.1.preds = rda.1.preds$posterior
rda.2.preds = rda.2.preds$posterior

#bayes classifier
#need to speed up algorithm by changed to x-validation instead of bootstrap
tc = trainControl(method = 'cv', number = 10)
bayes_nonp.1 = train(train,as.factor(Ytr),method='nb',trControl = tc)
bayes_nonp.2 = train(train.s,as.factor(Ytr),method='nb',trControl = tc)

bayes_par.1 = train(data.tr,as.factor(Ytr),method='nb',tuneGrid=data.frame(usekernel=F,fL=F),
 trControl = tc)
bayes_par.2 = train(data.tr.s,as.factor(Ytr),method='nb',tuneGrid=data.frame(usekernel=F,fL=F),
 trControl = tc)

#bayes_nonp.1.preds = predict(bayes_nonp.1$finalModel,test)$posterior[,2]
#bayes_nonp.2.preds = predict(bayes_nonp.2$finalModel,test.s)$posterior[,2]

bayes_par.1.predsf = predict(bayes_par.1$finalModel,test)
bayes_par.2.predsf = predict(bayes_par.2$finalModel,test.s)

bayes_par.1.preds = bayes_par.1.predsf$posterior[,2]
bayes_par.2.preds = bayes_par.2.predsf$posterior[,2]

bayes_par.1.preds = ifelse(is.na(bayes_par.1.preds),
 rawnum(bayes_par.1.predsf$class),bayes_par.1.preds)

bayes_par.2.preds = ifelse(is.na(bayes_par.2.preds),
 rawnum(bayes_par.2.predsf$class),bayes_par.2.preds)

#try using e1071
bayes.nonp.1 = NaiveBayes(train,as.factor(Ytr),type='raw',usekernel=T)
bayes.nonp.1.preds = predict(bayes.nonp.1,test,type='raw')$posterior[,2]
bayes.nonp.2 = NaiveBayes(train.s,as.factor(Ytr),type='raw',usekernel=T)
bayes.nonp.2.preds = predict(bayes.nonp.2,test.s,type='raw')$posterior[,2]
#results are effectively the same for both nonparametric models
#try using klaR for parametric
library(klaR)
bayes.par.1=naiveBayes(train,as.factor(Ytr),usekernel=F)
bayes.par.1.preds=predict(bayes.par.1,test,type='raw')[,2]#$posterior[,2]

bayes.par.2=naiveBayes(train.s,as.factor(Ytr),usekernel=F)
bayes.par.2.preds=predict(bayes.par.1,test.s,type='raw')[,2]#$posterior[,2]

#these models are better
##PROBLEM 3: SVM
#methods take kind of a long time...use a smaller grid search
lin.pars = expand.grid(C=1:5)#redundant
quad.pars = expand.grid(C=1:5,scale = 1:5,degree=2)

lin.cv = numeric(5)
quad.cv = numeric(25)
trControl = trainControl(method='cv')
gaus.cv = numeric(15)

#save(list=ls(all=TRUE),file='preSVM.RData')
load(file='preSVM.RData')
for (i in 1:5){
	model = ksvm(Y~.,data=data.tr,C=i,kernel='vanilladot',cross=5,cache=50,
	 tol=1e-5)
	lin.cv[i] = cross(model)
}

#cannot run properly; extremely slow and causes RGui graphics to distort
#for (i in 1:25){
#	model = ksvm(Y~.,data=data.tr,kernel='polydot',cross=5,
#	 C=quad.pars[i,'C'],
#	 kpar=list(degree=2,scale=quad.pars[i,'scale'],offset=1),cache=50,
#	 tol=1e-5)
#	quad.cv[i] = cross(model)
#	print(i)
#}

quad.cv = train(Y~.,data=data.tr,method='svmPoly',tol=1e-5,
 tuneGrid=quad.pars,trControl=trainControl(method='cv',number=5))
#save(list=ls(all=TRUE),file='midSVM.RData')

for (i in 1:15){
	model = ksvm(Y~.,data=data.tr,kernel='rbfdot',cross=5,
	 C=i,cache=50,tol=1e-5)
	gaus.cv[i] = cross(model)
}
save(list=ls(all=TRUE),file='postSVM.RData')

lin.C = which.min(lin.cv)
gaus.C = which.min(gaus.cv)#8
quad.C = quad.cv$bestTune$C
quad.scale = quad.cv$bestTune$scale

#need to relevel Y because some idiot decided to use variable levels as data frame names
#library(plyr)
#nata.tr = data.tr; nata.tr$Y = mapvalues(nata.tr$Y,from=0:1,to=c('v0','v1'))
#nata.tr.s = data.tr.s; nata.tr.s$Y = mapvalues(nata.tr.s$Y,from=0:1,to=c('v0','v1'))
#nata.te = data.te; nata.te$Y = mapvalues(nata.te$Y,from=0:1,to=c('v0','v1'))
#nata.te.s = data.te.s; nata.te.s$Y = mapvalues(nata.te.s$Y,from=0:1,to=c('v0','v1'))

svm.lin.1 = ksvm(Y~.,data=data.tr,kernel='vanilladot',C=lin.C,tol=1e-5,
 prob.model=T)
svm.lin.2 = ksvm(Y~.,data=data.tr.s,kernel='vanilladot',C=lin.C,tol=1e-5,
 prob.model=T)
svm.quad.1 = ksvm(Y~.,data=data.tr,kernel='polydot',tol=1e-5,
 C=quad.C,kpar=list(scale=quad.scale,degree=2,offset=1),prob.model=T)
svm.quad.2 = ksvm(Y~.,data=data.tr.s,kernel='polydot',tol=1e-5,
 C=quad.C,kpar=list(scale=quad.scale,degree=2,offset=1),prob.model=T)
svm.gaus.1 = ksvm(Y~.,data=data.tr,kernel='rbfdot',C=gaus.C,tol=1e-5,
 prob.model=T)
svm.gaus.2 = ksvm(Y~.,data=data.tr.s,kernel='rbfdot',C=gaus.C,tol=1e-5,
 prob.model=T)

svm.lin.1.preds = predict(svm.lin.1,data.te,type=c('probabilities'))[,2]
svm.lin.2.preds = predict(svm.lin.2,data.te.s,type=c('probabilities'))[,2]
svm.quad.1.preds = predict(svm.quad.1,data.te,type='probabilities')[,2]
svm.quad.2.preds = predict(svm.quad.2,data.te.s,type='probabilities')[,2]
svm.gaus.1.preds = predict(svm.gaus.1,data.te,type='probabilities')[,2]
svm.gaus.2.preds = predict(svm.gaus.2,data.te.s,type='probabilities')[,2]

##PROBLEM 4: Classification Tree
#save(list=ls(all=T),file='preProblem4.RData')
library(tree)
sprinkler = tree.control(nobs=dim(data.tr)[1],minsize=10,mincut=5,mindev=0.007)
tree.1 = tree(Y~.,data=data.tr,split='deviance',control=sprinkler)
plot(tree.1);text(tree.1)

cut.tree<-function(tree){
devi = tree$frame$dev
nodes = as.numeric(row.names(tree$frame))
n = length(nodes)
alphas = numeric(n)
for (i in 1:n){
	if (sum(nodes==2*nodes[i])>0){
		L = 2*nodes[i]
		R = L+1
		alphas[i] = devi[i]-devi[nodes==L]-devi[nodes==R]
	}
}
mini = which.min(alphas+ifelse(alphas==0,Inf,0))
 print(alphas)
 snip.tree(tree,nodes[mini]) 
}

cut.tree(tree.f.1)
tree.cv = cv.tree(tree.f.1,eps=1e-5)
kbest = tree.cv$size[which.min(tree.cv$dev+tree.cv$size*lN)]
tree.15=tree.1
tree.5=tree.1

while(summary(tree.5)$size>5){
tree.5 = cut.tree(tree.5)
}
while(summary(tree.15)$size>15)
tree.15=cut.tree(tree.15)
tree.4=cut.tree(tree.5)

tree.15.preds = predict(tree.15,newdata=data.te)[,2]
tree.5.preds = predict(tree.5,newdata=data.te)[,2]
png('hw3_trees.png',height=400,width=800)
par(mfrow=c(1,2))
plot(tree.5,main='Classification Tree with 5 Leaves');text(tree.5)
plot(tree.4,main='Classification Tree with 4 Leaves');text(tree.4)
dev.off()

#tree.1=prune.tree(tree.f.1,best=kbest)
#tree.2=prune.tree(tree.2,best=kbest)
#tree.preds = predict(tree.2,data=data.te)[,2]

##PROBLEM 5: RANDOM FOREST
forest = randomForest(Y~.,data=data.tr)
forest.preds = predict(forest,data.te,type='prob')[,2]
library(reshape2)
forest.i = as.data.frame(forest$importance)
forest.i$variable = rownames(forest$importance)

forest.i=plyr::arrange(forest.i,forest.i$MeanDecreaseGini)
forest.i$variable = factor(forest.i$variable,levels=as.character(forest.i$variable))
png('hw3_importance.png',height=700,width=400)
print(
ggplot(forest.i,aes(x=variable,y=MeanDecreaseGini))+
 geom_bar(stat='identity')+coord_flip()+ylab("Mean Decrease in Gini Impurity")+
 ggtitle("Variable Importances for Random Forest Model")
)
dev.off()
plot(forest$importance)



##PROBLEM 6: ADABOOST
data.tr.g = data.tr
data.tr.g$Y = rawnum(data.tr.g$Y)
data.te.g = data.te
data.te.g$Y = rawnum(data.te.g$Y)

#ntrees=seq(100,500,10)
#a huge issue with these is overfitting error...
#gbm.1=gbm(Y~.,data=data.tr,distribution='adaboost',n.trees=25000,cv.folds=10,
#interaction.depth=2,bag.fraction=1)
#gbm.1.shrink=gbm(Y~.,data=data.tr,distribution='adaboost',n.trees=20000,cv.folds=10,
#interaction.depth=2,bag.fraction=1,shrinkage=0.0005)
#gbm.1.shrink2=gbm(Y~.,data=data.tr,distribution='adaboost',n.trees=20000,cv.folds=10,
#interaction.depth=2,bag.fraction=1,shrinkage=0.00001)
#gbm.1.shrink2.1 = gbm.more(gbm.1.shrink2,n.new.trees=8000)
#gbm.1.shrink2.2 = gbm.more(gbm.1.shrink2.1,n.new.trees=16000)
#gbm.1.shrink2.3 = gbm.more(gbm.1.shrink2.2,n.new.trees=36000)
#gbm.1.shrink2.4 = gbm.more(gbm.1.shrink2.3,n.new.trees=36000)
#gbm.1.shrink2.5 = gbm.more(gbm.1.shrink2.4,n.new.trees=36000)
#gbm.1.shrink3=gbm(Y~.,data=data.tr,distribution='adaboost',n.trees=20000,
#cv.folds = 10,interaction.depth=3,bag.fraction=1,shrinkage=0.00001)
#gbm.1.shrink3a=gbm(Y~.,data=data.tr.g,distribution='adaboost',n.trees=12000,
#cv = 10,interaction.depth=3,bag.fraction=1,shrinkage=0.00001,train.fraction=0.5)
#gbm.1.shrink4=gbm(Y~.,data=data.tr.g,distribution='adaboost',n.trees=2500,
#shrinkage=0.01,bag.fraction=1,cv.folds=5,train.fraction=0.5)

gbm.perf(gbm.1.shrink4)
#try ada again


#(ada.preds=predict(gbm.1.shrink3,data.te,type='link',n.trees=2002))

#gbm.shrink5<-gbm(Y~.,distribution='adaboost',data=data.tr.g,
# n.trees=500,interaction.depth=2,keep.data=T,n.minobsinnode=4,shrinkage=0.01,
# cv.folds=5,train.fraction=0.5)

gbm.s6 = gbm(Y~.,distribution='adaboost',data=data.tr.g,n.trees=5000,
 interaction.depth=2,keep.data=T,n.minobsinnode=4,shrinkage=0.01,cv.folds=5,
 train.fraction=0.7)

png('hw3_adaboost.png')
gbm.perf(gbm.s6,method='cv')#choose 1,189 trees
dev.off()

gbm.final = gbm(Y~.,distribution='adaboost',data=data.tr.g,n.trees=1189,
interaction.depth=2,keep.data=T,n.minobsinnode=4,shrinkage=0.01)

gbm.preds=predict(gbm.final,data.te.g,n.trees=1189,type='response')

save(list=ls(all=T),file='adaboost.RData')
##PROBLEM 7: NEURAL NETWORKS
grid=expand.grid(size=seq(1,32,4),decay = c(0,0.1,0.01,0.001,0.0001,0.00005))

nnet.1 = train(train,Ytr,method='nnet',trControl=trainControl(method='cv',
 number=5),maxit=2500,tuneGrid=grid)
save(nnet.1,file='nnet.1.RData')
grid2 = expand.grid(size=10:15,decay=c(0.08,0.09,0.1,0.11,0.12))
nnet.2 = train(train,Ytr,method='nnet',trControl=trainControl(method='cv',
 number=5),maxit=2500,tuneGrid=grid2)

nnet.final = nnet(Y~.,data=data.tr,size=15,decay=0.12,maxit=2500)
nnet.preds = predict(nnet.final,data.te,type='raw')








gbm.perf(gbm.1.shrink,method='cv')

save(gbm.1,file='gbm.1.RData')

gbm.model=gbm.perf(gbm.1,method='cv')


gbm.preds = predict(gbm.1,data.te,n.trees=500)#c(100,500,1000,2000,3000))
#spits out results for any given prediction

#takes in probabilities for class 1
create_element<-function(preds,name,Y=Yte){
	Y=rawnum(Y)
	misclass=1-mean(round(preds)==Y)
	devs = ifelse(preds*(1-preds)!=0,
	 Y*log(preds)+(1-Y)*log(1-preds),
	 ifelse(preds==Y,0,-Inf))
	data.frame(model = name,'Misclass Rate' = misclass,'LogLikelihood' = sum(devs))
}
model_frame=rbind(
create_element(aic.preds,'AIC Logistic'),
create_element(bic.preds,'BIC Logistic'),
create_element(lasso.preds,'Lasso Logistic'),
create_element(full.preds,'Full Logistic'),
create_element(lda.1.preds,'LDA w/o Selection'),
create_element(lda.2.preds,'LDA with Selection'),
create_element(rda.1.preds,'RDA w/o Selection'),
create_element(rda.2.preds,'RDA with Selection'),
create_element(bayes.nonp.1.preds,'Bayes NP w/o Selection'),
create_element(bayes.nonp.2.preds, 'Bayes NP with Selection'),
create_element(bayes.par.1.preds,'Bayes Parametric w/o Selection'),
create_element(bayes.par.2.preds,'Bayes Parametric with Selection'),
create_element(svm.lin.1.preds,'Linear SVM w/o Selection'),
create_element(svm.lin.2.preds,'Linear SVM with Selection'),
create_element(svm.quad.1.preds,'Quadratic SVM w/o Selection'),
create_element(svm.quad.2.preds,'Quadratic SVM with Selection'),
create_element(svm.gaus.1.preds,'Gaussian SVM w/o Selection'),
create_element(svm.gaus.2.preds,'Gaussian SVM with Selection'),
create_element(tree.5.preds,'Tree Model (5 leaves)'),
create_element(tree.15.preds,'Tree Model (15 leaves)'),
create_element(forest.preds,'Random Forest'),
create_element(gbm.preds,'Adaboost'),
create_element(nnet.preds,'Neural Network (1 layer, 15 Nodes)')
)

save(list = ls(all=TRUE),file='nearEND.RData')
library(xtable)
xtable(model_frame,digits=4)


nvecs = data.frame(
Model=c('Linear Full','Linear Selection','Quadratic Full',
 'Quadratic Selection','Gaussian Full','Gaussian Selection'),
NumVecs = c(727,728,617,606,1031,1039))
xtable(nvecs,digits=0)

