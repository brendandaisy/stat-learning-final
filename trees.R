require(tidyverse)
require(zoo)
require(ggthemes)

setwd("C:/Users/lafla/Downloads/stat-learning-final-master (1)/stat-learning-final-master")

cont_train = read.csv("cont-train.csv")
cont_test = read.csv("cont-test.csv")

disc_train = read.csv("disc-train.csv")
disc_test = read.csv("disc-test.csv")

#random forest doesnt like na's, so remove them with complete.cases
cont_test = cont_test[complete.cases(cont_test), ]
cont_train = cont_train[complete.cases(cont_train), ]

disc_test = disc_test[complete.cases(disc_test), ]
disc_train = disc_train[complete.cases(disc_train), ]

#for comparison
lm.fit = lm(pos_rate~., data = cont_train[,!colnames(cont_train) %in% c("date", "key")])
lm.pred = predict(lm.fit, cont_test)
mean((lm.pred-cont_test$pos_rate)^2)
#lm seems to produce an MSE of 0.01611311?

##Regression Trees--------------------------------------------------------------
library(tree)
library(randomForest)
library(gbm)

#basic full tree
tree.fit = tree(pos_rate~. -date - key, data=cont_train)                     
summary(tree.fit)
plot(tree.fit)
text(tree.fit)
tree.pred = predict(tree.fit, cont_test)
mean((tree.pred-cont_test$pos_rate)^2)
# a regression tree w/ full model produces an MSE of 0.01171584

#try pruning the tree
#first need to find the best split w/out cross validation(maybe?)
min.mse = 10
num.splits = 0
for(i in 2:13){
  pruned.tree = prune.tree(tree.fit, best=i)
  tree.pred = predict(pruned.tree, cont_test)
  mse = mean((tree.pred-cont_test$pos_rate)^2)
  if(mse<min.mse){
    min.mse = mse
    num.splits = i
  }
}

#best MSE with ideal number of splits
min.mse
num.splits

#the number of splits (without cv/random sampling) that produced the best MSE
#was 5 splits
pruned.tree = prune.tree(tree.fit, best=num.splits)
plot(pruned.tree)
text(pruned.tree)
tree.pred = predict(pruned.tree, cont_test)
mean((tree.pred-cont_test$pos_rate)^2)
#pruned tree w best = 5 produces and MSE of 0.0101623

#move from a single tree to a handful, random forests
set.seed(1)
tree.rf = randomForest(pos_rate~. -date - key, data=cont_train, mtry = 6, importance = TRUE)
rf.pred = predict(tree.rf, cont_test)
mean((rf.pred-cont_test$pos_rate)^2)
#random forest produces slightly worse MSE of 0.01199998

#find some of our most important random forest predictors
importance(tree.rf)



##Classification Trees----------------------------------------------------------

#full tree
disc_train$dir = factor(disc_train$dir)
disc_test$dir = factor(disc_test$dir)
tree.fit = tree(dir~. -date - key, data=disc_train)
summary(tree.fit)
plot(tree.fit)
tree.pred = predict(tree.fit, disc_train)
tree.res = ifelse(tree.pred<0.5, 0, 1)
table(tree.res, disc_test$dir)

#random Forest
set.seed(1)
tree.rf = randomForest(dir~. -date - key, data=disc_train, mtry = 3, importance = TRUE)
rf.pred = predict(tree.rf, disc_test)
rf.res = ifelse(rf.pred < 0.5, 0, 1)
table(rf.res, disc_test$dir)

glm.fit = glm(dir~., data = disc_train[,!colnames(disc_train) %in% c("date", "key")], family = binomial)
glm.prob = predict(glm.fit, disc_test)
glm.pred <- ifelse(glm.prob < 0.5, 0, 1)
table(glm.pred, disc_test$dir)
