require(tidyverse)
require(zoo)
require(ggthemes)

setwd("C:/Users/lafla/Downloads/stat-learning-final-master")

cont_train = read_csv("cont-train.csv") %>%
    mutate_at(
        vars(
            school_closing:restrictions_on_internal_movement,
            debt_relief,
            testing_policy:contact_tracing,
            facial_coverings
        ),
        as.factor
    )

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
tree.fit = tree(dir~. -date - key, data=disc_train )
summary(tree.fit)
tree.pred = predict(tree.fit, disc_test, type = "class")
table(tree.pred, disc_test$dir)
#classification cannot produce anything more than a single node tree
#Predicts Up every time
#41 percent accurate on the test data


#random Forest
set.seed(1)
tree.rf = randomForest(dir~. -date - key, data=disc_train, mtry = 2, importance = TRUE, ntree = 15)
rf.pred = predict(tree.rf, disc_test)
table(rf.pred, disc_test$dir)
#produces better results
#50 percent accurate, pretty much exactly the same as flipping a coin
importance(tree.rf)

#logistic regression(for comparison)
glm.fit = glm(dir~., data = disc_train[,!colnames(disc_train) %in% c("date", "key")], family = binomial)
glm.prob = predict(glm.fit, disc_test)
glm.pred <- ifelse(glm.prob < 0.5, 0, 1)
table(glm.pred, disc_test$dir)
#57 percent accurate
#logistic regression performs better for classification

##LDA---------------------------------------------------------------------------
library(MASS)
lda.fit = lda(dir~., data=disc_train[,3:10])
lda.fit
lda.pred = predict(lda.fit, disc_test[,3:10], type="class")
table(lda.pred$class, disc_test$dir)
#initial results are about 42 pecent accurate

lda.fit = lda(dir~mobility_retail_and_recreation+mobility_grocery_and_pharmacy+mobility_parks, data=disc_train[,3:10])
lda.fit
lda.pred = predict(lda.fit, disc_test[,3:10], type="class")
table(lda.pred$class, disc_test$dir)
#46 pecent accurate after decreasing model to just 3 variables
