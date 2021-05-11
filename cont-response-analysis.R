require(tidyverse)
require(ggthemes)
require(MASS, exclude = 'select')
library(tree)
library(randomForest)
library(gbm)

set.seed(1234)

cont_dat = read_csv("cont-response-may10.csv") %>%
    mutate_at(vars(-date, -key, -pos_rate), as.factor) %>%
    mutate(pos_rate = log(pos_rate)) # log-transform throughout

s <- sample(nrow(cont_dat), nrow(cont_dat) %/% 3)
cont_train <- cont_dat[-s,]
cont_test <- cont_dat[s,]

### Model Fitting

## linear regression
lm.fit = lm(pos_rate~. - date - key, data = cont_train)
summary(lm.fit)
lm_pred <- predict(lm.fit, cont_test)

## regression Trees

#basic full tree
tree.fit = tree(pos_rate~. -date - key, data=cont_train)                     
summary(tree.fit)
plot(tree.fit)
text(tree.fit)

#try pruning the tree
#first need to find the best split w/out cross validation(maybe?)
min.mse = 10
num.splits = 0
for(i in 2:10){
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

## the number of splits (without cv/random sampling) was all 10
pruned.tree = prune.tree(tree.fit, best=num.splits)
plot(pruned.tree)
text(pruned.tree)
tree_pred = predict(pruned.tree, cont_test)

## Random forests
rf6 = randomForest(pos_rate~. -date - key, data=cont_train, mtry = 6, importance = TRUE)
rf6.pred = predict(rf6, cont_test)

rf3 = randomForest(pos_rate~. -date - key, data=cont_train, mtry = 3, importance = TRUE)
rf3.pred = predict(rf3, cont_test)


### Performance: calculate validation set MSE

tibble(
    lm = lm_pred,
    tree = tree_pred,
    rf3 = rf3.pred,
    rf6 = rf6.pred
) %>%
    map(~mean((.x - cont_test$pos_rate)^2))

## interpretation: find some of our most important random forest predictors
importance(rf6) %>%
    as_tibble(rownames = 'Variable') %>%
    ggplot(aes(reorder(Variable, `%IncMSE`), `%IncMSE`, fill = `%IncMSE`)) +
    geom_col() +
    coord_flip() +
    labs(x = 'Predictor') +
    theme_few() +
    theme(legend.position = 'none')
    
    
