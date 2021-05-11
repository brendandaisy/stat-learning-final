library(tree)
library(randomForest)
library(gbm)
require(tidyverse)
require(ggthemes)
require(MASS, exclude = 'select')

disc_dat <- read_csv('disc-response-may10.csv')
set.seed(10101)

### Create the validation set
disc_test <- disc_dat %>%
    mutate(month = format(date, format = '%m/%y')) %>%
    filter(month != '05/21') %>%
    group_by(month, key) %>%
    slice_sample(prop = 1/3) %>%
    ungroup %>%
    select(-month)

disc_train <- anti_join(disc_dat, disc_test)

### Model fitting

## logistic regression
glm.fit = glm(dir ~ . - date - key - sev_day, data = disc_train, family = binomial)
glm.prob = predict(glm.fit, disc_test)
glm.pred <- ifelse(glm.prob < 0.5, 0, 1)
tab_glm <- table(glm.pred, disc_test$dir)

## full tree
disc_train$dir = factor(disc_train$dir)
disc_test$dir = factor(disc_test$dir)
tree.fit = tree(
    dir ~ . -date - key - sev_day,
    data=disc_train,
    control = tree.control(mindev = .001, nobs = nrow(disc_train))
)
summary(tree.fit)
tree.pred = predict(tree.fit, disc_test, type = "class")
tab_tree <- table(tree.pred, disc_test$dir)


#random Forest
rf1 = randomForest(
    dir~. -date - key - sev_day,
    data=disc_train,
    mtry = 1,
    importance = TRUE
)

rf1.pred = predict(rf1, disc_test)
tab_rf1 <- table(rf1.pred, disc_test$dir)

rf3 = randomForest(
    dir~. -date - key - sev_day,
    data=disc_train,
    mtry = 3,
    importance = TRUE
)

rf3.pred <- predict(rf3, disc_test)
tab_rf3 <- table(rf3.pred, disc_test$dir)

##LDA---------------------------------------------------------------------------
lda.fit = lda(dir ~ . -date - key - sev_day, data=disc_train)
lda.fit
lda.pred = predict(lda.fit, disc_test, type="class")
tab_lda <- table(lda.pred$class, disc_test$dir)

## QDA
qda.fit = qda(dir ~ . -date - key - sev_day, data=disc_train)
qda.fit
qda.pred = predict(qda.fit, disc_test, type="class")
tab_qda <- table(qda.pred$class, disc_test$dir)


### Model performance
tabs <- list(glm = tab_glm, tree = tab_tree, rf1 = tab_rf1, rf3 = tab_rf3, lda = tab_lda, qda = tab_qda)

perf <- map_dfr(tabs, ~{
    tibble_row(
        Sensitivity = .x[2, 2] / sum(.x[,2]),
        Specificity = .x[1, 1] / sum(.x[,1]),
        Accuracy = (.x[1, 1] + .x[2, 2]) / sum(.x)
    )
}) %>%
    mutate(Model = names(tabs))

perf %>%
    pivot_longer(-Model, 'Metric', values_to = 'Value') %>%
    ggplot(aes(Model, Value, fill = Metric)) +
    geom_col(position = position_dodge()) +
    theme_minimal()
