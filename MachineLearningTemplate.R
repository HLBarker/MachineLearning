# Machine Learning Template
# Author = Hilary Barker
# 08/08/2017

## Load packages
library(tidyverse)
library(caret)
library(caretEnsemble)
library(plotly)
source("~/Documents/Data Science/R_Workshop_Hope_041117/panelutils.R")


## Load + explore data
data <- read.csv("~/Downloads/data.csv")

data %>% 
  apply(function(x) sum(is.na(x))) # need data imputation?

op <- par(mfrow = c(1,1), pty = "s")
pairs(data, lower.panel = panel.smooth, upper.panel = panel.cor, 
      method = "pearson", diag.panel = panel.hist, 
      main = "Bivariate plots with histograms and smooth curves")
par(op)


## Transform data
preprocessParams <- preProcess(data[,1:4], method=c("scale")) # calculate the pre-process parameters from the dataset
 # "scale"
 # "center"
 # standardize = c("center", "scale")
 # normalize = "range"
 # "BoxCox"
 # "YeoJohnson" (supports raw values equal to zero or negative, unlike BoxCox)
 # Principal components = c("center", "scale", "pca")
 # Independent components = c("center", "scale", "ica") n.comp = X
print(preprocessParams) # summarize transform parameters
transformed <- predict(preprocessParams, data[,1:4]) # transform the dataset using the parameters
summary(transformed) # summarize the transformed dataset


## Remove redundant features
correlationMatrix <- cor(data[ , 1:8])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)
print(highlyCorrelated) # remove attributes with an absolute correlation of >= 0.75


## Rank features by importance
control <- trainControl(method="repeatedcv", number = 10, repeats = 3) # prepare training scheme
model <- train(response ~ ., data = data, method = "lvq", preProcess = "scale", trControl = control) # train the model
importance <- varImp(model, scale = FALSE) # estimate variable importance
print(importance) # summarize importance
plot(importance) # plot importance


## Feature selection
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10) # define the control using a random forest selection function
results <- rfe(data[,1:8], response, sizes = c(1:8), rfeControl = control) # run the RFE algorithm
print(results) # summarize the results
predictors(results) # list the chosen features
plot(results, type=c("g", "o")) # plot the results

data %>% 
  select(predictors that are important/not correlated)


## Spot-checking ML algorithms
data %>%  # subset data for testing if the original dataset is large
  sample_n(number of rows, replace = FALSE) # could also use sample_frac for fractional splits
 # may need to test this out with a glm to assess how big this test set needs to be for quick training (~30s)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy" # define test metric to compare models
 # classification = "Accuracy" or "Kappa" (accuracy that takes the base distribution of classes into account)
 # regression = "RMSE" root mean square error

algorithms <- c("lda", "glm", "glmnet", "svmRadial", "knn", "nb", "rpart", "C5.0", "treebag", "rf", "gbm")
 # select algorithms, 10-20 with varying complexity (use caret's default tuning functions)
 # http://machinelearningmastery.com/spot-check-machine-learning-algorithms-in-r/

models <- caretList(response ~ ., data = dataset, trControl = control, methodList = algorithms)
results <- resamples(models)
summary(results)
dotplot(results)


## Tune best ML algorithms
 # identify the parameters to tune via the help page for each ML model function
 # e.g., for random forests, this is mtry
mtry <- sqrt(ncol(predictor data)) # default for tuning parameter (e.g., 7)
tunegrid <- expand.grid(.mtry = c(1:15)) # grid search with 1 through 15 (for example) as mtry
setseed(seed)
rf_gridsearch <- train(response ~ ., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
dotplot(rf_gridsearch)


## Ensemble modeling
 # bagging - building multiple models (typically of the same type) from different subsamples of the training data
   # bagged CART (treebag) and random forest (rf)
# boosting - building multiple models (typically of the same type) with of which learns to fix the prediction errors of a prior model in the chain
   # C5.0 and stochastic gradient boosting (gbm)
 # stacking - building multiple models (typically of differing types) and supervisor model that learns how to best combine the predictions of the primary models

control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(seed)
models <- caretList(response ~ ., data = dataset, trControl = control, methodList = algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

modelCor(results) # sub-models that are highly correlated (>0.75) are not useful and should be pruned
splom(results)

# stack using glm (for example)
set.seed(seed)
stack.glm <- caretStack(models, method = "glm", metric = "Accuracy", trControl=stackControl) # stack models with glm
  # try out different kinds of models to stack to see which one is most accurate
print(stack.glm)


