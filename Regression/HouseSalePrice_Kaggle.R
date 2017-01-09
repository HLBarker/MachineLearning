# Author = Hilary Barker
# This script explores Kaggle's House Prices data set and analyzes various 
  # models to predict sale price.

# -----------------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------------
source("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/R/outlierKD.R")
library(leaps)
library(glmnet)
library(boot)

# -----------------------------------------------------------------------------
# Import + explore data
# -----------------------------------------------------------------------------
pred <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/HouseSalePrice/test.csv")
train <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/HouseSalePrice/train.csv")
pred$SalePrice <- 0
combined <- rbind(train, pred)
str(combined)
View(combined)

# Several variables have NAs when a feature is absent, but this
# the data isn't missing, so we'll change these NAs to "None
addnone <- function(x) {
  # Changes NAs in a factor variable into "None"
  #
  # Args:
  #  x: a factor variable
  #
  # Returns:
  #  The factor variable with "None" instead of NA
  #    as one of the factor levels
  levels <- levels(x)
  levels[length(levels) + 1] <- "None"
  x <- factor(x, levels = levels)
  x[is.na(x)] <- "None"
  x
} 

combined$Alley <- addnone(combined$Alley)
combined$FireplaceQu <- addnone(combined$FireplaceQu)
combined$PoolQC <- addnone(combined$PoolQC)
combined$Fence <- addnone(combined$Fence)
combined$GarageType <- addnone(combined$GarageType)
combined$MiscFeature <- addnone(combined$MiscFeature)
combined$BsmtQual <- addnone(combined$BsmtQual)
combined$BsmtCond <- addnone(combined$BsmtCond)
combined$BsmtExposure <- addnone(combined$BsmtExposure)
combined$BsmtFinType1 <- addnone(combined$BsmtFinType1)
combined$BsmtFinType2 <- addnone(combined$BsmtFinType2)
combined$GarageFinish <- addnone(combined$GarageFinish)
combined$GarageQual <- addnone(combined$GarageQual)
combined$GarageCond <- addnone(combined$GarageCond)
str(combined)
View(combined)


# Data transformations to increase normality + changing data types
hist(combined$SalePrice) # highly skewed (will use BoxCox transformation)
combined$MSSubClass <- as.factor(combined$MSSubClass)

hist(sqrt(combined$LotFrontage))
combined$LotFrontage <- sqrt(combined$LotFrontage)

hist(log(combined$LotArea))
combined$LotArea <- log(combined$LotArea)

hist(1/(combined$YearBuilt))
combined$YearBuilt <- 1/(combined$YearBuilt)

hist(sqrt(combined$MasVnrArea))
combined$MasVnrArea <- sqrt(combined$MasVnrArea)

hist(sqrt(combined$BsmtFinSF1))
combined$BsmtFinSF1 <- sqrt(combined$BsmtFinSF1)

hist(sqrt(combined$BsmtFinSF2))
combined$BsmtFinSF2 <- sqrt(combined$BsmtFinSF2)

hist(sqrt(combined$LowQualFinSF))
combined$LowQualFinSF <- sqrt(combined$LowQualFinSF)

hist(log(combined$GrLivArea))
combined$GrLivArea <- log(combined$GrLivArea)

hist(log(combined$TotRmsAbvGrd))
combined$TotRmsAbvGrd <- log(combined$TotRmsAbvGrd)

hist(sqrt(combined$GarageArea))
combined$GarageArea <- sqrt(combined$GarageArea)

hist(sqrt(combined$WoodDeckSF))
combined$WoodDeckSF <- sqrt(combined$WoodDeckSF)

hist(sqrt(combined$OpenPorchSF))
combined$X3SsnPorch <- sqrt(combined$OpenPorchSF)

hist(sqrt(combined$EnclosedPorch))
combined$EnclosedPorch <- sqrt(combined$EnclosedPorch)

hist(sqrt(combined$ScreenPorch))
combined$ScreenPorch <- sqrt(combined$ScreenPorch)

hist(sqrt(combined$X3SsnPorch))
combined$X3SsnPorch <- sqrt(combined$X3SsnPorch)

hist(sqrt(combined$PoolArea))
combined$PoolArea <- sqrt(combined$PoolArea)

hist(sqrt(combined$MiscVal))
combined$MiscVal <- sqrt(combined$MiscVal)

combined$TotalSF <- combined$X1stFlrSF + combined$X2ndFlrSF + combined$TotalBsmtSF 
hist(log(combined$TotalSF))
combined$TotalSF <- log(combined$TotalSF)

plot(SalePrice ~ TotalSF, data = combined) # okay predictor
plot(SalePrice ~ GrLivArea, data = combined)
plot(SalePrice ~ FullBath, data = combined)

plot(SalePrice ~ X1stFlrSF, data = combined)
hist(log(combined$X1stFlrSF))
combined$X1stFlrSF <- log(combined$X1stFlrSF)

plot(SalePrice ~ X2ndFlrSF, data = combined)
hist(sqrt(combined$X2ndFlrSF))
combined$X2ndFlrSF <- sqrt(combined$X2ndFlrSF)

plot(SalePrice ~ OverallQual, data = combined) # good predictor but needs a transformation


# -----------------------------------------------------------------------------
# Subset data into training and test sets
# -----------------------------------------------------------------------------
train2 <- combined[1:1460, 2:82]
pred2 <- combined[1461:2919, 2:82]

set.seed(5)
train.set <- sample(c(TRUE, FALSE), nrow(train2), rep = TRUE)
test = (!train.set)
#test.x <- train2[test, -80]
test.y <- train2[test, ]$SalePrice
#train.y <- train2[train.set, ]$SalePrice
#train.x <- train2[train.set, -80]

train.set.original <- sample(c(TRUE, FALSE), nrow(train), rep = TRUE)
test.original = (!train.set.original)
#test.x.original <- train[test, -80]
test.y.original <- train[test.original, ]$SalePrice
#train.y.original <- train[train.set, ]$SalePrice
#train.x.original <- train[train.set, -80]


# -----------------------------------------------------------------------------
# Hypothesis model with K-fold cross validation
# -----------------------------------------------------------------------------
# I predict, LotArea, OverallQual, TotalSF, TotRmsAbvGrd, Neighborhood, 
# MSSubClass, MSZoning, and SaleCondition are all good predictors of SalePrice
 
train3 <- cbind.data.frame(train2$SalePrice, train2$LotArea, train2$OverallQual, 
                           train2$TotalSF, train2$TotRmsAbvGrd, train2$Neighborhood,
                           train2$MSSubClass, train2$MSZoning, train2$SaleCondition)
names(train3) <- (c("SalePrice", "LotArea", "OverallQual", "TotalSF", 
                    "TotRmsAbvGrd", "Neighborhood", "MSSubClass", "MSZoning",
                    "SaleCondition"))

glm.fit <- glm(log(SalePrice) ~ ., family = gaussian, train3)
glm.cv <- cv.glm(train3, glm.fit, K=10)
glm.cv$delta # Test MSE = 0.02318266

summary(glm.fit)
anova(glm.fit)
plot(glm.fit)


# -----------------------------------------------------------------------------
# Model selection with cross validation (for numeric variables, only)
# -----------------------------------------------------------------------------
# this approach doesn't work great with factor/categorical variables, so we'll
# first extract just the numeric and integer variables from the data
nums <- sapply(train2, is.numeric)
train4 <- cbind.data.frame(train2[ , nums])
str(train4)
train4 <- na.omit(train4) # need to na.omit for model.matrix, below
y <- log(na.omit(train4[test, ]$SalePrice))

set.seed(1)
regfit.best <- regsubsets(log(SalePrice) ~ ., data = train4[train.set, ], 
                          nvmax =36, really.big = TRUE)
test.mat <- model.matrix(SalePrice ~ ., train4[test, ]) # removes any rows with missing data
val.error = rep(NA, 36)

for(i in 1:36) {
  coefi <- coef(regfit.best, id = i)
  prediction <- test.mat[, names(coefi)] %*% coefi
  val.error[i] <- mean((y-prediction)^2)
}

val.error 
which.min(val.error) # which model size has the lowest MSE?
val.error[2] # test MSE = 0.028866000
coef(regfit.best, 2) # OverallQual and TotalSF are the best numeric predictors


best.fit <- glm(log(SalePrice) ~ OverallQual + TotalSF, family = gaussian, 
                data = train2)
summary(best.fit)
plot(best.fit) # obs 1299 and 534 are outliers with high leverage
train2[534, ]
train2[1299, ]
outlierKD(train2, OverallQual)

# -----------------------------------------------------------------------------
# Lasso with cross-validation (for numeric variables only)
# -----------------------------------------------------------------------------
train.mat <- model.matrix(SalePrice ~ ., train4[train.set, ])
y.train <- log(na.omit(train4[train.set, ]$SalePrice))

# Note: doesn't work well with factor/categorical variables
cv.lasso <- cv.glmnet(train.mat, y.train, alpha = 1)
plot(cv.lasso)
best.lambda.lasso <- cv.lasso$lambda.min
best.lambda.lasso

lasso.mod <- glmnet(train.mat, y.train, alpha = 1, lambda = 
                      best.lambda.lasso, thresh = 1e-12)
coef(lasso.mod) # included variables in model
lasso.pred <- predict(lasso.mod, s = best.lambda.lasso, newx = test.mat)

# compute the test Mean Squared Error (MSE)
mean((y - lasso.pred)^2) # test MSE = 0.03613829


# -----------------------------------------------------------------------------
# PCR with cross-validation
# -----------------------------------------------------------------------------

# compute the test Mean Squared Error (MSE)

# -----------------------------------------------------------------------------
# PLS with cross-validation
# -----------------------------------------------------------------------------

# compute the test Mean Squared Error (MSE)


# -----------------------------------------------------------------------------
# Generalized Additive Model with cross-validation on original data (train)
# -----------------------------------------------------------------------------

# compute the test Mean Squared Error (MSE)


# -----------------------------------------------------------------------------
# Rerun the best ML model on the entire dataset with the best tuning 
# parameter value
# -----------------------------------------------------------------------------





