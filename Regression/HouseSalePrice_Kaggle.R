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
library(pls)
library(mice)

# -----------------------------------------------------------------------------
# Import + explore data
# -----------------------------------------------------------------------------
pred <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/HouseSalePrice/test.csv")
train <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/HouseSalePrice/train.csv")
pred$SalePrice <- 0
combined <- rbind(train, pred)
str(combined)
View(combined)

# Several variables have NAs when a feature is absent, but 
# the data are not missing, so we'll change these NAs to "None
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


# Change some factor variables to numeric scales ranked by quality and 
# identify which of these variables appear to be meaningful in predicting
# house sale price
# Informative numeric scale predictors = CentralAir, FireplaceQu, GarageQual, PavedDrive
# All other predictors do not appear to be informative, and thus I will
# not change them into numeric scales 

#combined$Street <- as.numeric(factor(combined$Street, levels = 
#                    c("Pave", "Grvl")))
#boxplot(combined$SalePrice ~ combined$Street) 
# Street appears to be uninformative

#combined$LandSlope <- as.numeric(factor(combined$LandSlope, levels = 
#                    c("Gtl", "Mod", "Sev")))
#boxplot(combined$SalePrice ~ combined$LandSlope)
# LandSlope appears to be uninformative

#combined$ExterQual <- as.numeric(factor(combined$ExterQual, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po")))
#boxplot(combined$SalePrice ~ combined$ExterQual)
# ExterQual appears to be uninformative

#combined$ExterCond <- as.numeric(factor(combined$ExterCond, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po")))
#boxplot(combined$SalePrice ~ combined$ExterCond)
# ExterCond appears to be uninformative

#combined$BsmtQual <- as.numeric(factor(combined$BsmtQual, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po", "None")))
#boxplot(combined$SalePrice ~ combined$BsmtQual)
# BsmtQual appears to be uninformative

#combined$BsmtCond <- as.numeric(factor(combined$BsmtCond, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po", "None")))
#boxplot(combined$SalePrice ~ combined$BsmtCond)
# BsmtCond appears to be uninformative

#combined$BsmtExposure <- as.numeric(factor(combined$BsmtExposure, levels = 
#                   c("Gd", "Av", "Mn", "No", "None")))
#boxplot(combined$SalePrice ~ combined$BsmtExposure)
# BsmtExposure appears to be uninformative

#combined$BsmtFinType1 <- as.numeric(factor(combined$BsmtFinType1, levels = 
#                    c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "None")))
#boxplot(combined$SalePrice ~ combined$BsmtFinType1)
# BsmtFinType1 appears to be uninformative

#combined$BsmtFinType2 <- as.numeric(factor(combined$BsmtFinType2, levels = 
#                    c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "None")))
#boxplot(combined$SalePrice ~ combined$BsmtFinType2)
# BsmtFinType2 appears to be uninformative

#combined$HeatingQC <- as.numeric(factor(combined$HeatingQC, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po")))
#boxplot(combined$SalePrice ~ combined$HeatingQC)
# HeatingQC appears to be uninformative

combined$CentralAir <- as.numeric(factor(combined$CentralAir, levels = 
                    c("Y", "N")))
boxplot(combined$SalePrice ~ combined$CentralAir)
hist(combined$CentralAir)
# CentralAir may be informative ##############################################

#combined$KitchenQual <- as.numeric(factor(combined$KitchenQual, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po")))
#boxplot(combined$SalePrice ~ combined$KitchenQual)
# KitchenQual appears to be uninformative

#levels(combined$Functional) <- c("Typ", "Min1", "Min1", "Min1", "Maj1", "Maj1", "Maj1", "Maj1")
#levels(combined$Functional)
#combined$Functional <- as.numeric(factor(combined$Functional, levels = 
#                    c("Typ", "Min1", "Maj1")))
#boxplot(combined$SalePrice ~ combined$Functional)
#hist(combined$Functional)
# Functional may be informative if levels are combined? ######################
# Scale: 1 = "Typ"; 2 = "Min1", "Min2", "Mod"; 3 = "Maj1", "Maj2", "Sev", "Sal"

levels(combined$FireplaceQu) <- c("Gd", "Gd", "Gd", "Po", "Po", "Po")
levels(combined$FireplaceQu)
combined$FireplaceQu <- as.numeric(factor(combined$FireplaceQu, levels = 
                    c("Gd", "Po")))
boxplot(combined$SalePrice ~ combined$FireplaceQu)
hist(combined$FireplaceQu)
# FireplaceQU may be informative if levels are combined? ######################
# Scale: 1 = "Ex", "Gd", "TA"; 2 = "Fa", "Po", "None"

#combined$GarageFinish <- as.numeric(factor(combined$GarageFinish, levels = 
#                    c("Fin", "RFn", "Unf", "None")))
#boxplot(combined$SalePrice ~ combined$GarageFinish)
# GarageFinish appears to be uninformative

combined$GarageQual <- as.numeric(factor(combined$GarageQual, levels = 
                    c("Ex", "Gd", "TA", "Fa", "Po", "None")))
boxplot(combined$SalePrice ~ combined$GarageQual)
hist(combined$GarageQual)
# GarageQual may be informative ###########################################

#combined$GarageCond <- as.numeric(factor(combined$GarageCond, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po", "None")))
#boxplot(combined$SalePrice ~ combined$GarageCond)
#hist(combined$GarageCond)
# GarageCond may be informative, but frequencies are perhaps too skewed

levels(combined$PavedDrive) <- c("Y", "N", "N")
levels(combined$PavedDrive)
combined$PavedDrive <- as.numeric(factor(combined$PavedDrive, levels = 
                    c("Y", "N")))
boxplot(combined$SalePrice ~ combined$PavedDrive)
hist(combined$PavedDrive)
# Paved drive may be informative is levels are combined? ####################
# Scale: 1 = "Y"; 2 = "P", "N"

#combined$PoolQC <- as.numeric(factor(combined$PoolQC, levels = 
#                    c("Ex", "Gd", "TA", "Fa", "Po", "None")))
#boxplot(combined$SalePrice ~ combined$PoolQC)
#hist(combined$PoolQC)
# PoolQC may be informative, but the frequency of pools is too low....

#combined$Fence <- as.numeric(factor(combined$Fence, levels = 
#                    c("GdPrv", "GdWo", "MnPrv", "MnWw", "None")))
#boxplot(combined$SalePrice ~ combined$Fence)
# Fence appears to be uninformative

str(combined)

# Data transformations to increase normality + changing data types
hist(combined$SalePrice) # highly skewed (will use BoxCox transformation)

boxplot(combined$SalePrice ~ combined$MSSubClass)
hist(as.numeric(combined$MSSubClass))
combined$MSSubClass <- as.factor(combined$MSSubClass)

# Combining various Neighborhoods that have similar house prices together
boxplot(combined$SalePrice ~ combined$Neighborhood)
combined$NeighborhoodGroups <- combined$Neighborhood
levels(combined$NeighborhoodGroups)
levels(combined$NeighborhoodGroups) <- c("A", "D", "C", "C", "A", "A", "D", "C", "D", "D", "D", 
                                 "D", "A", "B", "D", "D", "A", "D", "D", "D", "D", "D", 
                                 "C", "A", "D")
# Group A: Blmngtn, ClearCr, NAmes, NWAmes, Timber, CollgCr
# Group B: NoRidge
# Group C: BrDale, BrkSide, Edwards, SWISU
# Group D: Blueste, Crawfor, Gilbert, IDOTRR, MeadowW, Mitchel, NPkVill, NridgHt, OldTown, Sawyer,
  # SawyerW, Somerst, StoneBr, Veenker
boxplot(combined$SalePrice ~ combined$NeighborhoodGroups)

# Combining various MSZoning classes that have similar house prices together
boxplot(combined$SalePrice ~ combined$MSZoning)
combined$MSZoningGroups <- combined$MSZoning
levels(combined$MSZoningGroups)
levels(combined$MSZoningGroups) <- c("A", "B", "C", "C", "B")
# Group A: C (all)
# Group B: FV, RM
# Group C: RH, RL
boxplot(combined$SalePrice ~ combined$MSZoningGroups)


# Combine Sale Conditions for "new homes" (aka, "Partial") and everything else
boxplot(combined$SalePrice ~ combined$SaleCondition)
combined$New <- combined$SaleCondition
levels(combined$New)
levels(combined$New) <- c("N", "N", "N", "N", "N", "Y")
boxplot(combined$SalePrice ~ combined$New)


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
train2 <- combined[1:1460, 2:85]
pred2 <- combined[1461:2919, 2:85]

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
glm.cv$delta # Test MSE = 0.02324290

summary(glm.fit)
anova(glm.fit)
plot(glm.fit) # 1299 and 524 are outliers

train3 <- train3[-1299, ]
train3 <- train3[-524, ]

glm.fit.no <- glm(log(SalePrice) ~ ., family = gaussian, train3)
summary(glm.fit.no)
anova(glm.fit.no)
plot(glm.fit.no)


# -----------------------------------------------------------------------------
# Model selection with cross validation (for numeric variables, only)
# -----------------------------------------------------------------------------
# This approach doesn't work great with factor/categorical variables, so we'll
# First extract just the numeric and integer variables from the data
nums <- sapply(train2, is.numeric)
train4 <- cbind.data.frame(train2[ , nums])
str(train4) # 40 predictors
train4 <- na.omit(train4) # need to na.omit for model.matrix, below
y <- log(na.omit(train4[test, ]$SalePrice))

regfit.best <- regsubsets(log(SalePrice) ~ ., data = train4[train.set, ], 
                          nvmax = 40, really.big = TRUE)
test.mat <- model.matrix(SalePrice ~ ., train4[test, ]) # removes any rows with missing data
val.error = rep(NA, 40)

for(i in 1:40) {
  coefi <- coef(regfit.best, id = i)
  prediction <- test.mat[, names(coefi)] %*% coefi
  val.error[i] <- mean((y-prediction)^2)
}

val.error 
which.min(val.error) # which model size has the lowest MSE?
val.error[2] # test MSE = 0.02886601
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
mean((y - lasso.pred)^2) # test MSE = 0.03558018

# The best fit lasso model includes these predictors:
# LotArea (0.07), OverallQual (0.09), OverallCond (0.03), YearBuilt (-0.007),
# YearRemodAdd (0.001), BsmtFinSF1 (0.003), CentralAir (-0.02), 
# X1stFlrSF (0.07), GrLivArea (0.2), HalfBath (0.03), KithenAbvGr (-0.04),
# GarageCars (0.07), WoodDeckSF (0.00002), OpenPorchSF (0.00008),
# PoolArea (-0.06), TotalSF (0.19)

# After looking at the best fit lasso model, I would like to further investigate
# these predictors:
#
# LotArea
# OverallQual
# X1stFlrSF
# GrLivArea
# GarageCars
# PoolArea
# TotalSF
#

# -----------------------------------------------------------------------------
# Hypothesis 2 model with K-fold cross validation
# -----------------------------------------------------------------------------
# Given the predictors discovered via lasso, I now hypothesize that SalePrice is 
# best predicted by:
# LotArea, OverallQual, TotalSF, GarageCars, Neighborhood, MSZoning, SaleCondition

train5 <- cbind.data.frame(train2$SalePrice, train2$LotArea, train2$OverallQual, 
                           train2$TotalSF, train2$GarageCars, 
                           train2$Neighborhood,train2$MSZoning, train2$SaleCondition)
names(train5) <- (c("SalePrice", "LotArea", "OverallQual", "TotalSF", 
                    "GarageCars", "Neighborhood", "MSZoning",
                    "SaleCondition"))

glm.fit.hyp2 <- glm(log(SalePrice) ~ ., family = gaussian, train5)
glm.cv.hyp2 <- cv.glm(train5, glm.fit.hyp2, K=10)
glm.cv.hyp2$delta # Test MSE = 0.02291672

summary(glm.fit.hyp2)
anova(glm.fit.hyp2)
plot(glm.fit.hyp2) # 1299 and 524 are outliers

train5 <- train5[-1299, ]
train5 <- train5[-524, ]

glm.fit.hyp2.no <- glm(log(SalePrice) ~ ., family = gaussian, train5)
summary(glm.fit.hyp2.no)
anova(glm.fit.hyp2.no)
plot(glm.fit.hyp2.no)


# -----------------------------------------------------------------------------
# Generalized Additive Model with cross-validation on original data (train)
# -----------------------------------------------------------------------------

# compute the test Mean Squared Error (MSE)


# -----------------------------------------------------------------------------
# Rerun the best ML model on the entire dataset with the best tuning 
# parameter value
# -----------------------------------------------------------------------------





