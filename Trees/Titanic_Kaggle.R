# Author = Hilary Barker
# This script explores Kaggle's titanic data and predicts survival.

# -----------------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------------
library(randomForest)
library(gbm)

# -----------------------------------------------------------------------------
# Import + explore data
# -----------------------------------------------------------------------------
pred <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/TitanicData/test.csv")
train <- read.csv("~/Dropbox/Hils ToDo List/Data Science/MachineLearning_GitHub_scripts/TitanicData/train.csv")
pred$Survived <- 0
combined <- rbind(train, pred)
str(combined) # Cabin, Embarked, and Age have missing data

# Make new variables based on the passenger's surname and title
combined$Name <- as.character(combined$Name)
combined$Surname <- as.factor(sapply(combined$Name, function(x) {strsplit(x, split = '[,.]')[[1]][1]}))
combined$Title <- as.factor(sapply(combined$Name, function(x) {strsplit(x, split = '[,.]')[[1]][2]}))
summary(combined$Title)
combined$Title[c(823, 600, 31)] = " Mr" # replace Sir, Don, and Jonkheer with Mr
combined$Title[c(746, 450, 537)] = " Col" # replace Capt + Major with Col 
  # (to group all of the military together)
combined$Title[c(370, 557, 760)] = " Mrs" # replace Mme, Lady, and the Countess with Mrs
combined$Title[c(1306, 444, 980, 642, 711)] = " Miss" # replace Ms, Dona, and Mlle with Miss
summary(combined$Title)

boxplot(Fare ~ Survived, data = combined)
boxplot(Parch ~ Survived, data = combined)
boxplot(SibSp ~ Survived, data = combined)

# impute missing age values for combined data
combined.imputed <- rfImpute(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                            Fare + Embarked, combined)
View(combined.imputed)
str(combined.imputed)

# compare distributions of imputed and original data
boxplot(Age ~ Survived, data = combined)
boxplot(Age ~ Survived, data = combined.imputed)

which(combined.imputed$Embarked == "")
combined.imputed$Embarked[c(62,830)] = "S" # since most passengers embarked from 
  # Southampton, we'll replace these missing values with "S"

combined2 <- cbind(combined[, 1:4], combined.imputed[, 2:8], combined[, 13:14])
str(combined2)

pred2 <- combined2[892:1309, ]
train2 <- combined2[1:891, ]  

# Comparing distributions of variables in training and prediction sets
par(mfrow = c(2,2))
hist(pred2$Age)
hist(train2$Age)
hist(pred2$Fare)
hist(train2$Fare)

par(mfrow = c(2,2))
hist(pred2$SibSp)
hist(train2$SibSp)
hist(pred2$Parch)
hist(train2$Parch)

par(mfrow = c(2,2))
plot(pred2$Embarked)
plot(train2$Embarked)
plot(pred2$Title)
plot(train2$Title)

# -----------------------------------------------------------------------------
# Subset data into training and test sets
# -----------------------------------------------------------------------------
set.seed(20)
train.set <- sample(c(TRUE, FALSE), nrow(train2), rep = TRUE)
test = (!train.set)
test.y <- train[test, ]$Survived

# need the same Factor classes present in Train and Test sets for boosting
summary(train2[train.set, 13])
summary(train2[test, 13])

summary(train2[train.set, 11])
summary(train2[test, 11])

summary(train2[train.set, 6])
summary(train2[test, 6])

# -----------------------------------------------------------------------------
# Bagging with cross-validation
# -----------------------------------------------------------------------------
bag.model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                            Fare + Embarked + Title,
                          data = train2, subset = train.set, mtry = 7,
                          importance = TRUE)
bag.model
yhat.bag <- predict(bag.model, newdata = train2[test, ])
mean((yhat.bag - test.y)^2) # MSE = 0.1250596
importance(bag.model)
varImpPlot(bag.model)

# -----------------------------------------------------------------------------
# Random Forests with cross-validation
# -----------------------------------------------------------------------------
random.model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                            Fare + Embarked + Title,
                          data = train2, subset = train.set,
                          importance = TRUE)
random.model
yhat.rand <- predict(random.model, newdata = train2[test, ])
mean((yhat.rand - test.y)^2) # MSE = 0.1182799
importance(random.model)
varImpPlot(random.model)


# -----------------------------------------------------------------------------
# Boosting with cross-validation
# -----------------------------------------------------------------------------
boost.model <- gbm(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                     Fare + Embarked + Title, data = train2[train.set, ], 
                   distribution = "bernoulli", n.trees = 5000, 
                   interaction.depth = 6, cv.fold = 0) # CV fold does not appear
  # to be working in the gbm package
summary(boost.model)


best.iter.oob <- gbm.perf(boost.model, method = "OOB")
print(best.iter.oob)
# best.iter.cv <- gbm.perf(boost.model, method = "cv")
# print(best.iter.cv)

summary(boost.model, n.trees = best.iter.oob)
# summary(boost.model, n.trees = best.iter.cv)

oob.predict <- predict(boost.model, train2[test, ], best.iter.oob, type = "response")
# cv.predict <- predict(boost.model, train.imputed[test, ], best.iter.cv, type = "response")
mean((oob.predict - test.y)^2) # MSE = 0.1228294
# mean((cv.predict - test.y)^2) # MSE = 0.1249019


# -----------------------------------------------------------------------------
# Rerun the best model on the entire dataset 
# -----------------------------------------------------------------------------
random.model.full <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                               Fare + Embarked + Title,
                             data = train2,
                             importance = TRUE)
random.model.full
yhat.rand <- predict(random.model.full, newdata = pred2)
yhat.rand




