#------------------------------------------------------------
# Name: Ng Jing Xun
# Datasets: insurance.csv
#------------------------------------------------------------

library(data.table)
library(corrplot)
library(caTools)
library(earth) 
library(randomForest)
library(car)

setwd('C:/Users/Jing Xun/Desktop/')

# Import data as data.table
data <- fread("insurance.csv")

#------------------------------------------------------------

# Question 1

# An overview of the data
summary(data)

# Categorize sex, smoker and region
data$sex <- factor(data$sex, levels = c('male', 'female'))
data$smoker <- factor(data$smoker, levels = c('yes', 'no'))
data$region <- factor(data$region)

# Convert categorical columns to numeric form to do corrplot
data[,":="(sex=ifelse(sex=="male",1,0))]
data[,":="(smoker=ifelse(smoker=="yes",1,0))]
data$region <- as.numeric(data$region)

corrplot(cor(data), type = "upper")
mtext("Correlation Plot of insurance.csv", at=4, line=2, cex=1)

plot(data$charges, data$smoker, title = "Correlation between charges and smoker")
mtext("Correlation between charges and smoker", line=2)
cor(data$charges, data$age)
cor(data$charges, data$bmi)
cor(data)

#------------------------------------------------------------

# Question 2

set.seed(2004)

data1 <- fread("insurance.csv")

# 70-30 train-test split
train <- sample.split(Y = data1$charges, SplitRatio = 0.7)
trainset <- subset(data1, train == T)
testset <- subset(data1, train == F)

# Categorize sex, smoker, region in train set and test set
trainset$sex <- factor(trainset$sex, levels = c('male', 'female'))
testset$sex <- factor(testset$sex, levels = c('male', 'female'))
trainset$smoker <- factor(trainset$smoker, levels = c('yes', 'no'))
testset$smoker <- factor(testset$smoker, levels = c('yes', 'no'))
trainset$region <- factor(trainset$region)
testset$region <- factor(testset$region)

# Verify that all the categorical levels in the categorical variables are represented in train set and test set
summary(trainset$sex)
summary(testset$sex)
summary(trainset$smoker)
summary(testset$smoker)
summary(trainset$region)
summary(testset$region)

#------------------------------------------------------------

# Question 3

# a. Backward Elimination Linear Regression
m1 <- lm(charges ~ ., data = trainset)
summary(m1)

# Backward Elimination
m1.step <- step(m1, direction = "backward")
summary(m1.step)

# Checking for multicollinearity issues
vif(m1.step)

# Apply model from trainset to predict on testset.
predict.m1.test <- predict(m1.step, newdata = testset)
testset.error.m1 <- testset$charges - predict.m1.test

# Testset Error of LR
RMSE.m1.test <- round(sqrt(mean(testset.error.m1^2)))
RMSE.m1.test

# b(i). MARS (degree 1)
m2 <- earth(charges ~ ., degree = 1, data = trainset)
summary(m2)

predict.m2.test <- predict(m2, newdata = testset)
testset.error.m2 <- testset$charges - predict.m2.test

# Testset Error of MARS
RMSE.m2.test <- round(sqrt(mean(testset.error.m2^2)))
RMSE.m2.test

# b(ii). MARS (degree 2)
m2a <- earth(charges ~ ., degree = 2, data = trainset)
summary(m2a)

predict.m2a.test <- predict(m2a, newdata = testset)
testset.error.m2a <- testset$charges - predict.m2a.test

# Testset Error of MARS
RMSE.m2a.test <- round(sqrt(mean(testset.error.m2a^2)))
RMSE.m2a.test

# c. Random Forest
m3 <- randomForest(charges ~ . , data=trainset, importance=T)
summary(m3)
# Error is stabilised before 500 trees
plot(m3)

predict.m3.test <- predict(m3, newdata = testset)
testset.error.m3 <- testset$charges - predict.m3.test

# Testset Error of RF
RMSE.m3.test <- round(sqrt(mean((testset.error.m3)^2)))
RMSE.m3.test

#------------------------------------------------------------

# Question 4

# Final model for Backward Elimination Linear Regression
summary(m1.step)

# Variable importance for MARS (degree 2)
var.impt.MARS <- evimp(m2a)
print(var.impt.MARS)

# Variable importance for Random Forest
var.impt.RF <- importance(m3)
var.impt.RF
varImpPlot(m3)

#------------------------------------------------------------

# Question 5

# Diagnostic Plot for Backward Elimination Linear Regression
par(mfrow = c(2,2))  
plot(m1.step)  

#------------------------------------------------------------

# Question 6

# Trying to remedy the linear regression assumptions by doing log transformation
m.test <- lm(log(charges) ~ log(age) + sex + log(bmi) + children + smoker + region, data = trainset)
summary(m.test)

# Backward Elimination
mtest.step <- step(m.test, direction = "backward")
summary(mtest.step)

# Checking for multicollinearity issues
vif(mtest.step)

# Apply model from trainset to predict on testset.
predict.m.test <- predict(mtest.step, newdata = testset)
testset.error.mtest <- log(testset$charges) - predict.m.test

# Testset Error of LR
RMSE.m.test <- sqrt(mean(testset.error.mtest^2))
RMSE.m.test

# Diagnostic Plot for Backward Elimination Linear Regression
par(mfrow = c(2,2))  
plot(mtest.step) 
