# Catherine Beazley, Mengyao Zhang, Ruoyan Chen
# House Prices Kaggle Competition
# SYS 6018
# 9/7/2018

# Reading in the datasets. 
library(tidyverse)
setwd('C:/Users/cathe/Desktop/SYS6018/HousePrices_KaggleCompetition')
train <- read_csv('train.csv')
test <- read_csv('test.csv')

# SalePrice is the response variable.

# Converting categorical variable to factors
train$MSSubClass <- as.factor(train$MSSubClass)
train$OverallQual <- as.factor(train$OverallQual)
train$OverallCond <- as.factor(train$OverallCond)
train$MoSold <- as.factor(train$MoSold)

test$MSSubClass <- as.factor(test$MSSubClass)
test$OverallQual <- as.factor(test$OverallQual)
test$OverallCond <- as.factor(test$OverallCond)
test$MoSold <- as.factor(test$MoSold)

############## Cleaning the Data #################

# Finding with NAs first

# For the training data
nas.cols <- as.vector(rep(0, 80))
for(i in 1:80){
  nas.cols[i] <- sum(is.na(train[i]))
}
nas.cols
# Naming the vector colums
names(nas.cols) <- names(train)[1:80]

# For the testing data
nas.cols.ts <- as.vector(rep(0, 80))
for(i in 1:80){
  nas.cols.ts[i] <- sum(is.na(test[i]))
}
nas.cols.ts
# Naming the vector colums
names(nas.cols.ts) <- names(train)[1:80]
nas.cols.ts[nas.cols.ts!=0]

# Finding columns with NAs
with.nas <- nas.cols[nas.cols!=0]
# 19 columns

# These 15 columns are the column for which NA means that the house does not have the specified feature
# Alley 
# BsmtQual
# BsmtCond
# BsmtExposure
# BsmtFinType1
# BsmtFinType2
# FireplaceQu
# GarageType
# GarageYrBlt
# GarageFinish
# GarageQual
# GarageCond
# PoolQC
# Fence
# MiscFeature


# Replacing NA values with "None" for instances when NA designates that the house 
# does not have the feature
indices <- unname(which(nas.cols != 0))
length(c(7,31,32,33,34,36,58, 59, 60, 61, 64, 65, 73, 74, 75))

for(i in c(7,31,32,33,34,36,58, 59, 60, 61, 64, 65, 73, 74, 75)){
  index <- which(is.na(train[,i]))
  train[index, i] <- "None"
}

# Now finding remaining NA values:

nas.cols2 <- as.vector(rep(0, 80))
for(i in 1:80){
  nas.cols2[i] <- sum(is.na(train[i]))
}
names(nas.cols2) <- names(train)[1:80]
nas.cols2[nas.cols2!=0]

# LotFrontage  MasVnrType  MasVnrArea  Electrical 
# 259           8           8           1

# As expected, we have four columns remainig with NA values.

# For LotFrontage, check to see if we can predict lot frontage by square footage of lot, which has no missing value.
indices.nas <- which(is.na(train$LotFrontage))
newData <- train[-indices.nas,c("LotFrontage", "LotArea")]
model <- lm(LotFrontage ~ LotArea, data =newData)
summary(model)
# Call:
#   lm(formula = LotFrontage ~ LotArea, data = newData)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -188.122  -11.158   -0.766    9.892  219.840 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 5.706e+01  1.018e+00   56.02   <2e-16 ***
#   LotArea     1.306e-03  8.007e-05   16.31   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.98 on 1199 degrees of freedom
# Multiple R-squared:  0.1816,	Adjusted R-squared:  0.1809 
# F-statistic:   266 on 1 and 1199 DF,  p-value: < 2.2e-16

# Significant p-values, so we think this will be a good model to impute LotFrontage

b0.hat <- coefficients(model)[[1]]
b1.hat <- coefficients(model)[[2]]

# Our fitted simple linear regression model is y.hat = 57.05504 + 0.001305799*x

# Imputing missing values for LotFrontage
train$LotFrontage[indices.nas] <- as.vector(as.integer(57.05504 + 0.001305799*train$LotArea[indices.nas]))
train$LotFrontage <- as.integer(train$LotFrontage)


# Because the number of rows with NA values is so low (9), we decided to delete the rows with 
# those values.
train <- na.omit(train)


# Checking instances of '0' and seeing if it makes sense or not
zero.cols <- as.vector(rep(0, 80))
for(i in 1:80){
  zero.cols[i] <- sum(train[i]==0)
}
zero.cols
# Naming the vector colums
names(zero.cols) <- names(train)[1:80]
zero.cols[zero.cols!=0]

# MasVnrArea    BsmtFinSF1    BsmtFinSF2     BsmtUnfSF   TotalBsmtSF      2ndFlrSF  LowQualFinSF 
# 860           464          1284           118            37           824          1425 
# BsmtFullBath  BsmtHalfBath      FullBath      HalfBath  BedroomAbvGr  KitchenAbvGr    Fireplaces 
# 853          1369             9           910             6             1           685 
# GarageCars    GarageArea    WoodDeckSF   OpenPorchSF EnclosedPorch     3SsnPorch   ScreenPorch 
# 81            81           755           653          1244          1427          1335 
# PoolArea       MiscVal 
# 1444          1399 

# It makes sense for zeros to be in all of these columns because they all involve square footage, 
# count, or value

# Since we have accounted for all 0s and NAs, we conclude we have cleaned the data.

########## Parametric Approach ##############################################

# We will use cross validation
sub <- sample(1:length(train$Id), length(train$Id)/2)
train.set <- train[sub,]
valid.set <- train[-sub,]


# Testing all variables
model.all <- lm(SalePrice ~ ., data=train.set)
options(max.print=1000000)
summary(model.all)

# MSSubClass, MSZoning, LotArea, LotShape, LandContour, LotConfig, Neighborhood, Condition2, OverallQual,
# YearBuilt, RoofStyle, RoofMatl, ExterQual, Foundation, BsmtQual, BsmtExposure, BsmtFinSF1, BsmtUnfSF, 
# `1stFlrSF` , `2ndFlrSF`, FullBath, KitchenQual, WoodDeckSF, ScreenPorch, MiscFeature, MiscVal, SaleType, 
# SaleCondition

# Testing with variables that were significant when testing all
model.2 <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+LotConfig+Neighborhood+Condition2+OverallQual+
              YearBuilt+RoofStyle+RoofMatl+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinSF1+BsmtUnfSF+
                `1stFlrSF` +`2ndFlrSF`+FullBath+KitchenQual+WoodDeckSF+ScreenPorch+MiscFeature+MiscVal+SaleType+
               SaleCondition,data=train.set)
summary(model.2)

# Testing everything as above except LotConfig because it was no longer significant
model.3 <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+Neighborhood+Condition2+OverallQual+
                YearBuilt+RoofStyle+RoofMatl+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinSF1+BsmtUnfSF+
                `1stFlrSF` +`2ndFlrSF`+FullBath+KitchenQual+WoodDeckSF+ScreenPorch+MiscFeature+MiscVal+SaleType+
                SaleCondition,data=train.set)
summary(model.3)

# We think model 3 is better, making presictions for test set
final.model <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+Neighborhood+Condition2+OverallQual+
                    YearBuilt+RoofStyle+RoofMatl+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinSF1+BsmtUnfSF+
                    `1stFlrSF` +`2ndFlrSF`+FullBath+KitchenQual+WoodDeckSF+ScreenPorch+MiscFeature+MiscVal+SaleType+
                    SaleCondition,data=train)
predictions <- predict(final.model, newdata=test)

