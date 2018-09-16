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



# Finding columns with NAs for train data
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
# mz- deleted as.vector here, doesn't seem to make a difference
train$LotFrontage[indices.nas] <- as.integer(57.05504 + 0.001305799*train$LotArea[indices.nas]) 
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

# We think model 3 is better, build model on entire train data
final.model <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+Neighborhood+Condition2+OverallQual+
                    YearBuilt+RoofStyle+RoofMatl+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinSF1+BsmtUnfSF+
                    `1stFlrSF` +`2ndFlrSF`+FullBath+KitchenQual+WoodDeckSF+ScreenPorch+MiscFeature+MiscVal+SaleType+
                    SaleCondition,data=train)

summary(final.model)


# Clean test data

# Find NAs
# For the testing data
nas.cols.ts <- as.vector(rep(0, 80))
for(i in 1:80){
  nas.cols.ts[i] <- sum(is.na(test[i]))
}
nas.cols.ts

# Naming the vector colums
names(nas.cols.ts) <- names(train)[1:80]
nas.cols.ts[nas.cols.ts!=0]

# Replacing NA values with "None" for instances when NA designates that the house 
# does not have the feature (15 variables)

for(i in c(7,31,32,33,34,36,58, 59, 60, 61, 64, 65, 73, 74, 75)){
  index <- which(is.na(test[,i]))
  test[index, i] <- "None"
}

# Check the remaining variables with NAs
nas.cols2.ts <- as.vector(rep(0, 80))
for(i in 1:80){
  nas.cols2.ts[i] <- sum(is.na(test[i]))
}
names(nas.cols2.ts) <- names(test)[1:80]
nas.cols2.ts[nas.cols2.ts!=0]

# MSZoning  LotFrontage    Utilities  Exterior1st  Exterior2nd   MasVnrType   MasVnrArea   BsmtFinSF1 
# 4          227            2            1            1           16           15            1 
# BsmtFinSF2    BsmtUnfSF  TotalBsmtSF BsmtFullBath BsmtHalfBath  KitchenQual   Functional   GarageCars 
# 1            1            1            2            2            1            2            1 
# GarageArea     SaleType 
# 1            1 

# We have 18 columns remainig with NA values.

# Imputing missing values for LotFrontage
indices.nas.ts <- which(is.na(test$LotFrontage))
test$LotFrontage[indices.nas.ts] <- as.integer(57.05504 + 0.001305799*test$LotArea[indices.nas.ts]) 
test$LotFrontage <- as.integer(test$LotFrontage)

# The remaining variables with NAs and we include in our model are

# MSZoning(4), BsmtFinSF1(1), BsmtUnfSF(1),  KitchenQual(1), SaleType(1)


# test_copy <- test[test$MSSubClass!=150,]   # mz-issue: MSSubClass has new level (150) in test data, just one obeservation
# 
# 
# 
# predictions <- predict(final.model, newdata=test_copy)

# Filling in NAs for MSZoning
test %>% group_by(MSZoning) %>% summarise(n())
# MSZoning `n()`
# <chr>    <int>
#   1 C (all)     15
# 2 FV          74
# 3 RH          10
# 4 RL        1114
# 5 RM         242
# 6 NA           4

# RL is by far the most frequent category of MSZoning so we are filling in the 4 missing MSZoning values with RL
test[which(is.na(test$MSZoning)), "MSZoning"] <- 'RL'

# Filling in NAs for BsmtFinSF1, BsmtUnfSF

# Checking if the rows for missing values for BsmtFinSF1, BsmtUnfSF have no basement

test[which(is.na(test$BsmtFinSF1)), "BsmtCond"]
# 1 None  
# This means there is no basement so the square footage should be 0

test[which(is.na(test$BsmtUnfSF)), "BsmtCond"]
# 1 None  
# This means there is no basement so the square footage should be 0

# Filling in 0 for NAs in BsmtFinSF1 and BsmtUnfSF
test[which(is.na(test$BsmtFinSF1)), "BsmtFinSF1"] <- 0
test[which(is.na(test$BsmtUnfSF)), "BsmtUnfSF"] <- 0

# Filling in NAs for KitchenQual
test %>% group_by(KitchenQual) %>% summarise(n())
# KitchenQual `n()`
# <chr>       <int>
#   1 Ex            105
# 2 Fa             31
# 3 Gd            565
# 4 TA            757
# 5 NA              1

# Replacing the NA with 'TA' since it is the most frequent
test[which(is.na(test$KitchenQual)), 'KitchenQual'] <- 'TA'

# Filling in NAs for SaleType
test %>% group_by(SaleType) %>% summarise(n())
# SaleType `n()`
# <chr>    <int>
#   1 COD         44
# 2 Con          3
# 3 ConLD       17
# 4 ConLI        4
# 5 ConLw        3
# 6 CWD          8
# 7 New        117
# 8 Oth          4
# 9 WD        1258
# 10 NA           1

# 'WD' is the most frequent so we are filling the NA with that
test[which(is.na(test$SaleType)), 'SaleType'] <- 'WD'

# FOr MSSubClass, we are changing the 150 to 120 since the 150 observation had a MSZonging of 'RL' and
# most of the 120 had a MSZoning of "RL"
test[which(test$MSSubClass==150), 'MSSubClass'] <- 120

# Now the testing dataseet is cleaned.

# Making our predicitons and writing them to a csv file
predictions <- predict(final.model, newdata=test)
predictions.table <- cbind(test$Id, predictions)
write.table(predictions.table, file="C1-10_Parametric1.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')



#################################################################################
#                                                                               #
#                                                                               #
#                            KNN Approach                                       #
#                                                                               #
#                                                                               #
#################################################################################

# Creating a function to calculate the distance between two points
# x is the point we would like to evaluate
# y is a neighboring point used in KNN
distance <- function(x, y){
  return(sqrt(sum((x-y)^2)))
}


# Resaving categorical variables as integers so we can use them for KNN
classes <- sapply(train, class)
indices_categorical <- which(classes != 'integer')
unname(indices_categorical)
for(i in indices_categorical){
  train[,i] <- as.integer(as.factor(unlist(train[,i])))
}

# Using Cross Validation. We subsetted the train set into a training and validation set earlier in this code
sub <- sample(1:length(train$Id), length(train$Id)/2)
train.set <- train[sub,]
valid.set <- train[-sub,]

# Row vector from validation set is the point we would like to evaluate
x <- as.vector(valid.set[1,2:80])
dists1 <- apply(train.set[, 2:80], MARGIN=1, distance, y=x)
test[,82] <- dists1
value <- quantile(test$V82, K/length(train.set$Id))
subset.of.k <- test[test$V82 <= value,]
mean(subset.of.k$SalePrice)



# Then calculate the distance from that point to all the points (row vectors) in training set
# x <- as.vector(valid.set[1,2:80])
# dists <- rep(0, times=length(train.set$Id))
# for(i in 1:length(train.set$Id)){
#   dists[i] <- distance(x=x, y=train.set[i, 2:80])
# }
# test[, 82] <- dists
# dists1-dists
# # Choose K nearest points from training set
# K <- 5
# test <- train.set
# test[,82] <- dists
# test <- test[order(test$V82),]
# k.neighbors <- head(test, K)
# mean(k.neighbors$SalePrice)
# # Average the houseing price from those K points, use that price as prediction for the point from validation set
# avg.price <- mean(k.neighbors$SalePrice)
# 
# rows <- as.list(rep(rep(0, times=80), times = length(test$Id)))
# for(i in 1:length(test$Id)){
#   rows[i] <- test[i, 2:80]
# }
# warnings()
# 
# K<- 5
# predicted.price <- rep(0, length(valid.set$Id))
# for(i in 1:length(valid.set$Id)){
#   x <- as.vector(valid.set[i,2:80])
#   dists <- apply(train.set[, 2:80], MARGIN=1, distance, y=x)
#   test[,82] <- dists
#   test <- test[order(test$V82),]
#   k.neighbors <- head(test, K)
#   predicted.price[i] <- mean(k.neighbors$SalePrice)
# }
# 
# # Subsetting instead of sorting so that the for loop is faster
# predicted.price1[i] <- mean(test[test$V82<= quantile(test$V82, K/length(test$Id)),]$SalePrice)
# K<- 5
# predicted.price1 <- rep(0, length(valid.set$Id))
# for(i in 1:length(valid.set$Id)){
#   x <- as.vector(valid.set[i,2:80])
#   dists <- apply(train.set[, 2:80], MARGIN=1, distance, y=x)
#   test[,82] <- dists
#   predicted.price1[i] <- mean(test[test$V82<= quantile(test$V82, K/length(test$Id)),]$SalePrice)
#   
# }
# 
# x <- as.vector(valid.set[1,2:80])
# dists <- apply(train.set[, 2:80], MARGIN=1, distance, y=x)
# test[,82] <- dists
# mean(test[test$V82<= quantile(test$V82, K/length(test$Id)),]$SalePrice)
# 
# 
# predicted.price[1:48]-valid.set[1:48,81]
# summary(abs(predicted.price[1:48]-valid.set[1:48,81]))
# 
# 
# predicted.prices <- rep(0, times=6)
# Ks <- c(5, 10, 25, 50, 75, 100)
# for(j in 1:length(Ks)){
#   K <- Ks[j]
#   test[,82] <- dists
#   test <- test[order(test$V82),]
#   k.neighbors <- head(test, K)
#   predicted.prices[j] <- mean(k.neighbors$SalePrice)
#   
# }
# 
# 
# ##################################################################################################
# # Long Method
# K <- 5
# test <- train.set
# x <- valid.set[1,2:80]
# dists <- rep(0, times=length(test$Id))
# for(i in 1:length(test$Id)){
#   dists[i] <- distance(x=x, y=test[i, 2:80])
# }
# test[, 82] <- dists
# k.neighbors <- test[test$V82 <= quantile(test$V82, K/length(test$Id)),]
# mean(k.neighbors$SalePrice)
# # [1] 205460

#####################################################################################
test <- train.set
K <- 5
dists <- rep(0, times=length(test$Id))
avg.price <- rep(0, times=length(valid.set$Id))
for(i in 1:length(valid.set$Id)){
  x <- valid.set[i, 2:80]
  dists <- apply(test[,2:80], MARGIN=1, distance, y=x)
  test[,82] <- dists
  k.neighbors <- test[test$V82 <= quantile(test$V82, K/length(test$Id)),]
  avg.price[i] <- mean(k.neighbors$SalePrice)
}
1-1
test2 <- train.set[, c('MSSubClass','MSZoning','LotArea','LotShape','LandContour','Neighborhood','Condition2','OverallQual',
  'YearBuilt','RoofStyle','RoofMatl','ExterQual','Foundation','BsmtQual','BsmtExposure','BsmtFinSF1','BsmtUnfSF',
  '1stFlrSF' ,'2ndFlrSF','FullBath','KitchenQual','WoodDeckSF','ScreenPorch','MiscFeature','MiscVal','SaleType',
  'SaleCondition', 'SalePrice')]
valid.set2 <- valid.set[, c('MSSubClass','MSZoning','LotArea','LotShape','LandContour','Neighborhood','Condition2','OverallQual',
                            'YearBuilt','RoofStyle','RoofMatl','ExterQual','Foundation','BsmtQual','BsmtExposure','BsmtFinSF1','BsmtUnfSF',
                            '1stFlrSF' ,'2ndFlrSF','FullBath','KitchenQual','WoodDeckSF','ScreenPorch','MiscFeature','MiscVal','SaleType',
                            'SaleCondition')]

length(c('MSSubClass','MSZoning','LotArea','LotShape','LandContour','Neighborhood','Condition2','OverallQual',
         'YearBuilt','RoofStyle','RoofMatl','ExterQual','Foundation','BsmtQual','BsmtExposure','BsmtFinSF1','BsmtUnfSF',
         '1stFlrSF' ,'2ndFlrSF','FullBath','KitchenQual','WoodDeckSF','ScreenPorch','MiscFeature','MiscVal','SaleType',
         'SaleCondition', 'SalePrice'))

K <- 5
dists <- rep(0, times=length(test2$MSSubClass))
avg.price <- rep(0, times=length(valid.set2$MSSubClass))
test2$Distance <- NA
for(i in 1:length(valid.set2$MSSubClass)){
  x <- valid.set2[i, 1:27]
  dists <- apply(test2[,1:27], MARGIN=1, distance, y=x)
  test2$Distance <- dists
  k.neighbors <- test2[test2$Distance <= quantile(test2$Distance, K/length(test2$MSSubClass)),]
  avg.price[i] <- mean(k.neighbors$SalePrice)
}


# Running with real test set
classes <- sapply(train, class)
indices_categorical <- which(classes != 'integer')
unname(indices_categorical)
for(i in indices_categorical){
  train[,i] <- as.integer(as.factor(unlist(train[,i])))
}

classes <- sapply(test, class)
indices_categorical <- which(classes != 'integer')
unname(indices_categorical)
for(i in indices_categorical){
  test[,i] <- as.integer(as.factor(unlist(test[,i])))
}

subsetted.train <- train[,c('MSSubClass','MSZoning','LotArea','LotShape','LandContour','Neighborhood','Condition2','OverallQual',
                            'YearBuilt','RoofStyle','RoofMatl','ExterQual','Foundation','BsmtQual','BsmtExposure','BsmtFinSF1','BsmtUnfSF',
                            '1stFlrSF' ,'2ndFlrSF','FullBath','KitchenQual','WoodDeckSF','ScreenPorch','MiscFeature','MiscVal','SaleType',
                            'SaleCondition', 'SalePrice')]
subsetted.test <- test[,c('MSSubClass','MSZoning','LotArea','LotShape','LandContour','Neighborhood','Condition2','OverallQual',
                          'YearBuilt','RoofStyle','RoofMatl','ExterQual','Foundation','BsmtQual','BsmtExposure','BsmtFinSF1','BsmtUnfSF',
                          '1stFlrSF' ,'2ndFlrSF','FullBath','KitchenQual','WoodDeckSF','ScreenPorch','MiscFeature','MiscVal','SaleType',
                          'SaleCondition')]



K <- 5
dists <- rep(0, times=length(subsetted.train$MSSubClass))
avg.price <- rep(0, times=length(subsetted.test$MSSubClass))
subsetted.train$Distance <- NA
for(i in 1:length(subsetted.test$MSSubClass)){
  x <- subsetted.test[i, 1:27]
  dists <- apply(subsetted.train[,1:27], MARGIN=1, distance, y=x)
  subsetted.train$Distance <- dists
  k.neighbors <- subsetted.train[subsetted.train$Distance <= quantile(subsetted.train$Distance, K/length(subsetted.train$MSSubClass)),]
  avg.price[i] <- mean(k.neighbors$SalePrice)
}
