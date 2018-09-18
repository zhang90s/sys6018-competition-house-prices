# Catherine Beazley, Mengyao Zhang, Ruoyan Chen
# House Prices Kaggle Competition
# SYS 6018
# 9/7/2018

# Reading in the datasets. 
library(tidyverse)

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

###################################################################################
#                                                                                 #
#                             Cleaning Test Data                                  #
#                                                                                 #
###################################################################################
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

###################################################################################
#                                                                                 #
#                             Parametric Approach                                 #
#                                                                                 #
###################################################################################
# We will use cross validation
sub <- sample(1:length(train$Id), length(train$Id)/2)
train.set <- train[sub,]
valid.set <- train[-sub,]


# Testing all variables
model.all <- lm(SalePrice ~ ., data=train.set)
#options(max.print=1000000)
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

# use the model to predict SalePrice on valid.set and calculate mse
sp2<- predict(model.2, newdata=valid.set)
mse.2.valid<-mean((valid.set[,81]-sp2)^2)
mse.2.valid
# mse.2.valid=1259246404

# Testing everything as above except LotConfig because it was no longer significant
model.3 <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+Neighborhood+Condition2+OverallQual+
                YearBuilt+RoofStyle+RoofMatl+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinSF1+BsmtUnfSF+
                `1stFlrSF` +`2ndFlrSF`+FullBath+KitchenQual+WoodDeckSF+ScreenPorch+MiscFeature+MiscVal+SaleType+
                SaleCondition,data=train.set)
summary(model.3)

# use the model to predict SalePrice on valid.set and calculate mse
sp3<- predict(model.3, newdata=valid.set)
mse.3.valid<-mean((valid.set[,81]-sp3)^2)
mse.3.valid
# mse.3.valid=1255004611

final.model <- lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+Neighborhood+Condition2+OverallQual+
                    YearBuilt+RoofStyle+RoofMatl+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinSF1+BsmtUnfSF+
                    `1stFlrSF` +`2ndFlrSF`+FullBath+KitchenQual+WoodDeckSF+ScreenPorch+MiscFeature+MiscVal+SaleType+
                    SaleCondition,data=train.set)

summary(final.model)

# use the model to predict SalePrice on valid.set and calculate mse
spfinal<- predict(final.model, newdata=valid.set)
mse.final.valid<-mean((valid.set[,81]-spfinal)^2)
mse.final.valid
# mse.final.valid=1067579514

# We re-subsetted the training set using the following commented code to evaluate significance
# of varibales for different subsets of the training set.

# sub <- sample(1:length(train$Id), length(train$Id)/3)
# train.set <- train[-sub,]
# valid.set <- train[sub,]

# Build new model using variables that are more significant
final.model2 <- lm(SalePrice ~ MSSubClass+LotArea+OverallQual+Neighborhood+ YearBuilt+ExterQual+BsmtQual+BsmtExposure+BsmtFinSF1+`1stFlrSF`+
                     `2ndFlrSF`+KitchenQual+ ScreenPorch+SaleCondition ,data=train)
summary(final.model2)

# use the model to predict SalePrice on valid.set and calculate mse
spfinal.2<- predict(final.model2, newdata=valid.set)
mse.final2.valid<-mean((valid.set[,81]-spfinal.2)^2)
mse.final2.valid
# mse.final2.valid= 757702278

# This is our best model. Had the highest R^2 and R^2 adjusted. Therefore, these are the significant variables:
sig.cols <- c('MSSubClass', 'LotArea', 'Neighborhood', 'OverallQual', 'YearBuilt', 'ExterQual', 'BsmtQual', 'BsmtExposure', 'BsmtFinSF1', 
              "1stFlrSF", '2ndFlrSF', 'KitchenQual',  'ScreenPorch', 'SaleCondition')


# Still going to try more models to reduce the number of variables
final.model3 <- lm(SalePrice ~ MSSubClass+LotArea+YearBuilt+BsmtQual+BsmtFinSF1+`1stFlrSF`+
                     `2ndFlrSF`+KitchenQual+ ScreenPorch,data=train)

summary(final.model3)

# use the model to predict SalePrice on valid.set and calculate mse
spfinal.3<- predict(final.model3, newdata=valid.set)
mse.final3.valid<-mean((valid.set[,81]-spfinal.3)^2)
mse.final3.valid
# mse.final3.valid=1023837418
# Did worse in Kaggle

# Try more models 
final.model4 <- lm(SalePrice ~ MSSubClass+OverallQual+LotArea+Neighborhood+YearBuilt+BsmtFinSF1+`1stFlrSF`+
                     `2ndFlrSF`+KitchenQual+ ScreenPorch,data=train)
summary(final.model4)
# Adjusted MSE decreased so is not likely to be a good model. Still, tested this in Kaggle and did worse.

spfinal.4<- predict(final.model4, newdata=valid.set)
mse.final4.valid<-mean((valid.set[,81]-spfinal.4)^2)
mse.final4.valid
# mse.final4.valid=862105332

# Try more models 
final.model5 <- lm(SalePrice ~ MSSubClass+LotArea+YearBuilt+`1stFlrSF`+`2ndFlrSF`+KitchenQual+ ScreenPorch,data=train)
summary(final.model5)

spfinal.5<- predict(final.model5, newdata=valid.set)
mse.final5.valid<-mean((valid.set[,81]-spfinal.5)^2)
mse.final5.valid
# mse.final5.valid=1203332127
# Adjusted MSE decreased so is not likely to be a good model. Still, tested this in Kaggle and did worse.

# The model final.model2 performed best. This is the model we will use for the Kaggle Competition for Parametric.

###################################################################################
#                                                                                 #
#                             Cleaning Test Data                                  #
#                                                                                 #
###################################################################################
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


# Now the testing dataset is cleaned.


################# Writing Parametric Predictions to CSV File ###################################

# Making our predicitons and writing them to a csv file
predictions <- predict(final.model, newdata=test)
predictions.table <- cbind(test$Id, predictions)
write.table(predictions.table, file="C1-10_Parametric1.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')

# CMB: test new varables 1
predictions <- predict(final.model2, newdata=test)
predictions.table <- cbind(test$Id, predictions)
write.table(predictions.table, file="C1-10_Parametric2.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')

# CMB: test new varables 2
predictions <- predict(final.model3, newdata=test)
predictions.table <- cbind(test$Id, predictions)
write.table(predictions.table, file="C1-10_Parametric3.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')

# CMB: test new varables 3
predictions <- predict(final.model4, newdata=test)
predictions.table <- cbind(test$Id, predictions)
write.table(predictions.table, file="C1-10_Parametric5.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')

# CMB: test new varables 4, adding neighborhood to best submission (14 variables)
predictions <- predict(final.model2, newdata=test)
predictions.table <- cbind(test$Id, predictions)
write.table(predictions.table, file="C1-10_Parametric6.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')


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


length(c('MSSubClass','MSZoning','LotArea','LotShape','LandContour','Neighborhood','Condition2','OverallQual',
         'YearBuilt','RoofStyle','RoofMatl','ExterQual','Foundation','BsmtQual','BsmtExposure','BsmtFinSF1','BsmtUnfSF',
         '1stFlrSF' ,'2ndFlrSF','FullBath','KitchenQual','WoodDeckSF','ScreenPorch','MiscFeature','MiscVal','SaleType',
         'SaleCondition'))
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
avg.price

sig.cols
sig.cols.non <- c(sig.cols, 'SalePrice')
# Using the sig.cols
subsetted.train2 <- train[, sig.cols.non]
subsetted.test2 <- test[,sig.cols]
K <- 102
dists <- rep(0, times=length(subsetted.train2$MSSubClass))
avg.price <- rep(0, times=length(subsetted.test2$MSSubClass))
subsetted.train2$Distance <- NA
for(i in 1:length(subsetted.test2$MSSubClass)){
  x <- subsetted.test2[i,1:13]
  dists <- apply(subsetted.train2[,1:13], MARGIN=1, distance, y=x)
  subsetted.train2$Distance <- dists
  k.neighbors <- subsetted.train2[subsetted.train2$Distance <= quantile(subsetted.train2$Distance, K/length(subsetted.train2$MSSubClass)),]
  avg.price[i] <- mean(k.neighbors$SalePrice)
}
avg.price
mean((subsetted.train2[,14]-avg.price)^2)
predictions.table <- cbind(test$Id, avg.price)
predictions.table[1,]

write.table(predictions.table, file="C1-10_Non_Parametric_k145.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')

predictions.table <- cbind(test$Id, avg.price)
write.table(predictions.table, file="C1-10_Non_Parametric6.csv", row.names=F, col.names = c("Id", "SalePrice"), sep=',')

predictions.table[1,]

#########################



train.set2  <- train.set[1:200, sig.cols.non]
valid.set2 <- train.set[201:400, sig.cols]

classes <- sapply(train.set2, class)
indices_categorical <- which(classes != 'integer')
unname(indices_categorical)
for(i in indices_categorical){
  train.set2[,i] <- as.integer(as.factor(unlist(train.set2[,i])))
}

classes <- sapply(valid.set2, class)
indices_categorical <- which(classes != 'integer')
unname(indices_categorical)
for(i in indices_categorical){
  valid.set2[,i] <- as.integer(as.factor(unlist(valid.set2[,i])))
}

K <- 13
dists <- rep(0, times=length(train.set2$MSSubClass))
avg.price <- rep(0, times=length(valid.set2$MSSubClass))
train.set2$Distance <- NA
for(i in 1:length(valid.set2$MSSubClass)){
  x <- valid.set2[i,1:13]
  dists <- apply(train.set2[,1:13], MARGIN=1, distance, y=x)
  train.set2$Distance <- dists
  k.neighbors <- train.set2[train.set2$Distance <= quantile(train.set2$Distance, K/length(train.set2$MSSubClass)),]
  avg.price[i] <- mean(k.neighbors$SalePrice)
}
avg.price

# This is for test and validation subset of 20 rows
#  MSE K=5
mean((valid.set2[,14]-avg.price)^2)
# [1] 28544064893

#  MSE K=6
mean((valid.set2[,14]-avg.price)^2)
# [1] 29731758320

#  MSE K=10
mean((valid.set2[,14]-avg.price)^2)
# [1] 32634212803

#  MSE K=2
mean((valid.set2[,14]-avg.price)^2)
# [1] 27201681601

#  MSE K=15
mean((valid.set2[,14]-avg.price)^2)

# Test and validation subset of 100 rows

# K=10-- 10%
mean((valid.set2[,14]-avg.price)^2)
# [1] 27753781670

# K=15 -- 15%
mean((valid.set2[,14]-avg.price)^2)
# [1] 28032779369

# K=5 -- 5%
mean((valid.set2[,14]-avg.price)^2)
# [1] 27226754730

# K=

# K=4 -- 4%
mean((valid.set2[,14]-avg.price)^2)
# [1] 27565702388

# Testing our theory that MSE is minimized for K= 5% of number of rows on test set of 200 rows
# K=10 -- 5%
mean((valid.set2[,14]-avg.price)^2)
#[1] 30493590293

# K=5 -- 2.5%
mean((valid.set2[,14]-avg.price)^2)
# [1] 30959572571

# K=15 -- 7.5%
mean((valid.set2[,14]-avg.price)^2)
# [1] 30198609740

# K=20 -- 10%
mean((valid.set2[,14]-avg.price)^2)
# 30690971029

# K=16 -- 8%
mean((valid.set2[,14]-avg.price)^2)
# [1] 30353216550

# K=14 -- 7%
mean((valid.set2[,14]-avg.price)^2)
# [1] 30102178878

# K=13 -- 6.5%
mean((valid.set2[,14]-avg.price)^2)
# [1] 30184335713


# We have determine that a K value that is 7% of the number of observations gives the lowest
# test MSE. We will use K= .07*1451=102 to optimize our KNN