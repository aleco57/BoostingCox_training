###########################################
############### Boosting in R #############
###########################################

#library
library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(SHAPforxgboost)

######################
### Classification
######################

#Split data to test and train
trainIndex <- createDataPartition(iris$Species, 
                                  list=F,
                                  p=.8)
irisTrain <- iris[ trainIndex,]
irisTest  <- iris[-trainIndex,]


#Train Model Using xgboost
#trainControl evaluates various hyperparamter values, we could also tune through specified values via tuneGrid = 
set.seed(123)
xgbmodel <- train(Species ~.,
                  data = irisTrain,
                  method = "xgbTree",
                  trControl = trainControl("cv", number = 5))


#Graphical display of hyperparameter tuning
plot(xgbmodel)


#Can use various selectionFunctions to select our best model e.g. oneSE/tolerance
xgbmodel$bestTune


#Evaluate performance metrics, in this
Pred <- xgbmodel %>% predict(irisTest)
confusionMatrix(Pred, irisTest$Species)



######################
### Regression
######################

data("Boston", package = "MASS")

trainIndex <- createDataPartition(Boston$medv, 
                                  list=F,
                                  p=.8)

BostonTrain <- Boston[ trainIndex,]
BostonTest  <- Boston[-trainIndex,]

xgbmodel_reg <-  train(
  medv ~., data = BostonTrain, method = "xgbTree",
  trControl = trainControl("cv", number = 5)
)

plot(xgbmodel_reg)
Pred2 <- xgbmodel_reg %>% predict(BostonTest)

R2(Pred2, BostonTest$medv)
RMSE(Pred2, BostonTest$medv)