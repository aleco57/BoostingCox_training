#Format data to DMatrix
lung <- lung

label <- ifelse(lung$status == 2, lung$time, -lung$time) #Format so - values are censored
Dmat <- as.matrix(lung[,-c(2,3)]) %>% xgb.DMatrix(label = label)

#First build a model with default parametes
params <- list(booster = "gbtree", 
               objective = "survival:cox", 
               eta=0.3, #This controls the learning rate, between 0-1
               gamma=0, #This is the regularisation parameter, start with 0 and look at CV error rate, if train error >>> test erro then increase gamma
               max_depth=3, #Maximum depth of tree, larger the depth the more complex model which means more likely to overfit
               min_child_weight=1, #Block feature interaction to prevent overfitting
               subsample=1, #Controls the number of observations supplied to a tree, 0-1
               colsample_bytree=1 #Controls the number of features supplied to a tree, 0-1
)


xgbcv <- xgb.cv(data = Dmat,
                params = params, #The default parameters we have listed previously
                nrounds = 500, #Maximum number of rounds
                nfold = 5, #Number of folds,
                showsd = T, #Shows sd of cross validation
                stratified =T, #Statified by values of outcome labels
                print_every_n = 10, #prints message each 10 round rather than every round
                early_stopping_rounds = 20, #If doesn't improve for 20 round then the code will stop running
                maximize = F, #We must specify this when specify an early stopping round
)

#The model stopped at nrounds=21, meaning the first iteration showed the lowest test_cox_nloglik_mean
#The output also suggested that test and train errors were comparible meaning we will leave gamma=0

xgb1 <- xgb.train(data = Dmat,
                  params = params, 
                  nrounds = 10, 
                  #watchlist = list(val=dtest,train=dtrain), this is used when we have split our data previously to test and train
                  #print.every.n = 10, default is 1 so when only doing 10 rounds this line of code is not necessary 
                  maximize = F, 
                  eval_metric = "cox-nloglik")

#We can further tune hyperparamters through a grid search


#We can evaluate our models performance
risk_scores <- predict(object = xgb1, newdata = as.matrix(lung[,-c(2,3)]), type = "risk")
hist(risk_scores)


### SHAP ###
############

#We can extract SHAP vaues for our features
shap_values <- shap.values(xgb_model = xgb1, X_train = Dmat)
shap_values$mean_shap_score

#The function wants the training data in matrix form under the X_train argument
#Here we are generating a large summary plot
shap_long <- shap.prep(xgb_model = xgb1, X_train = as.matrix(lung[,-c(2,3)]))
shap.plot.summary(shap_long)

#We can also generate dependence plots for individual features with this package
shap.plot.dependence(data_long = shap_long, x = "age")

#Shap interaction plot
#First we must prep the data as done previously
shap_int <- shap.prep.interaction(xgb_model = xgb1, X_train = as.matrix(lung[,-c(2,3)]))
# Then we can plot our interaction plot
shap.plot.dependence(data_long = shap_long,
                     data_int = shap_int,
                     x= "Column_WV",
                     y = "AOT_Uncertainty", 
                     color_feature = "AOT_Uncertainty")


