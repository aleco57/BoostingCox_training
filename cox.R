####################################
############# Survival #############
####################################

# Load packages
library("survival")
library("survminer")
library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(SHAPforxgboost)

#Fit our cox model adding sex as a covariate
res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
head(lung)

#Print the object we have created to observe covariates
res.cox
summary(res.cox)


#We can also use multivariate analysis
res.cox2 <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  lung)
summary(res.cox2)

###Plots
#This plots the mean value of covariates in the model are used by default
ggsurvplot(survfit(res.cox2), color = "#2E9FDF",
           ggtheme = theme_minimal(), data=lung)
#We can also plot by splitting covaraites, in this intance we are modelling sex
sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)
sex_df
fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"), data=lung,
           ggtheme = theme_minimal())



###########
#Boosting
###########

#Format data to DMatrix
lung <- lung

label <- ifelse(lung$status == 2, lung$time, -lung$time) #Format so - values are censored
Dmat <- as.matrix(lung[,4:10]) %>% xgb.DMatrix(label = label)

#No tuning of hyperparameters
model <- xgboost(data = Dmat, 
                 max.depth = 2, 
                 eta = 1, 
                 nthread = 2, 
                 nrounds = 1000, 
                 objective = "survival:cox")

#Tune hyperparameters
set.seed(123)
xgb.fit1 <- xgb.cv(
  data = Dmat,
  nrounds = 1000,
  nfold = 5,
  objective = "survival:cox", 
  verbose = 0               
)





#Feature Importance
a <- xgb.importance(model = model) 
xgb.plot.importance(a)

