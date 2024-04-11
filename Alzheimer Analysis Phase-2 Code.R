library(MASS)
library(plyr)
library(readr)
library(tidyr)
library(broom)
library(gtsummary)
library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance

setwd("C:/Users/DELL/Desktop/")
#Read in Datasets
Alz <- read.csv(file="alzheimer-1.csv")

#Check data is read in correctly
View(Alz)

head(Alz)

#Check Variable Names
names(Alz)


#Check Data structure
str(Alz)


#Check Missing Data
sum(is.na(Alz))

#Treat Missing Values

#Listwise Deletion
Alz_new <- na.omit(Alz)

#Check new data has no missing data
sum(is.na(Alz_new))
View(Alz_new)
head(Alz_new)

##########################################
Alz_new$Dementia<- revalue(Alz_new$Dementia,c("Alzheimer"=0, "No Alzheimer"=1))
Alz_new$Dementia<- as.factor(Alz_new$Dementia)
View(Alz_new)
##################################################################
#a)
set.seed(123)  # use a set seed point for reproducibility
split <- initial_split(Alz_new, prop = .7, strata = "Dementia")
train <- training(split)
test  <- testing(split)

#Logistic Regression

#For explaining dependent variable

Alz_new$Dementia <- as.factor(Alz_new$Dementia)

log_reg <- glm(
  Dementia ~ Gender + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF  ,
  family = "binomial", 
  data = Alz_new
)

summary(log_reg) 

tidy(log_reg) 

#Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 
"-----------------------------------------------------"
#b)

train$Dementia <- as.factor(train$Dementia)

#For Predicting dependent variable
log_reg = train(
  form = Dementia ~ Gender + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF, 
  data = train,
  method = "glm",
  family = "binomial"
)

pred <- predict(log_reg, test)
pred


#Confusion Matrix
confusionMatrix(pred, as.factor(test$Dementia))
vip(log_reg, num_features = 10)
"-----------------------------------------------------"
#c)

#ROC Curves

log_reg_train <- glm(Dementia ~ Gender + EDUC + Age + SES + MMSE + CDR+eTIV, 
                     data = train,  family = "binomial")
library(ROCR)

log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

preds <- prediction(as.numeric(log_reg_test_prob), test$Dementia)

perf <- performance(preds,"tpr","fpr")
plot(perf,colorize=TRUE)

library(precrec)
precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$Dementia)
autoplot(precrec_obj)

## Get AUCs
sm_aucs <- auc(precrec_obj)
## Shows AUCs
sm_aucs

precrec_obj2 <- evalmod(scores = log_reg_test_prob, labels = test$Dementia, mode="basic")
autoplot(precrec_obj2)  

library(ROCit)

ROCit_obj <- rocit(score=log_reg_test_prob,class=test$Dementia)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$Dementia,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure

















