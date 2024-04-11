library(MASS)
library(plyr)
library(readr)
setwd("C:/Users/DELL/Desktop")

#Read in Datasets
Alz = read.csv("alzheimer.csv")
View(Alz)

#Check dimensions of Alz
dim(Alz)
str(Alz)
head(Alz)

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(Alz))
#63 total missing values


#Treat Missing Values

#Listwise Deletion
Alz_new <- na.omit(Alz)

#Check new data has no missing data
sum(is.na(Alz_new))
View(Alz_new)
head(Alz_new)
################################################################################################

Alz_new$Dementia<- revalue(Alz_new$Dementia,c("Alzheimer"=0, "No Alzheimer"=1))
Alz_new$Dementia<- as.factor(Alz_new$Dementia)
View(Alz_new)

#Graph Data
library(psych)
pairs.panels(Alz_new[1:10],
             gap = 0,
             bg = c("red", "green", "blue")[Alz_new$Dementia],
             pch = 21)

###################################################################################################


#a) Performed of the classifier using cross-validation.
#With Cross Validation
# The dependent variable must be categorical
Alz_LDA <- lda(Dementia ~ ., data=Alz_new, CV=TRUE)
Alz_LDA

#To Plot the Data, you cannot use CV
Alz_LDA <- lda(Dementia ~ ., data=Alz_new)
Alz_LDA

p <- predict(Alz_LDA, newdata=Alz_new[,1:10])$class
p

table_1 <- table(p, Alz_new$Dementia)
table_1

sum(diag(table_1)/sum(table_1))
accuracy <- (378+564)/(378+564+6+3)
accuracy
"------------------------------------------------------"
"b) Performance of the classifier using training and testing"

require(caTools)  # loading caTools library
library(caTools)

set.seed(123) 
sample = sample.split(Alz_new,SplitRatio = 0.70)
train =subset(Alz_new,sample ==TRUE)
test=subset(Alz_new, sample==FALSE)

# The dependent variable must be categorical (Assuming No Cross-Validation)
Alz_LDA = lda(Dementia ~ ., data=train)
Alz_LDA

plot(Alz_LDA)

prd<-predict(Alz_LDA, train)$class
prd
Table<- table(prd, train$Dementia)
Table
sum(diag(Table)/sum(Table))
mean(prd== train$Dementia)


prd <- predict(Alz_LDA, train)
#Stacked Histogram of LDA Functions
ldahist(data=prd$x[,1], g = train$Dementia)




###############################################################################################

