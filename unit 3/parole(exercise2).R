parole=read.csv("parole.csv")
str(parole)
summary(parole)
#Show how many parole violates
table(parole$violator==1)
#convert variables to factor variables 
parole$crime=as.factor(parole$crime)
parole$state=as.factor(parole$state)
#split into train and test
set.seed(2000)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
#build the logistic regression model to predict violation of parole 
log_model_1=glm(violator~., data=train, family=binomial)
summary(log_model_1)
#predict violation of test set, and find max predict probability 
testpredict=predict(log_model_1, newdata=test, type="response")
max(testpredict)
#confusion matrix
table(test$violator, testpredict>0.5)
#Use ROCR package to obtain AUC value \
#AUC:The probability of differentiating between a randomly selected positive and negative example. 
#It is independent of the regression cutoff selected.
library(ROCR)
pred = prediction(testpredict, test$violator)
as.numeric(performance(pred, "auc")@y.values)