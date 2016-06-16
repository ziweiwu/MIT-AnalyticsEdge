loans=read.csv("loans.csv")
str(loans)
summary(loans)
#propertion of loan not fully paid
table(loans$not.fully.paid)
#fill the missing values with imputation 
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
#split the data into train and test sets, in approximate 70:30 
set.seed(2000)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
#build logistic regression model to predict not.fully.paid.
log_model_1=glm(not.fully.paid~., data=train, family="binomial")
summary(log_model_1)
#use log_model_1 to predict the odds of loan not paying back for test
predicted.risk=predict(log_model_1,newdata=test, type="response")
test$predicted.risk=predicted.risk
#confusion matrix of not paying back using threshold 0.5
table(test$not.fully.paid, predicted.risk>0.5)
#use ROCR to get AUC value
library(ROCR)
pred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)
#build a second model using only interest rate to predict not fully paid
log_model_2=glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(log_model_2)
#predict using the second model
predicted.risk2=predict(log_model_2,newdata=test, type="response")
summary(predicted.risk2)
#confusion matrix of second prediction with threshold 0.5
table(test$not.fully.paid, predicted.risk2>0.5)
#AUC value for prediction 2
pred = prediction(predicted.risk2, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)
#assign new variable profit to train/test set, assume loan amount is 1 dollar
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
train$profit = exp(train$int.rate*3) - 1
train$profit[train$not.fully.paid == 1] = -1
#subset the train datatset to only include higher interest rates=>0.15
highInterest=subset(test,int.rate>=0.15)
#check the  profit of high interest loans and proportion of loans not fully paid back
summary(highInterest$profit)
table(highInterest$not.fully.paid)
#select high interest loans with higher probability of paying back (100th cutoff)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
#sum of profit of selected loans
sum(selectedLoans$profit)