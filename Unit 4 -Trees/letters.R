letters=read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(3000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl==TRUE)
test = subset(letters, spl==FALSE)
table(letters$isB)

#build a classification tree to predict B
CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)
PredictB_CART=predict(CARTb, newdata=test,type="class")
table(test$isB, PredictB_CART)

#build a random forest tree to predict B
RFb=randomForest(isB ~ . - letter, data=train)
PredictB_RF=predict(RFb, newdata=test,type="class")
table(test$isB, PredictB_RF)

#build a classification tree to predict letter
CART = rpart(letter ~ . - isB, data=train, method="class")
prp(CART)
Predictletter_CART=predict(CART, newdata=test,type="class")
cart_table=table(test$letter, Predictletter_CART)

#build a random forest tree to predict letter
RF=randomForest(letter ~ . - isB, data=train)
Predictletter_RF=predict(RF, newdata=test,type="class")
RF_table=table(test$letter, Predictletter_RF)

#caret package can calculate the accuracy of confusion matrix for the models
library(caret) 
confusionMatrix(cart_table)
confusionMatrix(RF_table)