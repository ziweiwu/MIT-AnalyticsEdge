stock = read.csv("StocksCluster.csv")
str(stock)
summary(stock)
table(stock$PositiveDec)
cor(stock)


#split data into train and test
library("caTools")
set.seed(144)

spl = sample.split(stock$PositiveDec, SplitRatio = 0.7)

train = subset(stock, spl == TRUE)

test = subset(stock, spl == FALSE)

#logistic regression
StocksModel = glm(PositiveDec ~ ., data=train, family=binomial)

PredictTrain = predict(StocksModel, type="response")

table(train$PositiveDec, PredictTrain > 0.5)

Predict_test = predict(StocksModel, newdata=test, type="response")

table(test$PositiveDec, Predict_test > 0.5)

#remove dependent variables before clustering
limitedTrain = train

limitedTrain$PositiveDec = NULL

limitedTest = test

limitedTest$PositiveDec = NULL

#normalize
library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

#K mean
k=3
set.seed(1)
stock_Kmean = kmeans(normTrain, centers = k, iter.max = 1000)
table(stock_Kmean$cluster)

library(flexclust)

km.kcca = as.kcca(stock_Kmean, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

#subsetting 

stocksTrain1 = subset(train, clusterTrain == 1)

stocksTrain2 = subset(train, clusterTrain == 2)

stocksTrain3 = subset(train, clusterTrain == 3)

stocksTest1 = subset(test, clusterTest == 1)

stocksTest2 = subset(test, clusterTest == 2)

stocksTest3 = subset(test, clusterTest == 3)

#build logistic regression using subsets
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)

StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)

StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

 summary(StocksModel1) 
 summary(StocksModel2) 
 summary(StocksModel3)

 PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type="response")

PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type="response")

PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type="response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)

table(stocksTest2$PositiveDec, PredictTest2 > 0.5)

table(stocksTest3$PositiveDec, PredictTest3 > 0.5)

#overall accuracy using cluster and predict approach
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)