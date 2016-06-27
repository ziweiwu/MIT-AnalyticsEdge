census=read.csv("census.csv")
library(caTools)
set.seed(5000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
str(census)

#build a logistic regression model to predict whether income>50k
income_log=glm(over50k~., data=train, family="binomial")
summary(income_log)
income_log_predict=predict(income_log, newdata=test,type="response")
table(test$over50k, income_log_predict>=0.5)


#built a cart model 
income_CART=rpart(over50k~., data=train, method="class")
prp(income_CART)
income_CART_predict=predict(income_CART, newdata=test, type="class")
cart_table=table(test$over50k, income_CART_predict)
confusionMatrix(cart_table)

#built a randomforest model
#first need to downsample the train set to reduce computation load
set.seed(6000)
trainSmall = train[sample(nrow(train), 2000), ]
income_RF=randomForest(over50k~., data=trainSmall)
income_RF_predict=predict(income_CART, newdata=test,type="class")
rf_table=table(test$over50k, income_RF_predict)
confusionMatrix(rf_table)

#see which variable is important in randomforest model
vu = varUsed(income_RF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(income_RF$forest$xlevels[vusorted$ix]))
#observe the variable has most reduction in impurity
varImpPlot(income_RF)

#use cross validation to find cp value to make CART model better
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 
train(over50k~., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )


#cp value is 0.002, use it in CART and make a new prediction
income_CART2=rpart(over50k~., data=train, method="class",cp=0.002)
income_CART_predict2=predict(income_CART, newdata=test, type="class")
cart_table2=table(test$over50k, income_CART_predict2)
confusionMatrix(cart_table2)

#plot the trees for the new cart model, by using a small cp, number of splits has increased dramatically
prp(income_CART2)
