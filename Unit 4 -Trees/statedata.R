statedata=read.csv("statedatasimple.csv")
data(state)
statedata = data.frame(state.x77)
str(statedata)

#build a linear regression model 
state_LR=lm(Life.Exp~., data=statedata)
predict_LR=predict(state_LR)
summary(state_LR)
SSE=sum(state_LR$residuals^2)
SSE

#build a second LR model using fewer independent variables 
state_LR2=lm(Life.Exp~ Population+Murder+Frost+HS.Grad, data=statedata)
predict_LR2=predict(state_LR2)
summary(state_LR2)
SSE=sum(state_LR2$residuals^2)
SSE

#build a CART model 
state_CART=rpart(Life.Exp~., data=statedata, minbucket=5)
prp(state_CART)
state_CART_predict=predict(state_CART)
cart_table=table(state$Life.Exp, state_CART_predict)
confusionMatrix(cart_table)

#calculate the sum of squared error for CART model 
SSE=sum((statedata$Life.Exp-state_CART_predict)^2)
SSE

#fine tune the model by finding a good cp value
library(caret)
set.seed(3000)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 
train(Life.Exp~., data = statedata, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

#predict using CART with cp of 0.1
state_CART2=rpart(Life.Exp~., data=statedata, cp=0.1)
prp(state_CART2)
state_CART_predict2=predict(state_CART2)
SSE=sum((statedata$Life.Exp-state_CART_predict2)^2)
SSE


