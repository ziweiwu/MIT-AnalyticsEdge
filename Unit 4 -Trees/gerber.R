gerber=read.csv("gerber.csv")
str(gerber)
summary(gerber)
#build a logistic regression model 
voting_log_model=glm(voting~civicduty+hawthorne+self+neighbors, data=gerber, family="binomial")
summary(voting_log_model)
#predict using logistic regression model 
predict_voting_log=predict(voting_log_model, type="response")
table( gerber$voting, predict_voting_log>0.5)
#baseline model 
table(gerber$voting)
#calculate AUC
library(ROCR)
pred = prediction(predict_voting_log, gerber$voting)
as.numeric(performance(pred, "auc")@y.values)

# CART model
voting_cart_model = rpart(voting~civicduty+hawthorne+self+neighbors+sex+control, data = gerber, cp=0.0)
prp(voting_cart_model)


#Compared control vs control +sex on voting
CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
CARTsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTsex, digits=6)

#Create another logistic regression model using only control+sex and control AND sex
voting_log_model2=glm(voting~control+sex+control:sex, data=gerber, family="binomial")
summary(voting_log_model2)