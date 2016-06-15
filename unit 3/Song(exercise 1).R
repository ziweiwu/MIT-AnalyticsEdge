#load csv file and give name song to it
song=read.csv("songs.csv")
str(song)
#which function gives the array numbers for top 10 songs by michael jackson
song$songtitle[which(song$artistname=="Michael Jackson"& song$Top10==1)]
#a summary of variable discrete timesignature for songs
table(song$timesignature)
#the song with the fastest tempo
song$songtitle[which(song$tempo==max(song$tempo))]
#split the sample into train set an test set 
train=subset(song, year<=2009)
test=subset(song, year==2010)
#remove some independent variables that can not be used in logistic regression model 
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[ , !(names(train) %in% nonvars) ]
test = test[ , !(names(test) %in% nonvars) ]
#create the logistic regression model to predict top 10 hits
#AIC score measures the quality of model 
log_model_1=glm(Top10~., data=train, family=binomial)
summary(log_model_1)
#find the correlation between energy and loudness of the song
cor(train$loudness, train$energy)
#create another model without independent variable loudness
log_model_2=glm(Top10~.-loudness,data=train, family=binomial)
summary(log_model_2)
#create final model without energy variable
log_model_3=glm(Top10~.-energy,data=train, family=binomial)
summary(log_model_3)
#predict test set using model 3
testpredict=predict(log_model_3, newdata=test,type="response")
#calculate the accuracy of the model using threshold 0.45
table(test$Top10, testpredict >= 0.45)
#calculate the accuracy of baseline model 
table(test$Top10)