library(maps)
library(ggmap)
statesMap = map_data("state")
str(statesMap)	

#plot a map of states 
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "pink")
 

 #load polling data and create a logistic regression model 

polling = read.csv("PollingImputed.csv")

Train = subset(polling, Year < 2012)

Test = subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary)  

mean(TestPrediction)

#convert state variable to lower case and merge predictiondata frame and states map 
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#make sure the prediction map is in order
predictionMap = predictionMap[order(predictionMap$order),]

#plot the prediction map by binary value, 0 is republic state and 1 is democratic
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")


#plot by probability 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", name = "Prediction 2012")


