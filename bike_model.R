library(randomForest)
library(ggplot2)
library(gbm)

#http://gettinggeneticsdone.blogspot.com/2011/02/split-data-frame-into-testing-and.html
#http://blog.yhathq.com/posts/comparing-random-forests-in-python-and-r.html

setwd("/Users/Ying/Documents/Kaggle/Bike/")
trainData <- read.csv("train.csv", header=TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header=TRUE, stringsAsFactors = FALSE)

head(trainData)
head(testData)

trainData$season = as.factor(trainData$season)
trainData$holiday = as.factor(trainData$holiday)
trainData$workingday = as.factor(trainData$workingday)
trainData$weather = as.factor(trainData$weather)

testData$season = as.factor(testData$season)
testData$holiday = as.factor(testData$holiday)
testData$workingday = as.factor(testData$workingday)
testData$weather = as.factor(testData$weather)

# Extract hour, weekday, month, and year from datetime
datetime = as.POSIXlt(trainData$datetime)
hour = as.factor(datetime$hour)
weekday = as.factor(datetime$wday)
month = as.factor(datetime$mon)
year = 1900 + datetime$year
trainData$datetime = datetime

trainData = cbind(trainData, hour, weekday, month, year)

datetime2 = as.POSIXlt(testData$datetime)
hour = as.factor(datetime2$hour)
weekday = as.factor(datetime2$wday)
month = as.factor(datetime2$mon)
year = 1900 + datetime2$year
testData$datetime = datetime2

testData = cbind(testData, hour, weekday, month, year)


fcasual <- aov(trainData$casual ~ trainData$season + trainData$holiday + trainData$workingday + trainData$weather + trainData$temp + trainData$humidity + trainData$windspeed + trainData$hour + trainData$weekday + trainData$month + trainData$year)
summary(fcasual)
freg <- aov(trainData$registered ~ trainData$season + trainData$holiday + trainData$workingday + trainData$weather + trainData$temp + trainData$humidity + trainData$windspeed + trainData$hour + trainData$weekday + trainData$month + trainData$year)
summary(freg)

dataplot=aggregate(count ~ + workingday+ holiday, data =trainData, FUN=mean)
ggplot(dataplot, aes(x=workingday, y=holiday)) + geom_tile(aes(fill = count))+ scale_fill_gradient(low="white", high="red")

#drop datetime, temp, atemp, humidity, windspeed and all labels
#trainDataFinal = trainData[-c(1,6,7,8,9,12)]
#testDataFinal = testData[-c(1,6,7,8,9)]

training <- trainData
testing <- testData

rf <- randomForest(casual ~ season + holiday + workingday + weather + hour + weekday + month + year + temp, data = training , ntree = 200, importance = TRUE, keep.forest = TRUE, mtry = 8)
varImpPlot(rf, type = 1)

rf2 <- randomForest(registered ~ season + holiday + workingday + weather + hour + weekday + month + year + temp, data = training , ntree = 200, importance = TRUE, keep.forest = TRUE, mtry = 8)
varImpPlot(rf2, type = 1)

head(testing)

predicted_casual <- predict (rf, newdata = testing)
predicted_reg <- predict (rf2, newdata = testing)
predicted_count <- predicted_casual + predicted_reg

label <- testData$datetime
final_predict <- unname(predicted_count)

rf.sub <- list()
rf.sub$datetime <- testData$datetime
rf.sub$count <- predicted_count
write.csv(rf.sub, file = "rf.csv", row.names = FALSE)


