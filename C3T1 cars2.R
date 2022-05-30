install.packages("readr")
library(readr)

cars <- read.csv("C:\\Users\\johne\\Documents\\Purdue DA\\C3 rstudio\\RTutorialDataSets\\cars.csv", header=TRUE)
summary(cars)

   ### drop names column and rename speed and distance to simplify
car_df <- data.frame(cars$speed.of.car, cars$distance.of.car)
names(car_df) <-c("speed", "distance")
summary(car_df)

trainSize<-round(nrow(car_df)*0.8) 
testSize<-nrow(car_df)-trainSize
trainSize
testSize

set.seed(122)
     
train_indices<-sample(seq_len(nrow(car_df)),size =trainSize)
train_indices
trainSet<-car_df[train_indices,]
testSet<-car_df[-train_indices,] 
str(trainSet)
str(testSet)
trainSet
testSet

Model <-lm(distance~speed, data = car_df)
summary(Model)
Prediction <- predict(Model,testSet)
Prediction

yint <- -29.3371
slope <- 4.6959
verify <- testSet
verify$newcol<- with(testSet, newcol<-slope * speed + yint)
verify
