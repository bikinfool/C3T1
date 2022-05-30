#    secction 7 error tracking.

install.packages(readr)
library("readr")


Irisdf <- read.csv("C:\\Users\\johne\\Documents\\Purdue DA\\C3 rstudio\\RTutorialDataSets\\iris.csv")
Irisdf
### Deleted column 1, from excel file: "read" automatically added row names and had the name in twice.
### adding: row.names=NULL)[,-1] worked as well, but easier in excel.



##  EDA
attributes(Irisdf)
summary(Irisdf) 
str(Irisdf)
names(Irisdf)
hist(Irisdf$Species)
plot(Irisdf$Sepal.Length)
qqnorm(Irisdf$Sepal.Length)



# NOT SPECIFICALLY ASKED IN REPORT
#Is there a difference in species???
#### Yes, there are average differences, but all 3 groups fall along same trend line

by_species_sep_len_avg_df <-aggregate(Irisdf$Sepal.Length, list(Irisdf$Species), FUN = mean)
names(by_species_sep_len_avg_df) <-c("Species", "Sep_Len_avg")
by_species_sep_len_avg_df
by_species_sep_wid_avg_df <-aggregate(Irisdf$Sepal.Width, list(Irisdf$Species), FUN = mean)
names(by_species_sep_wid_avg_df) <-c("Species", "Sep_Wid_avg")
by_species_sep_wid_avg_df
by_species_pet_len_avg_df <-aggregate(Irisdf$Petal.Length, list(Irisdf$Species), FUN = mean)
names(by_species_pet_len_avg_df) <-c("Species", "Pet_Len_avg")
by_species_pet_len_avg_df
by_species_pet_wid_avg_df <-aggregate(Irisdf$Petal.Width, list(Irisdf$Species), FUN = mean)
names(by_species_pet_wid_avg_df) <-c("Species", "Pet_Wid_avg")
by_species_pet_wid_avg_df

FlowerAvg_df <- ""
FlowerAvg_df <- merge(by_species_sep_len_avg_df, by_species_sep_wid_avg_df,by="Species")
FlowerAvg_df2 <- merge(FlowerAvg_df, by_species_pet_len_avg_df,by="Species")
FlowerAvg_df3 <- merge(FlowerAvg_df2, by_species_pet_wid_avg_df,by="Species")
FlowerAvg_df3

## NOT NECESSARY, but convert characters to numeric.

# Irisdf$Species <- as.factor(ifelse(Irisdf$Species == "setosa", 1,
#                             ifelse(Irisdf$Species == "versicolor", 2, 
#                             ifelse(Irisdf$Species == "virginica", 3, 0))))
# 
# Irisdf$Species
# Irisdf$Species<- as.numeric(Irisdf$Species) 
# summary(Irisdf)
# str(Irisdf) 
# hist(Irisdf$Species)

set.seed(123)
trainSize <- round(nrow(Irisdf) * 0.8)
testSize <- nrow(Irisdf) - trainSize
trainSize
testSize

training_indices<-sample(seq_len(nrow(Irisdf)),size =trainSize)
training_indices 

trainSet <- Irisdf[training_indices, ]
trainSet

testSet <- Irisdf[-training_indices, ]
str(trainSet)
str(testSet)

#set.seed(405)
#trainSet <- Irisdf[training_indices, ]
#testSet <- Irisdf[-training_indices, ]

##   LINEAR MODEL analysis.
LinearModel <- lm(Petal.Length~Petal.Width, data = trainSet)
summary(LinearModel)
Prediction<-data.frame(predict(LinearModel, testSet))
Prediction

# add additional column to testSet to include data from "predict"

testSet$newpetlen<-Prediction$predict.LinearModel..testSet.
testSet

# Verify answers from Predict by calculation using yintercept and slope
# then add column to datafield testSet

yint<-1.08246
slope<-2.22203
testSet$verifycalculate<- with(testSet, round(verifycalculate<-slope * Petal.Width + yint,3))
testSet
#save.image()
