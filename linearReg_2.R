library(dplyr)
library(tidyr)
library(MASS)
library(caTools)
library(ISLR)
library(car)
library(ggplot2)
library(plotly)
library(gridExtra)

setwd("C:/F/NMIMS/DataScience/Sem-2/MktngAnalytics/Project")

regData<-read.csv("./data/Regression-Clean-Data.csv", header = TRUE, stringsAsFactors = FALSE)
regData<-regData[, 3:11]

dim(regData)

str(regData)
glimpse(regData)

colnames(regData)

head(regData, 3)

detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(regData, detectNAs)


summary(regData)

cat("\014")
regData$City_Category<-as.factor(regData$City_Category)
regData$Parking<-as.factor(regData$Parking)

p1<-ggplot(regData, aes(x=Dist_Hospital, y=House_Price)) + geom_point() + geom_smooth( method="lm", se=FALSE)+
  labs(title="Linear Relationship between Dist_Hospital and House_Price") +
  labs(x="Dist_Hospital") +
  labs(y="House_Price")

p2<-ggplot(regData, aes(x=Builtup, y=House_Price)) + geom_point() + geom_smooth( method="lm", se=FALSE)+
  labs(title="Linear Relationship between Education and House_Price") +
  labs(x="Builtup") +
  labs(y="House_Price")
grid.arrange(p1,p2, nrow=1, ncol=2)


regData$Dist_Taxi<-scale(regData$Dist_Taxi)
regData$Dist_Market<-scale(regData$Dist_Market)
regData$Dist_Hospital<-scale(regData$Dist_Hospital)
regData$Carpet<-scale(regData$Carpet)
regData$Builtup<-scale(regData$Builtup)
regData$Rainfall<-scale(regData$Rainfall)
regData$House_Price<-scale(regData$House_Price)

set.seed(100)
split<-sample.split(regData$House_Price, SplitRatio=0.8)
trainSet<-subset(regData, split==TRUE)
testSet<-subset(regData, split==FALSE)  

cat("\014")
lm.fit1<-lm(House_Price~(Dist_Taxi+Dist_Market+Dist_Hospital+Carpet+Builtup+Rainfall), data=trainSet)
lm.fit1
summary(lm.fit1)

lm.fit2<-lm(House_Price~(Dist_Market+Dist_Hospital+Carpet+Builtup+Rainfall), data=trainSet)
lm.fit2
summary(lm.fit2)

lm.fit3<-lm(House_Price~(Dist_Market+Dist_Hospital+Carpet+Builtup), data=trainSet)
lm.fit3
summary(lm.fit3)

lm.fit4<-lm(House_Price~(Dist_Market+Dist_Hospital+Builtup), data=trainSet)
lm.fit4
summary(lm.fit4)

lm.fit5<-lm(House_Price~(Dist_Hospital+Builtup), data=trainSet)
lm.fit5
summary(lm.fit5)

predict1<-predict(lm.fit5, newdata = testSet)
predict1_df<-as.data.frame(predict1)



lm._df<-data.frame(ActualHousePrice=testSet$House_Price, 
                   PredictedHousePrice=predict1_df$predict1)

lm._df
