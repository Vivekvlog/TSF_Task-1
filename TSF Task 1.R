##GRIP @ The Sparks Foundation
#Task 1
#Predict the percentage of an student based on the no. of study hours. 
#This is a simple linear  regression task as it involves just 2 variables.


##Important librarys
library(tidyverse)
library(caret)

##Read data
library(readxl)
TSF_Task_1 <- read_excel("~/TSF Task 1.xlsx")
View(TSF_Task_1)
summary(TSF_Task_1)

##Data Visualization
plot(TSF_Task_1,main="Hours VS Scores",col="blue")

##Checking for outliers in data set
boxplot(TSF_Task_1)

##Data Partition
set.seed(123)
training.sample=TSF_Task_1$Scores%>%
createDataPartition(p=0.8,list = FALSE)  
view(training.sample)

#train data
train.data=TSF_Task_1[training.sample,]
view(train.data)

#test data
test.data=TSF_Task_1[-training.sample,]
view(test.data)

##Building Model
#Y=MX+C Linear Regression model
#Y=Scores 
#X=Hours
#M=Slop
#C=Intercept
Model=lm(Scores~Hours,data = train.data)
summary(Model)

##Ploting data with Linear Regression Line
plot(TSF_Task_1,main="Hours VS Scores",col="blue");abline(Model,h=0,col="red")

##Make Prediction
Prediction=Model %>% predict(test.data)
Prediction

##Comparing test data with predicted data
Matrix=as.matrix(cbind(Hours=test.data$Hours,Actual=test.data$Scores,Prediction))
Matrix

##Ploting Actual value & Predicted value with Linear Regression Line
plot(Matrix,col="blue",main="Actual VS Prediction");abline(Model,h=0,col="red")
legend("topleft",c("Actual","Predicted"),col = "..")

##Predicting for new value
#What will be predicted score if a student studies for 9.25 hrs/ day? 
New=data.frame(Hours=c(9.25))
New.Score=predict(Model,newdata = New)
cat('if a student studies for 9.25hrs/day than that student may Score',New.Score)

##Model Permormance
#Random Mean Square error
RMSE(Prediction,test.data$Scores)

#Mean square Error
MAE(Prediction,test.data$Scores)
