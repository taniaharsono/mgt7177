#set working directory
setwd("/Users/taniaharsono/Documents/QUB/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(readxl) #reads in excel file
library(caret) #machine learning package
library(car) #vif function to find multicollinearity

#read in the data
train <- read_excel("kc_train.xlsx")
test <- read_excel("kc_test.xlsx")

data<-rbind(train, test) #binding the data per row, can also use cbind() to set the data in the next column
#need to be careful on when to use rbind() and cbind()

#exploration, dq, visualisation

#preprocess the data
data$condition <- as.factor(data$condition)
data$waterfront <- as.factor(data$waterfront)

#empty the test and train data
train<-NULL
test<-NULL

#split the data
#we use data$price because we want the data to be randomly pick using stratisfied on the underlying variable
set.seed(123)
index<-createDataPartition(data$price, p=0.8, list=FALSE)
train<-data[index,]
test<-data[-index,]

#build the model
formula<-price~condition + sqft_living + bedrooms
model<-lm(data=train, formula=formula)
#summarise the model
summary(model)
plot(model)

resid<-model$residuals
plot(resid)
std_resid<-rstandard(model)
larger_resid<-std_resid > 2|std_resid < -2
sum(larger_resid)

#check multicollinearity
vif(model)

cooks <- cooks.distance(model)
sum(cooks>1)



