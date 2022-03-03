#the code below accompanies the pre class video
#this focuses on linear regression
#we will be working on regression for the rest of the module
#this will provide a useful introduction, and the key regression tasks for log book 2
#please note that you also need to think about data quality, visualisaitona, and carry out the association analysis

setwd("/Users/taniaharsono/Documents/QUB/MGT7177 - Statistics for Business/Class Data Set")

#Load the packages
library(readxl) #reads in excel file
library(caret) #machine learning package

#read in the data
data<- read_excel("kc_house_data.xlsx")

#address any dq issues etc #log book 1

#split the data into a training and test set
#80% of the observations for training the model
#20% of the observations for testing the accuracy
#this is done for you for log book 2 -both datasets are on canvas
#n.b. often for inference only analyses you will not perform this step

#setting the seed allows the results to be reproducable
set.seed(123)

index <- createDataPartition(data$price, list=FALSE, p=0.8, times=1) #create 80% partition based on the price

train <- data[index,]
test <- data[-index,]


#simple linear regression model
#is lot area related to sale price?
#or, how well does lot area predict sale price?

simpleModel <- lm(price ~ sqft_living, data = train) 
#price (dependent var), sqft_living (independent var)

#review the model
summary(simpleModel)

#prediction using the model

simple_price_predictions <- predict(simpleModel, newdata = test)

#evaluate the accuracy of the predictions
#(i.e. difference between the actual sale value and the predicted sale value)

postResample(pred = simple_price_predictions, obs = test$price)

#focus on RMSE and R squared for now 
#RMSE is the difference between observed and predicted values calculated as:

sqrt(mean((simple_price_predictions - test$price)^2))

#R-squared is the amount of variance in the data that is accounted for by the model


#multiple linear regression
#are living area, condition, and bathrooms related to price
#or how well does lot area, condition and bathrooms predict price
#n.b. usually you shouldn't include variables that are very highly correlated
#we will return to the assumptions underpinning regression in class


multiple_model <- lm(price ~ sqft_living + as.factor(condition) + bathrooms, data = train)
#the value shows that there is a positive relationship between sqft_living with the price
#however, since the value is statistically significant, we could not say that there is a relation between the other two variables and price

summary(multiple_model)

#predict prices for houses in the test set

multiple_price_predictions <- predict(multiple_model, newdata = test)

#evaluate accuracy
postResample(multiple_price_predictions, test$price)

#for practice, you could try including additional variables in the model
#see if you can increase the accuracy and we can discuss in class next week

