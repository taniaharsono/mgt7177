#set working directory
setwd("/Users/taniaharsono/Documents/QUB/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(readxl) #reads in excel file
library(caret) 
library(dplyr)
library(car)

#read in the data
data <- read_excel("telco_churn.xlsx")

summary(data)

#DQ issues
data <- data %>% mutate_if(is.character, as.factor)

#split the data
set.seed(123)
index <- createDataPartition(data$Churn, p=0.8, list = F)
train <- data[index,]
test <- data[-index,]

#build a model
formula <- Churn ~ tenure + gender
  
model1 <- glm(formula = formula, data = train, family = "binomial")  
#to make a linear model using glm(), family="gaussian"

summary(model1)

#PseudoRSquared
#Source: Field, 2012
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
logisticPseudoR2s(model1)

#odds ratio
exp(model1$coefficients)
#to interpret, for every one unit increase in tenure, 
#there is a decrease in the odds of a person churning out of 0.96

train$PredictedProbabilities <- fitted(model1)
#we want to see if there are less than 5% of our data in the tail of the distribution
train$StandardiseResidual <- rstandard(model1)
sum(train$StandardiseResidual > 1.96) 
#check any influential cases
train$Cooks <- cooks.distance(model1)
sum(train$Cooks >1)
#check multicollinearity
vif(model1)
#check whether or not there is a linear relationship (linearity of the logit)
#the linearity only for the continuous predictor
train$tenLogInt <- log(train$tenure)*train$tenure
#rerun the model with this as a new variable
#if it is significant, then we have violate the 


#make prediction
predictions <- predict(model1, test, type="response")
class_pred<-as.factor(ifelse(predictions>0.5, "Yes", "No"))
summary(class_pred)
postResample(class_pred, test$Churn)
#looking for the kappa value at least 0.3
confusionMatrix(data=class_pred, test$Churn)
