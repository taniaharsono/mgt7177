#set working directory
setwd("/Users/taniaharsono/Documents/QUB/MGT7177 - Statistics for Business/Assignment 2 Data and Guidence")
#install packages
install.packages(sjPlot)
#load the packages
library(readxl) #reads in excel file
library(caret) 
library(dplyr)
library(car)
library(psych)
library(tidyverse)
library(tmvnsim)
library(plyr)
library(Hmisc)
library(tibble)

#read in the data
train <- read_excel("bank_train.xlsx")
test <- read_excel("bank_test.xlsx")
#combine the data
data <- rbind(train, test)


#DQ issues 
#change character data type to factor
data <- data %>% 
  mutate_if(is.character, as.factor)

summary(data)
describeBy(data)

#ID: Check if all IDs value are distinct
n_distinct(data$ID)

#age: min and max unreasonable
boxplot(data$age)
data %>% 
  top_n(age, n=3) %>%  
  select(age)
data$age[data$age>100]<-mean(data$age, na.rm = T) #3 observations
data$age[data$age<5]<-mean(data$age, na.rm = T) #2 obsercations
hist(data$age)
summary(data$age)

#marital: reorder level
data$marital <- ordered(data$marital, levels=c("unknown", "single", "married", "divorced"))
summary(data$marital)

#education: reorder level
data$education <- ordered(data$education, levels=c("unknown", "illiterate", "basic.4y", 
                                                   "basic.6y", "basic.9y", "high.school", 
                                                   "professional.course", "university.degree"))
summary(data$education)

#default: reorder level
#note-only 3 yes
data$default <- ordered(data$default, levels=c("unknown", "no", "yes"))
summary(data$default)

#housing: reorder level
data$housing <- ordered(data$housing, levels=c("unknown", "no", "yes"))
summary(data$housing)

#loan: 23 NAs is changed to unknown
data$loan[is.na(data$loan)] = "unknown"
summary(data$loan)
#loan: reorder level
data$loan <- ordered(data$loan, levels=c("unknown", "no", "yes"))
summary(data$loan)

#contact: inconsistency in inputting - "mobile" observation (1) will be revised as cellular
data$contact[data$contact=="mobile"] <- "cellular"
data$contact <- droplevels(data$contact)
summary(data$contact)

#month: reorder level
#note-no january and feb, high in May and low in Dec
data$month <- ordered(data$month, levels=c("mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
summary(data$month)

#day_of_week: inconsistency in writing
data$day_of_week[data$day_of_week=="Friday"] <- "fri"
data$day_of_week <- droplevels(data$day_of_week)
summary(data$day_of_week)
#day_of_week: reorder relevel
data$day_of_week <- ordered(data$day_of_week, levels=c("mon", "tue", "wed", "thu", "fri"))
summary(data$day_of_week)

#duration: check whether the max value is too high
boxplot(data$duration)
data %>% 
  top_n(duration, n=3) %>%  
  select(duration)
summary(data$duration)

#campaign: check whether the max value is too high?
boxplot(data$campaign)
data %>% 
  top_n(campaign, n=2) %>%  
  select(campaign)

#poutcome: reorder level
data$poutcome <- ordered(data$poutcome, levels=c("nonexistent", "failure", "success"))
summary(data$poutcome)

#consumer price index: change the observation on 149 with the mean
boxplot(data$cons.price.idx)
data$cons.price.idx[data$cons.price.idx>100]<-mean(data$cons.price.idx, na.rm = T) 
hist(data$cons.price.idx)

#consumer confidence index: change the negative to positive
data$cons.conf.idx <- abs(data$cons.conf.idx)
summary(data$cons.conf.idx)

#y: reorder level
data$y <- ordered(data$y, levels=c("no","yes"))
summary(data$y)

#Summary Statistics
summary(data)
describeBy(data)


#Data Transformation
#for visualization
y_y <- data %>% filter(data$y == "yes")
y_n <- data %>% filter(data$y == "no")
previousn <- data %>% filter(data$previous != 0)
#for correlation
datacor <- data %>% select(age, education, duration, campaign, previous, 
                           emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, 
                           nr.employed, y)
revalue(datacor$education, c("unknown"="0", "illiterate"="1", "basic.4y"="2", 
                             "basic.6y"="3", "basic.9y"="4", "high.school"="5", 
                             "professional.course"="6", "university.degree"="7"))
datacor <- mutate_if(datacor, is.factor, as.numeric)
#for modeling
data$y_model <- data$y
data$y_model <- revalue(data$y_model, c("no"="0", "yes"="1"))

#Visualizations
#Vis1: 
ggplot(data = data, mapping = aes(x = age, y = duration, color = y))+
  geom_point(position = "jitter", alpha = 0.2)+
  geom_smooth(method = 'lm') +
  scale_color_manual(values=c(yes="#FF8800", no="#4179AA"))+
  labs(title = "Scatter Plot Between Age and Duration", y = "Duration", x = "Age")+
  theme(plot.title=element_text(hjust=0.5)) 

#Vis2:
ggplot(data = data, mapping = aes(x = housing, fill = y))+
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.8)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c(yes="#FF8800", no="#4179AA"))+
  facet_grid(~marital)+
  labs(title = "Bar Plot of Housing Loan Based on Marital Status", y = "Percentage", x = "Housing Loan")+
  theme(plot.title=element_text(hjust=0.5))

#Vis3:
ggplot(data = data, mapping = aes(age))+
  geom_histogram(data = y_n, aes(y = (..count..)/sum(..count..), fill = "#4179AA"), alpha = 0.5)+
  geom_histogram(data = y_y, aes(y = (..count..)/sum(..count..), fill = "#FF8800"), alpha = 0.5)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(name = "y", values=c("#4179AA", "#FF8800"), labels = c("no", "yes"))+
  labs(title = "Histogram of Customer Response based on Their Age", y = "Percentage", x = "Customer Age")+
  theme(plot.title=element_text(hjust=0.5), legend.position = "right")

#Vis4:
ggplot(data = previousn, mapping = aes(x = y, y = previous))+
  geom_count()+
  scale_size(range = c(1, 20))+
  expand_limits(y = c(0.5, 7))+
  scale_color_manual(values=c(yes="#FF8800", no="#4179AA"))+
  labs(title = "Number of Subscription", 
       subtitle ="Based on the Number of Contact for Previous Campaign", 
       y = "Previously Contacted", x = "Subscription")+
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), legend.position = "right")


#Correlation
mdatacor <- as.matrix(datacor)
rcorr(mdatacor, type=c("spearman"))


#Predictive Model
#Train and Test Model Setup
test <- NULL
train <- NULL
set.seed(123)
index <- createDataPartition(data$y, p=0.8, list = F)
train <- data[index,]
test <- data[-index,]

#Build the Model
data$y_model <- as.factor(data$y_model)
formula1 <- y_model ~ age + job + marital + education + default + housing + loan + contact +
  month + day_of_week + duration + campaign + previous + poutcome + emp.var.rate + 
  cons.price.idx + cons.conf.idx + euribor3m + nr.employed

model1 <- glm(formula = formula1, data = train, family = "binomial")

summary(model1)


#Assumption:
#Check the Odds Ratio
exp(model1$coefficients)

#Check the Residual (should be less than .5)
train$StandardiseResidual <- rstandard(model1)
sum(train$StandardiseResidual > 1.96)/nrow(train)

#Check any Influential Cases
train$Cooks <- cooks.distance(model1)
sum(train$Cooks >1)

#Check the Multicollinearity
vif(model1)
#consider to make a new model without housing, loan, month, emp.var.rate, 
##cons.price.idx, eribor3m, and nr.employed

#Check the Linear Relationship (linearity of the logit)
##Only for the continuous predictor
train$ageLogInt <- log(train$age)*train$age
train$duraLogInt <- log(train$duration)*train$duration
train$campLogInt <- log(train$campaign)*train$campaign
train$previousLogInt <- log(train$previous)*train$previous
train$conspriceLogInt <- log(train$cons.price.idx)*train$cons.price.idx
train$euriborLogInt <- log(train$euribor3m)*train$euribor3m
train$nrempLogInt <- log(train$nr.employed)*train$nr.employed

formula_assump <- y_model ~ age + job + marital + education + default + housing + loan + contact +
  month + day_of_week + duration + campaign + previous + poutcome + emp.var.rate + 
  cons.price.idx + cons.conf.idx + euribor3m + nr.employed + ageLogInt + duraLogInt +
  campLogInt + previousLogInt + conspriceLogInt + euriborLogInt + nrempLogInt

model_assump <- glm(formula = formula_assump, data = train, family = "binomial")

summary(model_assump)
##duration and eribor3m violate the Linearity of the Logit assumption

#Renew model based on the assumptions
formula2 <- y_model ~ age + job + marital + education + default + contact + day_of_week + 
  duration + campaign + previous + poutcome + cons.conf.idx

model2 <- glm(formula = formula2, data = train, family = "binomial")

summary(model2)

#Create model based on the significant variable from model2
formula3 <- y_model ~ age + job + education + contact + duration + campaign + previous + 
  poutcome + cons.conf.idx

model3 <- glm(formula = formula3, data = train, family = "binomial")

summary(model3)

#Model Prediction:
#Prediction model1
predictions1 <- predict(model1, test, type="response")
sub_pred1 <- as.factor(ifelse(predictions1>0.5, "1", "0"))
sub_pred1 <- ordered(sub_pred1, levels=c("0", "1"))
summary(sub_pred1)
#Kappa Value: (at least 0.3)
postResample(sub_pred1, test$y_model)
confusionMatrix(data=sub_pred1, test$y_model)

#Prediction model2
predictions2 <- predict(model2, test, type="response")
sub_pred2 <- as.factor(ifelse(predictions2>0.5, "1", "0"))
sub_pred2 <- ordered(sub_pred2, levels=c("0", "1"))
summary(sub_pred2)
#Kappa Value: (at least 0.3)
postResample(sub_pred2, test$y_model)
confusionMatrix(data=sub_pred2, test$y_model)

#Prediction model3
predictions3 <- predict(model3, test, type="response")
sub_pred3 <- as.factor(ifelse(predictions3>0.5, "1", "0"))
sub_pred3 <- ordered(sub_pred3, levels=c("0", "1"))
summary(sub_pred3)
#Kappa Value: (at least 0.3)
postResample(sub_pred3, test$y_model)
confusionMatrix(data=sub_pred3, test$y_model)

postResample(sub_pred1, test$y_model)
postResample(sub_pred2, test$y_model)
postResample(sub_pred3, test$y_model)

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

logisticPseudoR2s(model1) #model1
logisticPseudoR2s(model2) #model2
logisticPseudoR2s(model3) #model3

#Overfitting Test
#Data Transformation: Create a Predicted Probabilites column into the Train Data
#model1
train$PredictedProbabilities1 <- fitted(model1)
pred1 <- as.factor(ifelse(train$PredictedProbabilities1>0.5, "1", "0"))
pred1 <- ordered(pred1, levels=c("0", "1"))
summary(pred1)
#Kappa Value: (at least 0.3)
postResample(pred1, train$y_model)

#model2
train$PredictedProbabilities2 <- fitted(model2)
pred2 <- as.factor(ifelse(train$PredictedProbabilities2>0.5, "1", "0"))
pred2 <- ordered(pred2, levels=c("0", "1"))
summary(pred2)
#Kappa Value: (at least 0.3)
postResample(pred2, train$y_model)

#model3
train$PredictedProbabilities3 <- fitted(model3)
pred3 <- as.factor(ifelse(train$PredictedProbabilities3>0.5, "1", "0"))
pred3 <- ordered(pred3, levels=c("0", "1"))
summary(pred3)
#Kappa Value: (at least 0.3)
postResample(pred3, train$y_model)

postResample(pred1, train$y_model)
postResample(pred2, train$y_model)
postResample(pred3, train$y_model)