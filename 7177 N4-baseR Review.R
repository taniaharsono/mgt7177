#set the working directory
setwd("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(ggplot2)
library(dplyr)
library(psych)
library(readxl)
library(tmvnsim)

#read the data
data<-read_excel("kc_house_data.xlsx")
ames_train<-read_excel("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Log Book 1 Data and Guidence/ames_train.xlsx")

#to convert all characters to a factor (using dplyr)
ames_train<-ames_train %>% mutate_if(is.character, as.factor)

#warning = probably okay
#error = didn't work - need to fix

#review of data quality issue with BaseR
#convert some characters to a factor
data$condition<-as.factor(data$condition)
#check the current factor levels
levels(data$condition)
#rename the factor levels
levels(data$condition)<-c("very poor", "poor", "average", "good", "very good")
#reorder the factor levels
#relevel changes which factor comes first
data$condition<-relevel(data$condition, "poor")
#or the entire order can be changed
data$condition<-factor(data$condition, levels=c("very good", "good", "average", "poor", "very poor"))
#rename the factor levels
levels(data$condition)<-list("v. poor"="very poor", "pr"="poor", "av"="average", "gd"="good", "v. good"="very good")
#if we forgot to put one level, it will be written as NA

summary(data$condition)

#n.b. it is also possible to relebel factors using dplyr
data<-read_excel("kc_house_data.xlsx")
#set dondition as a factor
data$condition<-as.factor(data$condition)
data$condition<-recode_factor(data$condition, "1"="terrible")
#the good thing to use dplyr is because we can only change one level without change the rest into NA
summary(data$condition)





