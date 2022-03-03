#set the working directory
setwd("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Class Data Set")
getwd() #check where we currently are

#install packages if don't have the packages yet
#load the packages
library(readxl)

#read in the data
data<-read_excel("kc_house_data.xlsx") #can also be done by "Import Dataset"

#view the data
View(data) #to open the data, but can also be done from the "Environment" tab

#summarize the data
summary(data) #this function give the descriptive statistics for each categories
#be careful that this function also calculate the ordinal data as numeric data
#to solve this, we should convert the "numeric" to factor

#convert numeric to factors
data$condition <- as.factor(data$condition)
summary(data$condition) #the summary will give the total observations of each category
#the result show that the condition of the house is mostly average
#later might need to discuss why there are only a few of the lowest level condition

#for year build and year renovated, it depends on how we want to use it
#make sure the data in the correct data type
#since there are lots of observation, the year might look like continuous data
#we can leave it as a numeric variable and make a line chart to see how the price change over time
#unless when we want to make other type of chart and we might need to change it into a factor
#we can also group it every 5 years (for ex) and with this we can change it into a factor - not recommended

summary(data$price) #need to interpret it
mean(data$price) #if only want to know the mean
median(data$price) #if only want to know the median
#if the mean is really different from the median, our data is skewed
min(data$price)
max(data$price) #the max is too high
sd(data$price)

plot(data$price)

#identifying data quality issue
dirty.data<-read_excel("kc_dirty.xlsx")
summary(dirty.data$price)

#different ways to find the outlier

#1. we can cut of the data from the third sd or using interquartile range
#however, be careful, this can eliminate too many values
outlier<-boxplot(data$price)$out #it is shown that there are more than 1000 outliers, so we should not remove it
plot(outlier)

#2. remove certain value
#code anything above $8,000,000 as NA
dirty.data$price[dirty.data$price > 8000000]<-NA
summary(dirty.data$price)
#we can also delete the value, but not recommended since lots of R function can take care of NA value
mean(dirty.data$price) #will show NA since there are missing value
mean(dirty.data$price, na.rm = TRUE) #will show the mean

