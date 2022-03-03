#set the working directory
setwd("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(readxl) #reads in excel file
library(psych) #for additional descriptive stats descriptive
library(ggplot2) #for visualization

#read in the kc dirty data
data<-read_excel("kc_dirty.xlsx")

#summarize the data
summary(data)

#use psych to get some additional summary measures
#e.g. to get standard deviation -> the measure of how spread out the data relative to the mean
describe(data)
#or to break down the descriptives by group
describeBy(data$price, data$bedrooms) #breakdown the descriptive of price by the number of bedrooms

#exploratory visualizations using plot and qplot
#aim: understand data and identiry quality issues
#there is no titles etc. since this is for our own use
#if visualisation are for communication they need titles, axes, etc.

#exploratory process:
#this can be used to get a better understanding of the data and to check whether there are quality issue with the data
hist(data$price) #plot -> there are some outlier
hist(data$bedrooms) #plot -> also some outlier
qplot(data$price) #qplot
qplot(as.factor(data$bedrooms)) #qplot -> by changing the data to factor, R create bar chart instead of histogram
#usually will look at the histogram for each variables and some scatter plot to see the relations between some variables

#address data quality issues
data$price[data$price>8000000]<-NA

#still there are expensive houses (over 200 above $2,000,000)
#we'll leave this for now

#final visualizations using ggplot2
#aim: answer a question/communicate results
#pick a question or hypothesis to adress
#for ex: what is the relationship between bedrooms and price?

ggplot(data=data)+
  geom_bar(mapping=aes(x=as.factor(bedrooms), y=price), stat="summary", fun.y="mean")+
  labs(title="average house price by bedrooms", x="number of bedrooms", y="mean price($)")
#stats if for statistics summary, fun.y is specifying the value y will not be count, but instead it will be the mean value


