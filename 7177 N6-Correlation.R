options(scipen = 999) #to not using scientific notation
#for previous example:
adverts <- c(5,4,4,6,8)
packets <- c(8,9,10,13,15)
sales <- data.frame(adverts, packets) #in R almost always we work with data frame
cor(sales$adverts, sales$packets) #by default we are calculating Pearson's Product Moment
#to calc Spearman's Rank Correlation
cor(sales$adverts, sales$packets, method="spearman")
#to calc Kendal Rank Correlation
cor(sales$adverts, sales$packets, method="kendall")

#a more detail function for correlation
cor.test() #will give p-value and confidence interval

#another function for correlation which is part of Hmisc package
rcorr() #will give correlation matrix


setwd("/Users/taniaharsono/Documents/QUB/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(readxl) #reads in excel file
library(psych) #for additional descriptive stats descriptive
library(ggplot2)
library(Hmisc)

#read in the kc dirty data
data<-read_excel("kc_house_data.xlsx")

#relationship between living area and sale price
ggplot(data=data, mapping=aes(x=sqft_living, y=price))+
  geom_point()+
  geom_smooth(method=lm)
cor(data$sqft_living, data$price)
cor.test(x=data$sqft_living, y=data$price)
#using rcorr()
mdata<-as.matrix(data[3:8])
rcorr(mdata)
allmdata<-as.matrix(data[2:15])
rcorr(allmdata)

cor(data$grade, data$price, method="spearman")
cor.test(x=data$grade, y=data$price, method="spearman")
