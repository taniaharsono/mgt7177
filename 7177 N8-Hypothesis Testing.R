options(scipen = 0)
setwd("/Users/taniaharsono/Documents/QUB/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(readxl) #reads in excel file
library(psych) #for additional descriptive summary (including skewness)
library(ggplot2)
library(Hmisc)

#read in the kc dirty data
data<-read_excel("kc_house_data.xlsx")

data$price[data$price>4000000]<-NA
hist(data$price, probability=T) #instead of count, we now use probability to make the histogram
lines(density(data$price, na.rm=T), col=2)
describe(data$price)

qqnorm(data$price)
qqline(data$price) #if the data diviate from the line, it means that this is not normally distributed

#class example:
pnorm(60, mean=60, sd=10)
pnorm(70, mean=60, sd=10)
1-pnorm(50, mean=60, sd=10)
pnorm(70, mean=60, sd=10)-pnorm(50, mean=60, sd=10)

#chi-squared test
df <- read.csv("https://goo.gl/j6lRXD")
table(df$treatment, df$improvement)
chisq.test(df$treatment, df$improvement, 
           correct = FALSE)

#t-test 
hospital<-read_excel("hospital.xlsx")
t.test(Admissions~Service, data = hospital)

