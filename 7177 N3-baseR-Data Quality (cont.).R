#set the working directory
setwd("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(readxl) #reads in excel file

#read in the kc dirty data
data<-read_excel("kc_dirty.xlsx")

#summarise the data
summary(data) #looking at each categories and see if there are some issues need to be addressed
#create a documentation at this stage on the issues and what to do to fix it

#finding total missing value in the data
sum(is.na(data))

#dealing with NUMERICAL VALUE quality issues
#look in depth about price since this will be our main variable in the assignment
#check quality of price variable
hist(data$price)

#replace the outlier with NA
#do not replace to much observations with NA
data$price[data$price>8000000]<-NA
summary(data$price) #can just be written down in the console since we do not need to keep it

#replace the data more than 3,000,000 to the mean value
data$price[data$price>3000000]<-mean(data$price,na.rm=TRUE)
summary(data$price)
hist(data$price)

#replace NA's with the mean
data$price[is.na(data$price)]<-mean(data$price, na.rm=T)
summary(data$price)
hist(data$price)

#dealing with CATEGORICAL VALUE quality issues
summary(data$waterfront) #this will show character data type

#convert character to factor
data$waterfront <- as.factor(data$waterfront)
summary(data$waterfront) #need to be re-level because there are "n", "no", "y", "yes"
levels(data$waterfront)

#2 ways to re-level
#1. change "n" to "no" manually by replacing it
data$waterfront[data$waterfront=="n"]<-"no" #there will still be "n" but just no observation
summary(data$waterfront)
data$waterfront<-droplevels(data$waterfront) #drop the empty levels
summary(data$waterfront)
#2. change "y" to "yes" by overwriting the "y" with "yes"
levels(data$waterfront)[2]<-"yes" #since "y" is currently the second level
summary(data$waterfront)

#change the order of factors
#using relevel
data$waterfront <- relevel(data$waterfront, "yes") #"yes" will be the first
summary(data$waterfront)