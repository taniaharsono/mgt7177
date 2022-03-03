#set the working directory
setwd("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(readxl)

#read in the data
data<-read_excel("kc_house_data.xlsx")

#histogram of living area (single variable)
hist(data$sqft_living, main="Histogram of Living Area", xlab="Living Area (sq ft.)", ylab="Frequency", breaks=50)
#histogram is similar with bar chart but only using one variable

#box plot of living area (single variable)
boxplot(data$sqft_living)

#box plot of living area and price under different conditions
boxplot(sqft_living ~ condition, data = data, main="Box Plot of Living Area", xlab="Conditions", ylab="Living Area (sq ft.)")
boxplot(price ~ condition, data = data, main="Box Plot of Price", xlab="Conditions", ylab="Price")

#scatter plot of living area
plot(data$sqft_living, data$price, main="Scatter Plot of Living Area", xlab="Living Area (sq ft.)", ylab="Price")
#create a regression line on the scatter plot
abline(lm(price~sqft_living, data=data)) #we will discuss about this later in the course

#create a bar chart of the conditions
barplot(table(data$condition)) #when using ggplot, we don't need to tabulate the data