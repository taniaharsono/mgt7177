#set the working directory
setwd("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Class Data Set")

#load the packages
library(ggplot2) #can also only library(tidyverse)
library(dplyr) #can also only library(tidyverse)
library(tmvnsim)
library(psych)
library(readxl)

#read the data
data<-read_excel("kc_house_data.xlsx")

#summary of the data
summary(data)
describe(data)

#address DQ issue
data$condition <- as.factor(data$condition)

#scatter plot using qgplot() #similar to base plotting
qplot(data = data, y = price, x = sqft_living, geom = c("point", "smooth"), xlab = "Living Area (Sq Ft)", ylab = "Sale Price ($)", main = "Sale Price by Living Area")
#if we want to predict price, usually the price is the y variable

#boxplot #similar to base plotting
qplot(condition, sqft_above, data = data, geom = c("boxplot"))

#ggplot() #main plotting function in ggplot2 #alpha is for transparency, 0-transparent
ggplot(data = data, mapping = aes(x = sqft_living, y = price)) +  #layer1: coordinate
  geom_point(mapping = aes(colour = condition, shape = condition, size = floors)) + #layer2: points
  geom_smooth(method = "lm", colour = "red") + #layer3: trend line
  labs(title = "Scatter Plot of Price by Lot Area", x = "Lot Area (sq ft)", y = "Price($)") + #layer4: labels
  facet_wrap(~condition) #layer5: Facet
#if we map x and y axis in each geom, we can actually make each geom use different variables

#ggplot() #bar chart
data$condition <- as.factor(data$condition) 
#if some change levels/rename levels needed, it can be here
ggplot(data = data, mapping = aes(condition)) +
  geom_bar(fill = "green", colour = "red") +
  labs(title = "Bar Chart of Price by House Condition", x = "House Condition", y = "Price($)")

#ggplot() #box plot
ggplot(data = data, mapping = aes(x=condition, y=price))+
  geom_boxplot()+
  labs(title = "Box Plot of Price by House Condition", x = "House Condition", y = "Price($)")

#ggplot() #histogram
ggplot(data = data)+
  geom_histogram(aes(price))+
  labs(title = "Histogram of Price by House Condition", x = "Price($)", y = "Number of Houses")

#how to create a chart in the order of the count?
#using reorder/sort/sort it with dplyr before create the chart
