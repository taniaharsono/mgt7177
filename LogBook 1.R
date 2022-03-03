#set the working directory
setwd("/Users/taniaharsono/Documents/MGT7177 - Statistics for Business/Log Book 1 Data and Guidence")

#load the packages
library(ggplot2)
library(dplyr)
library(psych)
library(readxl)
library(tmvnsim)

#read the data
data<-read_excel("ames_train.xlsx")
ames_train<-select(data, SalePrice, Overall.Cond, Year.Remod.Add, Heating, Heating.QC, Central.Air, Lot.Area, Lot.Shape, Neighborhood)

#making the character data type a factor
#can be more efficient if we can automate this
ames_train$Overall.Cond<-as.factor(ames_train$Overall.Cond)
ames_train$Neighborhood<-as.factor(ames_train$Neighborhood)
ames_train$Heating<-as.factor(ames_train$Heating)
ames_train$Heating.QC<-as.factor(ames_train$Heating.QC)
ames_train$Central.Air<-as.factor(ames_train$Central.Air)
ames_train$Lot.Shape<-as.factor(ames_train$Lot.Shape)

#reordered factors levels variables
ames_train$Heating<-ordered(ames_train$Heating, levels=c("GasA", "GasW", "Wall", "Floor", "Grav", "OthW"))
ames_train$Heating.QC<-ordered(ames_train$Heating.QC, level=c("Ex", "Gd", "TA", "Fa", "Po"))
ames_train$Central.Air<-relevel(ames_train$Central.Air, "Y")
ames_train$Lot.Shape<-ordered(ames_train$Lot.Shape, level=c("Reg", "IR1", "IR2", "IR3"))

#DATA QUALITY ISSUE
#year remodel or built should not be after 2010, replace with NA
ames_train$Year.Remod.Add[ames_train$Year.Remod.Add>2010]<-NA #3 observation is over 2010
summary(ames_train$Year.Remod.Add)

#lot area should not be 0, replace with NA
ames_train$Lot.Area[ames_train$Lot.Area==0]<-NA
summary(ames_train$Lot.Area) #6 observation is 0
#looking for the outlier in Lot Area
ggplot(data = ames_train, mapping = aes(Lot.Area)) +
  geom_histogram() +
  labs(title = "Histogram of Lot Area", x = "Lot Size (in sq. ft.)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))
#number of observation over 40,000 is 17
#anything over 40,000, the outlier is equal to mean
ames_train$Lot.Area[ames_train$Lot.Area>40000]<-mean(ames_train$Lot.Area, na.rm = T) #what can justify this action?
ggplot(data = ames_train, mapping = aes(Lot.Area)) +
  geom_histogram() +
  labs(title = "Histogram of Lot Area", x = "Lot Size (in sq. ft.)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

#looking for the outlier in SalePrice
ggplot(data = ames_train, mapping = aes(SalePrice))+
  geom_boxplot()+
  labs(title = "Box Plot of Sale Price", x = "Price($)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))
#anything over 1,000,000, the outlier is NA
ames_train$SalePrice[ames_train$SalePrice>1000000]<-NA #what can justify this action?
summary(ames_train$SalePrice) #3 outlier observation
#histogram of SalePrice #skewed left
ggplot(data = ames_train)+
  geom_histogram(aes(SalePrice))+
  labs(title = "Box Plot of Sale Price", y = "Frequency", x = "Price($)")+
  theme(plot.title=element_text(hjust=0.5)) 

#DATA SUMMARY
#numerical data summary
summary(ames_train)
#visual data summary
#box plot for variables with numerical data type
ggplot(data = ames_train, mapping = aes(Lot.Area)) +
  geom_boxplot() +
  labs(title = "Box Plot of Lot Area", x = "Lot Size (in sq. ft.)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data = ames_train, mapping = aes(Year.Remod.Add))+
  geom_boxplot()+
  labs(title = "Box Plot of Year Remodel or Additions", x = "Year", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data = ames_train, mapping = aes(SalePrice))+
  geom_boxplot()+
  labs(title = "Box Plot of Sale Price", x = "Price($)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

#bar chart for variables with factor data type
ggplot(data = ames_train, mapping = aes(Lot.Shape))+
  geom_bar()+
  labs(title = "Bar Chart of Lot Shape", x = "Lot Shape", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data = ames_train, mapping = aes(Neighborhood))+
  geom_bar()+
  labs(title = "Bar Chart of Neighborhood Location", x = "Neighborhood Location", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data = ames_train, mapping = aes(Overall.Cond))+
  geom_bar()+
  labs(title = "Bar Chart of Overall Condition", x = "Overall Condition", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

#FINAL VISUALIZATION
#visualization 1-"Overall Condition based on the Remodel Year"
ggplot(data = ames_train, aes(x=Overall.Cond, y=Year.Remod.Add))+
  geom_violin(scale="area")+
  labs(title ="Fig.1 - Overall Condition based on the Remodel Year", x="Overall Condition", y="Remodel Year")+
  theme(plot.title=element_text(hjust=0.5))
#visualization 2-"House Price based on Type of Heating and Heating Condition"
ggplot(data = ames_train)+
  geom_count(aes(x=Heating.QC, y=Heating, size=SalePrice))+
  labs(title ="Fig.2 - House Price based on Type of Heating and Heating Condition", x="Heating Condition", y="Type of Heating")+
  theme(plot.title=element_text(hjust=0.5)) 
#visualization 3-"Sale Price based on the Availability of Central Air Conditioning"
ggplot(data = ames_train, mapping=aes(x=Central.Air, y=SalePrice))+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.25)+
  labs(title ="Fig.3 - Sale Price based on the Availability of Central Air Conditioning", x="Availability", y="Sale Price($)")+
  theme(plot.title=element_text(hjust=0.5)) 
#visualization 4-"Sale Price based on the Lot Shape"
ggplot(data = ames_train, aes(x=Lot.Shape, y=SalePrice))+
  geom_boxplot()+ 
  labs(title ="Fig.4 - Sale Price based on the Lot Shape", x="Lot Shape", y="Sale Price($)")+
  theme(plot.title=element_text(hjust=0.5)) 
#visualization 5-"House Price based on the Lot Area per Neighborhood"
ggplot(data = ames_train, aes(x=SalePrice, y=Lot.Area))+
  geom_point()+
  facet_wrap(~Neighborhood)+
  labs(title ="Fig.5 - House Price based on the Lot Area per Neighborhood", x="Sale Price($)", y="Lot Area (sqft)")+
  theme(plot.title=element_text(hjust=0.5))

