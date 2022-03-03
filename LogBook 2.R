#set working directory
setwd("/Users/taniaharsono/Documents/QUB/MGT7177 - Statistics for Business/Log Book 2 Data and Guidence")

#load the packages
library(readxl) #reads in excel file
library(dplyr)
library(caret) #machine learning package
library(psych)
library(tmvnsim)
library(plyr)

#read the data
full_data <- read_excel("ames_fullSV.xlsx")
#selecting data for the model
model_data<-select(full_data, SalePrice, Year.Remod.Add, Lot.Area, Misc.Val, Lot.Shape, Utilities, Overall.Cond, Exter.Cond, Bsmt.Cond, Heating.QC, Electrical, Kitchen.Qual, Fireplace.Qu, Garage.Cond, Pool.QC, Fence)

#DATA QUALITY ISSUE (some already mentioned in Log Book1)
#update all chr to factor
model_data<-mutate_if(model_data, is.character, as.factor)
#update Overall.Cond and Pool.QC to factor
model_data$Overall.Cond<-as.factor(model_data$Overall.Cond)
model_data$Pool.QC<-as.factor(model_data$Pool.QC)

#revalue ordinal data
model_data$Lot.Shape<-ordered(model_data$Lot.Shape, levels=c("Reg", "IR1", "IR2", "IR3"))
model_data$Exter.Cond<-ordered(model_data$Exter.Cond, c("Ex", "Gd", "TA", "Fa", "Po"))
model_data$Bsmt.Cond<-ordered(model_data$Bsmt.Cond, c("Ex", "Gd", "TA", "Fa", "Po", "na"))
model_data$Heating.QC<-ordered(model_data$Heating.QC, c("Ex", "Gd", "TA", "Fa", "Po", "na"))
model_data$Electrical<-ordered(model_data$Electrical, c("SBrkr", "FuseA", "FuseB", "FuseP", "Mix", "na"))
model_data$Kitchen.Qual<-ordered(model_data$Kitchen.Qual, c("Ex", "Gd", "TA", "Fa", "Po"))
model_data$Fireplace.Qu<-ordered(model_data$Fireplace.Qu, c("Ex", "Gd", "TA", "Fa", "Po", "na"))
model_data$Garage.Cond<-ordered(model_data$Garage.Cond, c("Ex", "Gd", "TA", "Fa", "Po", "na"))
model_data$Fence<-ordered(model_data$Fence, c("GdPrv", "MnPrv", "GdWo","MnWo", "na"))

#replace NA with "NA" for: Bsmt.Cond, Heating.QC, Electrical, Fireplace.Qu, Garage.Cond, Fence
model_data$Bsmt.Cond[is.na(model_data$Bsmt.Cond)]="na"
model_data$Heating.QC[is.na(model_data$Heating.QC)]="na"
model_data$Electrical[is.na(model_data$Electrical)]="na"
model_data$Fireplace.Qu[is.na(model_data$Fireplace.Qu)]="na"
model_data$Garage.Cond[is.na(model_data$Garage.Cond)]="na"
model_data$Fence[is.na(model_data$Fence)]="na"

#revalue ordinal data
revalue(model_data$Lot.Shape, c("Reg"="4", "IR1"="3", "IR2"="2", "IR3"="1"))
revalue(model_data$Exter.Cond, c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1"))
revalue(model_data$Bsmt.Cond, c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1", "na"="0"))
revalue(model_data$Heating.QC, c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1", "na"="0"))
revalue(model_data$Electrical, c("SBrkr"="5", "FuseA"="4", "FuseB"="3", "FuseP"="2", "Mix"="1", "na"="0"))
revalue(model_data$Kitchen.Qual, c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1"))
revalue(model_data$Fireplace.Qu, c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1", "na"="0"))
revalue(model_data$Garage.Cond, c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1", "na"="0"))
revalue(model_data$Fence, c("GdPrv"="4", "MnPrv"="3", "GdWo"="2","MnWo"="1", "na"="0"))

#looking for the outlier in SalePrice
ggplot(data = model_data, mapping = aes(SalePrice))+
  geom_boxplot()+
  labs(title = "Box Plot of Sale Price", x = "Price($)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))
#anything over 1,000,000, the outlier replaced with mean
model_data$SalePrice[model_data$SalePrice>1000000]<-mean(model_data$SalePrice, na.rm = T)
summary(model_data$SalePrice) #3 outlier observation
#histogram of SalePrice #skewed left
ggplot(data = model_data)+
  geom_histogram(aes(SalePrice))+
  labs(title = "Box Plot of Sale Price", y = "Frequency", x = "Price($)")+
  theme(plot.title=element_text(hjust=0.5)) 

#looking for the outlier in Misc.Val
ggplot(data = model_data, mapping = aes(Misc.Val))+
  geom_boxplot()+
  labs(title = "Box Plot of Miscellaneous Value", x = "Value($)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))
#anything over 5,000, the outlier is NA
model_data$Misc.Val[model_data$Misc.Val>5000]<-NA
summary(model_data$Misc.Val) #5 outlier observation

#year remodel or built should not be after 2010, replace with NA
model_data$Year.Remod.Add[model_data$Year.Remod.Add>2010]<-NA #3 observation is over 2010
summary(model_data$Year.Remod.Add)
hist(model_data$Year.Remod.Add)

#lot area should not be 0, replace with NA
model_data$Lot.Area[model_data$Lot.Area==0]<-NA
summary(model_data$Lot.Area) #6 observation is 0
#looking for the outlier in Lot Area
ggplot(data = model_data, mapping = aes(Lot.Area)) +
  geom_histogram() +
  labs(title = "Histogram of Lot Area", x = "Lot Size (in sq. ft.)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))
#number of observation over 40,000 is 17
#anything over 40,000, the outlier is equal to mean
model_data$Lot.Area[model_data$Lot.Area>40000]<-mean(model_data$Lot.Area, na.rm = T)
ggplot(data = model_data, mapping = aes(Lot.Area)) +
  geom_histogram() +
  labs(title = "Histogram of Lot Area", x = "Lot Size (in sq. ft.)", y = "Frequency")+
  theme(plot.title=element_text(hjust=0.5))

summary(model_data)

#version 2 model dataset
model_data2<-select(model_data, -Utilities, -Overall.Cond, -Pool.QC)
model_data2<-mutate_if(model_data2, is.factor, as.numeric)

#matrix model dataset
data_ord<-select(model_data2, 1, 5:13)
mdata_ord<-as.matrix(data_ord)
data_num<-select(model_data2, 1:4)
mdata_num<-as.matrix(data_num)
mmodel_data2<-as.matrix(model_data2)

#correlation between each variables
rcorr(mdata_ord, type=c("spearman"))
rcorr(mdata_num)
rcorr(mmodel_data2, type=c("spearman"))

#final model dataset
fmodel_data<-select(model_data2, -Misc.Val, -Exter.Cond)

#setting seeds and separate the data into train and test model
set.seed(123)
#create 80% partition based on the price
index<-createDataPartition(fmodel_data$SalePrice, list=FALSE, p=0.8, times=1)
train<-fmodel_data[index,]
test<-fmodel_data[-index,]

#first regression model
multi_reg_model<-lm(SalePrice ~ Lot.Area, data = train)
summary(multi_reg_model)
#prediction
prediction<-predict(multi_reg_model, newdata = test)
#the difference between observed and predicted values
postResample(prediction, test$SalePrice)

#second regression model
multi_reg_model<-lm(SalePrice ~ Year.Remod.Add+Lot.Area, data = train)
summary(multi_reg_model)
#prediction
prediction<-predict(multi_reg_model, newdata = test)
#the difference between observed and predicted values
postResample(prediction, test$SalePrice)

#third regression model
multi_reg_model<-lm(SalePrice ~ Year.Remod.Add+Lot.Area+Lot.Shape+Bsmt.Cond+Heating.QC+Electrical, data = train)
summary(multi_reg_model)
#prediction
prediction<-predict(multi_reg_model, newdata = test)
#the difference between observed and predicted values
postResample(prediction, test$SalePrice)

#forth regression model
multi_reg_model<-lm(SalePrice ~ Year.Remod.Add+Lot.Area+Lot.Shape+Bsmt.Cond+Heating.QC+Electrical+Kitchen.Qual, data = train)
summary(multi_reg_model)
#prediction
prediction<-predict(multi_reg_model, newdata = test)
#the difference between observed and predicted values
postResample(prediction, test$SalePrice)

#fifth regression model
multi_reg_model<-lm(SalePrice ~ Year.Remod.Add+Lot.Area+Lot.Shape+Bsmt.Cond+Heating.QC+Electrical+Kitchen.Qual+Fireplace.Qu, data = train)
summary(multi_reg_model)
#prediction
prediction<-predict(multi_reg_model, newdata = test)
#the difference between observed and predicted values
postResample(prediction, test$SalePrice)

#final regression model
multi_reg_model<-lm(SalePrice ~ Year.Remod.Add+Lot.Area+Lot.Shape+Bsmt.Cond+Heating.QC+Electrical+Kitchen.Qual+Fireplace.Qu+Garage.Cond+Fence, data = train)
summary(multi_reg_model)
#prediction
prediction<-predict(multi_reg_model, newdata = test)
#the difference between observed and predicted values
postResample(prediction, test$SalePrice)

vif(multi_reg_model)
sum(cooks.distance(multi_reg_model)>1)
plot(multi_reg_model)


