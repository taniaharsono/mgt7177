#creating a dataset called "x" that contain certain value
x<-c(2,6,7,8,9,10,10,12,15,17) 
mean(x) #calculate the mean of a dataset
median(x) #calculate the mean of a dataset
sd(x) #calculate the standard deviation of a dataset
min(x) #find the smallest value in the dataset
max(x) #find the biggest value in the dataset

#since there is no builtin function to find the mode,
#we need to write the function manually

#creating a dataset called "y" that contain certain value
y<-c(NA,3,7,9,NA,10,15,17) #NA means that the data is empty
mean(y) #will not be able to be executed due to the NA values within the dataset
mean(y, na.rm=TRUE) #na.rm used to remove NA value from dataset

#shorcut to calculate the min, max, mean, and quartiles
summary(x) #can only be used for the same data type

#creating a dataset that contain "character" data type
#character and string values need to be written with ""
my_variable<-c("yes", "no", "yes", "no", "yes") 
#if we want to use this as categorical variable, we need to convert it to a factor
my_variable<-as.factor(my_variable) #setting it as the factor
summary(my_variable) #in a case like this, summary function will only show the top 5 results
