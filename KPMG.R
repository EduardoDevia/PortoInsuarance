
memory.size(TRUE)

options(java.parameters = "-Xmx12g")
#Load Libraries
library(ParamHelpers)
library(mlr)
library(plyr)
library(dplyr)
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
#Load the file
training<-read.table("C:/Users/chedevia/Downloads/train.csv", header=TRUE, sep=",")
#Check the file
summarizeColumns(training)
#Change the columns ended with _Cat to Categorical
#Add a list with the columns to change
columns_to_change<-colnames(training[,grepl("_cat",names(training))])
#Run a for with the columns to change to categorical
for(i in 1:length(columns_to_change)){
  column_number<-which(colnames(training)==columns_to_change[i])  
  training[,column_number]<-as.factor(training[,column_number])
  }
#Now we do the same for the binary columns "_bin"

#Change the columns ended with _Cat to Categorical
#Add a list with the columns to change
columns_to_change<-colnames(training[,grepl("_bin",names(training))])
#Run a for with the columns to change to categorical
for(i in 1:length(columns_to_change)){column_number<-which(colnames(training)==columns_to_change[i])  
  training[,column_number]<-as.factor(training[,column_number])
}
#change the target value to a factor
training$target<-as.factor(training$target)
#Change Integer to factor values
columns_to_change<-colnames(select_if(training, is.integer))
for(i in 1:length(columns_to_change)){column_number<-which(colnames(training)==columns_to_change[i])  
training[,column_number]<-as.factor(training[,column_number])
}
#Check that the columns are now factors, numeric and integer
summarizeColumns(training)
#Check for na values
sum(is.na(training))
#Remove id field as it is not needed.
training$id<-NULL
table_models<-AutoML(training,0.7,1,Y)


#There are no missing values
#Check the outliers of the data a table is created to have an easier approach to graph the data
#Create a list with the numeric columns 
columns_total<-colnames(select_if(training, is.numeric))
#Create a dataframe with the columns name and number
columns_to_change<-data.frame(colnames(select_if(training, is.numeric)))
columns_to_change$Column_Number<-0
for(i in 1:length(columns_total)){ columns_to_change[i,2]<-which(colnames(training)==columns_total[i])
}
#summarizing and plotting y
summary(subset(training,select = c(colnames(select_if(training, is.numeric)))))
column_check<-15
hist(training[,column_check], breaks = 20, col = rgb(0,0,1,0.5))
boxplot(training[,column_check], col = rgb(0,0,1,0.5), main = "Boxplot of y[,2]")
shapiro.test(training[,column_check])
qqnorm(training[,column_check], main = "Normal QQ Plot - y")
qqline(training[,column_check], col = "red")

c1<-makeCluster(4,type = "SOCK")
registerDoSNOW(c1)
#Check the best model
Table_with_models<-AutoML(training,0.7,1,Y)
stopCluster(c1)
