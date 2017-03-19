# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/Rossmann Stores")

#libraries
library(readr)
library(dplyr)
library(caret)
library(DMwR)

#importing data
data <- read.csv("~/Data Science/Rossmann Stores/Data WIP/train.csv")
View(data)
colnames(data)
str(data)
data$Store<- as.factor(as.character(data$Store))
data$DayOfWeek<- as.factor(as.character(data$DayOfWeek))
data$Date<- as.Date(data$Date)
data$Open <- as.factor(as.character(data$Open))
data$Promo <- as.factor(as.character(data$Promo))
data$StateHoliday <- as.factor(as.character(data$StateHoliday))
data$SchoolHoliday <- as.factor(as.character(data$SchoolHoliday))


View(data.frame(unique(data$Store)))
#There are 1115 Stores
View(data.frame(unique(data$Date)))
# There are 942 unique dates
max(data$Date)-min(data$Date)
# the data is available for 941 days
#Lets find out the total no of days of data that is avaible for each store
store_noOfDates <-data %>% group_by(Store) %>% summarise(count = n())
View(store_noOfDates)
# Store numbers 1-180 have on;y 758 rows and the rest have 942 rows, 
#except store 988 that has 941 rows
#The missing data is between 30/06/14 and 01/01/15 for 185 days
# Some fundamental questions
#Are the promotions working?
# Lets look at overall country level
store_promo<-data %>% group_by( Promo) %>% summarise(Sales_mean=mean(Sales))
View(store_promo)
# Yes they are improving the top line 
# Without promotions the average sales is $4406
# With promotions the average sales is $7991
#Are the promotions increasing the custommer visit?
store_cust<-data %>% group_by(Promo) %>% summarise(Customer_mean=mean(Customers))
View(store_cust)
# Yes the number of customers visting a store are improving the top line 
# Without promotions the average no of customers visited is 517
# With promotions the average sales is 820

#Lets try linear Regrssion

train_rows<-createDataPartition(data$Sales, times = 1, p=0.7, list=F)
train_data <- data[train_rows,]
test_data <- data[-train_rows,]
lm_mode=lm(data = train_data, train_data$Sales~.)
summary(lm_mode)
View(lm_mode$coefficients)
test_data_2<- subset(test_data, Sales!=0)
predicted_values<-predict(lm_mode, newdata = test_data_2)

regr.eval(test_data$Sales,predicted_values )

evaluation_function <- function(trues, preds){
  
  total_sums= sum(((trues-preds)/trues)^2)
  RMSPE= (total_sums/length(trues))^0.5
  RMSPE
}
evaluation_function(trues =test_data_2$Sales, preds = predicted_values )
test_data[test_data$Sales==0,]
View(data.frame(predicted_values, test_data$Sales))
