#Importing Libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)

#Importing Book2_excel 
my_data = read_excel("C:/Users/admin/OneDrive/Desktop/Prajesh/Office/datasets/Book2_excel.xlsx")

#Extracting required data from my_data
#Column names - Section, Quantity and Transaction Date
my_data2 = cbind.data.frame(my_data$Transaction_Date,my_data$Section,my_data$Quantity,my_data$Display_Price_Per_Ticket_Amount)


#Formatting Transaction Date from POSIXct to Date format
my_data2$`my_data$Transaction_Date` = as.Date(my_data2$`my_data$Transaction_Date`)

#Changing Column names
names(my_data2)[1] = "Date"
names(my_data2)[2] = "section"
names(my_data2)[3] = "Sales"
names(my_data2)[4] = "Price"


#Sorting by Date
x= my_data2[order(as.Date(my_data2$Date, format="%m/%d/%Y")),]


#Total Sales per day
transactionData1 <- ddply(x,c("section","Date"),
                          function(df1)sum(df1$Sales), .drop = FALSE)


#Average on per Day
transactionData2 <- ddply(x,c("section","Date"),
                          function(df1)mean(df1$Price), .drop = FALSE)

#Combing the columns of sum of sales and average price
data_1 = cbind.data.frame(transactionData1$Date,transactionData1$section,transactionData1$V1,transactionData2$V1)

#Changing Column names
names(data_1)[1] = "Date"
names(data_1)[2] = "section"
names(data_1)[3] = "Sales"
names(data_1)[4] = "Price"

#Omittung the Nan values
data_2 = na.omit(data_1)

#Converting the formats of the variables
data_2$Date = as.Date(data_2$Date,format = "%d-%m-%Y")
data_2$section = as.factor(data_2$section)
#data_2$Price = as.factor(data_2$Price)
View(data_2)

#Converting sections to numeric
Section = sapply(data_2$section, as.numeric)


#creating a dataframe
data_3 = cbind.data.frame(data_2$Date,Section,data_2$Price,data_2$Sales)

#Changing Column names
names(data_3)[1] = "Date"
names(data_3)[2] = "section"
names(data_3)[3] = "Price"
names(data_3)[4] = "Sales"
View(data_3)


library(caTools)
set.seed(3033)

split = sample.split(data_3, SplitRatio = 0.8)
training_set = subset(data_3, split == TRUE)
testing_set = subset(data_3, split == FALSE)

lm_a = lm(Sales ~.,data = data_3)
train_a = predict(lm_a,training_set)
View(train_a)

test_a = predict(lm_a,testing_set)
View(test_a)

e = "25-3-2021"
e = as.Date(e,format = "%d-%m-%Y")

input= data.frame(Date = e,section = 1, Price=654)
#input$Price = as.factor(input$Price)
View(input)
a = predict(lm_a,input)
View(a)
summary(lm_a)
