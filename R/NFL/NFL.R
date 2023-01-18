#Importing Libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)

data_1 = read_excel("C:/Users/admin/Downloads/NFL_Sales.xlsx")
str(data_1)

data_2 = as.data.frame(data_1[c(2,6,9,11,12,14,18,22)])
str(data_2)

data_2$Sale_Date <- format(as.POSIXct(data_2$Sale_Date,format='%y/%m/%d %H:%M:%S'),format='%y/%m/%d')
data_2$Event_Date = format(as.POSIXct(data_2$Event_Date,format='%y/%m/%d'),format='%y/%m/%d')

data_2["Date_difference"] = as.Date(data_2$Event_Date) - as.Date(data_2$Sale_Date)

x = unique(data_2$Event_Headliner)

team = x[1]

data_3 = filter(data_2, Event_Headliner == team)

y = unique(data_3$Event_Name)
y
y2 = gsub(".*:","",y)
y2
