#Importing Libraries
library(ggplot2)
library(plyr)
library(dplyr)

#Importing Book2_excel 
my_data = read_excel("C:/Users/vella/Downloads/Office/datasets/Book2_excel.xlsx")
str(my_data)

#Extracting required data from my_data
#Column names - Section, Quantity and Transaction Date
my_data2 = cbind.data.frame(my_data$Section,my_data$Quantity,my_data$Transaction_Date)
head(my_data2)
str(my_data2)

#Formatting Transaction Date from POSIXct to Date format
my_data2$`my_data$Transaction_Date` = as.Date(my_data2$`my_data$Transaction_Date`)
head(my_data2)

#Changing Column names
names(my_data2)[1] = "section"
names(my_data2)[2] = "Sales"
names(my_data2)[3] = "Date"
View(my_data2)
str(my_data2)

#Sorting by Date
x= my_data2[order(as.Date(my_data2$Date, format="%m/%d/%Y")),]
View(x)
str(x)

#Total Sales per day
transactionData1 <- ddply(x,c("section","Date"),
                          function(df1)sum(df1$Sales), .drop = FALSE)
View(transactionData1)

#Renaming 3rd column name
names(transactionData1)[3] = "Sales"

#Ommiting NaN values
dummy = transactionData1

#Selecting the Section
out <- dummy %>% dplyr::filter(section %in% c("Main","Club Hall of Fame"))
View(out)
str(out)

#Filtering the Date
out2  = out[out$Date >= "2021-03-23" & out$Date <= "2021-03-31",]
View(out2)

#Ploting a time series
trend = ggplot(out2,aes(x = Date, y= Sales, color  = section)) + geom_line() + geom_smooth(method = lm)
trend