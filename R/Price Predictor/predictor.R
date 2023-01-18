data = read.csv("C:/Users/vella/Downloads/fifa.csv")
View(data)
summary(data)
str(data)

data$Stage = as.factor(data$Stage)
data$City = as.factor(data$City)
data$Home = as.factor(data$Home)
data$Away = as.factor(data$Away)
data$Date = as.Date(data$Date,format = "%d-%m-%Y")

str(data)

Use_Data = data[,c("Stage","City","Home","Away","Date","Category_1")]
head(Use_Data)
summary(Use_Data)

na_clean_data = Use_Data
data.Stage = sapply(na_clean_data$Stage, as.numeric)
View(data.Stage)

data.City = sapply(na_clean_data$City, as.numeric)
View(data.City)

data.Home = sapply(na_clean_data$Home, as.numeric)
View(data.Home)

data.Away = sapply(na_clean_data$Away, as.numeric)
View(data.Away)

second_main_dataset = cbind(na_clean_data, Stage_id = data.Stage, 
                            City_id = data.City, Home_id = data.Home,
                            Away_id = data.Away)
View(second_main_dataset)

Main_data_set = second_main_dataset[,c("Stage","Stage_id","City","City_id",
                                       "Home","Home_id","Away","Away_id","Date",
                                       "Category_1")]


Main_data_set = subset(Main_data_set,select = -c(Stage,City,Home,Away))
View(Main_data_set)


library(caTools)
set.seed(3033)

split = sample.split(Main_data_set, SplitRatio = 0.8)
training_set = subset(Main_data_set, split == TRUE)
testing_set = subset(Main_data_set, split == FALSE)

lm_a = lm(Category_1 ~.,data = Main_data_set)
train_a = predict(lm_a,training_set)
View(train_a)

test_a = predict(lm_a,testing_set)
View(test_a)

#library(RCurl)
#saveRDS(lm_a, "lm.rds")

e = "9-1-2022"
e = as.Date(e,format = "%d-%m-%Y")

input= data.frame(Stage_id = 2, City_id =4,
                  Home_id = 3, Away_id = 2,
                  Date = e)
View(input)
a = predict(lm_a,input)
View(a)
summary(lm_a)

sample = read.csv("C:/Users/vella/Downloads/a.csv")
str(sample)
sample$Date = as.Date(sample$Date,format = "%d-%m-%Y")

b = predict(lm_a,sample)
View(b)
summary(lm_a)

library(ggplot2)
c = data.frame(sample$Date,b)
View(c)
ggplot(c, aes(x=sample.Date, y=b)) +
  geom_line()

dt = Sys.Date()
per = 1/as.integer(e - dt)
a= a + a*per
a
