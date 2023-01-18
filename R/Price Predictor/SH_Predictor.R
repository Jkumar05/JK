#Model part    
data = readxl::read_xlsx("C:/Users/vella/Downloads/Office/datasets/SH.xlsx")

data$Section = as.factor(data$Section)
data$Delivery_Option = as.factor(data$Delivery_Option)
data$Transaction_Date = as.Date(data$Transaction_Date,format = "%d-%m-%Y")

Use_Data = data[,c("Section","Quantity","Display_Price_Per_Ticket_Amount",
                   "Delivery_Option","Transaction_Date")]

na_clean_data = Use_Data
data.Section = sapply(na_clean_data$Section, as.numeric)

data.Delivery_Option = sapply(na_clean_data$Delivery_Option, as.numeric)


second_main_dataset = cbind(na_clean_data, Section_id = data.Section, 
                            Delivery_Option_id = data.Delivery_Option)

Main_data_set = second_main_dataset[,c("Section","Section_id",
                                       "Delivery_Option","Delivery_Option_id",
                                       "Quantity","Transaction_Date",
                                       "Display_Price_Per_Ticket_Amount")]


Main_data_set = subset(Main_data_set,select = -c(Section,Delivery_Option))

set.seed(3033)

split = sample.split(Main_data_set, SplitRatio = 0.8)
training_set = subset(Main_data_set, split == TRUE)
testing_set = subset(Main_data_set, split == FALSE)

lm_a = lm(Display_Price_Per_Ticket_Amount ~.,data = Main_data_set)
train_a = predict(lm_a,training_set)

test_a = predict(lm_a,testing_set)

e = "9-4-2021"
e = as.Date(e,format = "%d-%m-%Y")
input= data.frame(Section_id = 2, Delivery_Option_id =2,
                  Quantity = 4,Transaction_Date = e)
View(input)

a = predict(lm_a,input)
View(a)
summary(lm_a)
testing_set$a = test_a

confusionMatrix(table(testing_set$Display_Price_Per_Ticket_Amount,testing_set$a))
