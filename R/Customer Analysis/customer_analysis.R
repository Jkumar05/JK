#Importing libraries
library(stringr)
library(arm)
library(dplyr)
library(plyr)

#Importing data
data = read.csv("/Users/oppilisolutionsprivatelimited/Downloads/final_NFL.csv")

#Finding unique values of Total_cost by sorting
x = data.frame(sort(unique(data$Total_Cost)))

#Finding the values less than $1
y = x[1:20,]

#Neglecting the Total_cost less than $1
data_2 = data
for (i in y) {
  data_2 = subset(data_2, Total_Cost!= i)
}

#Grouping the Total_cost
data_3 = data_2
data_3$Total_Cost[data_3$Total_Cost <= 100] = 100
data_3$Total_Cost[data_3$Total_Cost> 100 & data_3$Total_Cost <= 200] = 200
data_3$Total_Cost[data_3$Total_Cost> 200 & data_3$Total_Cost <= 300] = 300
data_3$Total_Cost[data_3$Total_Cost> 300 & data_3$Total_Cost <= 500] = 500
data_3$Total_Cost[data_3$Total_Cost> 500 & data_3$Total_Cost <= 1000] = 1000
data_3$Total_Cost[data_3$Total_Cost> 1000 & data_3$Total_Cost <= 5000] = 5000
data_3$Total_Cost[data_3$Total_Cost> 5000 & data_3$Total_Cost <= 10000] = 10000
data_3$Total_Cost[data_3$Total_Cost> 10000 & data_3$Total_Cost <= 20000] = 20000
data_3$Total_Cost[data_3$Total_Cost> 20000] = 30000

#Renaming the Total_cost as Sections
data_4 = data_3
data_4$Total_Cost[data_4$Total_Cost == 100] = "A"
data_4$Total_Cost[data_4$Total_Cost == 200] = "B"
data_4$Total_Cost[data_4$Total_Cost == 300] = "C"
data_4$Total_Cost[data_4$Total_Cost == 500] = "D"
data_4$Total_Cost[data_4$Total_Cost == 1000] = "E"
data_4$Total_Cost[data_4$Total_Cost == 5000] = "F"
data_4$Total_Cost[data_4$Total_Cost == 10000] = "G"
data_4$Total_Cost[data_4$Total_Cost == 20000] = "H"
data_4$Total_Cost[data_4$Total_Cost == 30000] = "I"

#Removing unnecessary columns
data_5 = data_4[-c(2,3)]

#Renaming the Total_cost column as Section
names(data_5)[5] = "Section"

#Adding the Total_cost column from the old dataset
data_6 = cbind.data.frame(data_5,data_2$Total_Cost)

#Renaming the columns
names(data_6)[10] = "Cost"

#Changing the columns to factors
data_7 = data_6
data_7$Team1 = as.factor(data_7$Team1)
data_7$Team2 = as.factor(data_7$Team2)
data_7$Venue_Name = as.factor(data_7$Venue_Name)

#Found the values consisting parking in venue column and replaced the section as "I"
data_8 = data_7
parking = which(grepl("Parking", data_8$Venue_Name))
parking
for(i in parking){
  data_8$Section[i] = "J"
}
data_8$Section = as.factor(data_8$Section)

#Removing () and parking lots from the venue names
data_8$Venue_Name = gsub(r"{\s*\([^\)]+\)}","",as.character(data_8$Venue_Name))
wordsToRemove <- c("Parking","Parking Lots")

regex <- paste0("\\s*\\b(", paste(wordsToRemove, collapse="|"), ")\\b")
data_8$Venue_Name <- sub("^\\s+", "", gsub(regex, "", data_8$Venue_Name, ignore.case=TRUE))

#Removing negative date_difference rows
data_9 = data_8%>%filter(data_8$date_difference>=0)

#cooked up data
team_list = list('Philadelphia Eagles',' New York Giants',' Detroit Lions ',' New England Patriots ','San Francisco 49ers ','Tampa Bay Buccaneers',' Los Angeles Rams', ' Chicago Bears' ,'Denver Broncos','Atlanta Falcons' )
dummy1 = subset.data.frame(data_9,(data_9$Team1 %in% team_list) | (data_9$Team2 %in% team_list))
dummy1$cust_id = floor(runif(54929, min =1 ,max = 2500))

#saving it as csv 
#write.csv(dummy1,"/Users/oppilisolutionsprivatelimited/Documents/analysis/nfl_dummy.csv",row.names = FALSE)

#filtering customer id
customer_id = as.integer(readline(prompt = "Enter customer id : "))
customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == customer_id))

# Match wise analysis
match_counts = ddply(customer_wise,.(customer_wise$Team1,customer_wise$Team2),nrow)
names(match_counts) = c('Team1','Team2','Frequency')

max_watched_match = match_counts[match_counts$Frequency==max(match_counts$Frequency),]
max_watched_match = as.data.frame(max_watched_match,stringAsFactor = FALSE)

# Team wise analysis
teams = rbind.data.frame(list(customer_wise$Team1),list(customer_wise$Team2))
names(teams) = 'Teams'
team_counts = ddply(teams,.(teams$Teams),nrow)
names(team_counts) = c('Team','Frequency')

max_watched_team = team_counts[team_counts$Frequency==max(team_counts$Frequency),]
max_team = max_watched_team$Team
max_team = as.data.frame(max_team)

#stadium wise ticket analysis
stadium_counts = ddply(customer_wise,.(customer_wise$Venue_Name),nrow)
names(stadium_counts) = c('Venue','Frequency')

max_watched_stadium = stadium_counts[stadium_counts$Frequency==max(stadium_counts$Frequency),]
max_stadium = max_watched_stadium$Venue
max_stadium = as.data.frame(max_stadium)
#section wise analysis
sec_counts = ddply(customer_wise,.(customer_wise$Section),nrow)
names(sec_counts) = c('Section','Frequency')

max_watched_section = sec_counts[sec_counts$Frequency==max(sec_counts$Frequency),]
max_section = max_watched_section$Section
max_section = as.data.frame(max_section)
# Quantity analysis
q_counts = ddply(customer_wise,.(customer_wise$Quantity),nrow)
names(q_counts) = c('Quantity','Frequency')

max_watched_quantity = q_counts[q_counts$Frequency==max(q_counts$Frequency),]
max_quantity = max_watched_quantity$Quantity
max_quant = as.data.frame(max_quantity)
names(max_quant) = 'quantity'
cust_type = list()
for (i in 1:nrow(max_quant)) {
  if(max_quant$quantity[i] ==1){
    cust_type[i] = 'Single'
  }else if(max_quant$quantity[i]==2){
    cust_type[i] = 'Couple'
    
  }else if(max_quant$quantity[i]>2 & max_quant$quantity[i]<6){
    cust_type[i] = 'Family'
  }else if(max_quant$quantity[i]>=6){
    cust_type[i] = 'Group'
  }
  
}

cust_type = as.data.frame(unlist(cust_type))
names(cust_type) = 'types'

#Date difference analysis
cust_Date = as.POSIXct(customer_wise$Sale_Date,format = "%d-%m-%Y")
cust_date = as.data.frame(list(sort(cust_Date)))
names(cust_date) = 'Cust_Date'
cust_date_1 = as.data.frame(sort(unique(cust_date$Cust_Date)))
names(cust_date_1) = 'Cust_Date'

y = as.integer(nrow(cust_date_1))
cumm_freq_cust = list()

for(i in 1:y){
  x = as.integer(cust_date_1[i+1,] - cust_date_1[i,])
  cumm_freq_cust[[i]] = x
}

cumm_freq_cust[[y]] = 0
avg_cust_ddf = mean(unlist(cumm_freq_cust))


#active years of the customer
cust_date_2=as.data.frame(format(cust_date_1$Cust_Date,'%Y'))
names(cust_date_2) = 'customer_date'
date_counts = ddply(cust_date_2,.(cust_date_2$customer_date),nrow)
names(date_counts) = c('Customer_Date','Frequency')
max_date = date_counts[date_counts$Frequency==max(date_counts$Frequency),]
max_date = max_date$Customer_Date
max_date = as.data.frame(max_date)

# eagerness of a customer 
ticket_date = as.data.frame(customer_wise$date_difference)
names(ticket_date) = 'Date_Difference'
cust_eg = mean(ticket_date$Date_Difference)

#Pricing of sections 
A = "0-100"
B = "100-200"
C = "200-300"
D = "300-500"
E = "500-1000"
F = "1000-5000"
G = "5000-10000"
H = "10000-20000"
I = "Above 20000"
J = "Parking lot"

# outs 
cat("customer's favorite battle :" )
for (i in 1:nrow(max_watched_match)){
  cat(as.character(max_watched_match$Team1[i]),"vs",as.character(max_watched_match$Team2[i]),"\n")
}

cat("customer's favorite team: ")
for (i in 1:nrow(max_team)){
  cat(as.character(max_team[i]),"\n")
}

cat("frequently visited stadium by the customer: ")
for (i in 1:nrow(max_stadium)){
  cat(as.character(max_stadium[i]),"\n")
}
cat("customer's favorite section: ")
for (i in 1:nrow(max_section)){
  cat(as.character(max_section$max_section[i]),"\n")
}
cat("customer's ticket pricing range: ")
for (i in 1:nrow(max_section)){
  c = as.character(max_section$max_section[i])
  print(get(c))
}

cat("customer's most purchased number of tickets : ")
for (i in 1:nrow(max_quant)){
  cat(as.character(max_quant[i]),"\n")
}
cat("He/She is a")
for (i in 1:nrow(cust_type)){
  cat(as.character(cust_type$types[i]),"\n")
}
cat("customer's average  frequency to watch a match :",round(avg_cust_ddf))

cat("He/She is more active in the year/years :")
for (i in 1:nrow(max_date)){
  cat(as.character(max_date$max_date[i]),"\n")
}
cat("He/She purchases a ticket on an average of", round(cust_eg),"days before match.")



