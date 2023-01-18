#Importing Libraries
library(shinydashboard)
library(stringr)
library(arm)
library(dplyr)
library(plyr)
library(shiny)

#DATA PRE-PROCESSING
#Importing data
dummy1 = read.csv("C:/Users/admin/OneDrive/Desktop/Prajesh/Office/datasets/NFL/Cust_analysis_dashboard.csv")

##DASHBOARD
#UI
ui <- fluidPage(
  #pageheader
  headerPanel("Customer Analysis"),
  sidebarPanel(
    numericInput("Customer_id" , "Customer_id" ,""),
    actionButton("submitbutton","Analyse",
                 class = "btn btn-primary")
  ),
  mainPanel(
    sidebarPanel(width = 25, headerPanel("Customer's Analysis"),
                 verbatimTextOutput("value1"),verbatimTextOutput("value2"),verbatimTextOutput("value3")
                 ,verbatimTextOutput("value4"),verbatimTextOutput("value5"),verbatimTextOutput("value6")
                 ,verbatimTextOutput("value7"),verbatimTextOutput("value8"),verbatimTextOutput("value9")
                 ,verbatimTextOutput("value10"),verbatimTextOutput("value11"),verbatimTextOutput("value12")
                 ,verbatimTextOutput("value13"),verbatimTextOutput("value14"))
  )
)

#SERVER
server <- function(input, output) {
  
  #MATCHWISE ANALYSIS
  df1 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    # Match wise analysis
    match_counts = ddply(customer_wise,.(customer_wise$Team1,customer_wise$Team2),nrow)
    names(match_counts) = c('Team1','Team2','Frequency')
    
    max_watched_match = match_counts[match_counts$Frequency==max(match_counts$Frequency),]
    max_watched_match = as.data.frame(max_watched_match,stringAsFactor = FALSE)
    
    #Printing the Output
    cat("customer's favorite battle :" )
    for (i in 1:nrow(max_watched_match)){
      cat(as.character(max_watched_match$Team1[i]),"vs",as.character(max_watched_match$Team2[i]),"\n")
    }
  })
  
  df2 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    # Team wise analysis
    teams = rbind.data.frame(list(customer_wise$Team1),list(customer_wise$Team2))
    names(teams) = 'Teams'
    team_counts = ddply(teams,.(teams$Teams),nrow)
    names(team_counts) = c('Team','Frequency')
    
    max_watched_team = team_counts[team_counts$Frequency==max(team_counts$Frequency),]
    max_team = max_watched_team$Team
    max_team = as.data.frame(max_team)
    
    #Printing the Output
    cat("customer's favorite team: ")
    for (i in 1:nrow(max_team)){
      cat(as.character(max_team[i]),"\n")
    }
  })
  
  df3 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    #stadium wise ticket analysis
    stadium_counts = ddply(customer_wise,.(customer_wise$Venue_Name),nrow)
    names(stadium_counts) = c('Venue','Frequency')
    
    max_watched_stadium = stadium_counts[stadium_counts$Frequency==max(stadium_counts$Frequency),]
    max_stadium = max_watched_stadium$Venue
    max_stadium = as.data.frame(max_stadium)
    
    #Printing the Output
    cat("frequently visited stadium by the customer: ")
    for (i in 1:nrow(max_stadium)){
      cat(as.character(max_stadium[i]),"\n")
    }
  })
  
  df4 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    #section wise analysis
    sec_counts = ddply(customer_wise,.(customer_wise$Section),nrow)
    names(sec_counts) = c('Section','Frequency')
    
    sec_counts = as.data.frame(sec_counts)
    max_watched_section = sec_counts[sec_counts$Frequency==max(sec_counts$Frequency),]
    max_section = max_watched_section$Section
    max_section = as.data.frame(max_section)
    names(max_section) = 'section'
    
    #Printing the Output
    cat("customer's favorite section: ")
    for (i in 1:nrow(max_section)){
      cat(as.character(max_section$section[i]),"\n")
    }
  })
  
  df5 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    #section wise analysis
    sec_counts = ddply(customer_wise,.(customer_wise$Section),nrow)
    names(sec_counts) = c('Section','Frequency')
    
    sec_counts = as.data.frame(sec_counts)

    n = nrow(sec_counts)
    max_section_2 = sort(sec_counts$Frequency,partial = n-1)[n-1]
    max_watched_section_2 = sec_counts[sec_counts$Frequency==max_section_2,]
    max_section_2 = max_watched_section_2$Section
    max_section_2 = as.data.frame(max_section_2)
    names(max_section_2) = 'section'
    
    #Printing the Output
    cat("customer's second favorite section: ")
    for (i in 1:nrow(max_section_2)){
      cat(as.character(max_section_2$section[i]),"\n")
    }
  })
  
  df6 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    #section wise analysis
    sec_counts = ddply(customer_wise,.(customer_wise$Section),nrow)
    names(sec_counts) = c('Section','Frequency')
    
    costly_section = sort(sec_counts$Section,decreasing = TRUE)[1]
    
    #Printing the Output
    cat("customer's purchased costliest section: ",as.character(costly_section))
  })
  
  df7 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    #section wise analysis
    sec_counts = ddply(customer_wise,.(customer_wise$Section),nrow)
    names(sec_counts) = c('Section','Frequency')
    
    costly_section = sort(sec_counts$Section,decreasing = TRUE)[1]
    cheap_section =  sort(sec_counts$Section)[1]
    
    #Printing the Output
    cat("His range of sections are from ",as.character(cheap_section),'to',as.character(costly_section))
  })
  
  df8 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    #section wise analysis
    sec_counts = ddply(customer_wise,.(customer_wise$Section),nrow)
    names(sec_counts) = c('Section','Frequency')
    
    sec_counts = as.data.frame(sec_counts)
    max_watched_section = sec_counts[sec_counts$Frequency==max(sec_counts$Frequency),]
    max_section = max_watched_section$Section
    max_section = as.data.frame(max_section)
    names(max_section) = 'section'
    
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
    
    #Printing the Output
    cat("customer's ticket pricing range: ")
    for (i in 1:nrow(max_section)){
      c = as.character(max_section$section[i])
      print(get(c))
    }
  })
  
  df9 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    # Quantity analysis
    q_counts = ddply(customer_wise,.(customer_wise$Quantity),nrow)
    names(q_counts) = c('Quantity','Frequency')
    
    max_watched_quantity = q_counts[q_counts$Frequency==max(q_counts$Frequency),]
    max_quantity = max_watched_quantity$Quantity
    max_quant = as.data.frame(max_quantity)
    names(max_quant) = 'quantity'
    
    #Printing the Output
    cat("customer's most purchased number of tickets : ")
    for (i in 1:nrow(max_quant)){
      cat(as.character(max_quant$quantity[i]),"\n")
    }
  })
  
  df10 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
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
    
    #Printing the Output
    cat("He/She is a")
    for (i in 1:nrow(cust_type)){
      cat(as.character(cust_type$types[i]),"\n")
    }
  })
  
  df11 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
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
    
    #Printing the Output
    cat("customer's average  frequency to watch a match :",round(avg_cust_ddf))
  })
  
  df12 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    #Date difference analysis
    cust_Date = as.POSIXct(customer_wise$Sale_Date,format = "%d-%m-%Y")
    cust_date = as.data.frame(list(sort(cust_Date)))
    names(cust_date) = 'Cust_Date'
    cust_date_1 = as.data.frame(sort(unique(cust_date$Cust_Date)))
    names(cust_date_1) = 'Cust_Date'
    
    #active years of the customer
    cust_date_2=as.data.frame(format(cust_date_1$Cust_Date,'%Y'))
    names(cust_date_2) = 'customer_date'
    date_counts = ddply(cust_date_2,.(cust_date_2$customer_date),nrow)
    names(date_counts) = c('Customer_Date','Frequency')
    max_date = date_counts[date_counts$Frequency==max(date_counts$Frequency),]
    max_date = max_date$Customer_Date
    max_date = as.data.frame(max_date)
    
    #Printing the Output
    cat("He/She is more active in the year/years :")
    for (i in 1:nrow(max_date)){
      cat(as.character(max_date$max_date[i]),"\n")
    }
  })
  
  
  df13 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subset the dataset according to customer's number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    # eagerness of a customer 
    ticket_date = as.data.frame(customer_wise$date_difference)
    names(ticket_date) = 'Date_Difference'
    cust_eg = mean(ticket_date$Date_Difference)
    
    #Printing the Output
    cat("He/She purchases a ticket on an average of", round(cust_eg),"days before match.")
  })
  
  df14 = reactive({
    
    #Getting customer id
    dummy = data.frame(Customer_id = as.numeric(input$Customer_id))
    Customer_id = dummy$Customer_id
    
    #Subsetting the dataset according to the customer number
    customer_wise = subset.data.frame(dummy1,(dummy1$cust_id == Customer_id))
    
    
    #offer analysis 
    offer_counts = ddply(customer_wise,.(customer_wise$Offer),nrow)
    names(offer_counts) = c('Offer_no','Frequency')
    
    max_avail_offer = offer_counts[offer_counts$Frequency==max(offer_counts$Frequency),]
    max_avail_offer = as.data.frame(max_avail_offer,stringAsFactor = FALSE)
    
    #Offers 
    '1' = 'Get 1$ discount'
    '2' = 'No avail offers'
    
    '3' = 'Upto 2% discount'
    '4' = 'Free small soft drink'
    '5'= 'No service fee for next ticket'
    '6'='No avail offers'
    
    '7' = 'Upto 5$ discount'
    '8' = 'Get a free cap of your favourite team'
    '9' = 'Get 20% discount on your meal'
    '10'='No avail offers'
    
    '11'= 'Have a free combo meal'
    '12'='Get a  t shirt of your favourite team'
    '13'='Get 10% off on your parking ticket'
    '14'='No avail offers'
    
    '15'='Get upto 30% off on Adidas, puma and nike'
    '16'='Free parking pass'
    '17'= 'Get a signed jersey and cap of your favourite player'
    '18'='No avail offers'
    
    '19' ='Vallet parking' 
    '20'='Upto 100$ discount'
    '21'='No avail offers'
    
    '22'='Vallet parking'
    '23'='claim a free golf game ticket'
    '24'='No avail offers'
    
    '25'='Have a chance to meet your favourite players and get their autographs and click a pic with them'
    '26'='Vallet parking'
    '27'='No avail offers'
    
    '28'='Enjoy the experience at  your favourite team’s dressing room'
    '29'='Includes  accommodation and travel'
    '30'='No avail offers'
    
    #Printing the Output
    cat("Offers during which he purchased more:")
    for (i in 1:nrow(max_avail_offer)){
      c = as.character(max_avail_offer$Offer_no[i])
      print(get(c))
    }
    
    })
  
  ##PRINTING THE OUTPUT
  output$value1 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df1())
    }
  })
  output$value2 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df2())
    }
  })
  
  output$value3 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df3())
    }
  })
  output$value4 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df4())
    }
  })
  
  output$value5 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df5())
    }
  })
  output$value6 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df6())
    }
  })
  
  output$value7 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df7())
    }
  })
  output$value8 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df8())
    }
  })
  
  output$value9 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df9())
    }
  })
  output$value10 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df10())
    }
  })
  
  output$value11 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df11())
    }
  })
  output$value12 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df12())
    }
  })
  
  output$value13 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df13())
    }
  })
  output$value14 <- renderPrint({
    if (input$submitbutton>0) {
      isolate(df14())
    }
  })
}

shinyApp(ui, server)
