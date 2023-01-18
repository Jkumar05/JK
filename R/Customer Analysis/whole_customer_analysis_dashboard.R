#importing libraries
library(stringr)
library(arm)
library(dplyr)
library(plyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(heatmaply)
library(shiny)
library(shinydashboard)
library(cowplot)
library(gridExtra)
library(lubridate)
library(shinyfullscreen)

packages = c('shiny','shinydashboard','tidyverse','dplyr', 'magrittr', 'plotly', 'ggplot2', 'scales', 'DT')

#Importing data
data = read.csv("C:/Users/admin/Downloads/nfl_dummy.csv")

#cleaning venue name data
wordsToRemove <- c("Football","NY Jets","NFL")
regex <- paste0("\\s*\\b(", paste(wordsToRemove, collapse="|"), ")\\b")
data$Venue_Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(data$Venue_Name))
rep_str = c("Gillette Stadium - Foxboro" = "Gillette Stadium",
            "GEHA Field at Arrowhead Stadium"="Arrowhead Stadium","FedEx Field" = "FedExField",
            "Los Angeles Memorial Sports Arena and Coliseum"="Los Angeles Memorial Sports Arena",
            "Mercedes-Benz Superdome"= "Mercedes-Benz Stadium",
            "FirstEnergy Stadium-Cleveland"="FirstEnergy Stadium Cleveland",
            "Los Angeles Memorial Coliseum"="Los Angeles Memorial Sports Arena",
            "Levi's Stadium"="Levis Stadium")
data$Venue_Name = str_replace_all(data$Venue_Name,rep_str)

data$Event_Date = as.POSIXct(data$Event_Date,format = "%d-%m-%Y")
data$Sale_Date = as.POSIXct(data$Sale_Date,format = "%d-%m-%Y")
data_1 = data
data = subset.data.frame(data,(data$Section != "G") & (data$Section != "H") & (data$Section != "I"))

#List of team names 
team_list = list("Arizona Cardinals"     ,   "Atlanta Falcons"       ,   "Baltimore Ravens",        
                 "Buffalo Bills"        ,  "Carolina Panthers"        ,"Chicago Bears"      ,     
                 "Cincinnati Bengals"    ,   "Cleveland Browns"       ,  "Dallas Cowboys"    ,      
                 "Denver Broncos"       ,    "Detroit Lions"          ,  "Green Bay Packers"  ,     
                 "Houston Texans"       ,    "Indianapolis Colts"     ,  "Jacksonville Jaguars",    
                 "Kansas City Chiefs"   ,    "Las Vegas Raiders"      ,  "Los Angeles Chargers" ,   
                 "Los Angeles Rams"     ,    "Miami Dolphins"         ,  "Minnesota Vikings"     ,  
                 "New England Patriots" ,    "New Orleans Saints"     ,  "New York Giants"        , 
                 "New York Jets"        ,    "Oakland Raiders"        ,  "Philadelphia Eagles"     ,
                 "Pittsburgh Steelers"  ,    "San Diego Chargers"     ,  "San Francisco 49ers"     ,
                 "Seattle Seahawks"     ,    "St Louis Rams"          ,  "Tampa Bay Buccaneers"    ,
                 "Tennessee Titans"     ,    "Washington Football Team", "Washington Redskins"  )

## For UI

ui = dashboardPage(
  #Page header
  
  dashboardHeader(title = "Whole Customer Analysis"),
  dashboardSidebar(
    selectInput('Team_list','Choose the Team Name: ',team_list)
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Highest tickets sold in each year", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("fav_stadium_plot", height = 400)),
      
      box(
        title = "Total tickets sold in each stadium", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("Stadium_sales_2", height = 400)),
      
      box(
        title = "Tickets sold in each section throughout the years", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("liked_section_plot", height = 400)),

      box(
        title = "Overall sales in each section", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("overall_sales_plot", height = 400)),

      box(
        title = "Fan base during each year", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
        plotlyOutput("overall_sales_per_year_plot", height = 400)),

        box(
          title = "Revenue over the years", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("revenue_plot", height = 400)),

        box(
          title = "Correlation between sales and revenue", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("corr_sales_revenue_plot", height = 400)),

        box(
          title = "Audience type", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("audience_type_plot", height = 400)),

        box(
          title = "Offers over the years", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("opted_offers_plot", height = 400)),

        box(
          title = "Most Opted offers over year", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("max_offer_plot", height = 400)),
  
        box(
          title = "Win/Loss Sales analysis", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("Match_res_sales", height = 400)),
  
        box(
          title = "Customer's eagerness over years", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("Eagernes", height = 400)),

        box(
          title = "Mode of sales over years", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("Sales_mode_year", height = 400)),
  
        box(
          title = "Mode of sales in each section", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("Sales_mode_section", height = 400)),
  
        box(
          title = "Sales based on day and time", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("D_T_sales", height = 400)),
  
        box(
          title = "Revenue based on day and time", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("D_T_Rev", height = 400)),
  
        box(
          title = "Gender analysis over years", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("gender_date_plot", height = 400)),
  
        box(
          title = "Gender analysis based on sections", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("gender_section_plot", height = 400)),
  
        box(
          title = "Profession analysis over years", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("profession_date_plot", height = 400)),
  
        box(
          title = "Profession anlaysis based on section", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("profession_section_plot", height = 400)),
  
        box(
          title = "Sales day analysis", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
          plotlyOutput("sales_daywise", height = 400))
      ),
    fluidRow(
      column(width = 12,
        box(
          title = "VIP Ticket sales", background = "light-blue", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
          column(12, (DT::dataTableOutput("vip_sales")))),
        box(
          title = "Loyal customer", background = "light-blue", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
          column(12, (DT::dataTableOutput("Loyal_cust")))),

        box(
          title = "Match day counts", background = "light-blue", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
          column(12, (DT::dataTableOutput("Match_day")))),
  
        box(
          title = "Match day sales report", background = "light-blue", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
          column(12, (DT::dataTableOutput("Match_day_sales"))))
    )
    ),
    fluidRow(
      valueBoxOutput("Fans_match_day_pref")
    ),
    fluidRow(
      column(width = 12,
        box(
          title = "Competitive games based on sales", background = "light-blue", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
          column(12, (DT::dataTableOutput("most_rival_sales")))),
  
        box(
          title = "Competitive games based on revenue", background = "light-blue", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
          column(12, (DT::dataTableOutput("most_rival_revenue"))))
    )
    )
  )
)

server <- function(input, output,session){
  
  #Highest tickets sold stadiums in each year
  output$fav_stadium_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    Stadium_sales <- ddply(dummy_2,c("Event_Date","Venue_Name"),
                           function(df1)sum(df1$Quantity))
    names(Stadium_sales)[3] = "Quantity"
    
    max_sales_in_each_year = Stadium_sales %>% group_by(Event_Date) %>% top_n(1,Quantity)
    
    fav_stadium_plot = ggplot(data =max_sales_in_each_year,aes(x = Event_Date,y = Quantity,fill =Venue_Name))+
      geom_bar(stat = 'identity')
    
    ggplotly(fav_stadium_plot, tooltip = c("text"))
    
  })
  
  #Total tickets sales in each stadium
  output$Stadium_sales_2 <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    Stadium_sales_2 <- ddply(dummy_2,c("Venue_Name"),
                             function(df1)sum(df1$Quantity))
    names(Stadium_sales_2)[2] = "Quantity"
    
    fav_stadium_2_plot = ggplot(data =Stadium_sales_2,aes(x = Venue_Name,y = Quantity , fill = Venue_Name))+
      geom_bar(stat = 'identity')+
      theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=1,size = 5))
    
    ggplotly(fav_stadium_2_plot, tooltip = c("text"))
    
  })

  #Total tickets sales in each section throughout the years
  output$liked_section_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data_1,(data_1$Team1== input$Team_list)|(data_1$Team2== input$Team_list))
    
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    section_sales <- ddply(dummy_2,c("Event_Date","Section"),
                           function(df1)sum(df1$Quantity))
    names(section_sales)[3] = "Quantity"
    liked_section_plot = ggplot(data =section_sales,aes(x = Event_Date,y = Quantity,fill = Section))+
      geom_bar(stat = 'identity')
    
    ggplotly(liked_section_plot, tooltip = c("text"))
    
  })
  
  #Total tickets sales in each section
  output$overall_sales_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data_1,(data_1$Team1== input$Team_list)|(data_1$Team2== input$Team_list))
    
    #overall sales for individual section
    sec_quant_counts = with(custom_team_wise,tapply(Quantity, Section, FUN = sum))
    sec_quant_counts = as.data.frame(sec_quant_counts)
    sec_quant_counts <- cbind(Section = rownames(sec_quant_counts), sec_quant_counts)
    names(sec_quant_counts)[2] = 'Quant_Freq'
    
    overall_sales_plot = ggplot(data =sec_quant_counts,aes(x = Section,y =Quant_Freq,fill = Section))+
      geom_bar(stat = 'identity')
    
    ggplotly(overall_sales_plot, tooltip = c("text"))
    
  })
  
  #Fan Base during each year
  output$overall_sales_per_year_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    # overall sales /fan base during each year
    overall_sales <- ddply(dummy_2,c("Event_Date"),
                           function(df1)sum(df1$Quantity))
    names(overall_sales)[2] = 'Quantity'
    overall_sales_per_year_plot = ggplot(data =overall_sales,aes(x = Event_Date,y = Quantity))+
      geom_bar(stat = 'identity')
    
    ggplotly(overall_sales_per_year_plot, tooltip = c("text"))
    
  })
  
  #Total VIP tickets sales
  output$vip_sales <- DT::renderDataTable({
    custom_team_wise = subset.data.frame(data_1,(data_1$Team1== input$Team_list)|(data_1$Team2== input$Team_list))
    
    #overall sales for individual section
    sec_quant_counts = with(custom_team_wise,tapply(Quantity, Section, FUN = sum))
    sec_quant_counts = as.data.frame(sec_quant_counts)
    sec_quant_counts <- cbind(Section = rownames(sec_quant_counts), sec_quant_counts)
    names(sec_quant_counts)[2] = 'Quant_Freq'
    
    #Vip ticket sales
    vip_sales = sec_quant_counts[which(sec_quant_counts$Section == 'G' |sec_quant_counts$Section == 'H'|sec_quant_counts$Section == 'I'),]
    vip_sales
    
  })
  
  #Fan Base during each year
  output$revenue_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data_1,(data_1$Team1== input$Team_list)|(data_1$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    #Revenue over the years 
    revenue_sales = ddply(dummy_2,c('Event_Date','Section'),function(df1)sum(df1$Cost))
    names(revenue_sales)[3] = 'Cost'
    
    revenue_plot= ggplot(data =revenue_sales,aes(x = Event_Date,y = Cost,fill = Section,label = Cost))+
      geom_bar(stat = 'identity')+geom_text(size = 3, position = position_stack(vjust = 0.5))
    
    ggplotly(revenue_plot, tooltip = c("text"))
    
  })
  
  #Fan Base during each year
  output$corr_sales_revenue_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    overall_sales <- ddply(dummy_2,c("Event_Date"),
                           function(df1)sum(df1$Quantity))
    names(overall_sales)[2] = 'Quantity'
    
    #Correlation between revenue and sales excluding vip sales
    revenue_sales_year = ddply(dummy_2,c('Event_Date'),function(df1)sum(df1$Cost))
    rs_data  = cbind.data.frame(overall_sales,revenue_sales_year)
    rs_data = rs_data[-c(3)]
    names(rs_data)[3] = 'Cost'
    rs_data = rs_data[-1]
    
    cor.test(rs_data$Quantity,rs_data$Cost,method = 'pearson')
    corr_sales_revenue_plot = heatmaply_cor(x = cor(rs_data),xlab = 'Features',ylab = 'Features',k_col = 2,k_row = 2)
    
    ggplotly(corr_sales_revenue_plot, tooltip = c("text"))
    
  })
  
  #Audience type during each year
  output$audience_type_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    # Most viewed type of audience based on quantity
    q_counts = dummy_2[c(3,6)]
    
    for (i in 1:nrow(q_counts)) {
      if(q_counts$Quantity[i] ==1){
        q_counts$cust_type[i] = 'Single'
      }else if(q_counts$Quantity[i]==2){
        q_counts$cust_type[i] = 'Couple'
        
      }else if(q_counts$Quantity[i]>2 & q_counts$Quantity[i]<6){
        q_counts$cust_type[i] = 'Family'
      }else if(q_counts$Quantity[i]>=6){
        q_counts$cust_type[i] = 'Group'
      }
      
    }
    
    q_type_count =ddply(q_counts,.(q_counts$cust_type,q_counts$Event_Date),nrow)
    names(q_type_count)[1] = 'Customer_type'
    names(q_type_count)[2] = 'Event_Date'
    names(q_type_count)[3] = 'Frequency'
    
    audience_type_plot = ggplot(data =q_type_count,aes(x = Event_Date,y = Frequency,fill = Customer_type,label = Frequency))+
      geom_bar(stat = 'identity')+geom_text(size = 3, position = position_stack(vjust = 0.5))
    
    ggplotly(audience_type_plot, tooltip = c("text"))
    
  })
  
  #Offer Analysis
  output$opted_offers_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    #Most offers opted over the year
    offers = dummy_2[c(3,12)]
    
    offer_count =ddply(offers,.(offers$Offer,offers$Event_Date),nrow)
    names(offer_count)[1] = 'offer_no'
    names(offer_count)[2] = 'Event_Date'
    names(offer_count)[3] = 'Frequency'
    offer_count = offer_count[offer_count$Frequency>4,]
    
    opted_offers_plot = ggplot(data =offer_count,aes(x = Event_Date,y = Frequency,fill = as.character(offer_no),label = Frequency))+
      geom_bar(stat = 'identity')+geom_text(size = 3, position = position_stack(vjust = 0.5))
    
    ggplotly(opted_offers_plot, tooltip = c("text"))
    
  })
  
  #Most Opted offers over year
  output$max_offer_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    #Most offers opted over the year
    offers = dummy_2[c(3,12)]
    
    offer_count =ddply(offers,.(offers$Offer,offers$Event_Date),nrow)
    names(offer_count)[1] = 'offer_no'
    names(offer_count)[2] = 'Event_Date'
    names(offer_count)[3] = 'Frequency'
    offer_count = offer_count[offer_count$Frequency>4,]
    
    max_offer_in_each_year = offer_count %>% group_by(Event_Date) %>% top_n(3,Frequency)
    
    max_offer_plot = ggplot(data =max_offer_in_each_year,aes(x = Event_Date,y = Frequency,fill = as.character(offer_no),label = Frequency))+
      geom_bar(stat = 'identity')+geom_text(size = 3, position = position_stack(vjust = 0.5))
    
    ggplotly(max_offer_plot, tooltip = c("text"))
    
  })
  
  #Match day count
  output$Match_day <- DT::renderDataTable({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    
    #COMPARISION BETWEEN WEEKEND AND WEEKDAY SALES
    
    #Adding a column of match day happening
    dummy3 = ddply(custom_team_wise,c("Event_Date"),
                   function(df1)sum(df1$Quantity))
    names(dummy3)[2] = "Quantity"
    
    dummy3$Match_day <- wday(dummy3$Event_Date, label=TRUE, abbr=FALSE)
    
    for (i in 1:nrow(dummy3)) {
      if (dummy3$Match_day[i] == "Sunday" |dummy3$Match_day[i] == "Saturday" ){
        dummy3$Day_type[i] = "Weekend"
      }else{
        dummy3$Day_type[i] = "Weekday"
      }
    }
    
    
    day_count =ddply(dummy3,.(dummy3$Day_type),nrow)
    names(day_count)[2] = 'Frequency'
    names(day_count)[1] = 'Day_type'
    day_count
  })
  
  #Match day count
  output$Match_day_sales <- DT::renderDataTable({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    
    #COMPARISION BETWEEN WEEKEND AND WEEKDAY SALES
    
    #Adding a column of match day happening
    dummy3 = ddply(custom_team_wise,c("Event_Date"),
                   function(df1)sum(df1$Quantity))
    names(dummy3)[2] = "Quantity"
    
    dummy3$Match_day <- wday(dummy3$Event_Date, label=TRUE, abbr=FALSE)
    
    for (i in 1:nrow(dummy3)) {
      if (dummy3$Match_day[i] == "Sunday" |dummy3$Match_day[i] == "Saturday" ){
        dummy3$Day_type[i] = "Weekend"
      }else{
        dummy3$Day_type[i] = "Weekday"
      }
    }
    
    
    day_count =ddply(dummy3,.(dummy3$Day_type),nrow)
    names(day_count)[2] = 'Frequency'
    names(day_count)[1] = 'Day_type'
    day_count_quantity = ddply(dummy3,c('Day_type'),function(df1)sum(df1$Quantity))
    names(day_count_quantity)[2] = 'Quantity'
    
    day_count_quantity
  })
  
  output$Fans_match_day_pref <- renderValueBox({
    
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    
    #COMPARISION BETWEEN WEEKEND AND WEEKDAY SALES
    
    #Adding a column of match day happening
    dummy3 = ddply(custom_team_wise,c("Event_Date"),
                   function(df1)sum(df1$Quantity))
    names(dummy3)[2] = "Quantity"
    
    dummy3$Match_day <- wday(dummy3$Event_Date, label=TRUE, abbr=FALSE)
    
    for (i in 1:nrow(dummy3)) {
      if (dummy3$Match_day[i] == "Sunday" |dummy3$Match_day[i] == "Saturday" ){
        dummy3$Day_type[i] = "Weekend"
      }else{
        dummy3$Day_type[i] = "Weekday"
      }
    }
    
    
    day_count =ddply(dummy3,.(dummy3$Day_type),nrow)
    names(day_count)[2] = 'Frequency'
    names(day_count)[1] = 'Day_type'
    day_count_quantity = ddply(dummy3,c('Day_type'),function(df1)sum(df1$Quantity))
    names(day_count_quantity)[2] = 'Quantity'
    
    #ratio of weekdays and weekends wrt sales
    weekend_ratio = day_count_quantity[2,2]/day_count[2,2]
    weekday_ratio = day_count_quantity[1,2]/day_count[1,2]
    
    #Customer type based on match day
    
    if((weekend_ratio-weekday_ratio)>0.5){
      customer_type = "Weekend"
    }else if((weekday_ratio - weekend_ratio)>0.5){
      customer_type = "Weekday"
    }else{
      customer_type = "Both weekday and weekend"
    }
    
    
    valueBox(
      paste0(customer_type),
      "Fans preferred days",
      color = "yellow",
      icon = icon("fas fa-calendar"))
  })
  
  #Win/Loss Sales analysis
  output$Match_res_sales <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    
    ## Win /loss sales analysis 
    
    #Summing up quantity based on date
    
    quan_res_Data = ddply(custom_team_wise,c('Event_Date'),function(df1)sum(df1$Quantity))
    quan_res_Data_1 = quan_res_Data
    quan_res_Data_1$Event_Date = as.POSIXct(quan_res_Data_1$Event_Date,format = "%d-%m-%Y")
    quan_res_Data_2 = quan_res_Data_1[order(quan_res_Data_1$Event_Date),]
    names(quan_res_Data_2)[2] = 'Quantity'
    
    #Adding the result of the match column
    
    quan_res_Data_2$result = sample(c("Win","loss"),size = nrow(quan_res_Data_2),replace = TRUE)
    quan_res_Data_2$Event_Date = as.character(quan_res_Data_2$Event_Date)
    
    #reindexing the dataframe
    
    rownames(quan_res_Data_2) = 1:nrow(quan_res_Data_2)
    
    result_sales_date_plot = ggplot(data =quan_res_Data_2,aes(x = Event_Date,y = Quantity,fill = result,label = Quantity))+
      geom_bar(stat = 'identity')+geom_text(size = 3, position = position_stack(vjust = 0.5))+
      theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=1,size = 5))
    
    
    ggplotly(result_sales_date_plot, tooltip = c("text"))
    
  })
  
  #Loyal customer
  output$Loyal_cust <- DT::renderDataTable({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Loyal customers
    
    loyal_data = ddply(dummy_2,.(dummy_2$cust_id),nrow)
    names(loyal_data)[1] = 'cust_id'
    names(loyal_data)[2] = 'Frequency'
    
    first_max = max(loyal_data$Frequency)
    second_max =max(loyal_data$Frequency[loyal_data$Frequency!=max(loyal_data$Frequency)])
    
    loyal_data_1 = which(grepl(c(first_max),loyal_data$Frequency))
    loyal_data_2 = which(grepl(c(second_max),loyal_data$Frequency))
    
    loyal_data_3 = append(loyal_data_1,loyal_data_2)
    loyal_data_4 = data.frame()
    for(i in loyal_data_3){
      loyal_data_4 = rbind(loyal_data_4,loyal_data[i,])
    }
    names(loyal_data_4)[2] = 'Frequency'
    
    #reindexing the dataframe
    
    rownames(loyal_data_4) = 1:nrow(loyal_data_4)
    loyal_data_4
    
  })
  
  #Eagernes
  output$Eagernes <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Audience eagerness for the match by date difference on Sections
    
    eagerness = ddply(dummy_2,c("Event_Date","Section"),
                      function(df1)round(mean(df1$date_difference)))
    names(eagerness)[3] = 'Avg_dd'
    
    #Visualizing the eagerness of customers based on sections 
    
    customer_eagerness_plot = ggplot(data =eagerness,aes(x = Event_Date,y = Avg_dd,fill = Section,label = Avg_dd))+
      geom_bar(stat = 'identity')+geom_text(size = 3, position = position_stack(vjust = 0.5))
    
    ggplotly(customer_eagerness_plot, tooltip = c("text"))
    
  })
  #Best duo based on sales
  output$most_rival_sales <- DT::renderDataTable({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Best Rival based on sales 
    
    rival_sales = ddply(dummy_2,c("Team1","Team2"),
                        function(df1)sum(df1$Quantity))
    
    for (i in 1:nrow(rival_sales)) {
      if(rival_sales[i,1]==input$Team_list){
        rival_sales$opponent[i] = rival_sales[i,2]
      }else{
        rival_sales$opponent[i] = rival_sales[i,1]
      }
      
    }
    
    rival_sales_2 = ddply(rival_sales,c("opponent"),
                          function(df1)sum(df1$V1))
    names(rival_sales_2)[2] = 'Quantity'
    rival_sales_2 = arrange(rival_sales_2,desc(Quantity))
    
    most_rival_sales = head(rival_sales_2,10)
    
    most_rival_sales
  })
  
  #Best duo based on revenue
  output$most_rival_revenue <- DT::renderDataTable({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Best rival based on revenue
    
    rival_revenue = ddply(dummy_2,c("Team1","Team2"),
                          function(df1)sum(df1$Cost))
    
    for (i in 1:nrow(rival_revenue)) {
      if(rival_revenue[i,1]==input$Team_list){
        rival_revenue$opponent[i] = rival_revenue[i,2]
      }else{
        rival_revenue$opponent[i] = rival_revenue[i,1]
      }
      
    }
    
    rival_revenue_2 = ddply(rival_revenue,c("opponent"),
                            function(df1)sum(df1$V1))
    names(rival_revenue_2)[2] = 'Cost'
    rival_revenue_2 = arrange(rival_revenue_2,desc(Cost))
    
    most_rival_revenue = head(rival_revenue_2,10)
    most_rival_revenue
  })
  
  #Mode of sales over years
  output$Sales_mode_year <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Mode of ticket sales analysis
    
    #Adding a column of the mode of sales
    dummy_2$sales_mode <- sample(c("Online","Offline"),
                                 size = nrow(dummy_2), 
                                 replace = TRUE)
    
    sales_section = ddply(dummy_2,c("sales_mode","Section","Event_Date"),
                          function(df1)sum(df1$Quantity))
    names(sales_section)[4] = 'Quantity'
    
    #Visualising the sales mode
    
    sales_mode_date_plot = ggplot(sales_section , aes(Event_Date,Quantity,fill = sales_mode))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    ggplotly(sales_mode_date_plot, tooltip = c("text"))
    
  })
  
  #Mode of sales in each section
  output$Sales_mode_section <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Mode of ticket sales analysis
    
    #Adding a column of the mode of sales
    dummy_2$sales_mode <- sample(c("Online","Offline"),
                                 size = nrow(dummy_2), 
                                 replace = TRUE)
    
    sales_section = ddply(dummy_2,c("sales_mode","Section","Event_Date"),
                          function(df1)sum(df1$Quantity))
    names(sales_section)[4] = 'Quantity'
    
    #Visualising the sales mode
    
    sales_mode_section_plot = ggplot(sales_section , aes(Section,Quantity,fill = sales_mode))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    ggplotly(sales_mode_section_plot, tooltip = c("text"))
    
  })
  
  #Sales based on day and time
  output$D_T_sales <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    #Adding a column of Match Time period 
    dummy_2$Match_day <- wday(custom_team_wise$Event_Date, label=TRUE, abbr=FALSE)
    
    dummy_2$Match_time <- sample(c("Forenoon", "Afternoon","Evening","Late Night"),
                                 size = nrow(dummy_2), 
                                 replace = TRUE)
    
    
    match_time_sales = ddply(dummy_2,c("Match_time","Event_Date","Match_day"),
                             function(df1)sum(df1$Quantity))
    names(match_time_sales)[4] = 'Quantity'
    
    match_time_sales_plot = ggplot(match_time_sales , aes(Event_Date,Quantity,fill = interaction(Match_time,Match_day)))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    
    ggplotly(match_time_sales_plot, tooltip = c("text"))
    
  })
  
  #revenue based on day and time
  output$D_T_Rev <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ## Match time analysis 
    
    #Adding a column of Match Time period 
    dummy_2$Match_day <- wday(custom_team_wise$Event_Date, label=TRUE, abbr=FALSE)
    
    dummy_2$Match_time <- sample(c("Forenoon", "Afternoon","Evening","Late Night"),
                                 size = nrow(dummy_2), 
                                 replace = TRUE)
    
    match_time_revenue = ddply(dummy_2,c("Match_time","Event_Date","Match_day"),
                               function(df1)sum(df1$Cost))
    names(match_time_revenue)[4] = 'Cost'
    
    match_time_revenue_plot = ggplot(match_time_revenue , aes(Event_Date,Cost,fill = interaction(Match_time,Match_day)))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    
    
    ggplotly(match_time_revenue_plot, tooltip = c("text"))
    
  })
  
  #Gender analysis over years
  output$gender_date_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Gender analysis
    
    gender_data = as.data.frame(lapply(dummy_2, rep,dummy_2$Quantity))
    
    gender_data$gender = sample(c("Male","Female"),size = nrow(gender_data),replace = TRUE)
    
    gender_section = ddply(gender_data,.(gender_data$Section,gender_data$gender,gender_data$Event_Date),nrow)
    names(gender_section)[1] = 'Section'
    names(gender_section)[2] = 'Gender'
    names(gender_section)[3] = 'Event_Date'
    names(gender_section)[4] = 'Frequency'
    
    #visualising the gender data based on section and event year
    
    gender_date_plot = ggplot(gender_section , aes(Event_Date,Frequency,fill = Gender))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    
    
    ggplotly(gender_date_plot, tooltip = c("text"))
    
  })
  
  #Gender analysis based on sections
  output$gender_section_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Gender analysis
    
    gender_data = as.data.frame(lapply(dummy_2, rep,dummy_2$Quantity))
    
    gender_data$gender = sample(c("Male","Female"),size = nrow(gender_data),replace = TRUE)
    
    gender_section = ddply(gender_data,.(gender_data$Section,gender_data$gender,gender_data$Event_Date),nrow)
    names(gender_section)[1] = 'Section'
    names(gender_section)[2] = 'Gender'
    names(gender_section)[3] = 'Event_Date'
    names(gender_section)[4] = 'Frequency'
    
    #visualising the gender data based on section and event year
    
    gender_section_plot = ggplot(gender_section , aes(Section,Frequency,fill = Gender))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    
    ggplotly(gender_section_plot, tooltip = c("text"))
    
  })
  
  #Gender analysis based on sections
  output$profession_date_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Gender analysis
    
    gender_data = as.data.frame(lapply(dummy_2, rep,dummy_2$Quantity))
    
    gender_data$gender = sample(c("Male","Female"),size = nrow(gender_data),replace = TRUE)
    
    ##Proffesion analyis
    #Adding a column of the type of customer which can used to classify the age
    gender_data$cust_edu <- sample(c("School","College","Working","Retired"),
                                   size = nrow(gender_data),
                                   replace = TRUE)
    
    edu_section = ddply(gender_data,.(gender_data$Section,gender_data$cust_edu,gender_data$Event_Date),nrow)
    names(edu_section)[1] = 'Section'
    names(edu_section)[2] = 'Education'
    names(edu_section)[3] = 'Event_Date'
    names(edu_section)[4] = 'Frequency'
    
    # visualising the profession data based on section and date
    
    profession_date_plot = ggplot(edu_section , aes(Event_Date,Frequency,fill = Education))+
      geom_bar(stat = 'identity',position = 'dodge') 
   
    ggplotly(profession_date_plot, tooltip = c("text"))
    
  })
  
  #Gender analysis based on sections
  output$profession_section_plot <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    dummy_2 = custom_team_wise
    dummy_2$Event_Date = format(custom_team_wise$Event_Date,'%Y')
    
    ##Gender analysis
    
    gender_data = as.data.frame(lapply(dummy_2, rep,dummy_2$Quantity))
    
    gender_data$gender = sample(c("Male","Female"),size = nrow(gender_data),replace = TRUE)
    
    ##Proffesion analyis
    #Adding a column of the type of customer which can used to classify the age
    gender_data$cust_edu <- sample(c("School","College","Working","Retired"),
                                   size = nrow(gender_data),
                                   replace = TRUE)
    
    edu_section = ddply(gender_data,.(gender_data$Section,gender_data$cust_edu,gender_data$Event_Date),nrow)
    names(edu_section)[1] = 'Section'
    names(edu_section)[2] = 'Education'
    names(edu_section)[3] = 'Event_Date'
    names(edu_section)[4] = 'Frequency'
    
    # visualising the profession data based on section and date
    
    profession_section_plot = ggplot(edu_section , aes(Section,Frequency,fill = Education))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    
    ggplotly(profession_section_plot, tooltip = c("text"))
    
  })
  
  #Gender analysis based on sections
  output$sales_daywise <- renderPlotly({
    custom_team_wise = subset.data.frame(data,(data$Team1== input$Team_list)|(data$Team2== input$Team_list))
    
    ##Sales day anlaysis
    dummy4 = ddply(custom_team_wise,c("Sale_Date"),
                   function(df1)sum(df1$Quantity))
    names(dummy4)[2] = "Quantity"
    
    dummy4$Match_day <- wday(dummy4$Sale_Date, label=TRUE, abbr=FALSE)
    
    sale_day = ddply(dummy4,c("Match_day"),
                     function(df1)sum(df1$Quantity))
    names(sale_day)[2] = "Quantity"
    
    sales_daywise = ggplot(sale_day , aes(x = Match_day,y = Quantity,fill = Match_day, label = Quantity))+
      geom_bar(stat = 'identity',position = 'dodge')
    
    
    ggplotly(sales_daywise, tooltip = c("text"))
    
  })
  
}


shinyApp(ui, server)
