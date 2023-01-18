library(shiny)
library(shinydashboard)
library(data.table)
library(caTools)
library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
ui = fluidPage(
  #pageheader
  
  headerPanel("Trend Line for Sales"),
  sidebarPanel(
    selectInput("section","section",
                c("Club Hall of Fame","Club Main","Club Mezzanine",
                  "Floor FE","Floor FN","Floor FS","Floor FW","FN5",
                  "Founders Club","Hall of Fame","Hall of Fame Suite",
                  "Loge","Main","Mezzanine","Owners Club Suite",
                  "Star Suite","Upper Reserved")),
    dateInput("From_Date","From_Date",startview = "year",min = "2021-03-23",
                   max = "2021-05-09"),
    dateInput("To_Date","To_Date",startview = "year",min = "2021-03-23",
              max = "2021-05-09"),
    actionButton("submitbutton","Show Trend",
                 class = "btn btn-primary")
  ),
  mainPanel(
    sidebarPanel(width = 25, headerPanel("Sales Trend:"),
                 plotOutput("trend"))
  )
)

server = function(input,output){
  datasetInput = reactive({
    
    df = data.frame(
      Name = c(
        "section",
        "From_Date",
        "To_Date"),
      Value = c(as.character(input$section),
                as.character(input$From_Date),
                as.character(input$To_Date)),
      stringsAsFactors = FALSE)
    
    
    input = transpose(df)
    input = input[-c(1),]
    
    
    names(input)[1] = "section"
    names(input)[2] = "From_Date"
    names(input)[3] = "To_Date"
    #End of input data
    
    #Importing Book2_excel 
    my_data = read_excel("C:/Users/vella/Downloads/Office/datasets/Book2_excel.xlsx")
    
    #Extracting required data from my_data
    #Column names - Section, Quantity and Transaction Date
    my_data2 = cbind.data.frame(my_data$Section,my_data$Quantity,my_data$Transaction_Date)
    
    #Formatting Transaction Date from POSIXct to Date format
    my_data2$`my_data$Transaction_Date` = as.Date(my_data2$`my_data$Transaction_Date`)
    
    #Changing Column names
    names(my_data2)[1] = "section"
    names(my_data2)[2] = "Sales"
    names(my_data2)[3] = "Date"
    
    #Sorting by Date
    x= my_data2[order(as.Date(my_data2$Date, format="%m/%d/%Y")),]
    
    #Total Sales per day
    transactionData1 <- ddply(x,c("section","Date"),
                              function(df1)sum(df1$Sales), .drop = FALSE)
    
    #Renaming 3rd column name
    names(transactionData1)[3] = "Sales"
    
    #Ommiting NaN values
    dummy = transactionData1
    
    #Selecting the Section
    out <- dummy %>% dplyr::filter(section %in% c(input$section))

    #Filtering the Date
    out2  = out[out$Date >= input$From_Date & out$Date <= input$To_Date,]
    
    #Ploting a time series
    ggplot(out2,aes(x = Date, y= Sales, color  = section)) + geom_line() + geom_smooth(method = lm)
    
  })
  output$trend <- renderPlot({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
    
  })
}

shinyApp(ui,server)
