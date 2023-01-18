library(shiny)
library(shinydashboard)
library(data.table)
library(caTools)

ui = fluidPage(
  #pageheader
  
  headerPanel("Ticket price predictor"),
  sidebarPanel(
    numericInput("Section_id" , "Section_id" ,""),
    numericInput("Delivery_Option_id","Delivery_Option_id ",""),
    numericInput("Quantity","Quantity",""),
    textInput("Transaction_Date","Transaction_Date",""),
    actionButton("submitbutton","Predict",
                 class = "btn btn-primary")
  ),
  mainPanel(
    sidebarPanel(width = 25, headerPanel("Price of the Ticket is:"),
                 textOutput("value"))
  )
)

server = function(input,output){
  datasetInput = reactive({
    
    df = data.frame(
      Name = c(
        "Section_id",
        "Delivery_Option_id",
        "Quantity",
        "Transaction_Date"),
      
      Value = c(as.numeric(input$Section_id),
                as.numeric(input$Delivery_Option_id),
                as.numeric(input$Quantity),
                as.character(input$Transaction_Date)),
      stringsAsFactors = FALSE)
    
    input = transpose(df)
    input = input[-c(1),]
    
    #Model part    
    data = readxl::read_xlsx("C:/Users/vella/Downloads/Office/datasets/Book2_excel.xlsx")
    
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
    View(Main_data_set)
    set.seed(3033)
    
    split = sample.split(Main_data_set, SplitRatio = 0.8)
    training_set = subset(Main_data_set, split == TRUE)
    testing_set = subset(Main_data_set, split == FALSE)
    
    lm_a = lm(Display_Price_Per_Ticket_Amount ~.,data = Main_data_set)
    train_a = predict(lm_a,training_set)
    
    test_a = predict(lm_a,testing_set)
    
    dummy = data.frame(Section_id = as.numeric(input$V1),
                       Delivery_Option_id = as.numeric(input$V2),
                       Quantity = as.numeric(input$V3),
                       Transaction_Date = as.Date(input$V4,"%d-%m-%Y"))

    output = predict(lm_a,dummy)
    
    print(output)
  })
  output$value <- renderPrint({
    if (input$submitbutton>0) {
      isolate(datasetInput())
    }
  })
}

shinyApp(ui,server)
