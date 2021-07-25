#load all the necessary libraries and the splitData.r file

#source("splitData.R")
library(shiny)
library(data.table)
library(shinythemes)
library(caret)
library('RANN')
library(shinyWidgets)
library(dplyr)
library(tidyverse)
library(e1071)

parametergrid <- function(model_type){
  as.character(modelLookup(model_type)$parameter)->p
  return (p)
}

# Define UI for data upload app ----

ui <- fluidPage(theme = shinytheme("cerulean"),
                
  
  tabsetPanel(
    tabPanel("Upload Data",
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----

    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file", "Choose primary data CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      fileInput("wfile", "Choose secondary data CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           ".csv")),
      tags$hr(),
      checkboxInput("header","First row as colnames",value=TRUE)
    )
    ,
    
    # Main panel for displaying outputs ----
    mainPanel( h4("Datasets"),
      dataTableOutput("table1"),
      dataTableOutput("table2")
      
    )
    
    )
    ),#end of tab 1
  tabPanel("Summary of data",
          
               verbatimTextOutput("fsum"),
               verbatimTextOutput("wsum")
             
           ), #end of tab 2
    tabPanel("Split Data",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "type",label = "Type of prediction", choices= c("classification","regression")),
                 
                 checkboxGroupInput("parameters","Choose parameters for training data",choices =NULL),
                 tags$hr(),
                 selectInput(inputId = "label",label = "Choose label",choices = NULL),
                 tags$hr(),
                 selectInput(inputId = "fk",label = "Choose key for primary data",choices = NULL),
                 tags$hr(),
                 selectInput(inputId = "wk",label = "Choose key for secondary data",choices = NULL),
                 tags$hr(),
                 actionButton(inputId = "split",label =  "Split data into training and testing"),
                 textOutput(outputId = 't1'),
                 tags$hr()
               ),
               mainPanel( h4("Training Data"),
                 dataTableOutput("training")
               )
             )
    ), #end of tab 3
  tabPanel("Train Model",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "model",label = "Choose Caret model",choices = c("gbm", "xgbDART","xgbLinear","xgbTree"))
               ),
             mainPanel( h4("Parameter Grid Values"),
               uiOutput("parametergrid"),    
               actionButton("grid","Create Grid"),
               tableOutput("t2"),
               actionButton("train","Train model"),
               verbatimTextOutput("t3")
              
             )
           )
           
           ),#end of tab 4
  tabPanel("User Test Data",
           sidebarLayout(
             sidebarPanel(
               actionButton("sampledata","Click to enter sample data")
             ),
             mainPanel(
               uiOutput("userdata"),
               actionButton("testsample","Predict"),
               tableOutput("table3"),
               verbatimTextOutput("result")
               
             )
             )
           ),#end of tab 5
  tabPanel("Test Model",
           actionButton("testmodel","Run test data on the model"),
           plotOutput("plot"),
           verbatimTextOutput("error")
  )#end of tab 6
  )#end of tabset
)

server <- function(input, output,session) {
  
  options(shiny.maxRequestSize=(30*1024^2)*10)
  #GET FLIGHT DATASET FOR AIRPORT INFORMATION
  data <- reactive({
    print('In reactive')
    file1<-input$file
    if(is.null(file1)) return()
    print('Reading file')
    dataSet<- read_csv(file1$datapath)
    gsub(colnames(dataSet),pattern = "[\\(\\)\\% ]",replacement = "_")-> colnames(dataSet)
    cols<-colnames(dataSet)
    
    setDT(dataSet)
    updateCheckboxGroupInput(session, "parameters", label = "Choose parameters for training data", choices = cols)
    updateSelectInput(session,"label",label="Choose label",choices=cols)
    dataSet
  })
  
  #GET WEATHER DATASET
  wdata <- reactive({
    file1<-input$file
    file2<-input$wfile
    if(is.null(file2)) return()
  
    dataSet<- read_csv(file2$datapath)
    gsub(colnames(dataSet),pattern = "[\\(\\)\\% ]",replacement = "_")-> colnames(dataSet)
    
    cols<- c(colnames(data()),colnames(dataSet))
    setDT(dataSet)
    updateCheckboxGroupInput(session, "parameters", label = "Choose parameters for training data", choices = cols)
    updateSelectInput(session,"label",label="Choose label",choices=cols)
    updateSelectInput(session,"fk",label="Choose key for primary data",choices=colnames(data()))
    updateSelectInput(session,"wk",label="Choose key for secondary data",choices=colnames(dataSet))
    dataSet
  })
  output$table1<- renderDataTable(
    head(data())
  )
  output$table2<- renderDataTable(
    head(wdata())
  )
  output$fsum<-renderPrint(summary(data()))
  output$wsum<-renderPrint(summary(wdata()))
  
  #SPLIT DATA{}
  observeEvent(input$split, {
    
    output$training<- renderDataTable({

      if(!is.null(input$wfile)){
      fdataset<-data()
      wdataset<-wdata()
      parameters<-input$parameters
      l<-input$label
      fk<-input$fk
      wk<-input$wk
      fdataset[,d:=fdataset[[fk]]]
      wdataset[,d:=wdataset[[wk]]]
      fdataset$d<-as.POSIXct(fdataset$d, format="%Y-%m-%d %H:%M:%S",tz="UTC",origin="1970-01-01")
      wdataset$d<-as.POSIXct(wdataset$d, format="%Y-%m-%d %H:%M:%S",tz="UTC",origin="1970-01-01")
      wdataset<-wdataset[!is.na(d)]
      setkey(fdataset,d)
      setkey(wdataset,d)
      dataset<<-wdataset[fdataset[order(d)],roll="nearest"]
      label<-dataset[[l]]
      dataset<<-subset(dataset,select=parameters)
      gsub(colnames(dataset),pattern = "[\\(\\)\\% ]",replacement = "_")-> colnames(dataset)
      trid<-sample.int(nrow(dataset),round(0.9*nrow(dataset)))
      df_training<<-dataset[trid,]
      df_test<<-dataset[-trid,]
      label_tr<<-label[trid]
      label_test<<-label[-trid]
      return (head(df_training))
      }
      dataset<<-data()
      parameters<-input$parameters
      l<-input$label
      label<-dataset[[l]]
      if(input$type == "classification")
        label<-as.factor(label)
      dataset<-subset(dataset,select=parameters)
      gsub(colnames(dataset),pattern = "[\\(\\)\\% ]",replacement = "_")-> colnames(dataset)
      trid<-sample.int(nrow(dataset),round(0.9*nrow(dataset)))
      df_training<<-dataset[trid,]
      df_test<<-dataset[-trid,]
      label_tr<<-label[trid]
      label_test<<-label[-trid]
      return (head(df_training))
    })
   
    })
    #CHOOSE MODEL
   observeEvent(input$model,{
     params <- parametergrid(input$model)
     len <- length(params)
     
       output$parametergrid <- renderUI({
           textbox <- function(i){textInput(params[i],params[i])}
            lapply(1:len, textbox)
            
       })
   })
   
   #PARAMETER GRID
   observeEvent(input$grid,{
     params <- parametergrid(input$model)
     len <- length(params)
     eval({
     p<-list()
     for(i in 1:len){
        p[[params[i]]]<-  eval(parse(text=input[[params[i]]]))
       }
     output$t2<- renderTable(
       expand.grid(p)
     )  
     })
    })
   
   #TRAIN MODEL
   observeEvent(input$train,{
     params <- parametergrid(input$model)
     len <- length(params)
     p<-list()
     for(i in 1:len){
       p[[params[i]]]<-  eval(parse(text=input[[params[i]]]))
     }
     grid<-expand.grid(p)
     
       ctrl <- trainControl(method="cv", number = 5,verboseIter = TRUE)
      
       df_training<-data.matrix(df_training)
       model<<-train(df_training,label_tr,method=input$model, metric="MAE", tuneGrid=grid, trControl=ctrl )
       output$t3<-renderPrint(model)
   })
           
   #SAMPLE DATA
    observeEvent(input$sampledata,{
      params<-input$parameters
      len <- length(params)
      output$userdata <- renderUI({
        rowdat <- df_test[sample.int(nrow(df_test),1)]
        print(params)
        print(rowdat)
        textbox <- function(i){numericInput(params[i],params[i],value=rowdat[[params[i]]])}
        lapply(1:len, textbox)
       })
    })
    observeEvent(input$testsample,{
      params<-input$parameters
      len <- length(params)
      
        p<-list()
        for(i in 1:len){
          p[[params[i]]]<-  input[[params[i]]]
        }
        output$table3<- renderTable(
          expand.grid(p)
        )
        dtest<<-expand.grid(p)
        dtest<-data.matrix(dtest)
        pred<-predict.train(model,dtest,type="raw")
        output$result<-renderPrint(pred)
        
    })
    #TEST MODEL
    observeEvent(input$testmodel,{
      df_test<-data.matrix(df_test)
      pred<-predict.train(model,df_test,type="raw")
      label_test - pred ->error
      if(input$type == "classification"){
        sum(label_test!=pred) -> error
        print(error)}
      output$plot<- renderPlot(
      plot(varImp(object=model),main="Variable Importance")
      )
      output$error<-renderPrint(quantile(abs(error),seq(0,1,.1)) %>% round())
    })

  
}

shinyApp(ui,server)