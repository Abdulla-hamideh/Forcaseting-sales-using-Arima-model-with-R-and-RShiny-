library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
install.packages("forecast")
install.packages("ggbio")
install.packages("tsbox")
library(tsbox)
install.packages('prophet')
install.packages("hts")
library(zoo)

# reading the file 
read_csv("sales data-set.csv")

Features_data_set <- read_csv("Features data set.csv")
View(Features_data_set)

sales_data_set <- read_csv("sales data-set.csv")
View(sales_data_set)

stores_data_set <- read_csv("stores data-set.csv")
View(stores_data_set)

stores_data_set %>% left_join(sales_data_set) %>%
  mutate(overallstores = 1) %>% select(overallstores,Store,Type, Dept, Date, Weekly_Sales, ) -> Abdulla

Abdulla %>% mutate(Date = as.Date(Abdulla$Date,"%d/%m/%Y" )) -> salesdata_cleaned
View(salesdata_cleaned)


# ui
ui<- shinyUI(
  dashboardPage( 
    dashboardHeader(title= "Retail Predictive Model", titleWidth = 250),
    dashboardSidebar(
      menuItem("Sales Dashboard")
    ),
    dashboardBody(
      fluidRow(
        tabBox( title = "Levels by", height = "250px",
          tabPanel( title = "Type", 
                    selectInput("Type", "Choose a Type", choices = c("Enter a Type","A","B","C")),        
                    sliderInput("Horizen", "Number of Weeks to Forecast", 1,100,1)),
          tabPanel( title = "Store", 
                    selectInput("store", "Choose a Store", choices = c("Enter a Store Number",seq(1,42,1))),
                    sliderInput("Horizen2", "Number of Weeks to Forecast", 1,100,1)),
          tabPanel(title = "Overall",
                    sliderInput("Horizen1", "Number of Weeks to Forecast", 1,100,1)
                   ),
          actionButton("go", "Predict")
        ),
        
      ),
      fluidRow(
        box(title = "Selection plot",status = "primary",solidHeader = TRUE,plotOutput("arima", height =  350), height = 400), 
        box(title = "Overall plot", status = "primary",solidHeader = TRUE,plotOutput("overall", height = 350), height = 400)
        
      ),
      fluidRow(
        box(title = "Selection Forcasted Points",status = "danger", solidHeader = TRUE, tableOutput("auto.arima3")),
        box(title = "Overall Forcasted Points",status = "danger", solidHeader = TRUE, tableOutput("auto.arima4"))
      )
    )
  )
)
#server part 
server <- shinyServer(function(input,output){
  output$arima <- renderPlot({
    hl = input$Horizen
    i <- input$Type
    kk <- input$store
    print(input$store)
    if(input$Type == "A"){
      salesdata_cleaned %>% filter(Type == as.character(i)) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = hl) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = hl ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=hl) %>% autoplot() }else{
        data %>% ets() %>% forecast(h=hl) %>% autoplot()}
    } else if(input$Type == "B"){
      salesdata_cleaned %>% filter(Type == as.character(i)) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = hl) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = hl ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=hl) %>% autoplot() }else{
        data %>% ets() %>% forecast(h=hl) %>% autoplot()}
    } else if(input$Type == "C"){
      salesdata_cleaned %>% filter(Type == as.character(i)) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = hl) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = hl ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=hl) %>% autoplot() }else{
        data %>% ets() %>% forecast(h=hl) %>% autoplot()}
    }else{
      salesdata_cleaned %>% filter(Store == kk) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = hl) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = input$Horizen2 ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=input$Horizen2) %>% autoplot() }else{
        data %>% ets() %>% forecast(h=input$Horizen2) %>% autoplot()}
    }
    })
  output$overall <- renderPlot({
    hl = input$Horizen1
    salesdata_cleaned %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales)) -> data_overallstore
    Salesdates3 <- as.Date(data_overallstore$Date, "%Y/%m/%d")
    aoverallstore <- zoo(data_overallstore[,2, drop=FALSE], order.by = Salesdates3)
    Salesdata_ts_store <- ts_zoo(aoverallstore)
    data_overallstores<- ts(Salesdata_ts_store, frequency=52, start = 2010)
    
    train_overallstore <- head(data_overallstores, round(length(data_overallstores) * 0.8))
    h_store <- length(data_overallstores) - length(train_overallstore)
    test_overallstore <- tail(data_overallstores, h_store)
    
    zarima_overallstore <- auto.arima(train_overallstore)
    checkresiduals(zarima_overallstore)
    
    zets_overallstore <- ets(train_overallstore)
    checkresiduals(zets_overallstore)
    
    a1 <- zarima_overallstore %>% forecast(h = hl) %>%
      accuracy(qcement)
    a1[,c("RMSE","MAE","MAPE","MASE")]
    
    a2 <- zets_overallstore %>% forecast(h = hl ) %>%
      accuracy(qcement)
    a2[,c("RMSE","MAE","MAPE","MASE")]
    
    if(a1[2,2] < a2[2,2]){data_overallstores %>%  auto.arima() %>% forecast(h=hl) %>% autoplot() }else{
      data_overallstores %>% ets() %>% forecast(h=hl) %>% autoplot()}
  })
  output$auto.arima4 <- renderTable({
    hl = input$Horizen1
    salesdata_cleaned %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales)) -> data_overallstore
    Salesdates3 <- as.Date(data_overallstore$Date, "%Y/%m/%d")
    aoverallstore <- zoo(data_overallstore[,2, drop=FALSE], order.by = Salesdates3)
    Salesdata_ts_store <- ts_zoo(aoverallstore)
    data_overallstores<- ts(Salesdata_ts_store, frequency=52, start = 2010)
    
    train_overallstore <- head(data_overallstores, round(length(data_overallstores) * 0.8))
    h_store <- length(data_overallstores) - length(train_overallstore)
    test_overallstore <- tail(data_overallstores, h_store)
    
    zarima_overallstore <- auto.arima(train_overallstore)
    checkresiduals(zarima_overallstore)
    
    zets_overallstore <- ets(train_overallstore)
    checkresiduals(zets_overallstore)
    
    a1 <- zarima_overallstore %>% forecast(h = hl) %>%
      accuracy(qcement)
    a1[,c("RMSE","MAE","MAPE","MASE")]
    
    a2 <- zets_overallstore %>% forecast(h = hl ) %>%
      accuracy(qcement)
    a2[,c("RMSE","MAE","MAPE","MASE")]
    
    if(a1[2,2] < a2[2,2]){data_overallstores %>%  auto.arima() %>% forecast(h=hl)}
  })
  output$auto.arima3 <- renderTable({
    hl = input$Horizen
    i <- input$Type
    kk <- input$store
    if(input$Type == "A"){
      salesdata_cleaned %>% filter(Type == as.character(i)) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = hl) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = hl ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=hl)}
    } else if(input$Type == "B"){
      salesdata_cleaned %>% filter(Type == as.character(i)) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = hl) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = hl ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=hl)}
    } else if(input$Type == "C"){
      salesdata_cleaned %>% filter(Type == as.character(i)) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = hl) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = hl ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=hl)}
    }else{
      salesdata_cleaned %>% filter(Store == kk) %>% group_by(Date) %>% summarise(WS = sum(Weekly_Sales))-> data1
      Salesdates <- as.Date(data1$Date, "%Y/%m/%d")
      atype <- zoo(data1[,2, drop=FALSE], order.by = Salesdates)
      Salesdata_ts <- ts_zoo(atype)
      data<- ts(Salesdata_ts, frequency=52, start = 2010)
      train <- head(data, round(length(data) * 0.8))
      h <- length(data) - length(train)
      test <- tail(data, h)
      zarima <- auto.arima(train)
      checkresiduals(zarima)
      zets <- ets(train)
      checkresiduals(zets)
      a1 <- zarima %>% forecast(h = input$Horizen2) %>%
        accuracy(test)
      a1[,c("RMSE","MAE","MAPE","MASE")]
      
      a2 <- zets %>% forecast(h = input$Horizen2 ) %>%
        accuracy(test)
      a2[,c("RMSE","MAE","MAPE","MASE")]
      if(a1[2,2] < a2[2,2]){data %>% auto.arima() %>% forecast(h=input$Horizen2)}
    }
  })
 
})
shinyApp(ui, server)
