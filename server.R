library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(skimr)
library(zoo)
library(ggfortify)
library(dplyr)
library(ggplot2)
library(skimr)
library(lubridate)

AAPL <- read.csv("AAPL.csv")
app <- read.csv("AAPL.csv")
sam <- read.csv("Samsung.csv")

Dates <- ymd(AAPL$Date)
typeof(Dates)
class(Dates)
AAPL$Date <- Dates

Dates2 <- ymd(sam$Date)
typeof(Dates2)
class(Dates2)
sam$Date <- Dates2

dates <- ymd(app$Date)
typeof(dates)
class(dates)
app$Date <- dates

m <- mean(app$Volume)
sd <- sd(app$Volume)

model <- lm(app$Close ~ app$Open)

app_new <- app[app$Date > "2021-03-24" &    # Extract data frame subset
                 app$Date < "2022-03-24", ]

app_new2 <- app[app$Date > "2022-02-24" &    # Extract data frame subset
                  app$Date < "2022-03-24", ]

shinyServer(function(input,output,session){
  output$mydatatable <- renderDataTable({
    AAPL
  })
  output$plot1 <- renderPlotly({
    ggplot(app, aes(x = Date, y = Close ,col ='red')) +
      geom_line() + 
      scale_x_date(date_labels = "%Y-%m")
  })
  output$plot2 <- renderPlotly({
    ggplot(app_new, aes(x = Date, y = Close)) +            # Draw ggplot2 plot
      geom_line() + 
      scale_x_date(date_labels = "%Y-%m")
  })
  output$plot3 <- renderPlotly({
    ggplot(app_new2, aes(x = Date, y = Close)) +            # Draw ggplot2 plot
      geom_line() + 
      scale_x_date(date_labels = "%m-%d")
  })
  
  output$plot4 <- renderPlotly({
    plot_ly(data= AAPL,y = ~Close, type = "box")
  })
  
  output$plot5 <- renderPlotly({
    ggplot(sam, aes(x = Date, y = Close)) +            # Draw ggplot2 plot
      geom_line() + 
      scale_x_date(date_labels = "%Y-%m")
  })
  
  output$plot6 <- renderPlotly({
    plot_ly(data= sam,y = ~Close, type = "box")
  })
  
  output$plot7 <- renderPlotly({
    plot_ly(data = app,x=~Volume,type = "histogram",color = "red")
  })
  
  
  output$plot9 <- renderPlotly({
    plot_ly(data = sam,x=~Volume,type = "histogram",color = "red")
  })
  
  
  
  
  
  
  
  #summary of APPLE
  
  
  output$mS <- renderPrint({
    app %>% 
      summary()
    
  })
  
  output$mDS <- renderPrint({
    sam %>% 
      summary(sam)
  })
  
  
  
  
  
  
  #regression model
  
  
  output$RS <- renderPrint({
    model %>% 
      summary()
  })
  
  output$PD <- renderPlotly({
    ggplot(app, aes(x = Volume, y = pnorm(Volume,mean =m,sd = sd))) +            # Draw ggplot2 plot
      geom_point() 
  })
  
  
  
  output$P <- renderValueBox({
    valueBox(
      value = 2.8529 +  0.9997*174.070007 ,subtitle = "Expected Closing of AAPL depending on Opening value"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
})
