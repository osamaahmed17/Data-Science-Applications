#Application 06

library(shiny)

shinyServer(function(input, output) {
  
  output$datatable <- renderTable(
  {
    if(is.null(input$file) ) 
    {
      return() 
    } 
    else 
    { 
      #globally defining "globalData" variable which have data from csv file
      globalData <- read.csv(input$file$datapath, header=T,stringsAsFactors =T)
    } 
  })
  
  observeEvent(input$pbinom, {
    output$result <- renderText({
      pbinom(input$val1,input$val2,input$val3,input$LowerTail)
    })
  })
  
  observeEvent(input$qbinom, {
    output$result <- renderText({
      qbinom(input$val1,input$val2,input$val3)
    })
  })
  
  observeEvent(input$dbinom, {
    output$result <- renderText({
      dbinom(input$val1,input$val2, input$val3)
    })
  })
  
  observeEvent(input$pnorm, {
    output$result <- renderText({
      pnorm(input$val1,input$val2, input$val3)
    })
  })
  
  observeEvent(input$qnorm, {
    output$result <- renderText({
      qnorm(input$val1,input$val2, input$val3)
    })
  })
  
  observeEvent(input$dnorm, {
    output$result <- renderText({
      dnorm(input$val1,input$val2, input$val3)
    })
  })
  
  observeEvent(input$ppois, {
    output$result <- renderText({
      ppois(input$val1,input$val2)
    })
  })
  
  observeEvent(input$qpois, {
    output$result <- renderText({
      qpois(input$val1,input$val2)
    })
  })
  
  observeEvent(input$dpois, {
    output$result <- renderText({
      dpois(input$val1,input$val2)
    })
  })
})