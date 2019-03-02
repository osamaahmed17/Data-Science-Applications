#Application 09

library(shiny)
shinyServer(function(input,output)
{
  observeEvent(input$b2,
  {
    output$vara<-renderPlot({
       library(kernlab)
       library(caret)
       library(quantmod)
       getSymbols("DE", src = "yahoo")
       DE100 <- data.frame(as.xts(DE))
       data = createDataPartition(DE100$DE.Volume,p=0.75, list = FALSE)
       training=DE100[data,]
       testing=DE100[-data,]
       Preproc = preProcess(log10(training[,-5]+1),method="pca", pcaComp = 2)
       DEC = predict(Preproc, log10(training[,-5]+1))
     
       plot(DEC[,1],DEC[,2])
    })
    
  })
  observeEvent(input$b1,
  {
    output$table <- renderTable(
    {
      Data = reactive(
        {
          library(kernlab)
          library(caret)
          library(caTools)
          library(quantmod)
          getSymbols("DE", src = "yahoo")
          df <- data.frame(as.xts(DE))
          data<-df[1:50,]
          return(data)
          
        })
      Data()
    })
  })
})
