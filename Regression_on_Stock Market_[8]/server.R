#Application 08

library(shiny)
library(kernlab)
library(caret)
library(caTools)
library(quantmod)


shinyServer(function(input,output)
{
  
  output$vara<-renderPlot(
  {
     library(kernlab)
     library(caret)
     library(quantmod)
     getSymbols("DE", src = "yahoo")
     barChart(DE,theme='white.mono',bar.type='hlc')
     
  })

  output$vara1<-renderPlot(
  {
     library(kernlab)
     library(caret)
     library(quantmod)
     getSymbols("DE", src = "yahoo")
     chartSeries(DE,subset='last 3 months')
     addBBands(n=20,sd=2,ma="SMA",draw='bands',on=-1)
    
  })
  
  output$vara2<-renderPlot(
  {
     library(kernlab)
     library(caret)
     library(quantmod)
     getSymbols("DE", src = "yahoo")
     KSE100 <- data.frame(as.xts(DE))
     plot(density(KSE100$DE.Volume))      
  })
  
  output$vara3<-renderPlot(
  {
     library(kernlab)
     library(caret)
     library(quantmod)
     getSymbols("DE", src = "yahoo")
     KSE100 <- data.frame(as.xts(DE))
     plot(KSE100)    
  })
  
  output$vara4<-renderPlot(
  {
     library(kernlab)
     library(caret)
     library(caTools)
     library(quantmod)
     getSymbols("DE", src = "yahoo")
     KSE100 <- data.frame(as.xts(DE))
     rand=KSE100[sample(nrow(KSE100)),]
     sample =  sample.split(KSE100$DE.Open, SplitRatio = .75)
     tr = subset(rand, sample =  TRUE) 
     ts = subset(rand, sample =  FALSE)
     model2 = lm(rand$DE.Volume~rand$DE.Open,data=tr)
     pred = predict(model2,ts$open) 
     plot(pred,ts$DE.Volume, xlab="Observed Values", ylab="Predicted Values" )
     abline(a=0, b=1)
     
  })
  
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

  output$table <- renderTable(
  {
    Data()
  })

})
