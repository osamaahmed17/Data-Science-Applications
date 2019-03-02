#Applicaion 11

library(shiny)
library(randomForest)
library(e1071)

shinyServer(function(input,output)
{
output$datatable <- renderTable(
{
    if(is.null(input$file) ) 
    {
        return() 
    } 
    else 
     { 
      #globally defining "globalData" variable which have data from csv file
      globalData <<- read.csv(input$file$datapath, header=T,stringsAsFactors =T) 

     } 
})
output$vara<-renderPlot({
    
    if(is.null(input$file)) {"Data not avaliable"}  else 
    {
       library(randomForest)
       library(caret)
       mydata <- data.frame(globalData)
       names(mydata) = c("vendor_name","Model_Name","MYCT","MMIN","MMAX","CACH","CHMIN","CHMAX","PRP","ERP")
       mydata = mydata[,-c(1,2,9)]
       mydata$catgorical = as.factor(with(mydata, ifelse(mydata$ERP>mean(mydata$ERP),"high","low")))
       inTrain = createDataPartition(y=mydata$ERP, p = 0.7, list = FALSE)
       training = mydata[inTrain,]
       testing = mydata[-inTrain,]
       classification <- randomForest(catgorical~. - ERP,data =        training,ntree=500,importance=T)
       regression <- randomForest(ERP~ . - catgorical,data =        training,ntree=500,importance=T)
       par(mfrow=c(1,2))
       varImpPlot(classification,sort = T,main="Variable Importance",n.var=5)
       plot(classification)

    }
})
output$vara1<-renderPlot({
    
   library(randomForest)
   library(caret)
   globalData<<-read.csv(input$file$datapath, header=T,stringsAsFactors =T)
   mydata=data.frame(globalData)
   names(mydata) = c("vendor_name","Model_Name","MYCT","MMIN","MMAX","CACH","CHMIN","CHMAX","PRP","ERP")
   mydata = mydata[,-c(1,2,9)]
   mydata$catgorical = as.factor(with(mydata, ifelse(mydata$ERP>mean  (mydata$ERP),"high","low")))
   inTrain = createDataPartition(y=mydata$ERP, p = 0.7, list = FALSE)
   training = mydata[inTrain,]
   testing = mydata[-inTrain,]
   classification <- randomForest(catgorical~. - ERP,data =    training,ntree=500,importance=T)
   regression <- randomForest(ERP~ . - catgorical,data =    training,ntree=500,importance=T)
   predclass = predict(classification ,testing)
   predregress = predict(regression ,testing)
   confusionMatrix(data = predclass,reference =testing$catgorical)
   table(predclass, testing$ERP)
   plot(testing$catgorical, predclass)
   plot(testing$ERP, predregress)

})
output$vara2<-renderPlot({
  
  if(is.null(input$file)) {"Data not avaliable"}  else 
  {
    library(randomForest)
    library(caret)
    mydata <- data.frame(globalData)
    names(mydata) = c("vendor_name","Model_Name","MYCT","MMIN","MMAX","CACH","CHMIN","CHMAX","PRP","ERP")
    mydata = mydata[,-c(1,2,9)]
    mydata$catgorical = as.factor(with(mydata, ifelse(mydata$ERP>mean(mydata$ERP),"high","low")))
    inTrain = createDataPartition(y=mydata$ERP, p = 0.7, list = FALSE)
    training = mydata[inTrain,]
    testing = mydata[-inTrain,]
    classification <- randomForest(catgorical~. - ERP,data =        training,ntree=500,importance=T)
    regression <- randomForest(ERP~ . - catgorical,data =        training,ntree=500,importance=T)
    par(mfrow=c(1,2))
    varImpPlot(classification,sort = T,main="Variable Importance",n.var=5)
    plot(regression)
    
    
  }
})
output$vara3<-renderPlot({
  
  if(is.null(input$file)) { "Data not avaliable" }  else 
  {
    library(randomForest)
    library(caret)
    mydata <- data.frame(globalData)
    names(mydata) = c("vendor_name","Model_Name","MYCT","MMIN","MMAX","CACH","CHMIN","CHMAX","PRP","ERP")
    mydata = mydata[,-c(1,2,9)]
    mydata$catgorical = as.factor(with(mydata, ifelse(mydata$ERP>mean(mydata$ERP),"high","low")))
    inTrain = createDataPartition(y=mydata$ERP, p = 0.7, list = FALSE)
    training = mydata[inTrain,]
    testing = mydata[-inTrain,]
    classification <- randomForest(catgorical~. - ERP,data =        training,ntree=500,importance=T)
    regression <- randomForest(ERP~ . - catgorical,data =        training,ntree=500,importance=T)
    par(mfrow=c(1,2))
    varImpPlot(classification,sort = T,main="Variable Importance",n.var=5)
    
    
  }
})

output$vara4<-renderPlot({
  
  if(is.null(input$file)) { "Data not avaliable" }  else 
  {
    library(randomForest)
    library(caret)
    mydata <- data.frame(globalData)
    names(mydata) = c("vendor_name","Model_Name","MYCT","MMIN","MMAX","CACH","CHMIN","CHMAX","PRP","ERP")
    mydata = mydata[,-c(1,2,9)]
    mydata$catgorical = as.factor(with(mydata, ifelse(mydata$ERP>mean(mydata$ERP),"high","low")))
    inTrain = createDataPartition(y=mydata$ERP, p = 0.7, list = FALSE)
    training = mydata[inTrain,]
    testing = mydata[-inTrain,]
    classification <- randomForest(catgorical~. - ERP,data = training,ntree=500,importance=T)
    regression <- randomForest(ERP~ . - catgorical,data = training,ntree=500,importance=T)
    par(mfrow=c(1,2))
    plot(testing$catgorical)
  }
})
})
