#Application 07

library(shiny)

shinyServer(function(input, output, session) 
{
  
  output$filetable <- renderTable(
  {
    if(is.null(input$files)) 
    { 
      return() 
    }  
    else 
    { 
      #Variable in which data is going to be stored from csv file
      globalData <<- read.csv(input$files$datapath, header=T, stringsAsFactors =T)
    } 
  })

  #------------------------------------------------------------------
  #                         Q_01
  #------------------------------------------------------------------

  x <- c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)
  output$q0<-renderPrint(
  {
    t.test(x, alternative = "greater", mu = 0.3)
  })
  
  #------------------------------------------------------------------
  #                         Q_02
  #------------------------------------------------------------------
  
  Control <- c(91, 87, 99, 77, 88, 91)
  Treat <- c(101, 110, 103, 93, 99, 104)
  output$q1<-renderPrint(
  {
    t.test(Control, Treat, alternative = "less", var.equal = TRUE)
  })
    
  #------------------------------------------------------------------
  #                         Q_03
  #------------------------------------------------------------------
  
  observeEvent(input$go,{
    
    myFile <- globalData
    
    x <- myFile$IQbio
    y <- myFile$IQfoster
    output$q2<-renderPrint({
      t.test(x,y,alternative = "two.sided")
    })
    
  })

  #------------------------------------------------------------------
  #                       Q_04
  #------------------------------------------------------------------
  
  reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
  prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
  output$q3<-renderPrint(
  {
    t.test(prem, reg, alternative = "greater", paired = TRUE)
  })

  #------------------------------------------------------------------
  #                       Q_05
  #------------------------------------------------------------------
  
  #------------------------------------------------------------------
  #                       Q_06
  #------------------------------------------------------------------
  
  stim = c(12, 7, 3, 11, 8, 5, 14, 7, 9, 10)
  NoStim = c(8, 7, 4, 14, 6, 7, 12, 5, 5, 8)
  output$q4<-renderPrint(
  {
    t.test(stim, NoStim)
  })
  
  #------------------------------------------------------------------
  #                       Q_07
  #------------------------------------------------------------------
  
  REM_DEPRIVED = c(26, 15, 8, 44, 26, 13, 38, 24, 17, 29)
  Contcon = c(20, 4, 9, 36, 20, 3, 25, 10, 6, 14)
  output$q5<-renderPrint(
  {
    t.test(REM_DEPRIVED, Contcon, alternative = "greater")
  })

  #------------------------------------------------------------------
  #                       Q_08
  #------------------------------------------------------------------
  
  control_over_plant = c(23, 12, 6, 15, 18, 5, 21, 18, 34, 10, 23, 14, 1, 23, 8)
  no_control_over_plant = c(35, 21, 26, 24, 17, 23, 37, 22, 16, 38, 23, 41, 27, 24, 32)
  output$q6<-renderPrint({
    t.test(no_control_over_plant,control_over_plant, alternative = "greater")
  })

  #------------------------------------------------------------------
  #                       Q_09
  #------------------------------------------------------------------
  
  data(mtcars)
  output$q7<-renderPrint(
  {
    head(mtcars)
  })
  vara4 = mtcars$mpg[mtcars$cyl == 4]
  vara6 = mtcars$mpg[mtcars$cyl == 6]
  output$q8<-renderPrint(
  {
    t.test(vara4,vara6,alternative = "t")
  })

})