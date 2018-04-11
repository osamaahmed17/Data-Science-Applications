#Application 11

library(shiny)

shinyUI(fluidPage(
  titlePanel("Application Number 11"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose File From the Application 11 Folder", multiple=TRUE)
   #selectInput("something","select cluster",list('1','2','3'),selected = 2)
   ),
  mainPanel(
    h2("Random Forest"),
      tabsetPanel(
        tabPanel("Data", 
                 tableOutput("datatable")),
        tabPanel("Class", 
                 h3("Classification"), 
                 plotOutput("vara", width = "1000px", height = "500px")),
        tabPanel("Reg", 
                 h3("Regression"), 
                 plotOutput("vara2", width = "1000px", height = "500px")),
        tabPanel("PredegressTest", 
                 h3("testing$ERP, predregress"), 
                 plotOutput("vara1", width = "100%", height = "500px")),
        tabPanel("VarImp", 
                 h3("Variable Importance"), 
                 plotOutput("vara3", width = "100%", height = "500px")),
        tabPanel("CatTesting", 
                 h3("Categorical Testing"), 
                 plotOutput("vara4", width = "1000px", height = "500px"))
      )
   )
  )))