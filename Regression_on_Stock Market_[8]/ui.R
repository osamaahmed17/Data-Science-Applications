#Application 08

library(shiny)

shinyUI(fluidPage(
  titlePanel("Regression on Stock Market"),
  sidebarLayout(
    sidebarPanel(
      h2("This Data is of Deere & Company Stocks"),
      h4("Deere & Company is an American corporation that manufactures agricultural, construction, and forestry machinery, diesel engines, drivetrains used in heavy equipment, and lawn care equipment.")
      
      
      ),
  mainPanel(
    h2("Regression on Stock Market Data"),
    tabsetPanel(
      tabPanel("Data",br(),h4("This Table shows the stocks price when it was opened along with the its closing.It also show the high and low rates along with its value over the period."),br(),tableOutput("table")),
      
      tabPanel("BarChart",br(),h4("This Barchat shows the stocks price and volume rising and falling from the period of 03/01/2007 to 30/05/2017."),br(),
               plotOutput("vara")),
      
      tabPanel("Chart Series",br(),h4("This chart series shows more eloborated detail stocks price and volume rising and falling from the period of 03/01/2007 to 30/05/2017."),br(),
             plotOutput("vara1")),
      
      tabPanel("Density Graph",br(),h4("This density graph shows the density of the stocks volume."),br(),
               plotOutput("vara2")),
      
      tabPanel("All Densities",br(),h4("The graph shows the graphical aspect of Deere & Company Stock's volume, high,low, open,closing and opening."),br(), 
               plotOutput("vara3")),
     
        tabPanel("Volume Prediction",br(),h4("The graph shows the graphical aspect of Deere & Company Stock's volume and its prediction of it."),br(), 
               plotOutput("vara4"),br())
        
     
    )
   )
  )
))