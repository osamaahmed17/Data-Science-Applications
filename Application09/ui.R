#Application 09

library(shiny)
shinyUI(fluidPage(
  titlePanel("Application Number 09"),
  sidebarLayout(
    sidebarPanel(
      h3('This Data is of Deere & Company Stocks'),h5("Deere & Company is an American corporation that manufactures agricultural, construction, and forestry machinery, diesel engines, drivetrains used in heavy equipment, and lawn care equipment"),
      actionButton("b1", "Get Data"),
      actionButton("b2", "Perform PCA")
      ),
    mainPanel(
      h2("Prinicple Component Analysis"),h4("A method of analysis which involves finding the linear combination of a set of variables that has maximum variance and removing its effect, repeating this successively."),
      tabsetPanel(
        tabPanel("Data",h5("This Table shows the stocks price when it was opened along with the its closing.It also show the high and low rates along with its value over the period."), tableOutput("table")),
        tabPanel("Scatter Plot",
           h3('Scatter Plot'),h5("To complete the analysis we often times would like to produce a scatter plot of the component scores.
                                 In looking at the program, you will see a gplot procedure at the bottom where we are plotting the second component against the first component."),
           plotOutput("vara", width = "100%"),br(),hr())
        )
      )
    )
  ))