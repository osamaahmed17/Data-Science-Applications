#Application 06

library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Lab-06 Probability Distributions"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose File From the Application 06 Folder", multiple=TRUE),h4("The file is about different types of servings of drinks in different countries."),
        br(),numericInput("val1", "Value 1", 0, min = 1, max = 1000,width = "150px"),
        numericInput("val2", "Value 2", 0, min = 1, max = 1000,width = "150px"),
        numericInput("val3", "Value 3", 0, min = 1, max = 1000,width = "150px"),
        checkboxInput("LowerTail", "Lower Tail", FALSE),
        tags$hr(),
        h2("Binomial Distribtion"),
        h5("Binomial Distribution shows frequency distribution of the possible number of successful outcomes in a given number of trials in each of which there is the same probability of success."),
        actionButton("pbinom", "pbinom"),
        actionButton("qbinom", "qbinom"),
        actionButton("dbinom", "dbinom"),
        tags$hr(),
        h2("Normal Distribution"),
        h5("A normal distribution is an arrangement of a data set in which most values cluster in the middle of the range and the rest taper off symmetrically toward either extreme."),
        actionButton("pnorm", "pnorm"),
        actionButton("qnorm", "qnorm"),
        actionButton("dnorm", "dnorm"),
        tags$hr(),
        h2("Poisson Distribution"),
        h5("The Poisson distribution can be used to calculate the probabilities of various numbers of success based on the mean number of successes."),
        actionButton("ppois", "ppois"),
        actionButton("qpois", "qpois"),
        actionButton("dpois", "dpois")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data", tableOutput("datatable")),
          tabPanel("Result", h3(textOutput("result")))
        )
        
      )
    )
  )  
)
