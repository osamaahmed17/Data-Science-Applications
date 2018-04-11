#Application 05

library(shiny)

shinyUI(fluidPage(
  titlePanel("Application 05"),
  sidebarLayout(
    sidebarPanel(
      h4("This is the data analysis of twitter posts and we have selected name using the Text input and tweets using the slider"),
      textInput("text1", label = h3("Enter any Twitter Name you Want")),
      sliderInput("slider1", min = 0, max = 1000, value = 0,label = h3("Select Tweets")),
      actionButton("twt", "Show Tweets")
      ),
  mainPanel(
    h2("Twitter Data Analysis"),
    tabsetPanel(
      tabPanel("FreqBarChart", 
               h3("Frequency Bar Chart"),h5("A bar chart is a type of graph in which each column (plotted either vertically or horizontally) represents a categorical variable or a discrete ungrouped numeric variable. It is used to compare the frequency (count) for a category or characteristic with another category or characteristic."),br()
               ,plotOutput("plot1", width = "100%", height = "800px")),
      tabPanel("WordCloud",
               h3("Word Cloud"),h5("A word cloud is a graphical representation of word frequency. Type or paste text into the box below and press the arrow button to view the word cloud generated. The appearance of a word cloud can be altered using the graphical buttons above the cloud."),br(),
               plotOutput("plot2")),
      tabPanel("Association",
               h3("Word Association Diagram"),h5("The game is based on the noun phrase word association, meaning stimulation of an associative pattern by a word or the connection and production of other words in response to a given word, done spontaneously as a game, creative technique, or in a psychiatric evaluation.This whole concept is represented through the means of diagram."),br(),
               plotOutput("plot3", width = "100%", height = "800px")),
      tabPanel("Stack",
               h3("Stack Diagram for Word Association"),h5("Stack diagram is used to mainly track variable to function name mapping."),
               plotOutput("plot4")),
      tabPanel("Sentiment",
               h3("Sentiment Analysis"),h5("the process of computationally identifying and categorizing opinions expressed in a piece of text, especially in order to determine whether the writer's attitude towards a particular topic, product, etc. is positive, negative, or neutral.
"),br(),
               plotOutput("plot5"))
    )
  )
)
))