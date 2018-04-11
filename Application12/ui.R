#Application 12

shinyUI(fluidPage(
  titlePanel("Application 12"),
  sidebarLayout(
  sidebarPanel
  (
    fileInput("files", "Choose File From the Application 12 Folder", multiple=TRUE),
    sliderInput("slider1", min = 0, max = 5, value = 0, step = 1, label = h3("Clusters"))
  ),
  
  mainPanel(
    h2("Clustering"),
     tabsetPanel(
       tabPanel("Data", tableOutput("datatable")),
       
       tabPanel("BoxPlot", 
                h3("BoxPlot"),
                plotOutput("vara")),
       
       tabPanel("Distance Matrix",
                h3("Distance Matrix"),
                plotOutput("vara1")),
       
       tabPanel("Clusters", 
                h3("Clusters Those are optimal"),
                plotOutput("vara2")),
       
       tabPanel("Best Clusters", 
                h3("Clusters those are best"),
                plotOutput("vara3")),
       
       tabPanel("Optimal clustering", 
                h3("Result of Clustering"),
                plotOutput("vara4"),br(),hr(),
                h4("According to the clustering, the optimal number of clusters - k=2")),
       
       tabPanel("K-Mean", 
                h3("Plot of Clusters Using K-Means"),
                plotOutput("vara5")),
       
       tabPanel("K-medoid", 
                h3("Results After Applyin the K-medoid method"),
                plotOutput("vara6")),
       
       tabPanel("Ward Method", 
                h3("Ward's Method Clustering for Hierarchical Clustering"),
                plotOutput("vara7")),
       
       tabPanel("SplitCluster", 
                h3("Spliting Clustering into 2 Clusters and computation of Clusters"),
                plotOutput("vara8"))
         
      )
   )
  )
))