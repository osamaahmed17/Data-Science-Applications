#Application 12

library(shiny)

shinyServer(function(input, output) 
  {
    output$datatable <- renderTable(
    {
      if(is.null(input$files) ) 
      { 
        return() 
      }  
      else 
      { 
        #Variable in which data is going to be stored from csv file
        globalData <<- read.csv(input$files$datapath, header=T, stringsAsFactors =T)
        
      } 
    })
 
    #correlation plotting
    output$vara<-renderPlot({
      
      if(is.null(input$files)) 
      { 
        "No Data Available" 
      }  
      else 
      { 
        library(data.table)
        newDataSet <- globalData
        
        #-----------------------------------------------------------
        #                       Fresh
        #-----------------------------------------------------------
        
        plota = boxplot(newDataSet$Fresh, horizontal = T)
        plota$out
        newFresh <- newDataSet$Fresh[newDataSet$Fresh<32000]
        plotb = boxplot(newFresh, horizontal = T)
        plotb$out
        #Function outlier replacing
        outlierReplace = function(dataframe, cols, rows, newValue = NA) {
          if (any(rows)) {
            set(dataframe, rows, cols, newValue)
          }
        }
        
        outlierReplace(newDataSet, "Fresh", which(newDataSet$Fresh > 32000), 32000)
        
       #-------------------------------------------------------------------------------
                              #Milk
       #-------------------------------------------------------------------------------
        
        plota = boxplot(newDataSet$Milk, horizontal = T)
        plota$out
        newMilk <- newDataSet$Milk[newDataSet$Milk<14000]
        plotb = boxplot(newMilk, horizontal = T)
        plotb$out
        #Function outlier replacing
        outlierReplace = function(dataframe, cols, rows, newValue = NA) {
          if (any(rows)) {
            set(dataframe, rows, cols, newValue)
          }
        }
        
        outlierReplace(newDataSet, "Milk", which(newDataSet$Milk > 14000), 14000)
        
        #-------------------------------------------------------------------------------
        #                               Grocery
        #-------------------------------------------------------------------------------
        
        plota = boxplot(newDataSet$Grocery, horizontal = T)
        plota$out
        newGrocery <- newDataSet$Grocery[newDataSet$Grocery<17000]
        plotb = boxplot(newGrocery, horizontal = T)
        plotb$out
        #Function outlier replacing
        outlierReplace = function(dataframe, cols, rows, newValue = NA) {
          if (any(rows)) {
            set(dataframe, rows, cols, newValue)
          }
        }
        
        outlierReplace(newDataSet, "Grocery", which(newDataSet$Grocery > 17000), 17000)
        
        #-------------------------------------------------------------------------------
        #                               Frozen
        #-------------------------------------------------------------------------------
        
        plota = boxplot(newDataSet$Frozen, horizontal = T)
        plota$out
        newFrozen <- newDataSet$Frozen[newDataSet$Frozen<5000]
        plotb = boxplot(newFrozen, horizontal = T)
        plotb$out
        #Function outlier replacing
        outlierReplace = function(dataframe, cols, rows, newValue = NA) {
          if (any(rows)) {
            set(dataframe, rows, cols, newValue)
          }
        }
        
        outlierReplace(newDataSet, "Frozen", which(newDataSet$Frozen > 5000), 5000)
        
        #-------------------------------------------------------------------------------
        #                               BoxPlot Detergent Papers
        #-------------------------------------------------------------------------------
        
        plota = boxplot(newDataSet$Detergents_Paper, horizontal = T)
        plota$out
        newDetergents_Paper <- newDataSet$Detergents_Paper[newDataSet$Detergents_Paper < 1000]
        plotb = boxplot(newDetergents_Paper, horizontal = T)
        plotb$out
        #Function outlier replacing
        outlierReplace = function(dataframe, cols, rows, newValue = NA) {
          if (any(rows)) {
            set(dataframe, rows, cols, newValue)
          }
        }
        
        outlierReplace(newDataSet, "Detergents_Paper", which(newDataSet$Detergents_Paper > 1000), 1000)
        
        #-------------------------------------------------------------------------------
        #                               Boxplot outliers
        #-------------------------------------------------------------------------------
        
        plota = boxplot(newDataSet$Delicassen, horizontal = T)
        plota$out
        newDelicassen <- newDataSet$Delicassen[newDataSet$Delicassen<3000]
        plotb = boxplot(newDelicassen, horizontal = T)
        plotb$out
        #Function outlier replacing
        outlierReplace = function(dataframe, cols, rows, newValue = NA) {
          if (any(rows)) {
            set(dataframe, rows, cols, newValue)
          }
        }
        
        outlierReplace(newDataSet, "Delicassen", which(newDataSet$Delicassen > 3000), 3000)
        
        #-------------------------------------------------------------------------------
        #                               Outliers end
        #-------------------------------------------------------------------------------

        # Remove NA values 
        newDataSet <- na.omit(newDataSet)
        
        # Scale variables
        newDataSet <- scale(newDataSet)
        
        # output just 1st 4 rows
        head(newDataSet, n = 4)
        library("cluster")
        library("factoextra")
        
        #-------------------------------------------------------------------------------
        #                              Distance Matrix
        #-------------------------------------------------------------------------------
        
        res.dist <- get_dist(newDataSet, stand = TRUE, method = "spearman") #"pearson", "kendall" and "spearman"
        
        output$vara1<-renderPlot(
        {
          fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
        })
        
        #-------------------------------------------------------------------------------
        #                              Clustering
        #-------------------------------------------------------------------------------
        
        output$vara2<-renderPlot(
        {
          fviz_nbclust(newDataSet, kmeans, method = "gap_stat") #for finding the Clusters Those are optimal
        })
        library("NbClust")
        set.seed(123)
        
        #-------------------------------------------------------------------------------
        #                              Best Clustering
        #-------------------------------------------------------------------------------  
        
        output$vara3<-renderPlot(
        {
          res.nbclust <<- NbClust(newDataSet, distance = "euclidean",min.nc = 2, max.nc = 10, method = "complete", index ="all")
          output$summ3<-renderUI(
          {
           # NbClust()
          })
        })
        
        #-------------------------------------------------------------------------------
        #                              K-Means
        #-------------------------------------------------------------------------------
        
        output$vara4<-renderPlot(
        {
          factoextra::fviz_nbclust(res.nbclust) + theme_minimal()
        })
        km.res <- kmeans(newDataSet, input$slider1, nstart = 25)
        
        output$vara5<-renderPlot(
        {
          fviz_cluster(km.res, data = newDataSet, frame.type = "convex")+ theme_minimal() #ploting clusters
        })
        
        #-------------------------------------------------------------------------------
        #                              K-medoid Method
        #-------------------------------------------------------------------------------
        
        library("cluster")
        pam.res <- pam(newDataSet, input$slider1) 
        
        # Visualization
        output$vara6<-renderPlot(
        {
          fviz_cluster(pam.res)
        })
        # Dissimilarity matrix computation
        d <- dist(newDataSet, method = "euclidean")
        # Ward's method hierarchical clustering
        res.hc <- hclust(d, method = "ward.D2" )
        # Group of 2 rather than 3
        grp <- cutree(res.hc, k = input$slider1)
        
        # Visualization 
        output$vara7<-renderPlot(
        {
          plot(res.hc, cex = 0.6) # tree ploting 
          rect.hclust(res.hc, k = input$slider1, border = 2:5) # adding rectangle to the tree
        })
        # Spliting Clustering into 2 Clusters and computation of Clusters
        res <- hcut(newDataSet, k = input$slider1, stand = TRUE)
        
        # Visualization
        output$vara8<-renderPlot(
        {
          fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#FC4E07"))
        })
        
      }
    
    })
  
})