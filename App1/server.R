######################  
#### Shiny Server ####
######################

shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectData <- reactive({
    mySelection <- iris[, c(input$xCol, input$yCol)]
    return(mySelection)
  })
  
  # Compute the hierarchical clustering
  makeClusters <- reactive({
    hierarClus <- agnes(selectData(), metric = input$typeDist, method = input$typeCrit)
    return(hierarClus)
  })
  
  # Make xy plot
  output$plotxy <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    clusId <- cutree(makeClusters(), input$nbClus)
    plot(selectData(),
         col = clusId,
         pch = 20, cex = 3)
  })
  
  # Make tree plot
  output$plottree <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    hierarTree <- as.dendrogram(makeClusters())
    plot(hierarTree, leaflab = "none")
  })
  
  # Make the table
  output$userdata <- renderDataTable({
    iris$CLUSTER <- cutree(makeClusters(), input$nbClus)
    return(iris)
  })
  
})
