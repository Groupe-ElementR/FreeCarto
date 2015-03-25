######################  
#### Shiny Server ####
######################

shinyServer(function(input, output, session) {

  
  output$fullDF <- renderDataTable({
    baseData$data
  })
  
  output$baseMap <- renderPlot({
    plot(baseData$spdf)
  })
  
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
  
  output$cahTree <- renderPlot({
    plot(baseData$data)
  })
  
  observe({
    baseData$data
    updateSelectInput(session = session, inputId = "cahVariables",
                      label = "Choose CAH variables",
                      selected = isolate(input$cahVariables),
                      choices = colnames(baseData$data))
  })
  
  
  ####### ACP
  
  
  observe({
    if (length(input$acpActives) > 3) {
      updateSelectInput(session, inputId = "acpAxes",
                        choices = 1:(length(input$acpActives) -1),
                        selected = 1:2)
#       if (is.null(input$acpSup)) {
#         quantvar <- sapply(baseData$data[,input$acpSup], is.numeric)
#         qualivar <- !quantvar
#       } else {
#         quantvar <- NULL
#         qualivar <- NULL
#       }
      #rajouter variables supplémentaires
      pcaData$acpRes <- PCA(baseData$data[,input$acpActives], ncp = 10, graph = FALSE)
    }
  })
  

    
  output$acpCircle <- renderPlot({
    if (all(!is.na(pcaData$acpRes))) {
      plot.PCA(pcaData$acpRes, axes = as.integer(input$acpAxes), choix = "var")
    }
  })
  
  output$acpScatter <- renderPlot({
    if (all(!is.na(pcaData$acpRes))) {
      plot.PCA(pcaData$acpRes, axes = as.integer(input$acpAxes), choix = "ind")
    }
  })
  
  
})
