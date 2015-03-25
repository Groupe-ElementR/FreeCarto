######################  
#### Shiny Server ####
######################


shinyServer(function(input, output, session) {

  
  output$fullDF <- renderDataTable({
    baseData$data
  })
  
  output$baseMap <- renderPlot({
    sp::plot(baseData$spdf)
  })
  
  # Combine the selected variables into a new data frame
  selectData <- reactive({
    mySelection <- iris[, c(input$xCol, input$yCol)]
    return(mySelection)
  })
  
  selectDataMap <- reactive({
    mySelection <-  input$circlevar
    return(mySelection)
  })
  
  
  output$map <- renderPlot({
    par(mar=c(0,0,1.2,0))
    StaticMap(obj = TNdeleg.spdf, add = FALSE,col = input$couleurFdc, 
              border = input$couleurBorder, lwd = input$epaisseurBorder)
    LayoutMap(title = input$titreCarte, north = input$nord, 
              scale = input$scaleSize, sources = input$sourceMap, 
              author = input$authorMap,
              col = input$colFrame, txtcol = input$colTitle )
    SymbolsMap(obj = TNdeleg.spdf,title = input$titreLegende,col2 = input$colSymbols2, breakval = input$breakval,
               pos = input$positionLegende,col = input$couleurSymboles,
               data = TNdeleg, datavar = input$propVar, 
               add=TRUE, symbols = input$symboles, k = input$tailleSymbole)
    if(input$labelcheck==TRUE){
    LabelMap(obj = TNdeleg.spdf, data = TNdeleg, txt = input$labeltxt)
#     , col = input$labelcol, cex = input$labelcex)
    }
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
    if (any(!is.na(analysisData$cah))){
      cah <- analysisData$cah
      clusters <- analysisData$clusters
      classOrder <- unique(clusters[cah$order])
      cPal <- rainbow(n = isolate(input$cahClusters))
      treePlot <- A2Rplot(x = cah,
                          k = isolate(input$cahClusters),
                          boxes = FALSE,
                          col.up = "gray50",
                          col.down = cPal[classOrder],
                          show.labels = FALSE)
    }
  })
  
  output$cahInertia <- renderPlot({
    
    if (any(!is.na(analysisData$cah))){
      barValues <- cumsum(sort(analysisData$cah$height / sum(analysisData$cah$height), decreasing = TRUE))[1:15] * 100
      barColors <- rep(x = "white", times = length(barValues))
      barColors[input$cahClusters] <- "red"
      barplot(barValues, col = barColors, xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)",
              names.arg=1:15, main="Inertie expliquée")
    }
  })
  
  output$cahProfiles <- renderPlot({
    if (any(!is.na(analysisData$clusters))){
      cPal <- rainbow(n = isolate(input$cahClusters))
      
      plotProfiles(df = isolate(baseData$data),
                   columns = isolate(input$cahVariables),
                   clusters = analysisData$clusters,
                   Cpal = cPal,
                   scale = input$cahScale)    
    }
  
  })
  
  observe({
    baseData$data
    updateSelectInput(session = session, inputId = "cahVariables",
                      label = "Choose CAH variables",
                      selected = isolate(input$cahVariables),
                      choices = colnames(baseData$data))
  })
  
  observe({
    if (any(!is.null(input$cahVariables) && length(input$cahVariables) > 1)){
      analysisData$cah <- runCAH(baseData$data, input$cahVariables,
                                 userDistance = input$cahDistance,
                                 userMethod = input$cahMethod) 
    }
  })
  
  observe({
    if (any(!is.na(analysisData$cah))){
      analysisData$clusters <- cutree(analysisData$cah, k = input$cahClusters) 
    }
  })
  
  observeEvent(input$cahAddColumn, {
    newName <- paste(isolate(input$cahNamePrefix), isolate(input$cahClusters), "Clusters", sep = "_")
    baseData$data[[newName]] <- isolate(analysisData$clusters)
  })
  
  
})
