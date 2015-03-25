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
    StaticMap(obj = baseData$spdf, add = FALSE,col = input$couleurFdc, 
              border = input$couleurBorder, lwd = input$epaisseurBorder)
    LayoutMap(title = input$titreCarte, north = input$nord, 
              scale = input$scaleSize, sources = input$sourceMap, 
              author = input$authorMap,
              col = input$colFrame, txtcol = input$colTitle )
    SymbolsMap(obj = baseData$spdf,title = input$titreLegende,col2 = input$colSymbols2, breakval = input$breakval,
               pos = input$positionLegende,col = input$couleurSymboles,
               data = baseData$data, datavar = input$propVar, 
               add=TRUE, symbols = input$symboles, k = input$tailleSymbole)
    if(input$labelcheck==TRUE){
    LabelMap(obj = baseData$spdf, data = baseData$data, txt = input$labeltxt)
#     , col = input$labelcol, cex = input$labelcex)
    }
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
  

  # Linear model ----
  
  # Compute linear regression
  linMod <- reactive({
    ComputeRegression(df = baseData$data, varx = input$regvarx, vary = input$regvary)
  })
  
  # Print scatter plot
  output$scatterplot <- renderPlot({
    ScatterPlot(df = baseData$data, varx = input$regvarx, vary = input$regvary)
  })
  
  # Print coefficients
  output$coefreg <- renderText({
    print(xtable(linMod()$TABCOEF), type = "HTML")
  })
  
  # Print correlation matrix
  output$matcor <- renderText({
    print(xtable(linMod()$MATCOR), type = "HTML")
  })
  
  # Add new variables (absolute and relative residuals)
  observeEvent(input$addabsresid, function() {
    newName <- paste("ABSRESID", truc, sep = "-")
    baseData$data[, newName] <- linMod()$TABRESID[, "ABSRESID"]
  })
  
  observeEvent(input$addabsresid, function() {
    newName <- paste("RELRESID", truc, sep = "-")
    baseData$data[, newName] <- linMod()$TABRESID[, "RELRESID"]
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
