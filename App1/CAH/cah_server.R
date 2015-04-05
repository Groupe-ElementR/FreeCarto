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
                    choices = colnames(baseData$data[, sapply(baseData$data, is.numeric)]))
})

observe({
  if (any(!is.null(input$cahVariables) && length(input$cahVariables) > 1)){
    analysisData$cah <- runCAH(baseData$data, input$cahVariables,
                               userDistance = input$cahDistance,
                               userMethod = input$cahMethod,
                               scale = input$cahNormalize) 
  }
})

observe({
  if (any(!is.na(analysisData$cah))){
    analysisData$clusters <- cutree(analysisData$cah, k = input$cahClusters) 
  }
})

observeEvent(input$cahAddColumn, {
  newName <- ifelse(isolate(input$cahNamePrefix) != "",
                    paste(isolate(input$cahNamePrefix), isolate(input$cahClusters), "Clusters", sep = "_"),
                    paste("CAH", isolate(input$cahClusters), "Clusters", sep = "_")) 
  baseData$data[[newName]] <- isolate(analysisData$clusters)
})