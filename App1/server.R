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
  
})
