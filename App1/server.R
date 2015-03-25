######################  
#### Shiny Server ####
######################

require(shiny)
require(cluster)
require(freeCarto)
require(shinysky.incubator)
load("TNdeleg.RData")

shinyServer(function(input, output, session) {
  
  baseData <- reactiveValues()
  baseData$spdf <- TNdeleg.spdf
  baseData$data <- TNdeleg
  
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
  
})
