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