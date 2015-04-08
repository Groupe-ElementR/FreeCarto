# output$map <- renderPlot({
#   par(mar=c(0,0,1.2,0))
#   staticLayer(spdf = baseData$spdf, add = FALSE,col = input$couleurFdc,
#             border = input$couleurBorder, lwd = input$epaisseurBorder)
#   layoutLayer(title = input$titreCarte, north = input$nord,
#             scale = input$scaleSize, sources = input$sourceMap,
#             author = input$authorMap,
#             col = input$colFrame, coltitle = input$colTitle )
#   propSymbolsLayer(spdf = baseData$spdf,title = input$titreLegende,col2 = input$colSymbols2, breakval = input$breakval,
#              pos = input$positionLegende,col = input$couleurSymboles,
#              df = baseData$data, var = input$propVar,
#              add=TRUE, symbols = input$symboles, k = input$tailleSymbole)
#   if(input$labelcheck==TRUE){
#     labelLayer(spdf = baseData$spdf, df = baseData$data, txt = input$labeltxt)
#     #     , col = input$labelcol, cex = input$labelcex)
#   }
# })

output$map <- renderPlot({
  par(mar=c(0,0,1.2,0))
  if (input$mapType == "choro"){

    choroLayer(spdf = baseData$spdf, add = FALSE, df = baseData$data, var = input$var,
               nbclass = 10, method = "quantile", spdfid = "INSEE_COM", breakval=0  )
    #             border = input$couleurBorder, lwd = input$epaisseurBorder)
  }
  if(input$mapType == "symbols"){
      staticLayer(spdf = baseData$spdf, add = FALSE,col = "grey50",
                border = "white", lwd = 0.75)
      propSymbolsLayer(spdf = baseData$spdf, add = TRUE, df = baseData$data, var = input$var2,spdfid="INSEE_COM")
  }


})


observe({
  baseData$data
  updateSelectInput(session = session, inputId = "var",
                    label = "Variable",
                    selected = isolate(input$var),
                    choices = colnames(baseData$data))
  updateSelectInput(session = session, inputId = "var2",
                    label = "Variable",
                    selected = isolate(input$var2),
                    choices = colnames(baseData$data))
})
