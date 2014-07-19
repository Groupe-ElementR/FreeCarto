library(shiny)

shinyServer(function(input, output, session) {
  require(sp)
  data(meuse)
  meuse.xy <- meuse[c("x", "y")]
  coordinates(meuse.xy) <- ~x+y
  meuse2 <- SpatialPointsDataFrame(coords = meuse.xy, data = meuse[-c(1,2)])
  
  
  uiValues <- reactiveValues()
  uiValues$firstTab <- tabPanel(title = "Welcome aboard !", renderPlot(spplot(meuse2, "cadmium")))
  
  observe({
    input$addTab
    
    randomNb <- round(runif(n = 1, min = 0, max = 100))
    myPanel <- tabPanel(title = as.character(randomNb), renderPlot(expr = plot(runif(randomNb))))
    myTab <- myPanel
    uiValues[[as.character(randomNb)]] <- myTab
  })
  
  
  output$userTabs <- renderUI({
    input$addTab
    values <- reactiveValuesToList(uiValues)
    myTabs <- list()
    for (i in values){
      myTabs <- append(myTabs, tagList(i))
    }
    tabList <- myTabs
    do.call(tabsetPanel, tabList)
  })
  
  output$debug <- renderPrint({
    values <- reactiveValuesToList(uiValues)
    myTabs <- tagList()
    for (i in values){
      myTabs <- append(myTabs, tagList(i))
    }
    #class(myTabs)
    str(myTabs)
    #tagList(myTabs)
    
  })
  

})
