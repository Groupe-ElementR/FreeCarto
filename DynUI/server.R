library(shiny)

shinyServer(function(input, output, session) {
  
  uiValues <- reactiveValues()
  uiValues$firstTab <- tabPanel(title = "ABC")
  
  observe({
    input$addTab
    
    randomNb <- round(runif(n = 1, min = 0, max = 100))
    myPanel <- tabPanel(title = as.character(randomNb)  )
    myTab <- list(myPanel)
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
    do.call(tabsetPanel, as.list(tabList))
  })
  
  output$debug <- renderPrint({
    values <- reactiveValuesToList(uiValues)
    myTabs <- tagList()
    for (i in values){
      myTabs <- append(myTabs, tagList(i))
    }
    tagList(myTabs)
    
  })
  

})
