
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  
  uiValues <- reactiveValues()
  uiValues$tabList <- tabPanel('ABC')
  
  observe({
    input$addTab
    myTab <- tabPanel(title = as.character(round(runif(n = 1, min = 0, max = 100))))
    abc <- isolate(uiValues$tabList)
    uiValues$tabList <- list(abc, myTab)
  })
  
  
  output$userTabs <- renderUI({
    input$addTab
    do.call(tabsetPanel, uiValues$tabList)
  })
  
  output$debugText <- renderPrint({
    uiValues$tabList
  })

})
