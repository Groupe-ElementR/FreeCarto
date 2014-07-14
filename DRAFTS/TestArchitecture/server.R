
library(shiny)

shinyServer(function(input, output, session) {
  
  module <- reactive(input$module)
  
  # génération des inputs 
  output$inputs <- renderUI({
    lapply(modules[[module()]][["inputs"]], eval)
  })
  
  # génération des outputs
  output$outputs <- renderUI({
    lapply(modules[[module()]][["outputs"]], eval)
  })
  
  # serveur proprement dit
  observe(lapply(modules[[module()]][["server"]], eval, envir=-2))

})
