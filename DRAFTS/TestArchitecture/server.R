
library(shiny)

shinyServer(function(input, output, session) {
  
  # génération des inputs 
  output$inputs <- renderUI({
    lapply(modules[[input$module]][["inputs"]], eval, envir=-2)
  })

  # serveur proprement dit
  observe(lapply(modules[[input$module]][["server"]], eval, envir=-2))
  
  
  # génération des outputs
  output$outputs <- renderUI({
    lapply(modules[[input$module]][["outputs"]], eval, envir=-2)
  })
  

})
