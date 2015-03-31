######################  
#### Shiny Server ####
######################

shinyServer(function(input, output, session) {
  
  source("Carto/carto_ui.R", local = TRUE, encoding = 'utf8')
  source("Carto/carto_server.R", local = TRUE, encoding = 'utf8')
  
  source("CAH/cah_ui.R", local = TRUE, encoding = 'utf8')
  source("CAH/cah_server.R", local = TRUE, encoding = 'utf8')
  
  source("ACP/acp_ui.R", local = TRUE, encoding = 'utf8')
  source("ACP/acp_server.R", local = TRUE, encoding = 'utf8')
  
  source("LM/lm_ui.R", local = TRUE, encoding = 'utf8')
  source("LM/lm_server.R", local = TRUE, encoding = 'utf8')
  
  source("Pivot/pivot_ui.R", local = TRUE, encoding = 'utf8')
  source("Pivot/pivot_server.R", local = TRUE, encoding = 'utf8')
  
  output$fullDF <- renderDataTable({
    baseData$data
  })
  
  output$baseMap <- renderPlot({
    plot(baseData$spdf)
  })



  
})
