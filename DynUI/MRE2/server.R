library(shiny)

shinyServer(function(input, output, session) {
  abc <- tagList({
    "slider" = renderUI({
      sliderInput(inputId = 'slideInput', label = "slider", min = 1, max = 10, value = 5)
    })
  "select" = renderUI({
      selectInput(inputId = 'selectInput', label = "select", choices = c("A", "B", "C"), selected = "A")
    })
  })
  
  output$userControls <- renderUI({
    input$reload
    do.call(uiOutput, abc)
  })

})
