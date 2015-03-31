output$pivot <- renderUI({
  fluidPage(
    rpivotTableOutput("pivotApp")
    )
})