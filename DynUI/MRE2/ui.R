library(shiny)

shinyUI(fluidPage(

  titlePanel("Dynamic UI test"),

  flowLayout(
    sidebarPanel(
      actionButton(inputId = "reload", label = "Reload")
    ),

    mainPanel(
      uiOutput("userControls")
    )
  )
))
