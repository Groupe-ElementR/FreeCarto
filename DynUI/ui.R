library(shiny)

shinyUI(fluidPage(

  titlePanel("FreeCarto"),

  flowLayout(
    sidebarPanel(
      actionButton(inputId = "addTab", label = "Add Tab")
    ),

    mainPanel(
      uiOutput("userTabs")
    )
  )
))
