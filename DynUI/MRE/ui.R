library(shiny)

shinyUI(fluidPage(

  titlePanel("Dynamic UI test"),

  flowLayout(
    sidebarPanel(
      radioButtons(inputId = "tabType",
                   label = "Tab type",
                   choices = c("SelectInput" = "select", "SliderInput" = "slider")),
      actionButton(inputId = "addTab",
                   label = "Add Tab")
    ),

    mainPanel(
      uiOutput("userTabs")
    )
  )
))
