library(shiny)

shinyUI(fluidPage(

  titlePanel("FreeCarto"),

  flowLayout(
    sidebarPanel(
      radioButtons(inputId = "tabType",
                   label = "Tab type",
                   choices = c("Scatterplot" = "scplot", "Histogram" = "histplot")),
      actionButton(inputId = "addTab",
                   label = "Add Tab")
    ),

    mainPanel(
      uiOutput("userTabs")
    )
  )
))
