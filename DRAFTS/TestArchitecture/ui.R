
library(shiny)

shinyUI(fluidPage(

  titlePanel("Test d'architecture"),

  sidebarLayout(
    sidebarPanel(
      selectInput("module",
                  label = "Choisir un module",
                  choices = names(modules)),
      uiOutput("inputs")
      

    ),

    mainPanel(
      uiOutput("outputs")
      
    )
  )
))
