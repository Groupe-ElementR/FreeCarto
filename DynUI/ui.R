
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("FreeCarto"),

  # Sidebar with a slider input for number of bins
  flowLayout(
    sidebarPanel(
      actionButton(inputId = "addTab", label = "Add Tab")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("debugText"),
      uiOutput("userTabs")
    )
  )
))
