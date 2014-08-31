
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(sp)
library(rgdal)
myShp <- readOGR(dsn = "data/DEPARTEMENT.SHP", layer = "DEPARTEMENT", stringsAsFactors = FALSE)
myShp@data$CODE_REG <- as.numeric(myShp@data$CODE_REG)
myAttributes <- myShp@data

shinyUI(fluidPage(

  

  # Application title
  titlePanel("d3carto"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "mapVar",
                  label = "Map variable",
                  choices = colnames(myAttributes),
                  multiple = FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("choroMap")
    )
  )
))
