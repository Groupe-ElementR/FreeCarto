library(shiny)

shinyServer(function(input, output, session) {
  library(sp)
  data(meuse)
  meuse.xy <- meuse[c("x", "y")]
  coordinates(meuse.xy) <- ~x+y
  meuse2 <- SpatialPointsDataFrame(coords = meuse.xy, data = meuse[-c(1,2)])
 
# Keep general informations  
  userTabsInfo <- reactiveValues()
  userTabsInfo$selectedTabTitle <- NULL # latest created tab
  userTabsInfo$tabsOrder <- list('firstTab') # list of tabs in the order of their creation
  
  userTabsValues <- reactiveValues() # used to keep settings of all tabs user inputs
  userTabsValues$firstTab <- list('varToMap' = 'cadmium')
  
  userTabs <- reactiveValues() # list of all tab objects created, that is reloaded each time a new tab is added
  userTabs$firstTab <- tabPanel(title = "Welcome aboard !",
          selectInput(inputId = paste("firstTab", "varToMap", sep="-"), label = "Choose var", choices = names(meuse2)),
          renderUI(tags$h2(userTabsValues$firstTab$varToMap)),
          renderPlot(spplot(meuse2, userTabsValues$firstTab$varToMap))
          
)


# save the values of the tabs inputs
# TODO : need a loop there to save all the Inputs
observe({
  input[[paste("firstTab", "varToMap", sep="-")]]
  userTabsValues$firstTab$varToMap <- input[[paste("firstTab", "varToMap", sep="-")]]

  })



# generate the new tabs when clicking on Add tab
# TODO : need to add a userInput on each tab to test
  observe({
    if (input$addTab > 0){
    randomNb <- round(runif(n = 1, min = 0, max = 100))
    myPanel <- tabPanel(title = as.character(randomNb),
                          renderPlot(expr = plot(runif(randomNb)))
    )
    myTab <- myPanel
    userTabs[[as.character(randomNb)]] <- myTab
    userTabsInfo$tabsOrder <- isolate(append(userTabsInfo$tabsOrder, as.character(randomNb)))
    userTabsInfo$selectedTabTitle <- myTab$attribs$title
    }
  })
  
  
  output$userTabs <- renderUI({
    input$addTab
    values <- userTabsInfo$tabsOrder
    myTabs <- list()
    for (i in values){
      if (!is.null(userTabs[[i]])){
        myTabs <- append(myTabs, tagList(userTabs[[i]]))
      }
    }
    tabList <- myTabs
    do.call(tabsetPanel, c(tabList, id="tabsetPanelID"))
  })
  
# this observe is used to select the newest tab created
  observe({
    userTabsInfo$selectedTabTitle
    updateTabsetPanel(session = session, inputId = "tabsetPanelID", selected = userTabsInfo$selectedTabTitle)
  })

# this observe is used to update the tabs inputs, and so, to keep them in their last known state each time a new tab is added 
# TODO : need a loop there to update all the selectInput
  observe({
    userTabsInfo$selectedTabTitle
    updateSelectInput(session = session, inputId = paste("firstTab", "varToMap", sep="-"), choices = names(meuse2), selected = userTabsValues$firstTab$varToMap)
  })

})
