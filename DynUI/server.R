library(shiny)

shinyServer(function(input, output, session) {
  library(sp)
  data(meuse)
  meuse.xy <- meuse[c("x", "y")]
  coordinates(meuse.xy) <- ~x+y
  meuse2 <- SpatialPointsDataFrame(coords = meuse.xy, data = meuse[-c(1,2)])
  
  userTabs <- reactiveValues()
  userTabs$firstTab <- tabPanel(title = "Welcome aboard !",
          renderPlot(spplot(meuse2, "cadmium")),
          renderPlot(spplot(meuse2, "dist"))          
)

  userTabsInfo <- reactiveValues()
  userTabsInfo$selectedTabTitle <- NULL
  userTabsInfo$tabsOrder <- list('firstTab')


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
  
  observe({
    userTabsInfo$selectedTabTitle
    print(userTabsInfo$selectedTabTitle)
    updateTabsetPanel(session = session, inputId = "tabsetPanelID", selected = userTabsInfo$selectedTabTitle)
  })

})
