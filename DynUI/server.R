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
          selectInput(inputId = paste("firstTab", "varToMap", sep="-"),
                      label = "Choose var",
                      choices = names(meuse2)),
          renderUI(tags$h2(userTabsValues$firstTab$varToMap)),
          renderPlot(spplot(meuse2, userTabsValues$firstTab$varToMap))
          )
  observe({
    userTabsInfo$selectedTabTitle
    userTabsValues$firstTab$varToMap <- input[[paste("firstTab", "varToMap", sep="-")]]
    updateSelectInput(session = session,
                      inputId = paste("firstTab", "varToMap", sep="-"),
                      choices = names(meuse2),
                      selected = userTabsValues$firstTab$varToMap)
  })



# generate the new tabs when clicking on Add tab
  observe({
    if (input$addTab > 0){
    randomNb <- round(runif(n = 1, min = 5, max = 200))
    switch(isolate(input$tabType),
           'scplot' = {
             tabName <- paste("scplot", as.character(randomNb), sep="_")
             myPanel <- tabPanel(title = tabName,
                                 selectInput(inputId = paste(tabName, "linetype", sep="-"),
                                             label = "Line type",
                                             choices = c("line" = "l", "point" = "p","both" = "b"),
                                             multiple = FALSE),
                                 renderPlot(expr = plot(runif(randomNb),
                                                        type = input[[paste(tabName, "linetype", sep="-")]]))
             )
             userTabsValues[[tabName]] <- list('linetype' = 'line')
             observe({
               userTabsInfo$selectedTabTitle
               userTabsValues[[tabName]][['linetype']] <- input[[paste(tabName, "linetype", sep="-")]]
               updateSelectInput(session = session,
                                 inputId = paste(tabName, "linetype", sep="-"),
                                 choices = c("line" = "l", "point" = "p","both" = "b"),
                                 selected = userTabsValues[[tabName]][['linetype']])
             })
           },
           'histplot' = {
             tabName <- paste("myhist", as.character(randomNb), sep="_")
             myPanel <- tabPanel(title = tabName,
                                 renderUI(tags$h2(input[[paste(tabName, "numcells", sep="-")]])),
                                 sliderInput(inputId = paste(tabName, "numcells", sep="-"),
                                             label = "Nb bars", min = 2, max = 10,
                                             value = 5),
                                 renderPlot(expr = hist(runif(randomNb),
                                                        #breaks = input[[paste(tabName, "numcells", sep="-")]]))
                                                        ))
                                 
             )
             userTabsValues[[tabName]] <- list('numcells' = 5)
             observe({
               userTabsInfo$selectedTabTitle
               userTabsValues[[tabName]][['numcells']] <- input[[paste(tabName, "numcells", sep="-")]]
               updateSliderInput(session = session,
                                 inputId = paste(tabName, "numcells", sep="-"),
                                 label = "Nb bars",
                                 value = userTabsValues[[tabName]][['numcells']])
             })
           }
           
    )
    myTab <- myPanel
    userTabs[[tabName]] <- myTab
    userTabsInfo$tabsOrder <- isolate(append(userTabsInfo$tabsOrder, tabName))
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


})
