library(shiny)

shinyServer(function(input, output, session) {
  
 
# Keep general informations  
  userTabsInfo <- reactiveValues()
  userTabsInfo$selectedTabTitle <- NULL # latest created tab
  userTabsInfo$tabsOrder <- list('firstTab') # list of tabs in the order of their creation
  
  userTabsValues <- reactiveValues() # used to keep settings of all tabs user inputs
  userTabsValues$firstTab <- list('txtToDisplay' = '1')
  
  userTabs <- reactiveValues() # list of all tab objects created, that is reloaded each time a new tab is added
  userTabs$firstTab <- tabPanel(title = "Welcome aboard !",
          selectInput(inputId = paste("firstTab", "txtToDisplay", sep="-"),
                      label = "Choose var",
                      choices = c('1', '2', '3'),
                      selected = '1'),
          renderUI(tags$h2(userTabsValues$firstTab$txtToDisplay))
          )
  observe({
    userTabsInfo$selectedTabTitle
    userTabsValues$firstTab$txtToDisplay <- input[[paste("firstTab", "txtToDisplay", sep="-")]]
    updateSelectInput(session = session,
                      inputId = paste("firstTab", "txtToDisplay", sep="-"),
                      choices = c('1', '2', '3'),
                      selected = userTabsValues$firstTab$txtToDisplay)
  })



# generate the new tabs when clicking on Add tab
  observe({
    if (input$addTab > 0){
    randomNb <- round(runif(n = 1, min = 5, max = 200))
    switch(isolate(input$tabType),
           'select' = {
             tabName <- paste("select", as.character(randomNb), sep="_")
             myPanel <- tabPanel(title = tabName,
                                 selectInput(inputId = paste(tabName, "txtToDisplay", sep="-"),
                                             selected = '1',
                                             label = "Choose var",
                                             choices = c('1', '2', '3'),
                                             multiple = FALSE),
                                 renderUI(tags$h2(input[[paste(tabName, "txtToDisplay", sep="-")]]))
             )
             userTabsValues[[tabName]] <- list('txtToDisplay' = '1')
             observe({
               userTabsInfo$selectedTabTitle
               userTabsValues[[tabName]][['txtToDisplay']] <- input[[paste(tabName, "txtToDisplay", sep="-")]]
               updateSelectInput(session = session,
                                 inputId = paste(tabName, "txtToDisplay", sep="-"),
                                 choices = c('1', '2', '3'),
                                 selected = userTabsValues[[tabName]][['txtToDisplay']])
             })
           },
           'slider' = {
             tabName <- paste("slider", as.character(randomNb), sep="_")
             myPanel <- tabPanel(title = tabName,
                                 sliderInput(inputId = paste(tabName, "txtToDisplay", sep="-"),
                                             label = "Choose value", min = 1, max = 3,
                                             value = 1),
                                 renderUI(tags$h2(input[[paste(tabName, "txtToDisplay", sep="-")]]))
                                 
             )
             userTabsValues[[tabName]] <- list('txtToDisplay' = 1)
             observe({
               userTabsInfo$selectedTabTitle
               userTabsValues[[tabName]][['txtToDisplay']] <- input[[paste(tabName, "txtToDisplay", sep="-")]]
               updateSliderInput(session = session,
                                 inputId = paste(tabName, "txtToDisplay", sep="-"),
                                 label = "Choose value",
                                 value = userTabsValues[[tabName]][['txtToDisplay']])
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
