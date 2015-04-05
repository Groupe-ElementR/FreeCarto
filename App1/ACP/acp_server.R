####### ACP

observe({
  if (length(input$acpActives) > 3) {
    updateSelectInput(session, inputId = "acpAxes",
                      choices = 1:(length(input$acpActives) -1),
                      selected = 1:2)
    #       if (is.null(input$acpSup)) {
    #         quantvar <- sapply(baseData$data[,input$acpSup], is.numeric)
    #         qualivar <- !quantvar
    #       } else {
    #         quantvar <- NULL
    #         qualivar <- NULL
    #       }
    #rajouter variables supplémentaires
    pcaData$acpRes <- PCA(baseData$data[,input$acpActives], ncp = 10, graph = FALSE)
  }
})



output$acpCircle <- renderPlot({
  if (all(!is.na(pcaData$acpRes))) {
    plot.PCA(pcaData$acpRes, axes = as.integer(input$acpAxes), choix = "var")
  }
})

output$acpScatter <- renderPlot({
  if (all(!is.na(pcaData$acpRes))) {
    plot.PCA(pcaData$acpRes, axes = as.integer(input$acpAxes), choix = "ind")
  }
})

# Update variable list
observe({
  baseData$data
  updateSelectInput(session = session, inputId = "acpActives",
                    label = "Choose PCA active variables",
                    selected = isolate(input$acpActives),
                    choices = colnames(isolate(baseData$data[, sapply(baseData$data, is.numeric)])))
  updateSelectInput(session = session, inputId = "acpSup",
                    label = "Choose PCA supplementary variables",
                    selected = isolate(input$acpSup),
                    choices = colnames(isolate(baseData$data)))
  
})
