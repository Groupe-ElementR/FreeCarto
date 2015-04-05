# Linear model ----


# Compute linear regression

drawGraph <- reactive({
  if (!is.null(input$uniquanti) & is.null(input$uniquali)){
    # quanti
    ggplot(baseData$data) + geom_hist()
  } else if (is.null(input$uniquanti) & !is.null(input$uniquali)) {
    # quali
    ggplot(baseData$data) + geom_bar()
  } else if (!is.null(input$uniquanti) & !is.null(input$uniquali)) {
    # quali-quanti
    ggplot(baseData$data) + geom_boxplot()
  } else {
    return()
  }
})


# Print scatter plot

output$scatterplot <- renderPlot({
  if (!is.null(input$regvarx) & !is.null(input$regvary)){
    ScatterPlot(df = baseData$data, varx = input$regvarx, vary = input$regvary)
  } else {
    return()
  }
})


# Print coefficients

output$coefreg <- renderText({
  if (!is.null(input$regvardep) & !is.null(input$regvarindep)){
    print.xtable(xtable(linMod()$TABCOEF), type = "HTML", include.rownames = FALSE, html.table.attributes = "frame = border")
  } else {
    return()
  }
})


# Print correlation matrix

output$matcor <- renderText({
  if (!is.null(input$regvardep) & !is.null(input$regvarindep)){
    print.xtable(xtable(linMod()$MATCOR), type = "HTML", include.rownames = TRUE, html.table.attributes = "frame = border")
  } else {
    return()
  }
})


# Add new variables (absolute and relative residuals)

observeEvent(input$addregresid, {
  if (isolate(input$regprefix) != ""){
    absName <- paste(isolate(input$regprefix), "AbsResid", sep = "_")
    relName <- paste(isolate(input$regprefix), "RelResid", sep = "_")
  } else {
    absName <- paste("lm", "AbsResid", sep = "_")
    relName <- paste("lm", "RelResid", sep = "_")
  }
  
  baseData$data[, c(absName, relName)] <- linMod()$TABRESID
})


# Updating variable list

observe({
  baseData$data
  updateSelectInput(session = session, inputId = "regvardep",
                    label = "Variable à expliquer",
                    selected = isolate(input$regvardep),
                    choices = colnames(baseData$data[, sapply(baseData$data, is.numeric)]))
  updateSelectInput(session = session, inputId = "regvarindep",
                    label = "Variable(s) explicative(s)",
                    selected = isolate(input$regvarindep),
                    choices = colnames(baseData$data[, sapply(baseData$data, is.numeric)]))
  updateSelectInput(session = session, inputId = "regvary",
                    label = "Graphique : variable Y",
                    selected = isolate(input$regvary),
                    choices = colnames(baseData$data[, sapply(baseData$data, is.numeric)]))
  updateSelectInput(session = session, inputId = "regvarx",
                    label = "Graphique : variable X",
                    selected = isolate(input$regvarx),
                    choices = colnames(baseData$data[, sapply(baseData$data, is.numeric)]))
})
