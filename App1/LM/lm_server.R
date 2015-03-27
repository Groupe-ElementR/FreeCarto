# Linear model ----


# Compute linear regression

linMod <- reactive({
  if (!is.null(input$regvardep) & !is.null(input$regvarindep)){
    ComputeRegression(df = baseData$data, vardep = input$regvardep, varindep = input$regvarindep)
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

observeEvent(input$addregresid, function() {
  absName <- paste(isolate(input$regprefix), "AbsResid", sep = "_")
  relName <- paste(isolate(input$regprefix), "RelResid", sep = "_")
  baseData$data[, c(absName, relName)] <- linMod()$TABRESID
})


