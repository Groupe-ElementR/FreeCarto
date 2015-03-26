# Linear model ----

# Compute linear regression
linMod <- reactive({
  ComputeRegression(df = baseData$data, vardep = input$regvardep, varindep = input$regvarindep)
})

# Print scatter plot
output$scatterplot <- renderPlot({
  ScatterPlot(df = baseData$data, varx = input$regvarx, vary = input$regvary)
})

# Print coefficients
output$coefreg <- renderText({
  print.xtable(xtable(linMod()$TABCOEF), type = "HTML", include.rownames = FALSE, html.table.attributes = "frame = border")
})

# Print correlation matrix
output$matcor <- renderText({
  print.xtable(xtable(linMod()$MATCOR), type = "HTML", include.rownames = TRUE, html.table.attributes = "frame = border")
})

# Add new variables (absolute and relative residuals)
observeEvent(input$addabsresid, function() {
  newName <- paste("ABSRESID", truc, sep = "-")
  baseData$data[, newName] <- linMod()$TABRESID[, "ABSRESID"]
})

observeEvent(input$addabsresid, function() {
  newName <- paste("RELRESID", truc, sep = "-")
  baseData$data[, newName] <- linMod()$TABRESID[, "RELRESID"]
})