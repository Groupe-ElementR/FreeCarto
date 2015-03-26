# Linear model ----

# Compute linear regression
linMod <- reactive({
  ComputeRegression(df = baseData$data, varx = input$regvarx, vary = input$regvary)
})

# Print scatter plot
output$scatterplot <- renderPlot({
  ScatterPlot(df = baseData$data, varx = input$regvarx, vary = input$regvary)
})

# Print coefficients
output$coefreg <- renderText({
  print(xtable(linMod()$TABCOEF), type = "HTML")
})

# Print correlation matrix
output$matcor <- renderText({
  print(xtable(linMod()$MATCOR), type = "HTML")
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