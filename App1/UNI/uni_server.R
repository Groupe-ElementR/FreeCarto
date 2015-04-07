# Linear model ----


# Draw univariate plot

output$uniplot <- renderPlot({
  if (!is.null(input$uniquanti) & is.null(input$uniquali)){
    # quanti
    if (!is.na(input$binstep)){
      pUni <- ggplot(baseData$data) + geom_histogram(aes(x = input$uniquanti), color = "white", fill = "grey30", binwidth = input$binstep) +
        scale_y_continuous("Fréquence") + theme_bw()
      if (isTRUE(input$drawsummary)){
        pUni <- pUni + 
          geom_vline(xintercept = mean(baseData$data[, input$uniquanti], na.rm = TRUE), color = "chocolate4") +
          geom_vline(xintercept = quantile(baseData$data[, input$uniquanti], probs = seq(0, 1, 0.25), na.rm = TRUE), color = "chartreuse4")
      }
    } else {
      pUni <- ggplot(baseData$data) + geom_histogram(aes(x = input$uniquanti), color = "white", fill = "grey30") +
        scale_y_continuous("Fréquence") + theme_bw()
      if (isTRUE(input$drawsummary)){
        pUni <- pUni + 
          geom_vline(xintercept = mean(baseData$data[, input$uniquanti], na.rm = TRUE), color = "chocolate4") +
          geom_vline(xintercept = quantile(baseData$data[, input$uniquanti], probs = seq(0, 1, 0.25), na.rm = TRUE), color = "chartreuse4")
      }
    }
 
  } else if (is.null(input$uniquanti) & !is.null(input$uniquali)) {
    # quali
    pUni <- ggplot(baseData$data) + geom_bar(baseData$data[, input$uniquanti], color = "white", fill = "grey30") +
      scale_y_continuous("Fréquence") + theme_bw()
    
  } else if (!is.null(input$uniquanti) & !is.null(input$uniquali)) {
    # quali-quanti
    pUni <- ggplot(baseData$data) + geom_boxplot(aes(x = input$uniquali, y = input$uniquanti), color = "grey30", fill = "grey70") +
      theme_bw()
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
