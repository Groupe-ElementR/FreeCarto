

# Compute linear model ----
ComputeRegression <- function(df, vardep, varindep){
  linMod <- summary(lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df))
  coefReg <- round(linMod$coefficients, digits = 2)[, 1:2]
  rawR2 <- round(linMod$r.squared, digits = 2)
  adjR2 <- round(linMod$adj.r.squared, digits = 2)
  matCor <- round(cor(df[, c(vardep, varindep)], use = "complete.obs", method = "pearson"), digits = 3)
  tabResid <- data.frame(ABSRESID = linMod$residuals, 
                         RELRESID = linMod$residuals / (df[, vardep] - linMod$residuals))
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination multiple",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  return(list(TABCOEF = tabResults, TABRESID = tabResid, MATCOR = matCor))
}

# Prepare scatter plot ----
ScatterPlot <- function(df, varx, vary){
  scatPlot <- ggplot(df) + 
    geom_point(aes_string(x = varx, y = vary), color = "grey60") + 
    geom_smooth(aes_string(x = varx, y = vary), method = "lm", se = FALSE, color = "chocolate") +
    theme_bw()
  
  return(scatPlot)
}