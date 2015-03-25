require(shiny)
require(freeCarto)
require(sp)
require(cluster)
require(FactoMineR)
library(ggplot2)
load("TNdeleg.RData")
require(reshape2)
require(xtable)
source("A2R.R")

baseData <- reactiveValues(spdf = TNdeleg.spdf, data = TNdeleg)
pcaData <- reactiveValues(acpRes = NA)

analysisData <- reactiveValues(cah = NA, clusters = NA)

# Compute linear model ----
ComputeRegression <- function(df, vardep, varindep){
  linMod <- summary(lm(formula = formula(eval(paste(eval(vardep), "~", paste(varindep, collapse = "+")))), data = df))
  coefReg <- round(linMod$coefficients, digits = 2)[, 1:2]
  rawR2 <- round(linMod$r.squared, digits = 2)
  adjR2 <- round(linMod$adj.r.squared, digits = 2)
  matCor <- cor(df[, c(vardep, varindep)], use = "complete.obs", method = "pearson")
  tabResid <- data.frame(ABSRESID = linMod()$residuals, 
                         RELRESID = linMod()$residuals / (df[, vardep] - linMod()$residuals))
  
  tabResults <- data.frame(CONCEPT = c("Coefficient de détermination",
                                       "Coefficient de détermination multiple",
                                       coefReg[, 1]),
                           VALEUR = c(rawR2, adjR2, coefReg[, 2]),
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

runCAH <- function(df, columns, userDistance , userMethod) {
  cahDF <- df[,columns]
  cah <- hclust(dist(cahDF, method = userDistance), method = userMethod)
  return(cah)
}

plotProfiles <- function(df, columns, clusters, Cpal, scale){
  nClusters <- length(unique(clusters))
  df$CLUSCSP <- factor(clusters, levels = 1:nClusters, labels = paste('Cluster', 1:nClusters, sep = " "))
  
  plotDF <- df[, columns]
  
  if (scale){plotDF <- sapply(X = plotDF, FUN = function(x) {scale(x, center = TRUE, scale = TRUE)})}
  
  clusProfile <- aggregate(plotDF,
                           by = list(df$CLUSCSP),
                           mean)
  
  colnames(clusProfile)[1] <- "CLUSTER"
  clusLong <- melt(clusProfile, id.vars = "CLUSTER")
  ggplot(clusLong) +
    geom_bar(aes(x = variable, y = value, fill = CLUSTER),
             stat = "identity") +
    scale_fill_manual(values = Cpal) +
    facet_wrap(~ CLUSTER) +
    coord_flip() + theme_bw()
}
