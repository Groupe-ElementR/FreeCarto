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

source("CAH/cah_global.R", local = TRUE, encoding = "utf8")
source("LM/lm_global.R", local = TRUE, encoding = "utf8")
source("ACP/acp_global.R", local = TRUE, encoding = "utf8")

baseData <- reactiveValues(spdf = TNdeleg.spdf, data = TNdeleg)




