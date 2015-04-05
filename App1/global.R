library(shiny)

load("data/parispc.RData")


source("Carto/carto_global.R", local = TRUE, encoding = "utf8")
source("CAH/cah_global.R", local = TRUE, encoding = "utf8")
source("LM/lm_global.R", local = TRUE, encoding = "utf8")
source("ACP/acp_global.R", local = TRUE, encoding = "utf8")
source("Pivot/pivot_global.R", local = TRUE, encoding = "utf8")

baseData <- reactiveValues(spdf = parispcPol, data = parispcTab)
