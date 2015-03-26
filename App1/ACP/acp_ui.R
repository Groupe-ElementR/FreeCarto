output$pca <- renderUI({
  fluidPage(
    column(width = 4,
           selectInput(inputId = "acpActives",
                       label = "Choose PCA active variables",
                       choices = colnames(isolate(baseData$data[, sapply(baseData$data, is.numeric)])),
                       multiple = TRUE)),
    column(width = 4,
           selectInput(inputId = "acpSup",
                       label = "Choose PCA supplementary variables",
                       choices = colnames(isolate(baseData$data)),
                       multiple = TRUE)),
    column(width = 4,
           ## à faire côté serveur
           selectInput(inputId = "acpAxes",
                       label = "Choose axes to plot",
                       choices = "",
                       multiple = TRUE) 
    ),
    column(width = 6,
           plotOutput("acpCircle")),
    column(width = 6,
           plotOutput("acpScatter")),
    column(width = 6,
           plotOutput("acpLoads")),
    column(width = 6,
           plotOutput("acpEigen"))
  )
})