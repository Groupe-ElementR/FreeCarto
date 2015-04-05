output$uni <- renderUI({
  fluidRow(
    column(3, wellPanel(
      tags$h4("Sélection des variables"),
      selectInput(inputId = "uniquanti", 
                  label = "Variable à expliquer", 
                  choices = colnames(isolate(baseData$data[, sapply(isolate(baseData$data), is.numeric)])), 
                  selected = ""),
      selectInput(inputId = "uniquali", 
                  label = "Variable(s) explicative(s)", 
                  choices = colnames(isolate(baseData$data[, sapply(isolate(baseData$data), is.numeric)])), 
                  selected = ""),
      checkboxInput("drawsummary", "Enregistrer les résidus", value = FALSE)
      )
    )),
    column(6, 
           tags$h4("Résumé graphique"),
           plotOutput("scatterplot")
    ),
    column(3,
           tags$h4("Résumé numérique"),
           htmlOutput("coefreg"),
           tags$h4("Matrice de corrélation"),
           htmlOutput("matcor")
    )
  )
})


