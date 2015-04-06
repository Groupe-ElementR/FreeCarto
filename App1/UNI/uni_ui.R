output$uni <- renderUI({
  fluidRow(
    column(3, wellPanel(
      tags$h4("Sélection des variables"),
      selectInput(inputId = "uniquanti", 
                  label = "Variable quantitative à explorer", 
                  choices = colnames(isolate(baseData$data[, sapply(isolate(baseData$data), is.numeric)])), 
                  selected = ""),
      selectInput(inputId = "uniquali", 
                  label = "Variable qualitative à explorer", 
                  choices = colnames(isolate(baseData$data[, sapply(isolate(baseData$data), is.numeric)])), 
                  selected = ""),
      checkboxInput("uniset", "Personnaliser l'histogramme"),
      conditionalPanel(condition = "input.uniset == true",
                       textInput(inputId = "binstep", label = "Taille des classes", value = NA),
                       checkboxInput("drawsummary", "Tracer les résumés (Q1, Q2, Q3, Moyenne)", value = FALSE)
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


