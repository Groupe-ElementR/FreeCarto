output$lm <- renderUI({
  fluidRow(
    column(3, wellPanel(
      tags$h4("Sélection des variables"),
      selectInput(inputId = "regvardep", 
                  label = "Variable à expliquer", 
                  choices = colnames(isolate(baseData$data)), 
                  selected = "", 
                  multiple = FALSE, 
                  selectize = TRUE),
      selectInput(inputId = "regvarindep", 
                  label = "Variable(s) explicative(s)", 
                  choices = colnames(isolate(baseData$data)), 
                  selected = "", 
                  multiple = TRUE, 
                  selectize = TRUE),
      selectInput(inputId = "regvary", 
                  label = "Graphique : variable Y", 
                  choices = colnames(isolate(baseData$data)), 
                  selected = "", 
                  multiple = FALSE, 
                  selectize = TRUE),
      selectInput(inputId = "regvarx", 
                  label = "Graphique : variable X", 
                  choices = colnames(isolate(baseData$data)), 
                  selected = "", 
                  multiple = FALSE, 
                  selectize = TRUE),
      checkboxInput("regsave", "Enregistrer les résidus"),
      conditionalPanel(condition = "input.regsave == true",
                       fluidRow(
                         column(2, textInput(inputId = "regprefix", label = "Préfixe", value = "")),
                         column(1, actionButton(inputId = "addregresid", label = "Ajouter les résidus"))
                       )
      )
    )),
    column(6, 
           tags$h4("Nuage de points et droite de régression"),
           plotOutput("scatterplot")
    ),
    column(3,
           tags$h4("Coefficients du modèle de régression"),
           htmlOutput("coefreg"),
           tags$h4("Matrice de corrélation"),
           htmlOutput("matcor")
    )
  )
})



