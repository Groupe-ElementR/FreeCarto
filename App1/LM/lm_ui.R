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
      actionButton("addabsresid", "Ajouter les résidus absolus"),
      actionButton("addrelresid", "Ajouter les résidus relatifs")
    )),
    column(9, 
           plotOutput("scatterplot"),
           htmlOutput("coefreg"),
           htmlOutput("matcor")
    )
  )
})