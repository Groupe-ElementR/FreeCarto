##############################  
#### Shiny User interface ####
##############################

# Shiny ui ----

shinyUI(fluidPage(
  titlePanel("FreeCarto"),
  tabsetPanel(
    tabPanel(title = "Accueil", icon = icon(name = "home")),
    tabPanel(title = "Données", icon = icon(name = "database"),
             plotOutput('baseMap'),
             dataTableOutput("fullDF")
    ),
    tabPanel(title = "Carto",
             icon = icon(name = "picture-o"),
             fluidRow(
               column(3, wellPanel( 
                 radioButtons(inputId = "typeCarte", label = "Type de Carte",
                              choices = c("Symboles proportionnels" = "symbols", "Choroplethe" = "choro")),
                 selectInput(inputId = "propVar", label = "Variable", 
                             selectize = TRUE, multiple = FALSE,
                             choices = colnames(isolate(baseData$data))[-c(1,2)], 
                             selected = colnames(isolate(baseData$data))[3]),
                 textInput(inputId = "titreCarte", label = "Titre de la carte", 
                           value = "Titre de la carte, année" ),
                 radioButtons(inputId = "symboles",
                              selected = "circles", 
                              label = "Type de symbole", 
                              choices = c("Cercles" = "circles", 
                                          "Carrés"="squares", "Barres" = "height")), 
                 sliderInput(inputId = "tailleSymbole", label = "Taille des symboles", animate = FALSE, 
                             min = 0.0, max = 1, value = 0.2, step = 0.01, ticks = FALSE),
                 textInput(inputId = "titreLegende", label = "Titre de la légende"), 
                 radioButtons(inputId = "positionLegende", label = NULL,inline = TRUE,  
                              choices = c("topleft","top", "topright", "left", "right", "bottomleft", "bottom", "bottomright")),
                 selectInput(inputId = "couleurSymboles", label = "Couleur des symboles", selected = "red",
                             choices = c("jaune" = "yellow","rouge" = "red", "vert" = "green", "bleu" = "blue"), 
                             multiple = FALSE, 
                             selectize = TRUE),
                 checkboxInput(inputId = "breakvalcheck", label = "Opposition des couleurs", value = FALSE ),
                 conditionalPanel(
                   condition = "input.breakvalcheck == true",
                   numericInput(inputId = "breakval", label = "Seuil", value = 0 ), 
                   selectInput(inputId = "colSymbols2", label = "Couleur en dessous du seuil", selected = "blue",
                               choices = c("jaune" = "yellow","rouge" = "red", "vert" = "green", "bleu" = "blue"), 
                               multiple = FALSE, 
                               selectize = TRUE)
                 ),
                 tabPanel(title = "Analyse",
                          icon = icon(name = "bar-chart"),
                          tabsetPanel(
                            tabPanel(title = "CAH",
                                     icon = icon(name = "tree"),
                                     column(width = 6,
                                            selectInput(inputId = "cahVariables",
                                                        label = "Choose CAH variables",
                                                        choices = colnames(isolate(baseData$data)),
                                                        multiple = TRUE)),
                                     column(width = 6,
                                            sliderInput(inputId = "cahClasses",
                                                        label = "Number of clusters",
                                                        min = 2, max = 10, value= 3)),
                                     plotOutput(outputId = "cahTree")
                            ),
                            
                            tabPanel(title = "Régression"))),
                 tabPanel(title = "Graphiques",
                          icon = icon(name = "pencil-square-o"),
                          sidebarPanel(
                            selectInput(inputId = "xCol", label = "Variable X", 
                                        selectize = TRUE, multiple = FALSE,
                                        choices = colnames(iris), selected = colnames(iris)[1]),
                            selectInput(inputId = "yCol", label = "Variable Y", 
                                        
                                        checkboxInput(inputId = "labelcheck", label = "Afficher des labels", value = FALSE ),
                                        conditionalPanel(
                                          condition = "input.labelcheck == true",
                                          selectInput(inputId = "labeltxt", label = "Variable", 
                                                      selectize = TRUE, multiple = FALSE,
                                                      choices = colnames(TNdeleg)
                                          )
                                        ),
                                        checkboxInput(inputId = "habillage", label = "Personaliser l'habillage", value = FALSE ),
                                        conditionalPanel(
                                          condition = "input.habillage == true",
                                          
                                          selectInput(inputId = "couleurFdc", label = "Couleur du fond de carte", selected = "green",
                                                      choices = c("jaune" = "yellow","rouge" = "red", "vert" = "green", "bleu" = "blue"), 
                                                      multiple = FALSE, 
                                                      selectize = TRUE),
                                          selectInput(inputId = "couleurBorder", label = "Couleur des contours", selected = "blue",
                                                      choices = c("jaune" = "yellow","rouge" = "red", "vert" = "green", "bleu" = "blue"), 
                                                      multiple = FALSE, 
                                                      selectize = TRUE),
                                          sliderInput(inputId = "epaisseurBorder", label = "Epaisseur des contours", animate = FALSE, 
                                                      min = 0, max = 5, value = 1, step = 0.1, ticks = FALSE), 
                                          checkboxInput(inputId = "nord", label = "Flèche nord", value = FALSE),
                                          numericInput(inputId = "scaleSize", value = 0, label = "Echelle", min = 0, step = 1),
                                          selectInput(inputId = "colFrame", label = "Couleur de la boite à carte", selected = "blue",
                                                      choices = c("jaune" = "yellow","rouge" = "red", "vert" = "green", "bleu" = "blue"), 
                                                      multiple = FALSE, 
                                                      selectize = TRUE),
                                          selectInput(inputId = "colTitle", label = "Couleur du titre", selected = "red",
                                                      choices = c("jaune" = "yellow","rouge" = "red", "vert" = "green", "bleu" = "blue"), 
                                                      multiple = FALSE, 
                                                      selectize = TRUE), 
                                          textInput(inputId = "authorMap", value = paste("Auteur", Sys.Date(), sep = " - "), label = "Auteur"),
                                          textInput(inputId = "sourceMap", value = "Source : inconnue, année", label = "Source")
                                        )
                            )), 
                          column(9,plotOutput("map"))                   
                 )),
                 tabPanel(title = "Analyse",
                          icon = icon(name = "bar-chart"),
                          tabsetPanel(
                            tabPanel(title = "CAH",
                                     icon = icon(name = "tree"),
                                     column(width = 6,
                                            selectInput(inputId = "cahVariables",
                                                        label = "Choose CAH variables",
                                                        choices = colnames(isolate(baseData$data)),
                                                        multiple = TRUE)),
                                     column(width = 6,
                                            sliderInput(inputId = "cahClusters",
                                                        label = "Number of clusters",min = 2, max = 10, value= 3)),
                                     checkboxInput("cahSettings", "More settings"),
                                     conditionalPanel(
                                       condition = "input.cahSettings == true",
                                       fluidRow(
                                         column(6, selectInput(inputId = "cahDistance",
                                                               selected = "euclidean",
                                                               multiple = FALSE,
                                                               choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan"),
                                                               label = "Distance measure")),
                                         column(6, selectInput(inputId = "cahMethod",
                                                               selected = "ward.D2",
                                                               multiple = FALSE,
                                                               choices = c("Min" = "single",
                                                                           "Max" = "complete",
                                                                           "Average" = "average",
                                                                           "Centroid" = "centroid",
                                                                           "Ward (D2)" = "ward.D2"),
                                                               label = "Agregation method"))
                                       )),
                                     checkboxInput("cahSave", "Save clusters"),
                                     conditionalPanel(condition = "input.cahSave == true",
                                                      fluidRow(
                                                        column(3, textInput(inputId = "cahNamePrefix", label = "Column Prefix", value = "")),
                                                        column(3, actionButton(inputId = "cahAddColumn", label = "Add Clusters to dataset"))
                                                      )),
                                     hr(),
                                     fluidRow(
                                       column(6, plotOutput(outputId = "cahTree")),
                                       column(6, plotOutput(outputId = "cahInertia"))),
                                     fluidRow(
                                       column(10,plotOutput(outputId = "cahProfiles")),
                                       column(2, checkboxInput(inputId = "cahScale", label = "Standardize", value = FALSE))
                                     )
                            ))
                 ),
                 tabPanel(title = "PCA", 
                          icon = icon(name = "stats", lib = "glyphicon", class = "fa-rotate-90"),
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
                 ),
                 tabPanel(title = "Régression",
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
                 )
               )
             )
    )
  )
)
