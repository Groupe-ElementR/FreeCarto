##############################  
#### Shiny User interface ####
##############################

# Shiny ui ----

shinyUI(fluidPage(
  # Application title
  titlePanel("Outil interactif de classification hiérarchique"),

  # Sidebar
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Accueil",
                 icon = icon(name = "home")),
        tabPanel(title = "Données",
                 icon = icon(name = "database"),
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
                                 choices = colnames(TNdeleg)[-c(1,2)], 
                                 selected = colnames(TNdeleg)[3]),
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
                   column(9,
                          plotOutput("map"))
                   
                 )),
        tabPanel(title = "Fouille",
                 icon = icon(name = "bar-chart")),
        tabPanel(title = "Graphiques",
                 icon = icon(name = "pencil-square-o"),
                 sidebarPanel(
                   selectInput(inputId = "xCol", label = "Variable X", 
                               selectize = TRUE, multiple = FALSE,
                               choices = colnames(iris), selected = colnames(iris)[1]),
                   selectInput(inputId = "yCol", label = "Variable Y", 
                               selectize = TRUE, multiple = FALSE,
                               choices = colnames(iris), selected = colnames(iris)[2]),
                   selectInput(inputId = "typeDist",
                               label = "Type de distance",
                               selectize = TRUE,
                               multiple = FALSE,
                               choices = c("Euclidienne" = "euclidean", "Manhattan" = "manhattan")),
                   selectInput(inputId = "typeCrit",
                               label = "Critère d'agrégation",
                               selectize = TRUE,
                               multiple = FALSE,
                               choices = c("Min" = "single", 
                                           "Max" = "complete", 
                                           "Moyenne" = "average", 
                                           "Barycentres" = "weighted", 
                                           "Ward" = "ward")),
                   sliderInput(inputId = "nbClus",
                               label = "Nombre de classes",
                               min = 1,
                               max = 12,
                               value = 4)
                 ),
                 mainPanel(plotOutput("plotxy"),
                 plotOutput("plottree"))),
        tabPanel(title = "Tableaux", 
                 icon = icon(name = "table"),
                 dataTableOutput("userdata"))
      )
    )
  )
)
