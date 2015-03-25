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
                 icon = icon(name = "picture-o")
                 
                 ),
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