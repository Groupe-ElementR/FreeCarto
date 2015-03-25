##############################  
#### Shiny User interface ####
##############################

# Shiny ui ----

shinyUI(fluidPage(
  # Application title
  titlePanel("Outil interactif de classification hiérarchique"),
  
  # Sidebar
    # Show a plot of the generated distribution
      actionButton('blob', label = "Blob"),
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
                  tabPanel(title = "Régression"))),
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
