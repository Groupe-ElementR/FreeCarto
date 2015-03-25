##############################  
#### Shiny User interface ####
##############################

# Shiny ui ----

shinyUI(fluidPage(
  titlePanel("FreeCarto"),
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
                                  sliderInput(inputId = "cahClusters",
                                              label = "Number of clusters",
                                              min = 2, max = 15, value= 3)),
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
                           
                           ),
                  tabPanel(title = "ACP", 
                           icon = icon(name = "stats", lib = "glyphicon", class = "fa-rotate-90")
                           
                           ),
                  tabPanel(title = "Régression")))
      )
    )
)
