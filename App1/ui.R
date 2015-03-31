##############################  
#### Shiny User interface ####
##############################

# Shiny ui ----

shinyUI(fluidPage(
  titlePanel("FreeCarto"),
  tabsetPanel(
    tabPanel(title = "Accueil", icon = icon(name = "home")),
    tabPanel(title = "Données", icon = icon(name = "database"),
             tabsetPanel(
               tabPanel(title = "Table",
                        icon = icon(name = "table", lib = "font-awesome"),
                        dataTableOutput("fullDF")
               ),
               tabPanel(title = "Base Map",
                        icon = icon(name = "map-marker"),
                        plotOutput("baseMap")
               ),
               tabPanel(title = "Pivot",
                        icon = icon(name = "retweet"),
                        uiOutput('pivot')
               )
             )
    ),
    tabPanel(title = "Carto",
             icon = icon(name = "picture-o"),
             uiOutput('carto')),                     
    tabPanel(title = "Analyse",
             icon = icon(name = "bar-chart"),
             tabsetPanel(
               tabPanel(title = "CAH",
                        icon = icon(name = "tree"),
                        uiOutput('cah')),
               tabPanel(title = "PCA",
                        icon = icon(name = "stats", lib = "glyphicon", class = "fa-rotate-90"),
                        uiOutput("pca")),
               tabPanel(title = "Régression",
                        uiOutput('lm'))
             )
    )
  )
)
)
