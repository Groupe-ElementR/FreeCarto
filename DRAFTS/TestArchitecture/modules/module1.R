### une deuxième classe

library(ggvis)

setClass("test2",
         contains = "test",
         slots = list("attr3" = "numeric"))

## print est inchangé

setMethod("plot",
          c(x = "test2"),
          function(x) hist(rnorm(100, x@attr3[1], x@attr3[2])))

setGeneric("plotD3", 
           def = function(obj) standardGeneric("plotD3"))

setMethod("plotD3", 
          signature = c(obj = "test2"),
          definition = function(obj) {
            df <- data.frame(x = rnorm(100, obj@attr3[1], obj@attr3[2]))
            df %>% ggvis(~x) %>% layer_histograms()
          })

modules$module1 <- list(inputs = expression(numericInput("attr1",
                                                         "valeur de l'attribut 1 :",
                                                         value = 0),
                                            textInput("attr2",
                                                      "valeur de l'attribut 2 :",
                                                      value = ""),
                                            numericInput("attr3Mean",
                                                         "Moyenne de l'attribut 3",
                                                         value = 0),
                                            numericInput("attr3SD",
                                                         "Écart-type de l'attrbut 3",
                                                         value = 1)),
                        outputs = list(expression(textOutput("resultat")),
                                       expression(plotOutput("histogramme")),
                                       expression(ggvisOutput("histD3"))
                        ),
                        server = list(
                          expression(objet <- reactive(new("test2", attr1 = input$attr1, attr2 = input$attr2, attr3 = c(ifelse(is.null(input$attr3Mean), 0, input$attr3Mean), ifelse(is.null(input$attr3SD), 1, input$attr3SD))))),
                          expression(output$resultat <- renderText({
                            print(objet())
                          })),
                          expression(output$histogramme <- renderPlot({
                            plot(objet())
                          })),
                          expression({
                            p <- plotD3(objet())
                            bind_shiny(p, "histD3")
                         })
                        )
)