### une deuxième classe

setClass("test2",
         contains = "test",
         slots = list("attr3" = "numeric"))

## print est inchangé

setMethod("plot",
          c(x = "test2"),
          function(x) hist(x@attr3))

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
                                       expression(plotOutput("histogramme"))
                        ),
                        server = list(
                          expression(output$resultat <- renderText({
                            print(new("test2", attr1 = input$attr1, attr2 = input$attr2))
                        })),
                          expression(output$histogramme <- renderPlot({
                            hist(rnorm(100, input$attr3Mean, input$attr3SD))
                          }))
                        )
)