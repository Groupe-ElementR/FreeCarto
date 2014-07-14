## modules : un nom, un ou des input, un ou des outputs, une classe, des méthodes

setClass("test",
         slots = list("attr1" = "numeric",
                      "attr2" = "character"))

setGeneric("print")

setMethod("print",
          c(x = "test"),
          function(x) paste('Un objet de classe "test", dont l\'attribut 1 est', x@attr1, 'et l\'attribut 2 est', x@attr2))


modules <- list(base = list(inputs = expression(numericInput("attr1",
                                                             "valeur de l'attribut 1 :",
                                                             value = 0),
                                                textInput("attr2",
                                                          "valeur de l'attribut 2 :",
                                                          value = "")),
                            outputs = list(resultat = expression(textOutput("resultat"))
                              ),
                            server = expression(output$resultat <- renderText({
                              print(new("test", attr1 = input$attr1, attr2 = input$attr2))
                            }))
                            )
                )

## attention : comme les modules sont sourcés dans global.R, ils doivent être les mêmes pour toutes les instances de l'application.
## cependant, on peut peut-être imaginer de charger de nouveaux modules "à la volée", en cours d'exécution -- mais est-ce vraiment nécessaire ?

source("./modules/module1.R")
