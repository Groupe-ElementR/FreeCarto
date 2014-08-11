## Test extension module1

code <- modules$module1

code$inputs[[length(code$inputs) + 1]] <- expression(textInput("nouveauTexteInput", "Un nouveau texte input", value = ""))
code$outputs[[length(code$outputs) + 1]] <- expression(textOutput("nouveauTexteOutput"))
code$server[[length(code$server) + 1]] <- expression(output$nouveauTexteOutput <- renderText({input$nouveauTexteInput}))

modules$module2 <- code