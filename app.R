library(shiny)

source('ui.r', local = TRUE)
source('server.r')

# Run the application
shinyApp(ui = ui, server = server)
