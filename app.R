library(shiny)
# library(DT)
# library(shinyBS)
# library(Rcpp)
# library(RcppArmadillo)
# library(ggplot2) 
# library(shinyjs)

# sourceCpp("Engine.cpp")

source('ui.r', local = TRUE)
source('server.r')

# Run the application 
shinyApp(ui = ui, server = server)