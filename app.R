library(shiny)
library(shinyMatrix)
library(shinyBS)
library(Rcpp)
sourceCpp("Engine.cpp")

# Default matrix entries
stage = c(1,2,3)
entries = c(1000,100,10)
years = c(1,1,1)
locs = c(1,4,8)
reps = c(1,2,3)
error = c(1,1,1)
yti = cbind(stage,entries,years,locs,reps,error)
# yti <- matrix(data = 1:ntraits, nrow = 3, ncol = 6)



ui <- fluidPage(title = "Cycle Scenario Comparison",
                
                titlePanel("Cycle Scenario Comparison"),            
                
                sidebarLayout(
                    
                    sidebarPanel(

                        tags$h4("Variance"),
                        # Set Genetic Variance 
                        numericInput("varG", "Genetic:",
                                     min = 0, max = 100, value = 1, step = 0.1), 
                        # Add tooltip with instructions/info
                        bsTooltip("varG", "Genetic variance. The variance between entries in the first stage of yield trials.",
                                  "right", "hover", NULL),
                        # Set GxL(Y) Variance 
                        numericInput("varGxL", "GxL(Y):",
                                     min = 0, max = 100, value = 1, step = 0.1), 
                        # Add tooltip with instructions/info
                        bsTooltip("varGxL", "Genotype-by-location nested in year interaction variance. This value is equivalent to the sum of genotype-by-location interaction variance and genotype-by-location-by-year interaction varaince.",
                                  "right", "hover", NULL),
                        # Set GxY Variance 
                        numericInput("varGxY", "GxY:",
                                     min = 0, max = 100, value = 1, step = 0.1), 
                        # Add tooltip with instructions/info
                        bsTooltip("varGxY", "Genotype-by-year interaction variance.",
                                  "right", "hover", NULL),
                        
                        tags$h4("Early Generations"),
                        numericInput("negen", "Early Generations",
                                     min = 0, max = 100, value = 4, step = 1), 
                        # Add tooltip with instructions/info
                        bsTooltip("negen", "GNumber of early generation years. This phase of the breeding program is modeled without selection.",
                                  "right", "hover", NULL),
                        
                        tags$h4("Yield Trials"),
                        matrixInput(
                            "tableYT",
                            value = yti,
                            rows = list(
                                extend = TRUE
                            ),
                            cols = list(
                                names = TRUE
                            ),
                            class = "numeric"
                        ),
    
                        tags$h4("Varieties"),
                        numericInput("varieties", "Varieties",
                                     min = 1, max = 10, value = 1, step = 1), 
                        # Add tooltip with instructions/info
                        bsTooltip("varieties", "The final number of selected entries. Must be smaller than or equal to the number of entries in the last stage.",
                                  "right", "hover", NULL),
                                            
                        actionButton("button", "Run"),
                        

                        
                    ), # endof SidebarPanel
                    # Show plots and charts 
                    mainPanel(
                        tabsetPanel(
                            tabPanel(title = "Scenario 1",
                                     plotOutput("scatPlot1")
                            ), # endof tabPanel   
                            tabPanel(title = "Scenario 2",
                                    # parcoordsOutput("pcPlot")
                            ), # endof tabPanel
                            tabPanel(title = "Help",
                                     tags$h2("Help"),
                                     tags$br(),
                                     tags$p("Comparing selection indices is important in breeding.
                     This application aims to help breeders explore the effect
                     of three indices in the selection of individuals."),
                                     tags$p("These indices are:"),
                                     tags$ol(
                                         tags$li("Base index"),
                                         tags$li("Heritability index"),
                                         tags$li("Smith-Hazel index")
                                     ),
                                     tags$br(),
                                     tags$p("To use the app, edit the matrices on the left sidebar
                     and view how your changes affect the selection of the indices.")
                            ) # endof tabPanel             
                        )
                    ), # endof mainPanel
                ), # endof sidebarLayout
                
)


# Define server logic required to draw charts
server <- function(input, output, clientData, session) {
    
    # TV     observe({
    # TV         c_num <- input$control_num
    # TV         #updateNumericInput(session, "i_traits", value = c_num)
    # TV         updateMatrixInput(session, "sampleG", value = 0.1*diag(c_num) + 0.9)
    # TV         updateMatrixInput(session, "sampleE", value = diag(c_num))
    # TV         updateMatrixInput(session, "sampleW", value = matrix(data = 1:c_num, nrow = 1, ncol = c_num))
    # TV     })

    
    # First Tab 
    output$scatPlot1 <- renderPlot({
        
        varG = input$varG
        varGxL = input$varGxL
        varGxY = input$varGxY
        
        entries = c(100,10)
        years = c(1,1)
        locs = c(1,2)
        reps = c(1,2)
        error = c(1,1)
        varieties = 1
        
        example = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
        boxplot(t(example),
                xlab="Stage",
                ylab="Mean Genetic Value")
        
    })    
    
    # Second Tab    
  #  output$pcPlot <- renderParcoords({
      

   # })   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
