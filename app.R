library(shiny)
library(DT)
library(shinyBS)
library(Rcpp)
sourceCpp("Engine.cpp")

# Default matrix values
stage = c(1,2,3)
entries = c(1000,100,10)
years = c(1,1,1)
locs = c(1,4,8)
reps = c(1,2,3)
error = c(1,1,1)
yti = cbind(stage,entries,years,locs,reps,error)
# yti <- matrix(data = 1:ntraits, nrow = 3, ncol = 6)



ui <- fluidPage(title = "Cycle Scenarios",
                
                titlePanel("Cycle Scenarios"),            
                
                sidebarLayout(
                    
                    sidebarPanel(

                        tags$h4("Variance"),
                        # Set Genetic Variance 
                        numericInput("varG", "Genetic:",
                                     min = 0, max = 100, value = 1, step = 0.1, width = '80px'), 
                        # Add tooltip with instructions/info
                        bsTooltip("varG", "Genetic variance. The variance between entries in the first stage of yield trials.",
                                  "right", "hover", NULL),
                        # Set GxL(Y) Variance 
                        numericInput("varGxL", "GxL(Y):",
                                     min = 0, max = 100, value = 1, step = 0.1, width = '80px'), 
                        # Add tooltip with instructions/info
                        bsTooltip("varGxL", "Genotype-by-location nested in year interaction variance. This value is equivalent to the sum of genotype-by-location interaction variance and genotype-by-location-by-year interaction varaince.",
                                  "right", "hover", NULL),
                        # Set GxY Variance 
                        numericInput("varGxY", "GxY:",
                                     min = 0, max = 100, value = 1, step = 0.1, width = '80px'), 
                        # Add tooltip with instructions/info
                        bsTooltip("varGxY", "Genotype-by-year interaction variance.",
                                  "right", "hover", NULL),
                        
                        numericInput("negen", "Early Generations",
                                     min = 0, max = 100, value = 4, step = 1, width = '80px'), 
                        # Add tooltip with instructions/info
                        bsTooltip("negen", "GNumber of early generation years. This phase of the breeding program is modeled without selection.",
                                  "right", "hover", NULL),
                        
                        tags$h4("Yield Trials"),
                        DT::DTOutput('stages_table'),
                        actionButton("add_btn", "Add"),
                        actionButton("delete_btn", "Delete"),
    
                  
                        numericInput("varieties", "Varieties",
                                     min = 1, max = 10, value = 1, step = 1, width = '80px'), 
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
                            ) # endof tabPanel
                        )
                    ), # endof mainPanel
                ), # endof sidebarLayout
                
)


# Define server logic required to draw charts
server <- function(input, output, clientData, session) {
  
  # Using reactiveVal to add a server side variable observable and mutable at the same time
  ytiDT <- reactiveVal(yti)
  yti <- reactiveVal(yti)

  
  # Observe Button Clicks for adding or removing rows (stages) from the DT
  observeEvent(input$add_btn, {
    # yti = rbind(yti, c(length(yti[,1])+1,2,1,1,1,1))
    yti(rbind(yti(), c(length(yti()[,1])+1,2,1,1,1,1)))
    
    # Manipulate DT settings
    ytiDT(datatable(yti(), 
                      class = "cell-border, compact, hover", 
                      rownames = TRUE,
                      colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error'),
                      filter = "none",
                      escape = FALSE,
                      autoHideNavigation = TRUE,
                      selection = "single",
                      editable = list(target = "all", disable = list(columns = 0)),
    ))
  })
  
  observeEvent(input$delete_btn, {
    # yti = yti[1:length(yti[,1])-1,]
    if (length(yti()[,1])>2) # TV for >1 it crushes!
        yti(yti()[1:length(yti()[,1])-1,])
    
    # Manipulate DT settings
    ytiDT(datatable(yti(), 
                  class = "cell-border, compact, hover", 
                  rownames = TRUE,
                  colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error'),
                  filter = "none",
                  escape = FALSE,
                  autoHideNavigation = TRUE,
                  selection = "single",
                  editable = list(target = "all", disable = list(columns = 0)),
    ))
  })
  

  # Render the manipulated table in Shiny  
  output$stages_table = DT::renderDT(ytiDT(), server = FALSE)
    
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
        
        if(input$button)
        {
          example = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
          boxplot(t(example),
                  xlab="Stage",
                  ylab="Mean Genetic Value")
        }
        
        #TV      observeEvent(input$button, {
        #TV           example = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
        #TV           boxplot(t(example),
        #TV                 xlab="Stage",
        #TV                 ylab="Mean Genetic Value")
        #TV      })

    })    
    
    # Second Tab    
  #  output$pcPlot <- renderParcoords({
      

   # })   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
