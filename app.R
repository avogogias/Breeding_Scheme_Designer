library(shiny)
library(DT)
library(shinyBS)
library(Rcpp)
sourceCpp("Engine.cpp")





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
                        DT::DTOutput("stages_table"),
                        # Add tooltip with instructions/info
                        # bsTooltip("stages_table", "Number of entries. Must be smaller than or equal to the number of entries in the previous stage.
                        #                           Number of years. Increasing this value will increase heritability by decreasing variation due to GxY, GxL(Y) and plot error.
                        #                           Number of locations. Increasing this value will increase heritability by decreasing variation due to GxL(Y) and plot error.
                        #                           Number of replications. Increasing this value will increase heritability by decreasing variation due to plot error.",
                        #                           "right", "hover", NULL),
                        actionButton("add_btn", "Add"),
                        actionButton("delete_btn", "Delete"),
    
                  
                        numericInput("varieties", "Varieties",
                                     min = 1, max = 10, value = 1, step = 1, width = '80px'), 
                        # Add tooltip with instructions/info
                        bsTooltip("varieties", "The final number of selected entries. Must be smaller than or equal to the number of entries in the last stage.",
                                  "right", "hover", NULL),
                                            
                        actionButton("run_btn", "Run"),
                        

                        
                    ), # endof SidebarPanel
                    # Show plots and charts 
                    mainPanel(
                      uiOutput('mytabs'), 
                        tabsetPanel(
                            tabPanel(title = "Scenarios",
                                     plotOutput("scatPlot1")
                            ) # endof tabPanel   
          #                  tabPanel(title = "Help",
                                    # parcoordsOutput("pcPlot")
          #                  ) # endof tabPanel
                        ) # endof tabsetPanel
                    ), # endof mainPanel
                ), # endof sidebarLayout
                
)


# Define server logic required to draw charts
server <- function(input, output, clientData, session) {
  
  
  # Default matrix values
  stage = c(1,2,3)
  entries = c(1000,100,10)
  years = c(1,1,1)
  locs = c(1,4,8)
  reps = c(1,2,3)
  error = c(1,1,1)
  yti = cbind(stage,entries,years,locs,reps,error)
  
  # Default rendering
  ytiDT = datatable(yti, 
            class = "cell-border, compact, hover", 
            rownames = TRUE,
            colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error'),
            filter = "none",
            escape = FALSE,
            autoHideNavigation = TRUE,
            selection = "none",
            editable = list(target = "cell", disable = list(columns = 0)),
  )
  # Render the manipulated table in Shiny  
  output$stages_table = DT::renderDT(ytiDT, server = FALSE)
  
  # Using reactiveVal to add a server side variable observable and mutable at the same time
  #ytiR <- reactiveVal(yti)
  yti <- reactiveVal(yti)
  
  # Observe Button Clicks for adding or removing rows (stages) from the DT
  observeEvent(input$add_btn, {
   #  yti = rbind(yti, c(length(yti[,1])+1,2,1,1,1,1))
   # ytiR(rbind(yti, c(length(yti[,1])+1,2,1,1,1,1)))
   
    yti(rbind(yti(), c(length(yti()[,1])+1,2,1,1,1,1)))
     
    # Manipulate DT settings
    ytiDT = datatable(yti(), 
                      class = "cell-border, compact, hover", 
                      rownames = TRUE,
                      colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error'),
                      filter = "none",
                      escape = FALSE,
                      autoHideNavigation = TRUE,
                      selection = "none",
                      editable = list(target = "cell", disable = list(columns = 0)),
    )
    # Render the manipulated table in Shiny after row add
    output$stages_table = DT::renderDT(ytiDT, server = FALSE)
  })
  
  observeEvent(input$delete_btn, {
    # yti = yti[1:length(yti[,1])-1,]
    if (length(yti()[,1])>2) # TV for >1 it crushes!
        yti(yti()[1:length(yti()[,1])-1,])
    
    # Manipulate DT settings
    ytiDT = datatable(yti(), 
                  class = "cell-border, compact, hover", 
                  rownames = TRUE,
                  colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error'),
                  filter = "none",
                  escape = FALSE,
                  autoHideNavigation = TRUE,
                  selection = "none",
                  editable = list(target = "cell", disable = list(columns = 0)),
    )
    # Render the manipulated table in Shiny after row delete 
    output$stages_table = DT::renderDT(ytiDT, server = FALSE)
  })
  

  # Render the manipulated table in Shiny  
 # output$stages_table = DT::renderDT(ytiR(), server = FALSE)
    
  
  observeEvent(input$run_btn, {
    # Create a new tab
    output$mytabs = renderUI({
      nTabs = input$run_btn # use this value as a tabs counter
      myTabs = lapply(paste('Scenario', 1: nTabs), tabPanel) 
      do.call(tabsetPanel, myTabs)
    })
    
    
#TV***    tabPanel(title = "Scenarios", plotOutput("scatPlot1"))

    
    
    # Plot results in created tab
    output$scatPlot1 <- renderPlot({
      
      varG = isolate(input$varG)
      varGxL = isolate(input$varGxL)
      varGxY = isolate(input$varGxY)
      
      entries = c(1000,100,10) # ytiR()[,1]
      years = c(1,1,1)
      locs = c(1,4,8)
      reps = c(1,2,3)
      error = c(1,1,1)
      varieties = isolate(input$varieties)
      
      example = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
      boxplot(t(example),
              xlab="Stage",
              ylab="Mean Genetic Value")

    })   
  })
  
 

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
