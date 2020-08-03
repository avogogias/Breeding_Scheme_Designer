library(shiny)
library(DT)
library(shinyBS)
library(Rcpp)
library(RcppArmadillo)

sourceCpp("Engine.cpp")

ui <- fluidPage(title = "Cycle Scenarios",
                
               # theme = "bootstrap.css",
                
                titlePanel("Cycle Scenarios"),            
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    width = 4,
                    
                    tags$h4("Variance"),
                    
                    # Make divs appear in one line
                    bootstrapPage(
                      # Set Genetic Variance 
                      div(style="display:inline-block",numericInput("varG", "Genetic:",
                                                                    min = 0, max = 100, value = 1, step = 0.1, width = '80px')),
                      # Set GxL(Y) Variance 
                      div(style="display:inline-block",numericInput("varGxL", "GxL(Y):",
                                                                    min = 0, max = 100, value = 1, step = 0.1, width = '80px')),
                      # Set GxY Variance 
                      div(style="display:inline-block",numericInput("varGxY", "GxY:",
                                                                    min = 0, max = 100, value = 1, step = 0.1, width = '80px'))
                    ),
                    
                    # Add tooltip with instructions/info
                    bsTooltip("varG", "Genetic variance. The variance between entries in the first stage of yield trials.",
                              "right", "hover", NULL),
                    # Add tooltip with instructions/info
                    bsTooltip("varGxL", "Genotype-by-location nested in year interaction variance. This value is equivalent to the sum of genotype-by-location interaction variance and genotype-by-location-by-year interaction varaince.",
                              "right", "hover", NULL),
                    # Add tooltip with instructions/info
                    bsTooltip("varGxY", "Genotype-by-year interaction variance.",
                              "right", "hover", NULL),
                    
                    numericInput("negen", "Early Generations",
                                 min = 0, max = 100, value = 4, step = 1, width = '80px'), 
                    # Add tooltip with instructions/info
                    bsTooltip("negen", "GNumber of early generation years. This phase of the breeding program is modeled without selection.",
                              "right", "hover", NULL),
                    
                    tags$h4("Yield Trials"),
                    div( # CUSTOMISE div style for DT
                      DT::DTOutput("stages_table"),
                      style = "font-size: 85%; width: 100%"
                      ),
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
                    uiOutput('mytabs')
                  ) # endof mainPanel
                ) # endof sidebarLayout
                
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
  yt = cbind(stage,entries,years,locs,reps,error)
  
  
  # Using reactiveVales to add a server side set of variable observable and mutable at the same time
  yti <- reactiveValues(data = yt)
  
  # Render DT with default data entries
  output$stages_table = DT::renderDT(yti$data, 
                                     options = list(
                                       searching = F, # no search box
                                       paginate = F,  # no num of pages
                                       lengthChange = F, # no show entries
                                       scrollX = T # horizontal slider
                                     ),
                                     class = "cell-border, compact, hover", 
                                     rownames = F, #TRUE,
                                     colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error'),
                                     filter = "none",
                                     escape = FALSE,
                                     autoHideNavigation = TRUE,
                                     selection = "none",
                                     editable = list(target = "cell", disable = list(columns = 0)),
                                     server = TRUE) # server = F doesn't work with replaceData() cell editing
  
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_table')
  #
  observeEvent(input$stages_table_cell_edit, {
    info = input$stages_table_cell_edit
    i = info$row
    j = info$col + 1 # +1 required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    yti$data[i, j] = DT::coerceValue(v, yti$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, yti$data, resetPaging = FALSE)  # important 
  })
  
  ### Reset table
  #TV  observeEvent(reset(), {
  #TV  yti$data <- yt # your default data
  #TV})
  

  
  # Observe Button Clicks for adding or removing rows (stages) from the DT
  observeEvent(input$add_btn, {
    
    yti$data = rbind(yti$data, c(length(yti$data[,1])+1,2,1,1,1,1))
    
    # replaceData(proxy, yti$data, resetPaging = FALSE)  # important 
    
  })
  
  observeEvent(input$delete_btn, {
    
    if (length(yti$data[,1])>2) # TV for >1 it crushes!
      yti$data = yti$data[1:length(yti$data[,1])-1,]
    
    # replaceData(proxy, yti$data, resetPaging = FALSE)  # important 
    
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$run_btn, {
    
    # Create a new tab in the UI every time Run is pressed
    output$mytabs = renderUI({
      nTabs = input$run_btn # use this value also as the tabs counter
      # TV myTabs = lapply(paste('Scenario', 1: nTabs), tabPanel) 
      herTabs = lapply(1: nTabs, function(i){
        tabPanel(paste('Scenario', sep = " ", i),
                 plotOutput(paste('cyPlot', sep = "", i))
        )
      }) 
      # TV do.call(tabsetPanel, myTabs)
      do.call(tabsetPanel, herTabs)
    })
    
    print(input$run_btn)
    
    # Plot results in latest created tab
    # TV WORKS    output$cyPlot1 <- renderPlot({
    # First save new plot in a variable before passing it to output
    nplot <- renderPlot({
      varG = isolate(input$varG)
      varGxL = isolate(input$varGxL)
      varGxY = isolate(input$varGxY)
      
      entries = isolate(yti$data[,2]) # c(1000,100,10) 
      years = isolate(yti$data[,3]) # c(1,1,1)
      locs = isolate(yti$data[,4]) # c(1,4,8)
      reps = isolate(yti$data[,5]) # c(1,2,3)
      error = isolate(yti$data[,6]) # c(1,1,1)
      varieties = isolate(input$varieties)
      
      example = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
      boxplot(t(example),
              xlab="Stage",
              ylab="Mean Genetic Value")
    })   # end of renderPlot
    
    
    if (input$run_btn == 1)
      output$cyPlot1 <- nplot
    else if (input$run_btn == 2)
      output$cyPlot2 <- nplot
    else if (input$run_btn == 3)
      output$cyPlot3 <- nplot
    else if (input$run_btn == 4)
      output$cyPlot4 <- nplot
    else if (input$run_btn == 5)
      output$cyPlot5 <- nplot
    else if (input$run_btn == 6)
      output$cyPlot6 <- nplot
    else if (input$run_btn == 7)
      output$cyPlot7 <- nplot
    else if (input$run_btn == 8)
      output$cyPlot8 <- nplot
    else if (input$run_btn == 9)
      output$cyPlot9 <- nplot
    else if (input$run_btn == 10)
      output$cyPlot10 <- nplot
    else if (input$run_btn == 11)
      output$cyPlot11 <- nplot
    else if (input$run_btn == 12)
      output$cyPlot12 <- nplot
    else if (input$run_btn == 13)
      output$cyPlot13 <- nplot
    else if (input$run_btn == 14)
      output$cyPlot14 <- nplot
    else if (input$run_btn == 15)
      output$cyPlot15 <- nplot
    else if (input$run_btn == 16)
      output$cyPlot16 <- nplot
    else if (input$run_btn == 17)
      output$cyPlot17 <- nplot
    else if (input$run_btn == 18)
      output$cyPlot18 <- nplot
    else if (input$run_btn == 19)
      output$cyPlot19 <- nplot
    else
      assign(paste('output$cyPlot', sep = "", input$run_btn), nplot) # DOESNOT WORK
    
    print(input$mytabs)
    
  }) # end of run button
  
  
} # endof server

# Run the application 
shinyApp(ui = ui, server = server)
