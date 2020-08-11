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
                    
                    tags$h3("Create a new scenario"),
                    
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
                    
                    numericInput("negen", "Crossing/Selfing Years",
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
                    
                    # Economic cost summary output for scenario
                    #textOutput("tYears", inline = T),
                    #textOutput("tLocs", inline = T),
                    #textOutput("tPlots", inline = T),
                    tags$h4("Summary Cost"),
                    div( # CUSTOMISE div style for DT
                      DT::DTOutput("cost_table"),
                      style = "font-size: 85%; width: 100%"
                    ),
                    
                    tags$br(),
                    
                    actionButton("run_btn", "Run")
                    
                    
                    
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
  h2 = c(0.5,0.5,0.5)
  yt = cbind(stage,entries,years,locs,reps,error,h2)
  
  
  # Using reactiveVales to add a server side set of variable observable and mutable at the same time
  yti <- reactiveValues(data = yt)
  
  # Render static table with total years, locs and plots
  # output$tYears = renderText({ sum(yti$data[,3]) })  # years
  # output$tLocs = renderText({ sum(yti$data[,4]) })  # locs
  # output$tPlots = renderText({ yti$data[1,2] })  # plots
  # cost_df = cbind(tYears, tLocs, tPlots)

  # function calculates Total Plots given a DT matrix as input
  totalPlots <- function(mtx = yt) {
    print(nrow(mtx))
    print(prod(mtx[1,1:4]))
    tp = 0
    for (i in 1:nrow(mtx))
      tp = tp + prod(mtx[i,1:4])
    print(tp)
  }
  
  output$cost_table = DT::renderDT(cbind(sum(yti$data[,3])+input$negen,sum(yti$data[,4]), totalPlots(yti$data)), 
                                   options = list(
                                     searching = F, # no search box
                                     paginate = F,  # no num of pages
                                     lengthChange = F, # no show entries
                                     scrollX = T # horizontal slider
                                   ),
                                   rownames = F,
                                   colnames = c('Total Years', 'Total Locs', 'Total Plots'),
                                   server = F )
  
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
                                     colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', 'h2'),
                                     filter = "none",
                                     escape = FALSE,
                                     autoHideNavigation = TRUE,
                                     selection = "none",
                                     editable = list(target = "cell", disable = list(columns = c(0, 6))),
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
    print(yti$data[1,3:6])
    # calc h2 for this stage
    her = input$varG / ( input$varG + input$varGxY/yti$data[1,3] + input$varGxL/yti$data[1,3]*yti$data[1,4] + yti$data[1,6]/yti$data[1,3]*yti$data[1,4]*yti$data[1,5])
    yti$data = rbind(yti$data, c(length(yti$data[,1])+1,2,1,1,1,1,round(her, 3)))
    
    # replaceData(proxy, yti$data, resetPaging = FALSE)  # important 
    
  })
  
  observeEvent(input$delete_btn, {
    
    if (length(yti$data[,1])>2) # TV for >1 it crushes!
      yti$data = yti$data[1:length(yti$data[,1])-1,]
    
    # replaceData(proxy, yti$data, resetPaging = FALSE)  # important 
    
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$run_btn, {
    
    varG = isolate(input$varG)
    varGxL = isolate(input$varGxL)
    varGxY = isolate(input$varGxY)
    
    entries = isolate(yti$data[,2]) # c(1000,100,10) 
    years = isolate(yti$data[,3]) # c(1,1,1)
    locs = isolate(yti$data[,4]) # c(1,4,8)
    reps = isolate(yti$data[,5]) # c(1,2,3)
    error = isolate(yti$data[,6]) # c(1,1,1)
    h2 = isolate(yti$data[,7])
    varieties = isolate(input$varieties)
    
    # store settings for summary plot TODO
    sumset_table = data.frame(entries, years, locs, reps, error, h2)
    
    # Create a new tab in the UI every time Run is pressed
    output$mytabs = renderUI({
      nTabs = input$run_btn # use this value also as the tabs counter
      # TV myTabs = lapply(paste('Scenario', 1: nTabs), tabPanel) 
      myTabs = lapply(1: nTabs, function(i){
        tabPanel(paste('Scenario', sep = " ", i),
                 plotOutput(paste('cyPlot', sep = "", i)),
                 # add a summary of settings used for this scenario. Could be a DT
                 DT::DTOutput(paste('stages_summary', sep = "", i)),
                 # maybe add an update scenario button
                 actionButton(paste("update_btn", sep = "", i), "Update")
        )
      }) 
      # TV do.call(tabsetPanel, myTabs)
      do.call(tabsetPanel, myTabs)
    })
    
    print(paste("Run", input$run_btn))
    
    # Plot results in latest created tab
    # TV WORKS    output$cyPlot1 <- renderPlot({
    # First save new plot in a variable before passing it to output
    nplot <- renderPlot({

      result = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
      boxplot(t(result),
              xlab="Stage",
              ylab="Mean Genetic Value")
    })   # end of renderPlot
    
    #reactDT <- reactiveValues(data = yt)
    # Global settings for all DTs in senarios
    sumset_DT = list(sumset_table,
                      options = list(
                        searching = F, # no search box
                        paginate = F,  # no num of pages
                        lengthChange = F, # no show entries
                        scrollX = T # horizontal slider
                      ),
                      class = "cell-border, compact, hover", 
                      rownames = F, #TRUE,
                      colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', 'h2'),
                      filter = "none",
                      escape = FALSE,
                      autoHideNavigation = TRUE,
                      selection = "none",
                      editable = list(target = "cell", disable = list(columns = c(0, 6))),
                      server = TRUE) # server = F doesn't work with replaceData() cell editing

    
    # source('plotInTab.R')
    
    if (input$run_btn == 1)
    {
      output$cyPlot1 <- nplot
      reactDT1 <- reactiveValues(data = sumset_table)
      output$stages_summary1 = DT::renderDT(reactDT1$data, options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
      
      # Update editable DT through a proxy DT on cell edit event
      proxy = dataTableProxy('stages_summary1')
      #
      observeEvent(input$stages_summary1_cell_edit, {
        info = input$stages_summary1_cell_edit
        i = info$row
        j = info$col # +1 required when rownames = F in DT
        v = info$value
        str(info)
        # Character string needs to be coerced to same type as target value. Here as.integer()
        reactDT1$data[i, j] = DT::coerceValue(v, reactDT1$data[i, j])
        # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
        replaceData(proxy, reactDT1$data, resetPaging = FALSE)  # important 
      })
      
      # Execute runScenario() for the current settings
      observeEvent(input$update_btn1, {
        
        varG = isolate(input$varG)
        varGxL = isolate(input$varGxL)
        varGxY = isolate(input$varGxY)
        
        entries = isolate(reactDT1$data[,1]) # c(1000,100,10) 
        years = isolate(reactDT1$data[,2]) # c(1,1,1)
        locs = isolate(reactDT1$data[,3]) # c(1,4,8)
        reps = isolate(reactDT1$data[,4]) # c(1,2,3)
        error = isolate(reactDT1$data[,5]) # c(1,1,1)
        varieties = isolate(input$varieties)
        
        output$cyPlot1 <- renderPlot({
          
          result = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
          boxplot(t(result),
                  xlab="Stage",
                  ylab="Mean Genetic Value")
        })   # end of renderPlot
        
      }) # endof update btn
    }
    else if (input$run_btn == 2)
    {
      output$cyPlot2 <- nplot
      reactDT2 <- reactiveValues(data = sumset_table)
      output$stages_summary2 = DT::renderDT(reactDT2$data, options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
      # Update editable DT through a proxy DT on cell edit event
      proxy = dataTableProxy('stages_summary2')
      #
      observeEvent(input$stages_summary2_cell_edit, {
        info = input$stages_summary2_cell_edit
        i = info$row
        j = info$col # +1 required when rownames = F in DT
        v = info$value
        str(info)
        # Character string needs to be coerced to same type as target value. Here as.integer()
        reactDT2$data[i, j] = DT::coerceValue(v, reactDT2$data[i, j])
        # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
        replaceData(proxy, reactDT2$data, resetPaging = FALSE)  # important 
      })
      
      # Execute runScenario() for the current settings
      observeEvent(input$update_btn2, {
        output$cyPlot2 <- renderPlot({
          result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                               isolate(reactDT2$data[,1]),isolate(reactDT2$data[,2]),
                               isolate(reactDT2$data[,3]),isolate(reactDT2$data[,4]),
                               isolate(reactDT2$data[,5]),isolate(input$varieties))
          boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
        })   # end of renderPlot
      }) # endof update btn      
    }
    else if (input$run_btn == 3)
    {
      output$cyPlot3 <- nplot
      reactDT3 <- reactiveValues(data = sumset_table)
      output$stages_summary3 = DT::renderDT(reactDT3$data, options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
      # Update editable DT through a proxy DT on cell edit event
      proxy = dataTableProxy('stages_summary3')
      #
      observeEvent(input$stages_summary3_cell_edit, {
        info = input$stages_summary3_cell_edit
        i = info$row
        j = info$col # +1 required when rownames = F in DT
        v = info$value
        str(info)
        # Character string needs to be coerced to same type as target value. Here as.integer()
        reactDT3$data[i, j] = DT::coerceValue(v, reactDT3$data[i, j])
        # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
        replaceData(proxy, reactDT3$data, resetPaging = FALSE)  # important 
      })
      
      # Execute runScenario() for the current settings
      observeEvent(input$update_btn3, {
        output$cyPlot3 <- renderPlot({
          result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                               isolate(reactDT3$data[,1]),isolate(reactDT3$data[,2]),
                               isolate(reactDT3$data[,3]),isolate(reactDT3$data[,4]),
                               isolate(reactDT3$data[,5]),isolate(input$varieties))
          boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
        })   # end of renderPlot
      }) # endof update btn  
    }
    else if (input$run_btn == 4)
    {
      output$cyPlot4 <- nplot
      output$stages_summary4 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
    }
    else if (input$run_btn == 5)
    {
      output$cyPlot5 <- nplot
      output$stages_summary5 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
    }
    else if (input$run_btn == 6)
    {
      output$cyPlot6 <- nplot
      output$stages_summary6 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
    }
    else if (input$run_btn == 7)
    {
      output$cyPlot7 <- nplot
      output$stages_summary7 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
    }
    else if (input$run_btn == 8)
    {
      output$cyPlot8 <- nplot
      output$stages_summary8 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
    }
    else if (input$run_btn == 9)
    {
      output$cyPlot9 <- nplot
      output$stages_summary9 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
    }
    else if (input$run_btn == 10)
    {
      output$cyPlot10 <- nplot
      output$stages_summary10 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
    }
    else
      assign(paste('output$cyPlot', sep = "", input$run_btn), nplot) # DOESNOT WORK
    
    # TODO create a summary for each scenario / stage to place under boxplot
    # output$stages_summary1 = DT::renderDT(summary_settings)

    
  }) # end of run button
  
  
} # endof server

# Run the application 
shinyApp(ui = ui, server = server)
