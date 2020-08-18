library(shiny)
library(DT)
library(shinyBS)
library(Rcpp)
library(RcppArmadillo)
library(ggplot2) 

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
                    # uiOutput('mytabs') # old
                    # #splitLayout(,)
                    tabsetPanel(
                      tabPanel("Scenarios", uiOutput('mytabs')),
                      tabPanel("Overview", plotOutput('sumtab'))
                    ) # endo of tabsetPanel
                  ), # endof mainPanel
                  fluid = T # layout is not fixed, default is T
                ) # endof sidebarLayout
                
)

#results_all = NULL

# Define server logic required to draw charts
server <- function(input, output, clientData, session) {
  
  
  # Default matrix values
  stage = c(1,2,3)
  entries = c(1000,100,10)
  years = c(1,1,2)
  locs = c(1,4,8)
  reps = c(1,2,3)
  error = c(1,1,1)
  h2 = c(0.5,0.5,0.5) # this is a calculated value
  
  # per-session object to store all results of this user session
  results_all <- NULL

  # updateH2 <- function(){
  #   for (i in stage)
  #   {
  #     print(paste("The index is",i))
  #     h2[i] = input$varG #/ ( input$varG + input$varGxY/years[i] + input$varGxL/years[i]*locs[i] + error[i]/years[i]*locs[i]*reps[i])
  #   }
  #   print(h2)
  # }
  
  yt = cbind(stage,entries,years,locs,reps,error,h2)
  # Using reactiveVales to add a server side set of variable observable and mutable at the same time
  yti <- reactiveValues(data = yt)
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(yti$data))
    {
      yti$data[i,7] = round(input$varG/(input$varG + input$varGxY/yti$data[i,3] + input$varGxL/(yti$data[i,3]*yti$data[i,4]) + yti$data[i,6]/(yti$data[i,3]*yti$data[i,4]*yti$data[i,5])), 3)
      # print(paste("H2 for stage", i, "is", yti$data[i,7]))
    }
  })
  
  # Render static table with total years, locs and plots
  # output$tYears = renderText({ sum(yti$data[,3]) })  # years
  # output$tLocs = renderText({ sum(yti$data[,4]) })  # locs
  # output$tPlots = renderText({ yti$data[1,2] })  # plots
  # cost_df = cbind(tYears, tLocs, tPlots)

  # function calculates Total Plots given a DT matrix as input
  totalPlots <- function(mtx = yt) {
    # print(nrow(mtx))
    # print(prod(mtx[1,1:4]))
    tp = 0
    for (i in 1:nrow(mtx))
      tp = tp + prod(mtx[i,1:4])
    print(tp)
  }
  # Display a table with costs calculated based on user input (stages etc.)
  output$cost_table = DT::renderDT(cbind(sum(yti$data[,3])+input$negen,sum(yti$data[,3]*yti$data[,4]), totalPlots(yti$data)), 
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
    new_h2 = input$varG / (input$varG + input$varGxY + input$varGxL + 1)
    yti$data = rbind(yti$data, c(length(yti$data[,1])+1,2,1,1,1,1,round(new_h2, 3)))
    
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
    
    stages = isolate(yti$data[,1]) # 
    entries = isolate(yti$data[,2]) # c(1000,100,10) 
    years = isolate(yti$data[,3]) # c(1,1,1)
    locs = isolate(yti$data[,4]) # c(1,4,8)
    reps = isolate(yti$data[,5]) # c(1,2,3)
    error = isolate(yti$data[,6]) # c(1,1,1)
    h2 = isolate(yti$data[,7])
    varieties = isolate(input$varieties)
    
    # store settings for summary plot TODO
    sumset_table = data.frame(stages, entries, years, locs, reps, error, h2)
    
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
    
    result = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
    
    # New Plot of ran result appearing in new tab
    # First save new plot in a variable before passing it to output
    nplot <- renderPlot({
      boxplot(t(result),
              xlab="Stage",
              ylab="Mean Genetic Value")
    })   # end of renderPlot
    
    # Build data frame with all scenario results to use in grouped boxplots overview tab
    # groupResults <- function(r){
    #   res = NULL
    #   for(i in 1:nrow(r)) 
    #   {
    #     res = cbind(res, rbind(Stage = as.integer(i), Value = r[i,]))
    #   }
    #  # print(res) 
    # }
    # myDF = groupResults(result)
    
    
    # Store results from all runs in a global matrix
    for(i in 1:nrow(result)) # 1:input$run_btn
    {
      results_all = cbind(results_all, rbind(Stage = i, Value = result[i,], Scenario = input$run_btn))
    }
    print(head(t(results_all)))
    print(tail(t(results_all)))
    
    # Global settings for all DTs in senario tabs
    sumset_DT = list( options = list(
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

    # Load content from file locally as if it was pasted here
    source('if_run_btn.r', local=TRUE)
    
    # TODO create a summary for each scenario / stage to place under boxplot
    # output$stages_summary1 = DT::renderDT(summary_settings)
  
    output$sumtab <- renderPlot({
      # result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
      #                      isolate(yti$data[,2]),isolate(yti$data[,3]),
      #                      isolate(yti$data[,4]),isolate(yti$data[,5]),
      #                      isolate(yti$data[,6]),isolate(input$varieties))
      # boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
      
      
      #boxplot(t(results_all))
      ggplot(as.data.frame(t(results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        geom_boxplot()
      
      # Dorcus multiple boxplots comparison code
      # Note that "Gain" is the mean of F1s
      # PYT = readRDS("PYT20.rds")
      # ggplot(PYT,aes(x=Crosses,y=Gain,fill=Parents))+
      #   geom_boxplot()+
      #   ylab("Gain (Relative to Mean)")+
      #   ggtitle("PYT Crossing Block (Year 20)")
      
      # First cbind results from all updated scenario runs and then plot
      #result_all = cbind(result1, result2, result3)
      
    })   # end of renderPlot
      
    
  }) # end of run button
  
  
} # endof server

# Run the application 
shinyApp(ui = ui, server = server)
