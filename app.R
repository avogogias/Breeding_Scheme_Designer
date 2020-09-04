library(shiny)
library(DT)
library(shinyBS)
library(Rcpp)
library(RcppArmadillo)
library(ggplot2) 

sourceCpp("Engine.cpp")

ui <- fluidPage(title = "Cycle Scenarios",
                
               # theme = "bootstrap.css", # CSS theme file could be added here
               withMathJax(), # display mathematical equations using LaTeX format
                
                titlePanel("Cycle Scenarios"),            
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    width = 4,
                    
                    tags$h3("Create a new scenario"),
                    
                    # User-defined scenario ID could be typed using the following:
                    # textInput("divID", "Enter a unique ID for your scenario:", ""),
                    # helpText("Leave the text input blank for automatically unique IDs."),
                    
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
                    div( # CUSTOMISE div CSS style for DT
                      DT::DTOutput("stages_table"),
                      style = "font-size: 85%; width: 100%"
                      ),
                    # Add tooltip with instructions/info
                    # bsTooltip("stages_table", "Number of entries. Must be smaller than or equal to the number of entries in the previous stage.
                    #                           Number of years. Increasing this value will increase heritability by decreasing variation due to GxY, GxL(Y) and plot error.
                    #                           Number of locations. Increasing this value will increase heritability by decreasing variation due to GxL(Y) and plot error.
                    #                           Number of replications. Increasing this value will increase heritability by decreasing variation due to plot error.",
                    #                           "right", "hover", NULL),
                    actionButton("add_btn", "Add"), # Add stage
                    actionButton("delete_btn", "Delete"), # Delete last stage
                    
                    
                    numericInput("varieties", "Varieties",
                                 min = 1, max = 10, value = 1, step = 1, width = '80px'), 
                    # Add tooltip with instructions/info
                    bsTooltip("varieties", "The final number of selected entries. Must be smaller than or equal to the number of entries in the last stage.",
                              "right", "hover", NULL),
                    
                    # Economic cost summary output for scenario
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
                    tabsetPanel(id = "my_tabs",
                      tabPanel("ALL", tags$div(id = "placeholder")),
                      tabPanel("Scenarios", uiOutput('mytabs')),
                      tabPanel("Overview", plotOutput('overviewTab'))
                    ) # endo of tabsetPanel
                  ), # endof mainPanel
                  fluid = T # layout is not fixed, default is T
                ) # endof sidebarLayout
                
)

# Define server logic required to draw charts
server <- function(input, output, clientData, session) {
  
  # TV to use complementary to generated divID for identifying Scenarios ID
  Scenarios <<- c() #simple list of scenario ids, so will be [1,2,3] if three scenarios
  
  # Default matrix values
  stage = c(1,2,3)
  entries = c(1000,100,10)
  years = c(1,1,2)
  locs = c(1,4,8)
  reps = c(1,2,3)
  error = c(1,1,1)
  h2 = c(0.5,0.5,0.5) # this is a calculated value
  
  # per-session reactive values object to store all results of this user session
  rv <- reactiveValues(results_all = NULL)

  # defines a common reactive list to store all scenario input info (stages DT + other) to replace reactDT
  scenariosInput <- reactiveValues(stagesDT = list(), varG = list(), varGxL = list(), varGxY = list(), varieties = list()) # initially will store stages_current and updated accordingly

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

  # function calculates Total Plots given a stages matrix as input
  totalPlots <- function(mtx = yt) {
    # print(nrow(mtx))
    # print(prod(mtx[1,1:4]))
    tp = 0
    for (i in 1:nrow(mtx))
      tp = tp + prod(mtx[i,1:4])
    print(tp)
  }
  # Display a DT table with costs calculated based on user input (stages etc.)
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
  
  # Render stages DT with default data entries
  output$stages_table = DT::renderDT(yti$data, 
                                     options = list(
                                       searching = F, # no search box
                                       paginate = F,  # no num of pages
                                       lengthChange = F, # no show entries
                                       scrollX = T # horizontal slider
                                     ),
                                     class = "cell-border, compact, hover", 
                                     rownames = F, #TRUE,
                                     colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', toString(withMathJax('$$h_2$$'))), # '$$h_2$$'),  # 'h2'),
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
  })
  
  observeEvent(input$delete_btn, {
    if (length(yti$data[,1])>2) # TV for >1 it crushes!
      yti$data = yti$data[1:length(yti$data[,1])-1,]
  })
  
  
  
  #***********************************************
  #-----------------------------------------------  
  # ************* RUN BUTTON OBSERVER ************
  # *************---------------------************
  #***********************************************
  #-----------------------------------------------
  # Execute runScenario() for the current settings
  observeEvent(input$run_btn, {
    
    # Increment Scenarios counter list e.g. [1, 2, 3] for 3 scenarios, used in place of input$run_btn
    if (length(Scenarios) == 0){
      Scenarios <<- c(1)                # if no Scenario defines so far, make the first one "1"
    }
    else{
      Scenarios <<- c(Scenarios,  tail(Scenarios,1)+1)   # if a Scenario is added, just add a number to the last number in the "Scenarios" vector
    }
    
    # Handle auto/user-defined IDs for scenarios
    # divID <- if (input$divID == "") gsub("\\.", "", format(Sys.time(), "%H%M%OS3")) 
    # else input$divID
    divID <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3")) # always auto-generated ID for each scenario
    dtID <- paste0(divID, "DT")
    rm_btnID <- paste0(divID, "rmv")
    up_btnID <- paste0(divID, "upd")
    boxID <- paste0(divID, "box")
    stagesID <- paste0(divID, "stg")
    # scenarioID <- paste0(divID, "scn") # Alternative to Scenarios ID vector
    
    
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
    stages_current = data.frame(stages, entries, years, locs, reps, error, h2)
    
    # Create a new tab in the UI every time Run is pressed
    # UI input elements of all Scenario tabs are rendered
    output$mytabs = renderUI({
      myTabs = lapply(1: tail(Scenarios,1), function(i){
        tabPanel(paste0('Scenario', i),
                 plotOutput(paste0('cyPlot', i)),
                 # input settings used for this scenario
                 DT::DTOutput(paste0('stages_summary', i)),
                 # update scenario button
                 actionButton(paste0("update_btn", i), "Update")
        )
      }) 
      do.call(tabsetPanel, myTabs)
    })
    
    print(paste("Start Run", tail(Scenarios,1))) # input$run_btn))
    
    result = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
    
    # New Plot of ran result appearing in new tab
    # First save new plot in a variable before passing it to output
    nplot <- renderPlot({
      boxplot(t(result),
              xlab="Stage",
              ylab="Mean Genetic Value")
    })   # end of renderPlot

    # Store results from all runs in a reactive matrix
    for(i in 1:nrow(result)) 
    {
      rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = result[i,], Scenario = tail(Scenarios,1))) # Scenario = scenarioID)) FAILS
    }
    #print(head(t(rv$results_all)))
    #print(tail(t(rv$results_all)))
    
    # Global settings for all DTs in senario tabs
    sumset_DT = list( options = list(
                        searching = F, # no search box
                        paginate = F,  # no num of pages
                        lengthChange = F, # no show entries
                        scrollX = T # horizontal slider
                      ),
                      class = "cell-border, compact, hover", 
                      rownames = F, #TRUE,
                      # colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', toString(withMathJax('$$h_2$$'))), # inserted manually
                      filter = "none",
                      escape = FALSE,
                      autoHideNavigation = TRUE,
                      selection = "none",
                      editable = list(target = "cell", disable = list(columns = c(0, 6))),
                      server = TRUE) # server = F doesn't work with replaceData() cell editing

    # Include content from R file locally as if it was pasted here to manage if-else
    # source('if_run_btn.r', local=TRUE) # OLD METHOD with if-else loop handling but with duplicated code for up to 5 scenarios WORKS!
    source('update_scenarios.r', local = TRUE) # alternative recursive method IN PROGRESS
    
    
    # # Attempt to enrich v object with I/O data for every scenario - NOT USED
    # v$data = list(v$data, list("id" = tail(Scenarios,1), 
    #                            "in" = list("varG" = varG,
    #                                        "varGxY" = varGxY,
    #                                        "varGxL" = varGxL,
    #                                        "stages_current" = stages_current,
    #                                        "varieties" = varieties), 
    #                            "out" = result)
    #               )
    
    
    ## Try to load input data using a for loop instead of if-else
    # for (i in 1:tail(Scenarios,1))
    # TODO
    
    output$overviewTab <- renderPlot({
      ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        geom_boxplot()+
        xlab("Stage")+
        ylab("Gain")+
        scale_fill_discrete(name="Scenario")+
        ggtitle("Comparison between stages across all scenarios")
    })   # end of renderPlot for Overview tab
    
      
    # ------------------------------ solution -----------------------------#
    # ///////////////////////////----------------\\\\\\\\\\\\\\\\\\\\\\\\\\#
    # Juxtapose I/O for each scenario in ALL tab using the placeholder
    # TV: IDs defined at the beginning of Run press loop
    # divID <- if (input$divID == "") gsub("\\.", "", format(Sys.time(), "%H%M%OS3")) 
    # else input$divID
    # dtID <- paste0(divID, "DT")
    # rm_btnID <- paste0(divID, "rmv")
    # up_btnID <- paste0(divID, "upd")
    # boxID <- paste0(divID, "box")
    # 
    # stagesID <- paste0(divID, "stg")
    # scenarioID <- paste0(divID, "scn")
    
    # only create button if there is none
    if (is.null(rv[[divID]])) {
      
      # Create a <div> for DT instance and delete and update buttons
      insertUI(
        selector = "#placeholder",
        ui = tags$div(id = divID,
                      DT::DTOutput(dtID),
                      actionButton(up_btnID, "Update"),
                      actionButton(rm_btnID, "Remove", class = "pull-right btn btn-danger"),
                      output[[boxID]] <- nplot,
                      hr()
        )
      )
      
      # TODO may need to replace stagesID with something like rv[[stID]]
      # stagesID1 <- reactiveValues(data = stages_table)
      rv[[stagesID]] <- stages_current
      
      # print(stagesID)
      # print(paste("h2 of stages table is", rv[[stagesID]][,7])) # WORKS!
      
      output[[dtID]] <- DT::renderDT(rv[[stagesID]], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', 'h2'), editable = sumset_DT$editable, server = sumset_DT$server)
      # Update editable DT through a proxy DT on cell edit event
      proxy = dataTableProxy(dtID)
      #
      observeEvent(input[[paste0(dtID, "_cell_edit")]], { # WORKS !
        info = input[[paste0(dtID, "_cell_edit")]]
        i = info$row
        j = info$col + 1 # required when rownames = F in DT
        v = info$value
        str(info)
        # Character string needs to be coerced to same type as target value. Here as.integer()
        rv[[stagesID]][i, j] = DT::coerceValue(v, rv[[stagesID]][i, j])
        print(paste0("new edit in ", rv[[stagesID]][i, j]))
        # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
        replaceData(proxy, rv[[stagesID]], resetPaging = FALSE)  # important 
      })
      
      # Execute runScenario() for the current settings
      observeEvent(input[[up_btnID]], {
        nresult = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                              isolate(rv[[stagesID]][,2]),isolate(rv[[stagesID]][,3]),
                              isolate(rv[[stagesID]][,4]),isolate(rv[[stagesID]][,5]),
                              isolate(rv[[stagesID]][,6]),isolate(input$varieties)) # WORKS!
        
        # NOT WORKING !!!!!!!!!!!!!!!! NOT UPDATING 
        # output$boxID <- SAME
        output[[boxID]] <- renderPlot({
          boxplot(t(nresult),xlab="Stage",ylab="Mean Genetic Value")
          print("HELLO I DONT WORK")
        })   # end of renderPlot
        
        # Update results_all entries
        # First remove previous run entries
        rv$results_all <- rv$results_all[,rv$results_all[3,]!=1] # WORKS for Scenario 1!
        # rv$results_all <- rv$results_all[,rv$results_all[3,]!=scenarioID] # FAILS
        # print("UPDATED RESULTS_ALL WORKS!")
        #print(head(t(rv$results_all)))
        #print(tail(t(rv$results_all)))
        # Then add to matrix
        for(i in 1:nrow(nresult)) # 1:input$run_btn
        {
          rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = nresult[i,], Scenario = 1)) # WORKS # Scenario = scenarioID)) FAILS!
        }
        #print(head(t(rv$results_all)))
        #print(tail(t(rv$results_all)))
        
        # Render Group Boxplot with updated entries --- WORKS!
        output$overviewTab <- renderPlot({
          ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
            geom_boxplot()+
            xlab("Stage")+
            ylab("Gain")+
            scale_fill_discrete(name="Scenario")+
            ggtitle("Comparison between stages across all scenarios")
        })   # end of renderPlot for Overview tab
      }) # endof update btn  
      
      # Update H2 for every stage as soon as input data that affect H2 change ---- WORKS!
      observe({
        for (i in 1:nrow(rv[[stagesID]]))
        {
          rv[[stagesID]][i,7] = round(input$varG/(input$varG + input$varGxY/rv[[stagesID]][i,3] + input$varGxL/(rv[[stagesID]][i,3]*rv[[stagesID]][i,4]) + rv[[stagesID]][i,6]/(rv[[stagesID]][i,3]*rv[[stagesID]][i,4]*rv[[stagesID]][i,5])), 3)
          #print(paste("H2 for stagesID", stagesID,"in stage", i, "is", rv[[stagesID]][i,7], "and years = ", rv[[stagesID]][i,3]))
        }
      })
      
      # make a note of the ID of this section, so that it is not repeated accidentally
      rv[[divID]] <- TRUE
      
      # create a listener on the newly-created button that will
      # remove it from the app when clicked
      # TODO also update Overview grouped boxplots
      observeEvent(input[[rm_btnID]], {
        removeUI(selector = paste0("#", divID))
        
        rv[[divID]] <- NULL
        
        # TV TODO: also remove scenario from results_all and update Overview plot accordingly
        # First remove previous run entries
        # rv$results_all <- rv$results_all[,rv$results_all[3,]!=1] 
        # Render Group Boxplot with updated entries
        # output$overviewTab <- renderPlot({
        #   ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        #     geom_boxplot()+
        #     xlab("Stage")+
        #     ylab("Gain")+
        #     scale_fill_discrete(name="Scenario")+
        #     ggtitle("Comparison between stages across all scenarios")
        # })   # end of renderPlot for Overview tab
        
      }, ignoreInit = TRUE, once = TRUE)
      
      # otherwise, print a message to the console
    } else {
      message("The button has already been created!")
    }
    
  }) # end of run button
  
  # TV hide ALL tab 
  # shiny::hideTab(inputId = "my_tabs", target = "ALL")
  
} # endof server

# Run the application 
shinyApp(ui = ui, server = server)
