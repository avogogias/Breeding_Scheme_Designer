library(DT)
library(Rcpp)
library(RcppArmadillo)
library(ggplot2) 
library(shinyjs)
library(data.table)
library(dplyr)

sourceCpp("Engine.cpp")

server <- function(input, output, clientData, session) {
  
  # ***************************************************** #
  # ********************* VARIABLES ********************* #
  # ********************* --------- ********************* #
  
  # TV to use complementary to generated divID for identifying Scenarios ID
  Scenarios <<- c() #simple list of scenario ids, so will be [1,2,3] if three scenarios
  
  # Define default matrix values
  stage = c(1,2,3)
  entries = c(1000,100,10)
  years = c(1,1,2)
  locs = c(1,4,8)
  reps = c(1,2,3)
  error = c(1,1,1)
  h2 = c(0.5,0.5,0.5) # this is a calculated value initialised here
  
  # per-session reactive values object to store all results of this user session
  rv <- reactiveValues(results_all = NULL, results_allxTime = NULL, results_allxCost = NULL, results_range = NULL)
  # defines a common reactive list to store all scenario input info (stages DT + other) to replace reactDT
  scenariosInput <- reactiveValues(stagesDT = list(), varG = list(), varGxL = list(), varGxY = list(), varieties = list()) # initially will store stages_current and updated accordingly
  yt = cbind(stage,entries,years,locs,reps,error,h2)
  # Using reactiveVales to add a server side set of variable observable and mutable at the same time
  yti <- reactiveValues(data = yt)

  # ***************************************************** #
  # ********************* FUNCTIONS ********************* #
  # ********************* --------- ********************* #
  # *********** All functions are defined here ********** #
  # ***************************************************** #
  
  # function calculates h2 for a given as input a row of a DT matrix and 3 variances (optional)
  updateH2 <- function(stg = yti$data[1,], vG=input$varG, vGxY=input$varGxY, vGxL=input$varGxL){
    h2 = round(vG/(vG + vGxY/stg[3] + vGxL/(stg[3]*stg[4]) + stg[6]/(stg[3]*stg[4]*stg[5])), 3)
    return(h2)
  }

  # function returns total number of years for a scenario
  totalYears <- function(scenarioDT = yti$data, selfingYears = input$negen) {
    ty = sum(scenarioDT[,3]) + selfingYears
    return(ty)
  }

  # function returns the total number of locations for a scenario
  totalLocs <- function(scenarioDT = yti$data) {
    tl = sum(scenarioDT[,3]*scenarioDT[,4])
    return(tl)
  }

  # function calculates Total Plots given a stages matrix as input
  totalPlots <- function(scenarioDT = yti$data) {
    tp = 0
    for (i in 1:nrow(scenarioDT))
      tp = tp + prod(scenarioDT[i,2:5])
    return(tp)
  }

  # function calculates total cost of locations
  totalLocsCost <-function(scenarioDT = yti$data, costPerLoc = input$costPerLoc) {
    tlc = totalLocs(scenarioDT) * costPerLoc
    return(tlc)
  }
  
  # function calculates total cost of plots
  totalPlotsCost <-function(scenarioDT = yti$data, costPerPlot = input$costPerPlot) {
    tpc = totalPlots(scenarioDT) * costPerPlot
    return(tpc)
  }
  
  # function calculates total cost of scenario
  totalCost <- function(scenarioDT = yti$data, costPerLoc = input$costPerLoc, costPerPlot = input$costPerPlot) {
    tc = totalLocsCost(scenarioDT, costPerLoc) + totalPlotsCost(scenarioDT, costPerPlot)
    return(tc)
  }
  
  # function returns total number of years passed until a particular stage is completed (default is stage 1)
  stageTotalYears <- function(scenarioDT = yti$data, stage = 1, selfingYears = input$negen) {
    scy = sum(scenarioDT[1:stage,3]) + selfingYears
    return(scy)
  }
  #
  # function calcucates Gain / Time dividing the gain with the number of years passed until a stage is completed
  gainTime <- function(scenarioDT = yti$data, result = result, stage = 1) {
    gt = result[stage,] / stageTotalYears(scenarioDT, stage) 
    return(gt)
  }
  
  # function returns total plots in a stage (default is stage 1)
  stageTotalPlots <-function(scenarioDT = yti$data, stage = 1) {
    # should be equal to the summary of products for up to that stage
    stp = 0
    for (i in 1:stage)
    {
      stp = stp + prod(scenarioDT[i,2:5])
    }
    #stp = sum(prod(scenarioDT[1:stage,1:4])) # + previous stages total plots
    return(stp)
  }

  # function returns total plots in a stage (default is stage 1)
  stageTotalLocs <-function(scenarioDT = yti$data, stage = 1) {
    stl = 0
    for (i in 1:stage)
    {
      stl = stl + prod(scenarioDT[i,3:4])
    }
    # stl = sum(prod(scenarioDT[1:stage,1:4]))
    return(stl)
  }

  # Return the gain over cost as this is calculated from plot and loc costs in the program
  gainCost <- function(scenarioDT = yti$data, result = result, stage = 1, costPerPlot = input$costPerPlot, costPerLoc = input$costPerLoc) {
    gc = result[stage,]  / (stageTotalPlots(scenarioDT, stage) * costPerPlot + stageTotalLocs(scenarioDT, stage) * costPerLoc)
    return(gc)
  }
  
  # function creates a new Tab in the UI for a given ScenarioID
  createTab <- function(scenarioID = 1) {
    myTabs = lapply(1: scenarioID, function(i){
      tabPanel(paste0('Scenario', i),
               plotOutput(paste0('cyPlot', i)),
               # input settings used for this scenario
               DT::DTOutput(paste0('stages_summary', i)),
               # update scenario button
               actionButton(paste0("update_btn", i), "Update"),
               # cost table for this scenario
               DT::DTOutput(paste0('costDT', i))
      )
    }) 
    do.call(tabsetPanel, myTabs)
  }



  # Store results from all runs in a reactive matrix
  storeScenarioResult <- function(result = result, results_all = rv$results_all, scenarioID = tail(Scenarios,1) ) {
    for(i in 1:nrow(result)) 
    {
      results_all = cbind(results_all, rbind(Stage = i, Value = result[i,], Scenario = scenarioID)) 
    } 
    return(results_all)
  }

  # Store all Gain results conditioned by Time
  storeScenarioResultxTime <- function(result = result, results_all = rv$results_allxTime, scenarioID = tail(Scenarios,1), scenarioDT =  yti$data) {
    for(i in 1:nrow(result)) 
    {
      results_all = cbind(results_all, rbind(Stage = i, Value = gainTime(scenarioDT, result, i), Scenario = scenarioID))
    }
    return(results_all)
  }
  
  # Store all Gain results conditioned by Cost
  storeScenarioResultxCost <- function(result = result, results_all = rv$results_allxCost, scenarioID = tail(Scenarios,1), scenarioDT =  yti$data) {
    for(i in 1:nrow(result)) 
    {
      results_all = cbind(results_all, rbind(Stage = i, Value = gainCost(scenarioDT, result, i), Scenario = scenarioID))
    }
    return(results_all)
  }
  
  # TV TODO fix bug to store resultLite properly : HINT refers to rr recursevely in two nested functions!!!!!!!!!!!!!!!!
  # Store results from all runs and ranges in a reactive matrix
  # storeScenarioResultRange <- function(scenarioDT = yti$data, it = it, entries = entries, varieties = input$varieties, result = resultLite, scenarioID = tail(Scenarios,1)) {
  #   print(entries)
  #   rr = NULL
  #   for(i in 1:nrow(result)) # for every stage do:
  #   {
  #     rr = cbind(rr, rbind(Scenario = scenarioID,
  #                          Iteration = it,
  #                          Stage = i, # Input Start
  #                          Entries = entries, 
  #                          Years = scenarioDT[i,3], 
  #                          Locs = scenarioDT[i,4],
  #                          Reps = scenarioDT[i,5],
  #                          Error = scenarioDT[i,6],
  #                          Varieties = varieties,
  #                          Mean = result[i,1],  # Output Start
  #                          SD = result[i,2]))
  #   } 
  #   print("STORE!!!")
  #   #print(rr)
  #   return(rr)
  # }
  
  # Remove scenario result from storage. By default remove last scenario.
  removeScenarioResult <- function(scenarioID = tail(Scenarios,1), results_all = rv$results_all) {
    results_all <- results_all[,results_all[3,] != scenarioID] 
    return(results_all)
  }

  # Ignore entries in first stage and instead run for a range of entries. Store the results in rv$results_range
  runScenarioRange <- function(min_entries = input$entries_range[1], max_entries = input$entries_range[2], 
                               min_reps = input$reps_range[1], max_reps = input$reps_range[2],
                               grain = input$grain,
                               scenarioDT = yti$data, 
                               varG = input$varG, 
                               varGxL = input$varGxL, 
                               varGxY = input$varGxY, 
                               varieties = input$varieties) 
                               # results_range = rv$results_range) 
    {
      stage = scenarioDT[,1]
      entries = scenarioDT[,2]
      years = scenarioDT[,3] 
      locs = scenarioDT[,4]
      reps = scenarioDT[,5]
      error = scenarioDT[,6]
      it = 0 # counter of iterations between range min max
      range_entries = rangeGrain(min_entries, max_entries, grain)
      range_reps = rangeGrain(min_reps, max_reps, grain)
      #print(range_reps)

      rr = NULL
      for (i in range_entries) 
        {
          for (j in range_reps)
          {
            it = it + 1
            entries[1] = i # replace first stage entries with range_entries
            reps[1] = j  # replace first stage reps with range_reps
            resultLite = runScenarioLite(varG, 
                                         varGxL, 
                                         varGxY, 
                                         entries,  
                                         years, 
                                         locs,
                                         reps,
                                         error,
                                         varieties)
            #print(resultLite) # WORKS
            resultLite = as.data.frame(resultLite)             # convert to a df
            colnames(resultLite) <- c("mean","sd")
            # Create df with I/O data and bind this to rr from previous iterations
            rr<-rbind(rr, cbind(scenario = tail(Scenarios,1), fs_entries = i, fs_reps = j, it, stage, entries, years, locs, reps, error, resultLite))
          }
        }   
      return(rr)
  }
  # function takes 2 vectors and returns a matrix with a grid between paired min max elements
  rangeGrain <- function(min = input$range[1], max = input$range[2], grain = input$grain) {
    qrt = NULL
    for (i in 1:length(min)) {
      if (min[i] < max[i] && grain>1) #  && min[i]>entries[i+1]
      {
        qrt = c(qrt, round(seq(min[i], max[i], by = (max[i]-min[i])/(grain-1)))) 
      }
      else qrt = c(qrt, max(min[i], max[1]))
    }
    return(qrt)
  }  
  
  #********************************
  #--------------------------------  
  # ************ PLOTS ************
  # -------------------------------
  #********************************
  
  # function plots the results of a scenario
  plotScenario <- function(result = result) {
    boxplot(t(result),
            xlab="Stage",
            ylab="Mean Genetic Value")
  }
  
  # function plots the results of all scenarios
  plotScenarioGroup <- function(results_all = rv$results_all, ylabel = "Gain", gtitle = "Genetic Gain by Stage") {
    ggplot(as.data.frame(t(results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
      geom_boxplot()+
      xlab("Stage")+
      ylab(ylabel)+
      scale_fill_discrete(name="Scenario")+
      ggtitle(gtitle) + 
      theme(plot.title = element_text(size = 14, face = "bold"))
  }
  
  # Plot of mean value with margins for standard deviation (copied from alphasimrshiny)
  plotMeanGrid = function(df = rv$results_range, myX = "fs_entries", myFilter = "fs_reps", myXl = "First Stage Entries", title = "Gain by First Stage Entries Range") { 
    df <- transform(df, stage = as.character(stage)) # use categorical colour instead of ordered
    df <- filter(df, as.numeric(unlist(df[myFilter])) %in% df[myFilter][1,]) # filter rows based on the first occurrence of fs_reps, which is the min
    myX <- as.numeric(unlist(df[myX]))
    print(df)
    print(sapply(df, mode))
    print(myX)
    yMin = min(df$mean)-1.01*max(df$sd)
    yMax = max(df$mean)+1.01*max(df$sd)

    gp = ggplot(df,aes(x=myX,y=mean,group=stage,color=stage))+
      geom_ribbon(aes(x=myX,ymin=mean-sd,ymax=mean+sd,
                      fill=stage),alpha=0.1,linetype=0)+
      geom_line(size=1)+
      guides(alpha=FALSE)+
      scale_color_brewer(palette="Set1")+ #(palette="Spectral")+
      scale_fill_brewer(palette="Set1")+ #(palette="Spectral")+ # palette="Set1")+
      # theme_bw()+
      # theme(legend.justification = c(0.02, 0.96), 
      #       legend.background = element_blank(),
      #       legend.box.background = element_rect(colour = "black"),
      #       legend.position = c(0.02, 0.96))+
      scale_x_continuous(myXl)+
      scale_y_continuous("Gain",
                         limits=c(yMin,yMax))+
      ggtitle(title)
    return(gp)
  }


  
  #*************************************
  #-------------------------------------  
  # ************* RENDERERS ************
  # ------------------------------------
  #*************************************
  
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
                                     colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', toString(withMathJax('$$h^2$$'))), # '$$h_2$$'),  # 'h2'),
                                     filter = "none",
                                     escape = FALSE,
                                     autoHideNavigation = TRUE,
                                     selection = "none",
                                     editable = list(target = "cell", disable = list(columns = c(0, 6))),
                                     server = TRUE) # server = F doesn't work with replaceData() cell editing  
  
  # Render a DT table with total costs (Years, Locs, Plots) calculated based on stages input
  output$cost_table = DT::renderDT(cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data)), 
                                   options = list(
                                     searching = F, # no search box
                                     paginate = F,  # no num of pages
                                     lengthChange = F, # no show entries
                                     scrollX = T # horizontal slider
                                   ),
                                   rownames = F,
                                   colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                   server = F )
  
  
  
  #*************************************
  #-------------------------------------  
  # ************* OBSERVERS ************
  # ------------------------------------
  #*************************************

  
  # Update editable DT stages_table through a proxy DT on cell edit event
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
  
  # Update H2 (7th col in yti DT) for every stage in sidebar DT, as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(yti$data))
    {
      yti$data[i,7] = updateH2(yti$data[i,])
    }
  })
  
  # Observe Button Clicks for adding or removing rows (stages) from the DT
  observeEvent(input$add_btn, {
    #print(yti$data[1,3:6])
    # calc h2 for this stage
    new_h2 = input$varG / (input$varG + input$varGxY + input$varGxL + 1)
    yti$data = rbind(yti$data, c(length(yti$data[,1])+1,2,1,1,1,1,round(new_h2, 3)))
  })
  
  observeEvent(input$delete_btn, {
    if (length(yti$data[,1])>1) # TV for >1 need to stop R coercing a matrix or array to the lowest possible number of dimensions
      yti$data = yti$data[1:length(yti$data[,1])-1,,drop=F]
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$run_btn, {
    
    print(length(Scenarios))
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
      createTab(scenarioID = tail(Scenarios,1))
    })
    
    print(paste("Start Run", tail(Scenarios,1))) # input$run_btn))
    
    result = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)

    # Store results for different ranges of first stage entries
    rv$results_range = rbind(rv$results_range, runScenarioRange())
    #print(rv$results_range)
    
    # range plot appears in tab Range and includes all scenarios
    output$entriesRangePlot <- renderPlot({
      plotMeanGrid(df = rv$results_range)
      #TV plotScenario(resultLite) #PREV plotScenario(result)
    })   # end of renderPlot  

    output$repsRangePlot <- renderPlot({
      plotMeanGrid(df = rv$results_range, myX = "fs_reps", myFilter = "fs_entries", myXl = "First Stage Reps", title = "Gain by First Stage Reps Range") 
    })
      
    # New Plot of ran result appearing in new tab
    # First save new plot in a variable before passing it to output
    nplot <- renderPlot({
      plotScenario(result)
    })   # end of renderPlot
    
    # Store results from all runs in a reactive matrix
    rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = tail(Scenarios,1))

    # Store all results of Gain per Year
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = tail(Scenarios,1), scenarioDT =  yti$data)

    # Store all results of Gain per cost 
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = tail(Scenarios,1), scenarioDT =  yti$data)
    
    
    # Global settings for all DTs in senario tabs
    sumset_DT = list( options = list(
      searching = F, # no search box
      paginate = F,  # no num of pages
      lengthChange = F, # no show entries
      scrollX = T # horizontal slider
    ),
    class = "cell-border, compact, hover", 
    rownames = F, #TRUE,
    # colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', toString(withMathJax('$$h^2$$'))), # inserted manually
    filter = "none",
    escape = FALSE,
    autoHideNavigation = TRUE,
    selection = "none",
    editable = list(target = "cell", disable = list(columns = c(0, 6))),
    server = TRUE) # server = F doesn't work with replaceData() cell editing
    
    # Include content from R file locally as if it was pasted here to manage if-else
    source('if_run_btn.r', local=TRUE) # OLD METHOD with if-else loop handling but with duplicated code for up to 5 scenarios WORKS!
    # source('update_scenarios.r', local = TRUE) # alternative recursive method IN PROGRESS
    
    
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
    
    # Render grouped boxplots for all scenario results without any processing
    output$overviewTab <- renderPlot({
      plotScenarioGroup(rv$results_all)
    })   # end of renderPlot for Overview tab
    
    # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
    output$overviewTabxTime <- renderPlot({
      plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
    })   # end of renderPlot for Overview tab
    
    # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
    output$overviewTabxCost <- renderPlot({
      plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })   # end of renderPlot for Overview tab
    
    
    source('all_in_one.r', local = TRUE) # alternative recursive method that uses divID -- IN PROGRESS    
    
  }) # end of run button
  
  # TV hide ALL tab 
  shiny::hideTab(inputId = "my_tabs", target = "ALL")
  # TV hide Gain per Cost plot with shinyjs package
  # hide("overviewTabxCost")
  
  
} # endof server