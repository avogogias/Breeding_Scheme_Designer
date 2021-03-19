library(DT)
library(Rcpp)
library(RcppArmadillo)
library(ggplot2) 
library(shinyjs)
library(data.table)
library(dplyr)
library(plotly)
library(openxlsx)
#library(gridExtra)

sourceCpp("Engine.cpp")

server <- function(input, output, clientData, session) {
  
  # ***************************************************** #
  # ********************* VARIABLES ********************* #
  # ********************* --------- ********************* #
  # ***************************************************** #
  
  # TV to use complementary to generated divID for identifying Scenarios ID
  Scenarios <<- c() #simple list of scenario ids, so will be [1,2,3] if three scenarios
  Ranges <<- c() #simple list of range scenario ids, so will be [1,2,3] if three scenarios of ranges
  
  # Define default matrix values
  stage = c(1,2,3)
  entries = c(1000,100,10)
  years = c(1,1,2)
  locs = c(1,4,8)
  reps = c(1,2,3)
  error = c(1,1,1)
  h2 = c(0.5,0.5,0.5) # this is a calculated value initialised here
  plotCost = c(10,10,10)
  locCost = c(1000,1000,1000)
  fixedCost = c(1000,1000,1000)
  # varieties = c(1,1,1)
  yt = cbind(stage,entries,years,locs,reps,error,h2,plotCost,locCost,fixedCost) #,varieties)
  
  # Ranges DT has 3 cols for min max and samples
  entries_r = c(100,1000,3)
  years_r = c(1,5,3)
  locs_r = c(1,5,3)
  reps_r = c(1,5,3)
  plotCost_r = 10 
  locCost_r = 1000
  fixedCost_r = 1000
  rt = rbind(entries_r, years_r, locs_r, reps_r)
  
  # per-session reactive values object to store all results of this user session
  rv <- reactiveValues(results_all = NULL, results_allxTime = NULL, results_allxCost = NULL, results_range = NULL, results_range_r = NULL)
  # defines a common reactive list to store all scenario input info (stages DT + other) to replace reactDT
  scenariosInput <- reactiveValues(stagesDT = list(), varG = list(), varGxL = list(), varGxY = list(), varieties = list()) # initially will store stages_current and updated accordingly
  # Using reactiveVales to add a server side set of variable observable and mutable at the same time
  yti <- reactiveValues(data = yt)
  
  rti <- reactiveValues(data = rt)
  
  fin = data.frame(stage_r = c("Final"), entries_r = c(1))
  print(paste("The type of final entries DT is : ", mode(fin)))
  yti$varieties <- fin
  
  # initialize empty vector to stores status of chk_ranges for each scenario
  rangesVec <- vector()
  # initialize empty list to store data frames of reactiveValues created for each Scenario, e.g. reactDT1
  reactDT.list <- reactiveValues() # list()

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
  # totalLocsCost <-function(scenarioDT = yti$data, costPerLoc = input$costPerLoc) {
  #   tlc = totalLocs(scenarioDT) * costPerLoc
  #   return(tlc)
  # }
  totalLocsCost <-function(scenarioDT = yti$data) {
    tlc = 0
    for (i in 1:nrow(scenarioDT))
    {
      tlc = tlc + stageLocsCost(scenarioDT, i)
    }
    return(tlc)
  }
  
  # function calculates total cost of plots
  # totalPlotsCost <-function(scenarioDT = yti$data, costPerPlot = input$costPerPlot) {
  #   tpc = totalPlots(scenarioDT) * costPerPlot
  #   return(tpc)
  # }
  totalPlotsCost <-function(scenarioDT = yti$data) {
    tpc = 0
    for (i in 1:nrow(scenarioDT))
    {
      tpc = tpc + stagePlotsCost(scenarioDT, i)
    }
    return(tpc)
  }
  
  # function calculates total cost of scenario
  # totalCost <- function(scenarioDT = yti$data, costPerLoc = input$costPerLoc, costPerPlot = input$costPerPlot, costFixed = input$costFixed) {
  #   tc = totalLocsCost(scenarioDT, costPerLoc) + totalPlotsCost(scenarioDT, costPerPlot) + costFixed
  #   return(tc)
  # }
  totalCost <- function(scenarioDT = yti$data) {
    tc = 0
    for (i in 1:nrow(scenarioDT))
    {
      tc = tc + stageCost(scenarioDT, i)
    }
    return(tc)
  }
  
  # function returns total number of years passed until a particular stage is completed (default is stage 1)
  stageTotalYears <- function(scenarioDT = yti$data, stage = 1, selfingYears = input$negen) {
    scy = sum(scenarioDT[1:stage,3]) + selfingYears
    return(scy)
  }
  
  # function calcucates Gain / Time dividing the gain with the number of years passed until a stage is completed
  gainTime <- function(scenarioDT = yti$data, result = result, stage = 1) {
    gt = result[stage,] / stageTotalYears(scenarioDT, stage) 
    return(gt)
  }

  # function returns total locs in a stage (default is stage 1)
  stageTotalLocs <-function(scenarioDT = yti$data, stage = 1) {
    stl = 0
    for (i in 1:stage)
    {
      stl = stl + prod(scenarioDT[i,3:4])
    }
    # stl = sum(prod(scenarioDT[1:stage,1:4]))
    return(stl)
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
  
  # function returns number of locs of a single stage 
  stageLocs <-function(scenarioDT = yti$data, stage = 1) {
    sl = prod(scenarioDT[stage,3:4])
    return(sl)
  }
  
  # function returns number of plots of a single stage 
  stagePlots <-function(scenarioDT = yti$data, stage = 1) {
    sp = prod(scenarioDT[stage,2:5])
    return(sp)
  }
  
  # function returns cost of Locs for a single stage
  stageLocsCost <-function(scenarioDT = yti$data, stage = 1) {
    slc = stageLocs(scenarioDT, stage) * scenarioDT[stage, 9]
    return(slc)
  }
  
  # function returns cost of Plots for a single stage
  stagePlotsCost <-function(scenarioDT = yti$data, stage = 1) {
    spc = stagePlots(scenarioDT) * scenarioDT[stage, 8]
    return(spc)
  }
  
  # function returns tolal cost of a single stage
  stageCost <-function(scenarioDT = yti$data, stage = 1) {
    stc = stageLocsCost(scenarioDT, stage) + stagePlotsCost(scenarioDT, stage) + scenarioDT[stage, 10] # + Fixed Cost
    return(stc)
  }
  
  # function returns gain per cost for a single stage independently of previous stages cost ****** --- N O T    U S E D ! ! ! --- ******
  stageGainCost <-function(scenarioDT = yti$data, result = result, stage = 1) {
    sgc = result[stage,] / stageCost(scenarioDT, stage)
    return(sgc)
  }

  # Return the gain over cost up until the end of a stage in the whole program
  # gainCost <- function(scenarioDT = yti$data, result = result, stage = 1, costPerPlot = input$costPerPlot, costPerLoc = input$costPerLoc) {
  #   gc = result[stage,]  / (stageTotalPlots(scenarioDT, stage) * costPerPlot + stageTotalLocs(scenarioDT, stage) * costPerLoc)
  #   return(gc)
  # }
  gainCost <- function(scenarioDT = yti$data, result = result, stage = 1) {
    gc = 0
    for (i in 1:stage)
    {
      gc = gc + stagePlotsCost(scenarioDT, i) + stageLocsCost(scenarioDT, i) + scenarioDT[i, 10]  # + Fixed Cost included!
    }
    gc = result[stage,] / gc  
    return(gc)
  }
  

  # function creates a new Tab in the UI for a given ScenarioID
  createTab <- function(scenarioID = 1, withRanges = rangesVec) {
    myTabs = lapply(1: scenarioID, function(i){
      tabPanel(paste0('Scenario', i),
               plotOutput(paste0('cyPlot', i)),
               # input settings used for this scenario
               DT::DTOutput(paste0('stages_summary', i)),
               # update scenario button
               actionButton(paste0("update_btn", i), "Update"),
               # downloadButton(paste0("download_btn", i), "Download Report"), # Disabled
               # cost table for this scenario
               DT::DTOutput(paste0('costDT', i)),
               # Start section with plots of ranges
               #conditionalPanel(
               #   condition = "input.chk_ranges", # needs a different condition local to the scenario
               #print(paste("Ranges is ", withRanges[i])),
               if (withRanges[i])
               {
                 tags$div(class = "div_plot_ranges", checked = NA, 
                          tags$h3("Plots for ranges of parameters at first stage"),
                          # Drop down lists to select plots to view for each Scenario Tab
                          selectInput(inputId = paste0('rangePlots', i), label =  "Show Range Plots:",
                                      choices =  c("Entries" = paste0('rangePlotEntries', i),
                                                   "Years" = paste0('rangePlotYears', i),
                                                   "Locs" = paste0('rangePlotLocs', i),
                                                   "Reps" = paste0('rangePlotReps', i),
                                                   "Entries by Years" = paste0('rangePlotEntriesYears', i),
                                                   "Entries by Locs" = paste0('rangePlotEntriesLocs', i),
                                                   "Entries by Reps" = paste0('rangePlotEntriesReps', i),
                                                   "Years by Locs" = paste0('rangePlotYearsLocs', i),
                                                   "Years by Reps" = paste0('rangePlotYearsReps', i),
                                                   "Locs by Reps" = paste0('rangePlotLocsReps', i)),
                                      selected = NULL,
                                      multiple = FALSE,
                                      selectize = TRUE
                          ),
                          textOutput(paste0('plotMe', i)),
                          
                          # range plot overwrites first stage entries
                          plotOutput(paste0('rangePlotEntries', i)),
                          # range plot overwrites first stage years
                          plotOutput(paste0('rangePlotYears', i)),
                          # range plot overwrites first stage locs
                          plotOutput(paste0('rangePlotLocs', i)),
                          # range plot overwrites first stage reps
                          plotOutput(paste0('rangePlotReps', i)),
                          
                          # bubble / plotly-heatmap range plot for x6 pairs of entries/years/locs/reps ranges in first stage
                          # Enable switching between plotlyOutput and plotOutput for bubble plots and heatmaps
                          # TODO
                          
                          # hide plots and only show when selected in drop box
                          plotlyOutput(paste0('rangePlotEntriesYears', i)),
                          #  
                          plotlyOutput(paste0('rangePlotEntriesLocs', i)),
                          #  
                          plotlyOutput(paste0('rangePlotEntriesReps', i)),
                          # 
                          plotlyOutput(paste0('rangePlotYearsLocs', i)),
                          #  
                          plotlyOutput(paste0('rangePlotYearsReps', i)),
                          #  
                          plotlyOutput(paste0('rangePlotLocsReps', i))
                        
               ) # endof div plot ranges
          } #) # endof Conditional Panel
      ) # endof Tab Panel
    }) 
    do.call("tabsetPanel", c(myTabs, id = "sc_tabs"))
  }
 
  # function updates Tab in the UI for a given ScenarioID and selectInput inputID with a list of plots (called by drop down list observer)
  showPlot <- function(selectedID = input$rangePlots1, scenarioID = 1) {   # (scenarioID = 1, inputID = "Entries by Reps") {
    
    choices =  c(paste0('rangePlotEntries', scenarioID),
                 paste0('rangePlotYears', scenarioID),
                 paste0('rangePlotLocs', scenarioID),
                 paste0('rangePlotReps', scenarioID),
                 paste0('rangePlotEntriesYears', scenarioID),
                 paste0('rangePlotEntriesLocs', scenarioID),
                 paste0('rangePlotEntriesReps', scenarioID),
                 paste0('rangePlotYearsLocs', scenarioID),
                 paste0('rangePlotYearsReps', scenarioID),
                 paste0('rangePlotLocsReps', scenarioID))

    print("HIDE ALL")
    for (i in choices)
    {
      hide(i)
      if (i == selectedID) 
      {
        print(paste("SELECTED ", i)) # 
        toggle(i) #show(i)
      }
      # else
      # {
      #   print(paste("NOT SELECTED ", i)) 
      #   #toggle(i)
      #   hide(i)
      # }
    }
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
  
  # Remove scenario result from storage. By default remove last scenario.
  removeScenarioResult <- function(scenarioID = tail(Scenarios,1), results_all = rv$results_all) {
    results_all <- results_all[,results_all[3,] != scenarioID] 
    return(results_all)
  }

  # Ignore entries in first stage and instead run for a range of entries. Store the results in rv$results_range
  runScenarioRange <- function(min_entries = input$entries_range[1], max_entries = input$entries_range[2], 
                               min_years = input$years_range[1], max_years = input$years_range[2],
                               min_locs = input$locs_range[1], max_locs = input$locs_range[2],
                               min_reps = input$reps_range[1], max_reps = input$reps_range[2],
                               grain = input$grain,
                               scenarioDT = yti$data, 
                               varG = input$varG, 
                               varGxL = input$varGxL, 
                               varGxY = input$varGxY, 
                               varieties = as.numeric(yti$varieties[1,2]))
                               #varieties = input$varieties) 
                               # results_range = rv$results_range) 
    {
      print(scenarioDT)
      stage = scenarioDT[,1]
      entries = scenarioDT[,2]
      min_entries = checkMinEntries(entries, min_entries) # entries at second stage must be less than the first
      years = scenarioDT[,3] 
      locs = scenarioDT[,4]
      reps = scenarioDT[,5]
      error = scenarioDT[,6]
      it = 0 # counter of iterations between range min max
      range_entries = rangeGrain(min_entries, max_entries, grain)
      range_years = rangeGrain(min_years, max_years, grain)
      range_locs = rangeGrain(min_locs, max_locs, grain)
      range_reps = rangeGrain(min_reps, max_reps, grain)
      #print(range_reps)

      # Show Progress Bar
      withProgress(message = 'Calculating results', value = 0, {
      rr = NULL
      for (i in range_entries)
      {
        for (k in range_years)
        {
          for (l in range_locs)
          {
            for (j in range_reps)
            {
              # update progress bar after a single iteration of the nested loop
              incProgress(1/(length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)), detail = paste("Iteration", it, "of", length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)))
              
              it = it + 1
              entries[1] = i # replace first stage entries with range_entries
              years[1] = k
              locs[1] = l
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
              rr<-rbind(rr, cbind(scenario = tail(Scenarios,1), fs_entries = i, fs_years = k, fs_locs = l, fs_reps = j, it, stage, entries, years, locs, reps, error, resultLite))
              
            }
          }
        }
      }
      })
      return(rr)
  }
  
  # Previous function adjusted to separate tab for ranges not dependent on DT ------------------------------- NOT USED -----------------
  # Ignore entries in first stage and instead run for a range of entries. Store the results in rv$results_range
  runScenarioRange_r_slider <- function(min_entries = input$entries_range_r[1], max_entries = input$entries_range_r[2], 
                               min_years = input$years_range_r[1], max_years = input$years_range_r[2],
                               min_locs = input$locs_range_r[1], max_locs = input$locs_range_r[2],
                               min_reps = input$reps_range_r[1], max_reps = input$reps_range_r[2],
                               # min_error = input$error_range_r[1], max_error = input$error_range_r[2],
                               grain = input$grain_r,
                               #TV scenarioDT = yti$data, 
                               varG = input$varG_r, 
                               varGxL = input$varGxL_r, 
                               varGxY = input$varGxY_r, 
                               varErr = input$varErr_r,
                               varieties = input$varieties_r) 
    # results_range = rv$results_range) 
  {
    #TV print(scenarioDT)
    #TV stage = scenarioDT[,1]
    #TV entries = scenarioDT[,2]
    
    min_entries = checkMinEntries(varieties, min_entries) 
    # varieties must be less than min_entries
    if (varieties > min_entries)
      min_entries = varieties
    years = NA #TV scenarioDT[,3] 
    locs = NA #TV scenarioDT[,4]
    reps = NA #TV scenarioDT[,5]
    error = varErr #TV scenarioDT[,6]
    it = 0 # counter of iterations between range min max
    range_entries = rangeGrain(min_entries, max_entries, grain)
    range_years = rangeGrain(min_years, max_years, grain)
    range_locs = rangeGrain(min_locs, max_locs, grain)
    range_reps = rangeGrain(min_reps, max_reps, grain)
    #range_error = rangeGrain(min_error, max_error, grain)
    #print(range_reps)
    
    # Show Progress Bar
    withProgress(message = 'Calculating results', value = 0, {
      rr = NULL
      for (i in range_entries)
      {
        for (k in range_years)
        {
          for (l in range_locs)
          {
            for (j in range_reps)
            {
              # for (e in range_error)
              # {
              # update progress bar after a single iteration of the nested loop
              # incProgress(1/(length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)*length(range_error)), detail = paste("Iteration", it, "of", length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)*length(range_error)))
              incProgress(1/(length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)), detail = paste("Iteration", it, "of", length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)))
              
              it = it + 1
              entries = i # replace first stage entries with range_entries
              years = k
              locs = l
              reps = j
             # error = e
              
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
            # rr<-rbind(rr, cbind(scenario = tail(Scenarios,1), fs_entries = i, fs_years = k, fs_locs = l, fs_reps = j, it, stage, entries, years, locs, reps, error, resultLite))
              rr<-rbind(rr, cbind(scenario = tail(Ranges,1), entries, years, locs, reps, error, it, resultLite))
              # }
            }
          }
        }
      }
    })
    
    colnames(rr) <- c("Scenario", "Entries", "Years", "Locs", "Reps", "Error", "IT", "Gain", "SD")
    #print(rr)
    #tail(rr)
    return(rr)
  }
  
  # Uses Ranges DT instead of sliders to also include samples set for each parameter
  runScenarioRange_r <- function(rangesDT = rti$data, 
                                 varG = input$varG_r, 
                                 varGxL = input$varGxL_r, 
                                 varGxY = input$varGxY_r, 
                                 varErr = input$varErr_r,
                                 varieties = input$varieties_r,
                                 plotCost = input$plotCost_r,
                                 locCost = input$locCost_r,
                                 fixedCost = input$fixedCost_r,
                                 nRepeats = input$nRepeats) 
  {
    min_entries = rangesDT[1,1]
    max_entries = rangesDT[1,2]
    sample_entries = rangesDT[1,3]
    min_years = rangesDT[2,1]
    max_years = rangesDT[2,2]
    sample_years = rangesDT[2,3]
    min_locs = rangesDT[3,1]
    max_locs = rangesDT[3,2]
    sample_locs = rangesDT[3,3]
    min_reps = rangesDT[4,1]
    max_reps = rangesDT[4,2]
    sample_reps = rangesDT[4,3]
    
    min_entries = checkMinEntries(varieties, min_entries) 
    # varieties must be less than min_entries
    if (varieties > min_entries)
      min_entries = varieties
    
    years = NA #TV scenarioDT[,3] 
    locs = NA #TV scenarioDT[,4]
    reps = NA #TV scenarioDT[,5]
    error = varErr #TV scenarioDT[,6]
    it = 0 # counter of iterations between range min max
    range_entries = rangeGrain(min_entries, max_entries, sample_entries)
    range_years = rangeGrain(min_years, max_years, sample_years)
    range_locs = rangeGrain(min_locs, max_locs, sample_locs)
    range_reps = rangeGrain(min_reps, max_reps, sample_reps)

    # Show Progress Bar
    withProgress(message = 'Calculating results', value = 0, {
      rr = NULL
      for (i in range_entries)
      {
        for (k in range_years)
        {
          for (l in range_locs)
          {
            for (j in range_reps)
            {
              # for (e in range_error)
              # {
              # update progress bar after a single iteration of the nested loop
              # incProgress(1/(length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)*length(range_error)), detail = paste("Iteration", it, "of", length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)*length(range_error)))
              incProgress(1/(length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)), detail = paste("Iteration", it, "of", length(range_entries)*length(range_years)*length(range_locs)*length(range_reps)))
              
              it = it + 1
              entries = i # replace first stage entries with range_entries
              years = k
              locs = l
              reps = j
              # error = e
              
              resultLite = runScenarioLite(varG,
                                           varGxL,
                                           varGxY,
                                           entries,
                                           years,
                                           locs,
                                           reps,
                                           error,
                                           varieties,
                                           nRepeats = nRepeats)
              #print(resultLite) # WORKS
              resultLite = as.data.frame(resultLite)             # convert to a df
              colnames(resultLite) <- c("mean","sd")

              # calculate mean gain per cost and bind it to rr
              meanGainxCost_r = plotCost * prod(entries,years,locs,reps) + locCost * prod(years,locs) + fixedCost  
              meanGainxCost_r = resultLite$mean / meanGainxCost_r

              # Create df with I/O data and bind this to rr from previous iterations
              # rr<-rbind(rr, cbind(scenario = tail(Scenarios,1), fs_entries = i, fs_years = k, fs_locs = l, fs_reps = j, it, stage, entries, years, locs, reps, error, resultLite))
              rr<-rbind(rr, cbind(scenario = tail(Ranges,1), entries, years, locs, reps, error, it, resultLite, meanGainxCost_r))
              # }              
              
            }
          }
        }
      }
    })
    
    colnames(rr) <- c("Scenario", "Entries", "Years", "Locs", "Reps", "Error", "IT", "Gain", "SD", "GainXCost")
    #print(rr)
    #tail(rr)
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
      else qrt = c(qrt, min(min[i], max[1]))
    }
    return(qrt)
  } 
  
  # function corrects min_entries if found smaller than entries of second stage
  checkMinEntries <- function(entries, min_entries) {
    if (length(entries)>1)
    {
      if (min_entries < entries[2])
        min_entries <- entries[2]
    }
    return(min_entries)
  }
  
  # function that creates a pop-up message and halts execution if entries vector is not in descending order
  validInput <- function(scenarioDT = yti$data) {
    entries = scenarioDT[,2]
    if (is.unsorted(rev(entries)))
      return(FALSE)
    return(TRUE)
  }
  
  # function that checks if varieties is smaller than Entries in last stage and smaller than min_entries in range
  validVarieties <- function(scenarioDT = yti$data, varieties = as.numeric(yti$varieties[1,2])) { # input$varieties) { #, min_entries = input$entries_range[1]) {
    entries = scenarioDT[,2]
    last_entries = tail(entries, 1)
    if (varieties > last_entries )
      return(FALSE)
    return(TRUE)
  }
  
  # function that returns vector of the incremental mean gain per stage calculated from runScenario() result matrix
  meanGain <- function(result = result) {
    # first calculate aggregated mean gain for each stage
    result <- round(apply(result, 1, mean), 3)
    return(result)
  } 
  
  # function that returns vector of the incremental mean gain per stage calculated from runScenario() result matrix
  meanGainInc <- function(result = result) {
    # first calculate aggregated mean gain for each stage
    result <- round(apply(result, 1, mean), 2)
    # replace accumulative gain with incremental gain per stage
    gain <- result
    if (length(gain)>1)
      for (i in 2:length(gain)) {result[i] <- round(gain[i]-gain[i-1], 3)} 
    return(result)
  } 
  
  # function that returns vector of the incremental mean gain per stage scaled by Time calculated from runScenario() result matrix
  meanGainxTime <- function(result = result, scenarioDT = yti$data) {
    # update contents of result with result by stage scaled by Years
    for(i in 1:nrow(result)) 
    {
      result[i,] = gainTime(scenarioDT, result, i)
    }
    return(meanGain(result))
  } 
  
  # function that returns vector of the incremental mean gain per stage scaled by Cost calculated from runScenario() result matrix 
  meanGainxCost <- function(result = result, scenarioDT = yti$data) {
    # update contents of result with result by stage scaled by Cost
    for(i in 1:nrow(result)) 
    {
      result[i,] = gainCost(scenarioDT, result, i) * 1000 # display gg per $1000
    }
    return(meanGain(result))
  } 
  
  # function returns a summary matrix of mean genetic gain for each stage (rows) of all scenarios (cols)
  meanGainSum <- function(result = rv$results_all) {
    result = as.data.frame(t(result)) # transform to data frame with Stage, Value, Scenario as colnames
    mtx = matrix(NA, nrow=50, ncol=tail(Scenarios,1)) # create large enough empty matrix to store all stages of the data
    maxr = 1 # initialize max stages of rnows in matrix 
    
    for(i in 1:tail(Scenarios,1))
    {
      sc <- filter(result, Scenario == i)
      
      for(j in 1:tail(sc$Stage, n=1))
      {
        st <- filter(sc, Stage == j)
        mtx[j,i] <- mean(st$Value) # store mean in matrix  - NOT ROUNDED !
      }
      
      if (maxr < tail(sc$Stage, n=1)) maxr <- tail(sc$Stage, n=1) # update max nrow for mtx
    }
    mtx <- mtx[1:maxr,, drop = FALSE] # truncate mtx at the end and avoid conversion into a vector if dim = 1.
    rownames(mtx) <- c(paste0("Stage ", 1:maxr))
    colnames(mtx) <- c(paste0("Scenario ", 1:ncol(mtx)))
    return(mtx)
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
  plotMeanGrid = function(df = isolate(rv$results_range), myX = "fs_entries", myFilter = c("fs_years", "fs_locs", "fs_reps"), myXl = "First Stage Entries", title = "Gain by First Stage Entries") { 
    df <- transform(df, stage = as.character(stage)) # use categorical colour instead of ordered
    # df <- filter(df, as.numeric(unlist(df[myFilter])) %in% df[myFilter][1,]) # filter rows not on the first occurrence (min) of myFilter
    for (i in myFilter)
    {
      df <- filter(df, as.numeric(unlist(df[i])) %in% df[i][1,]) # filter rows not on the first occurrence (min) of myFilter
    }
    df <- filter(df, as.numeric(unlist(df["scenario"])) %in% df["scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
    
    myX <- as.numeric(unlist(df[myX]))
    #print(df)
    #print(sapply(df, mode))
    #print(myX)
    print("plotMeanGrid() called")
    
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

  # TODO: a 3D Surface Plot shows the effect of 2 variable ranges combined (entries and reps)
  # ----
  
  # Bubble plot instead of a 3D plot shows peaks of gain encoded with size in a grid of x = entries and y = reps
  # see ggplot bug in https://stackoverflow.com/questions/34097133/passing-data-and-column-names-to-ggplot-via-another-function
  plotMeanGridBubble <- function(df = isolate(rv$results_range), myFilter = c("fs_years", "fs_locs"), myX = entries, myY = reps, myXl = "First Stage Entries", myYl = "First Stage Reps", title = "Gain by Entries by Reps") {
    df <- transform(df, stage = as.character(stage))
    # df <- filter(df, as.numeric(unlist(df["fs_years"])) %in% df["fs_years"][1,]) # filter rows not on the first occurrence (min) of fs_years
    # df <- filter(df, as.numeric(unlist(df["fs_locs"])) %in% df["fs_locs"][1,])
    for (i in myFilter)
    {
      df <- filter(df, as.numeric(unlist(df[i])) %in% df[i][1,]) # filter rows not on the first occurrence (min) of myFilter
    }
    df <- filter(df, as.numeric(unlist(df["scenario"])) %in% df["scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
    
    arg <- match.call()
    gp = ggplot(df, aes(x = eval(arg$myX), y = eval(arg$myY), color = stage))+ #,environment=environment())+
      # gp = ggplot(df, aes(x=entries, y=reps, color = stage))+
      geom_point(aes(size = mean, alpha=1))+
      geom_point(aes(size = mean+sd, stroke = 1, alpha = 1/20))+ # SD margins shown as homocentric bubbles with lower opacity
      scale_x_continuous(myXl)+
      scale_y_continuous(myYl)+
      
      ggtitle(title)
    
    return(gp)
  }
  
  # Heatmap plot for ranges in grid
  plotMeanGridHeatmap <- function(df = isolate(rv$results_range), myFilter = c("fs_years", "fs_locs"), myX = entries, myY = reps, myXl = "First Stage Entries", myYl = "First Stage Reps", title = "Gain by Entries by Reps") {
    # df <- transform(df, stage = as.character(stage))
    for (i in myFilter)
    {
      df <- filter(df, as.numeric(unlist(df[i])) %in% df[i][1,]) # filter rows not on the first occurrence (min) of myFilter
    }
    df <- filter(df, as.numeric(unlist(df["scenario"])) %in% df["scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
    df <- filter(df, as.numeric(unlist(df["stage"])) %in% df["stage"][1,]) # filter rows which do not belong to the first stage
    
    arg <- match.call()
    print("plotMeanGridHeatmap() called")
    
    # Create extra column "text" for plotly tooltip
    df <- df %>%
      mutate(text = "tip!")# paste0("x: ", eval(arg$myX), "\n", "y: ", eval(arg$myY), "\n", "Value: ",round(mean,2)))
    
    # Heatmap
    gp = ggplot(df, aes(x = eval(arg$myX), y = eval(arg$myY)))+ #,environment=environment())+
      geom_tile(aes(fill = mean)) +
      scale_fill_gradient(low="white", high="blue") +
      # scale_fill_distiller(palette = "RdPu") +
      # theme_ipsum()
      scale_x_continuous(myXl)+
      scale_y_continuous(myYl)+
      ggtitle(title)
    gp <- ggplotly(gp, tooltip="text")
    return(gp)
  }

  # Interactive X Y Heatmap
  plotRangesHeatmap <- function(df = isolate(rv$results_range_r), param = 'Gain', myX = input$xAxis, myY = input$yAxis, myXl = input$xAxis, myYl = input$yAxis, subSel1 = input$subSel1, subSel2 = input$subSel2, title = paste("Gain by", input$xAxis, "by", input$yAxis)) 
    { 

    df <- filter(df, as.numeric(unlist(df["Scenario"])) %in% df["Scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
    
    # more filters
    myFilter = c("Entries", "Years", "Locs", "Reps")
    toPlot = c(myX, myY)
    # Remove toPlot elements from myFilter
    myFilter = myFilter[!(myFilter %in% toPlot)]
    
    print(myFilter)
    
    # for (i in myFilter)
    # {
    #   df <- filter(df, as.numeric(unlist(df[i])) %in% df[i][1,]) # filter rows not on the first occurrence (min) of myFilter
    # }
    
    subSel1 = as.numeric(subSel1)
    subSel2 = as.numeric(subSel2)
    print(subSel1)
    print(subSel2)
    
    df <- filter(df, as.numeric(unlist(df[myFilter[1]])) %in% subSel1) # Filter what is not selected in first dynamic selectInput subSel1
    df <- filter(df, as.numeric(unlist(df[myFilter[2]])) %in% subSel2) # Filter what is not selected in second dynamic selectInput subSel2
    

    print(df)
    
    arg <- match.call()
    # Heatmap
    gp = ggplot(df, aes_string(x = eval(arg$myX), y = eval(arg$myY)))+ 
      # geom_tile(aes(fill = Gain)) +
      geom_tile(aes_string(fill = param)) +
      scale_fill_gradient(low="white", high="blue") +
      scale_x_continuous(myXl)+
      scale_y_continuous(myYl)+
      ggtitle(title)
    gp <- ggplotly(gp)
    return(gp)
  }
  
  
  # Interactive X Treatment Line Plot - - TODO
  plotRangesLine <- function(df = isolate(rv$results_range_r), param = 'Gain', myX = input$xAxisLine, myT = input$treatment, myXl = input$xAxisLine, subSel3 = input$subSel3, subSel4 = input$subSel4, title = paste("Gain by", input$xAxisLine, "by", input$treatment)) 
  { 
    # df <- transform(df, myT = as.character(myT)) # use categorical colour instead of ordered
    tLegent = myT
    
    df <- filter(df, as.numeric(unlist(df["Scenario"])) %in% df["Scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
    myFilter = c("Entries", "Years", "Locs", "Reps")  
    toPlot = c(myX, myT)
    # Remove toPlot elements from myFilter
    myFilter = myFilter[!(myFilter %in% toPlot)]
    
    print(myFilter)
    
    # for (i in myFilter)
    # {
    #   df <- filter(df, as.numeric(unlist(df[i])) %in% df[i][1,]) # filter rows not on the first occurrence (min) of myFilter
    # }
    
    subSel3 = as.numeric(subSel3)
    subSel4 = as.numeric(subSel4)
    print(subSel3)
    print(subSel4)
    
    df <- filter(df, as.numeric(unlist(df[myFilter[1]])) %in% subSel3) # Filter what is not selected in first dynamic selectInput subSel1
    df <- filter(df, as.numeric(unlist(df[myFilter[2]])) %in% subSel4) # Filter what is not selected in second dynamic selectInput subSel2
    
    print(df)
    
    myX <- as.numeric(unlist(df[myX]))
    myT <- as.factor(unlist(df[myT]))
    #print(df)
    #print(sapply(df, mode))
    #print(myX)
    print("plotRangesLine() called")
    print(df$SD)
    
    yMin = min(df$Gain)-1.01*max(df$SD)
    yMax = max(df$Gain)+1.01*max(df$SD)
    # yMin = min(df[[param]])-1.01*max(df$SD)
    # yMax = max(df[[param]])+1.01*max(df$SD)
    
    gp = ggplot(df,aes(x=myX,y=Gain,group=myT,color=myT))+
    # gp = ggplot(df,aes_string(x=myX,y=param,group=myT,color=myT))+
         geom_ribbon(aes(x=myX,ymin=Gain-SD,ymax=Gain+SD,
                      fill=myT),alpha=0.1,linetype=0)+
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
      # scale_y_continuous(param,
                         limits=c(yMin,yMax))+
      labs(colour = tLegent, fill = tLegent)
      ggtitle(title)
    #gp <- ggplotly(gp)
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
                                       dom = 't', # only display the table, and nothing else
                                       # searching = F, # no search box
                                       # paginate = F,  # no num of pages
                                       # lengthChange = F, # no show entries
                                       scrollX = T, # horizontal slider
                                       ordering = F # suppressing sorting 
                                     ),
                                     class = "cell-border, compact, hover", 
                                     rownames = F, #TRUE,
                                     colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', toString(withMathJax('$$h^2$$')), 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)'), # 'Selected Parents'),
                                     filter = "none",
                                     escape = FALSE,
                                     autoHideNavigation = TRUE,
                                     selection = "none",
                                     editable = list(target = "cell", disable = list(columns = c(0, 6))),
                                     server = TRUE) # server = F doesn't work with replaceData() cell editing  
  
  # Render a DT table with total costs (Years, Locs, Plots) calculated based on stages input
  output$cost_table = DT::renderDT(cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data)), 
                                   options = list(
                                     dom = 't', # only display the table, and nothing else
                                     # searching = F, # no search box
                                     # paginate = F,  # no num of pages
                                     # lengthChange = F, # no show entries
                                     scrollX = T, # horizontal slider
                                     ordering = F # suppressing sorting 
                                   ),
                                   rownames = F,
                                   colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                   server = F )
  
  # Render ranges DT 
  output$ranges_table = DT::renderDT(rti$data, 
                                     options = list(
                                       dom = 't', # only display the table, and nothing else
                                       # searching = F, # no search box
                                       # paginate = F,  # no num of pages
                                       # lengthChange = F, # no show entries
                                       scrollX = T, # horizontal slider
                                       ordering = F # suppressing sorting 
                                     ),
                                     class = "cell-border, compact, hover", 
                                     rownames = c('Entries', 'Years', 'Locs', 'Reps'),
                                     colnames = c('Min', 'Max', 'Samples'), 
                                     filter = "none",
                                     escape = FALSE,
                                     autoHideNavigation = TRUE,
                                     selection = "none",
                                     editable = list(target = "cell", disable = list(columns = c(0))),
                                     server = TRUE) # server = F doesn't work with replaceData() cell editing  

  
  # Render varieties DT with single cell
  output$varieties_table = DT::renderDT(yti$varieties, 
                                     options = list(
                                       dom = 't', # only display the table, and nothing else
                                       # searching = F, # no search box
                                       # paginate = F,  # no num of pages
                                       # lengthChange = F, # no show entries
                                       scrollX = F, # horizontal slider
                                       ordering = F # suppressing sorting 
                                     ),
                                     class = "cell-border, compact, hover", 
                                     rownames = F, #TRUE,
                                     colnames = c("Stage","Entries"),
                                     filter = "none",
                                     escape = FALSE,
                                     autoHideNavigation = TRUE,
                                     selection = "none",
                                     editable = list(target = "cell", disable = list(columns = c(0))),
                                     server = TRUE) # server = F doesn't work with replaceData() cell editing  
  
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
  
  # Update editable DT ranges_table through a proxy DT on cell edit event
  proxy = dataTableProxy('ranges_table')
  #
  observeEvent(input$ranges_table_cell_edit, {
    info = input$ranges_table_cell_edit
    i = info$row
    j = info$col # +1 required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    rti$data[i, j] = DT::coerceValue(v, rti$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, rti$data, resetPaging = FALSE)  # important 
  })
  
  # Update editable DT varieties_table through a proxy DT on cell edit event
  proxy1 = dataTableProxy('varieties_table') # BUG FIX use a different proxy name to avoid mendling with cell edits of the other DTs causing "No matching records found"
  #
  observeEvent(input$varieties_table_cell_edit, {
    info = input$varieties_table_cell_edit
    i = info$row
    j = info$col + 1 # +1 required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    yti$varieties[i, j] = DT::coerceValue(v, yti$varieties[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy1, yti$varieties, resetPaging = FALSE)  # important 
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
    yti$data = rbind(yti$data, c(length(yti$data[,1])+1,2,1,1,1,1,round(new_h2, 3),10,1000,1000))
  })
  
  observeEvent(input$delete_btn, {
    if (length(yti$data[,1])>1) # TV for >1 need to stop R coerce coercing a matrix or array to the lowest possible number of dimensions
      yti$data = yti$data[1:length(yti$data[,1])-1,,drop=F]
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$run_btn, {
    
    # store input in easier to use local variables 
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
    plotCost = isolate(yti$data[,8])
    locCost = isolate(yti$data[,9])
    fixedCost = isolate(yti$data[,10])
    varieties = isolate(as.numeric(yti$varieties[1,2])) # isolate(input$varieties) # isolate(yti$data[,11])  
    
    print(varieties)
    print(mode(varieties))
    print(mode(input$varieties))
    
    # store settings for summary plot TODO : append column with mean genetic gain for each stage
    stages_current = data.frame(stages, entries, years, locs, reps, error, h2, plotCost, locCost, fixedCost) #, varieties)

    # validate stage entries input
    # checkEntries()
    try(
      if (!validInput()) # (is.unsorted(rev(entries))) # 
      {
        # TODO pop-up message and handle exception
        shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
        stop("Invalid input: entries should not increase in later stages.")
      }
      else if (!validVarieties())
      {
        # TODO pop-up message and handle exception
        shinyalert("Oops!", "The number of final entries should be less than the number of entries in the last stage.", type = "error")
        stop("Invalid input: varieties should be less than entries in last stage.")
      }
      else
      {
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
        
        print(paste("ranges_chk = ", input$chk_ranges))
        rangesVec <<- c(rangesVec, input$chk_ranges) # add
        print(paste("rangesVec = ", rangesVec))
        
        # Create a new tab in the UI every time Run is pressed
        # UI input elements of all Scenario tabs are rendered
        output$mytabs = renderUI({
          createTab(scenarioID = tail(Scenarios,1), withRanges = rangesVec)
        })
      
      # Create a Download Report button in Overview Tab at first Run
      if (tail(Scenarios,1) == 1)
      {
        insertUI(
          selector = "#dbtn_placeholder",
          ui = tags$div(id = "dbtn_id",
                        downloadButton("download_all", "Download Report"),
          )
        )
      }

      
      # Focus on new tab
      #updateTabsetPanel(session = session, inputId = "sc_tabs", selected = paste0('Scenario', tail(Scenarios,1)))
      updateTabsetPanel(session = session, inputId = "my_tabs", selected = "Scenarios")
      
      print(paste("Start Run", tail(Scenarios,1))) # input$run_btn))
      # Run C++ code
      result = runScenario(varG,varGxL,varGxY,entries,years,locs,reps,error,varieties)
  
      if (input$chk_ranges)
      {
        # Store results for different ranges of first stage entries
        rv$results_range = rbind(rv$results_range, runScenarioRange())
        #print(rv$results_range)
        
        # First save new plot in a variable before passing it to output x4
        rpEntries <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range))
        })
        # 
        rpYears <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range), myX = "fs_years", myFilter = c("fs_reps", "fs_locs", "fs_entries"), myXl = "First Stage Years", title = "Gain by First Stage Years") 
        })
        #
        rpLocs <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range), myX = "fs_locs", myFilter = c("fs_years", "fs_reps", "fs_entries"), myXl = "First Stage Locs", title = "Gain by First Stage Locs") 
        })
        # 
        rpReps <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range), myX = "fs_reps", myFilter = c("fs_years", "fs_locs", "fs_entries"), myXl = "First Stage Reps", title = "Gain by First Stage Reps") 
        })
        # *******************************
        # Save bubble plot in variable x6
        # *******************************
        rpEntriesYears <- renderPlotly({# renderPlot({
          # plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_locs"), myX = entries, myY = years, myXl = "First Stage Entries", myYl = "First Stage Years", title = "Gain by Entries by Years")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_locs"), myX = entries, myY = years, myXl = "First Stage Entries", myYl = "First Stage Years", title = "Gain by Entries by Years")
        }) 
        #
        rpEntriesLocs <- renderPlotly({# renderPlot({
          # plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_years"), myX = entries, myY = locs, myXl = "First Stage Entries", myYl = "First Stage Locs", title = "Gain by Entries by Locs")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_years"), myX = entries, myY = locs, myXl = "First Stage Entries", myYl = "First Stage Locs", title = "Gain by Entries by Locs")
        }) 
        #
        rpEntriesReps <- renderPlotly({# renderPlot({
          # plotMeanGridBubble( myX = entries, myY = reps)
          plotMeanGridHeatmap( myX = entries, myY = reps)
        }) 
        #
        rpYearsLocs <- renderPlotly({# renderPlot({
          #plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_entries"), myX = years, myY = locs, myXl = "First Stage Years", myYl = "First Stage Locs", title = "Gain by Locs by Years")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_entries"), myX = years, myY = locs, myXl = "First Stage Years", myYl = "First Stage Locs", title = "Gain by Locs by Years")
        }) 
        #
        rpYearsReps <- renderPlotly({# renderPlot({
          #plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_locs", "fs_entries"), myX = years, myY = reps, myXl = "First Stage Years", myYl = "First Stage Reps", title = "Gain by Reps by Years")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_locs", "fs_entries"), myX = years, myY = reps, myXl = "First Stage Years", myYl = "First Stage Reps", title = "Gain by Reps by Years")
        }) 
        #
        rpLocsReps <- renderPlotly({# renderPlot({
          #plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_years", "fs_entries"), myX = locs, myY = reps, myXl = "First Stage Locs", myYl = "First Stage Reps", title = "Gain by Locs by Reps")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_years", "fs_entries"), myX = locs, myY = reps, myXl = "First Stage Locs", myYl = "First Stage Reps", title = "Gain by Locs by Reps")
        }) 
        #
        output[[paste0("rangePlotEntries", tail(Scenarios,1))]] <-  rpEntries
        output[[paste0("rangePlotYears", tail(Scenarios,1))]] <-  rpYears
        output[[paste0("rangePlotLocs", tail(Scenarios,1))]] <-  rpLocs
        output[[paste0("rangePlotReps", tail(Scenarios,1))]] <- rpReps
        output[[paste0("rangePlotEntriesYears", tail(Scenarios,1))]] <- rpEntriesYears
        output[[paste0("rangePlotEntriesLocs", tail(Scenarios,1))]] <- rpEntriesLocs
        output[[paste0("rangePlotEntriesReps", tail(Scenarios,1))]] <- rpEntriesReps
        output[[paste0("rangePlotYearsLocs", tail(Scenarios,1))]] <- rpYearsLocs
        output[[paste0("rangePlotYearsReps", tail(Scenarios,1))]] <- rpYearsReps
        output[[paste0("rangePlotLocsReps", tail(Scenarios,1))]] <- rpLocsReps
        #
        # Add observer for selectInput drop down list
        i <- tail(Scenarios,1)
        observeEvent(input[[paste0('rangePlots', i)]], {
          # print(paste("Show/Hide plot: ", input[[paste0('rangePlots', i)]]))
          showPlot(input[[paste0('rangePlots', i)]], i) # TODO : write function that handles showing/hiding based on selectInput status
        })
        #
      } # endof if chk ranges

      
      # Calculate and Store Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      stages_current$meanxTime <- meanGainxTime(result, yti$data)
      stages_current$meanxCost <- meanGainxCost(result, yti$data) # display GG for $1000

        
      # Pass plots to output scenario tabs
      output[[paste0("cyPlot", tail(Scenarios,1))]] <- renderPlot({
        plotScenario(result)
      })


        #  Update drop list for showing range plots for each Scenario           TODO make separate function
      # lapply(1: tail(Scenarios,1), function(i){
      #   # Show selection as text label
      #   output[[paste0('plotMe', i)]] <- renderText({
      #     paste("You chose", input[[paste0('rangePlots', i)]])   #  WORKS!!!
      #   })
      #   # Listener for each scenario selectInput 
      #   if (i == tail(Scenarios,1))
      #   {
      #     print(paste("lapply ", i))
      #     observeEvent(input[[paste0('rangePlots', i)]], {
      #       print(paste("Show/Hide plot: ", input[[paste0('rangePlots', i)]]))
      #       showPlot(input[[paste0('rangePlots', i)]], i) # TODO : write function that handles showing/hiding based on selectInput status
      #     })
      #   }
      #   # else      # hide previous scenario plots
      #   #  showPlot(input[[paste0('rangePlots', i)]], i) # TODO : write function that handles showing/hiding based on selectInput status
      # })
      

      
      # Store results from all runs in a reactive matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = tail(Scenarios,1))
      # Store all results of Gain per Year
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = tail(Scenarios,1), scenarioDT =  yti$data)
      # Store all results of Gain per cost 
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = tail(Scenarios,1), scenarioDT =  yti$data)
      
      # Render grouped boxplots for all scenario results for Overview tab
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })    
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })  
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxCost <- renderPlot({
        plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })   
      
            
      # Include content from R file locally as if it was pasted here to manage if-else
      source('if_run_btn.r', local=TRUE) # OLD METHOD with if-else loop handling but with duplicated code for up to 5 scenarios WORKS!
      #
      # NOT USED FOR NOW
      # source('update_scenarios.r', local = TRUE) # alternative recursive method IN PROGRESS
      # source('all_in_one.r', local = TRUE) # alternative recursive method that uses divID -- IN PROGRESS 
      #
      # # Attempt to enrich v object with I/O data for every scenario - NOT USED
      # v$data = list(v$data, list("id" = tail(Scenarios,1), 
      #                            "in" = list("varG" = varG,
      #                                        "varGxY" = varGxY,
      #                                        "varGxL" = varGxL,
      #                                        "stages_current" = stages_current,
      #                                        "varieties" = varieties), 
      #                            "out" = result)
      #               )
   
      
      # Focus on new tab
      updateTabsetPanel(session = session, inputId = "sc_tabs", selected = paste0('Scenario', tail(Scenarios,1)))
      #updateTabsetPanel(session = session, inputId = "my_tabs", selected = "Scenarios")
      
    }) #endof try()
  }) # end of run button
  
  
  # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
  # Event listener for calculating results for ranges of parameters.
  # * * * * * * * ** * * *   R A N G E S * * * * * * * * * * * * * * 
  # * * * * * * * * * * * * * * * * * * *  * * * * * * * * * * * * * 
  observeEvent(input$run_btn_r, {
    
    # Increment Scenarios counter list e.g. [1, 2, 3] for 3 scenarios, used in place of input$run_btn
    if (length(Ranges) == 0){
      Ranges <<- c(1)                # if no Scenario defines so far, make the first one "1"
    }
    else{
      Ranges <<- c(Ranges,  tail(Ranges,1)+1)   # if a Scenario is added, just add a number to the last number in the "Scenarios" vector
    }
    
    # Store results for different ranges of first stage entries
    rv$results_range_r = rbind(rv$results_range_r, runScenarioRange_r())
    # print(rv$results_range_r)
    
    # Create a drop-down list at first Run
    if (tail(Ranges,1) == 1)
    {
      insertUI(
        selector = "#plot_ranges_placeholder",
        ui = tags$div(class = "div_plot_ranges_r", checked = NA, 
        fluidRow(
           splitLayout(cellWidths = c("50%", "50%"), 
                       plotlyOutput('plotXY'), 
                       plotlyOutput('plotXYCost')),
                       # grid.arrange(plotlyOutput('plotXY'), plotlyOutput('plotXYCost'), nrow = 1),
                   
                   # Make divs appear in one line and centered
                   div( class = 'centerAlign',    
                     # Drop down list to select input for X axis
                     div(style="display:inline-block", # class = 'centerAlign', 
                         selectInput(inputId = 'xAxis', label =  "X Axis:",
                                     choices =  c("Entries", "Years", "Locs", "Reps"),
                                     selected = "Entries",
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     width = '90px'
                         )
                     ),
                     # Drop down list to select input for Y axis
                     div(style="display:inline-block", # class = 'centerAlign',    
                         selectInput(inputId = 'yAxis', label =  "Y Axis:",
                                     choices =  c("Entries", "Years", "Locs", "Reps"),
                                     selected = "Years",
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     width = '90px'
                         )
                     ),
                     # Dynamic drop down list 1
                     div(style="display:inline-block", # class = 'centerAlign',    
                         selectInput(inputId = 'subSel1', label =  "",
                                     choices = "",
                                     selected = "",
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     width = '90px'
                         )
                     ),
                     # Dynamic drop down list 2
                     div(style="display:inline-block", # class = 'centerAlign',    
                         selectInput(inputId = 'subSel2', label =  "",
                                     choices =  "",
                                     selected = "",
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     width = '90px'
                         )
                     )
                   ) # endof parent div
        ),   # END OF FLUIDROW
                   # splitLayout(cellWidths = c("50%", "50%"), 
                   #             plotlyOutput('plotXΤ'), 
                   #             plotlyOutput('plotXΤCost')),
                   plotOutput('plotXT'),
                   
                   # Make divs appear in one line and centered
                   div( class = 'centerAlign',    
                        # Drop down list to select input for X axis
                        div(style="display:inline-block", # class = 'centerAlign', 
                            selectInput(inputId = 'xAxisLine', label =  "X Axis:",
                                        choices =  c("Entries", "Years", "Locs", "Reps"),
                                        selected = "Entries",
                                        multiple = FALSE,
                                        selectize = TRUE,
                                        width = '90px'
                            )
                        ),
                        # Drop down list to select input for Y axis
                        div(style="display:inline-block", # class = 'centerAlign',    
                            selectInput(inputId = 'treatment', label =  "Treatment:",
                                        choices =  c("Entries", "Years", "Locs", "Reps"),
                                        selected = "Years",
                                        multiple = FALSE,
                                        selectize = TRUE,
                                        width = '90px'
                            )
                        ),
                        # Dynamic drop down list 1
                        div(style="display:inline-block", # class = 'centerAlign',    
                            selectizeInput(inputId = 'subSel3', label =  "",
                                        choices = "",
                                        selected = "",
                                        multiple = FALSE,
                                        width = '90px'
                                        # options = list(
                                        #   hideSelected = T # https://github.com/selectize/selectize.js/blob/master/docs/usage.md
                                        # )
                            )
                        ),
                        # Dynamic drop down list 2
                        div(style="display:inline-block", # class = 'centerAlign',    
                            selectizeInput(inputId = 'subSel4', label =  "",
                                        choices =  "",
                                        selected = "",
                                        multiple = FALSE,
                                        width = '90px'
                            )
                        )
                   ),
                   

                   # plotlyOutput('plotXYCost'),
                   # plotOutput('plotXTCost'),
                   
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br()
                   
                   
          ) # endof div plot ranges          
          
      )
    } 
    

    # Plot interactive heatmap that updates as X and Y change
    output$plotXY <- renderPlotly({       
      plotRangesHeatmap(df = isolate(rv$results_range_r), 
                        myX = input$xAxis, 
                        myY = input$yAxis, 
                        myXl = input$xAxis, 
                        myYl = input$yAxis, 
                        title = paste("Gain by", input$xAxis, "by", input$yAxis))
    })
    
    output$plotXT <- renderPlot({
      plotRangesLine(df = isolate(rv$results_range_r), 
                     myX = input$xAxisLine, 
                     myT = input$treatment, 
                     myXl = input$xAxisLine, 
                     subSel3 = input$subSel3, 
                     subSel4 = input$subSel4,
                     title = paste("Gain by", input$xAxisLine, "by", input$treatment))
      
    })
    
    output$plotXYCost <- renderPlotly({       
      plotRangesHeatmap(df = isolate(rv$results_range_r),
                        param = 'GainXCost',
                        myX = input$xAxis, 
                        myY = input$yAxis, 
                        myXl = input$xAxis, 
                        myYl = input$yAxis, 
                        title = paste("Gain per Cost by", input$xAxis, "by", input$yAxis))
    })
    
    # output$plotXTCost <- renderPlot({
    #   plotRangesLine(df = isolate(rv$results_range_r),
    #                  param = 'GainXCost',
    #                  myX = input$xAxisLine,
    #                  myT = input$treatment,
    #                  myXl = input$xAxisLine,
    #                  title = paste("Gain per Cost by", input$xAxisLine, "by", input$treatment))
    # })


    # }) # end of run ranges button - Moved below after observers
  
    
    # * * * * * * * * * * * * * * * * * * * * * * * *  * * * * 
    # Event listeners for dynamic selectInput drop down lists.
    # * * * * * * * * * *   R A N G E S  * * * * * * * * * * * 
    observeEvent(input$xAxis,{
      # TODO: should check what is selected in yAxis and from the remaining 2 vars show the first
      ops = c("Entries", "Years", "Locs", "Reps")
      sel_ops = c(input$xAxis, input$yAxis)
      ops = ops[!(ops %in% sel_ops)]
      
      df <- rv$results_range_r
      df <- filter(df, as.numeric(unlist(df["Scenario"])) %in% df["Scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
      
      updateSelectInput(session, "subSel1", ops[1],
                        choices =  df[ops[1]]) 
      updateSelectInput(session, "subSel2", ops[2],
                        choices =  df[ops[2]]) 
    })
    # * * * * * * * * * * * * * * * * * * * * * * * *  * * * * 
    observeEvent(input$yAxis,{
      # TODO: should check what is selected in xAxis and from the remaining 2 vars show the second
      ops = c("Entries", "Years", "Locs", "Reps")
      sel_ops = c(input$xAxis, input$yAxis)
      ops = ops[!(ops %in% sel_ops)]
      
      df <- rv$results_range_r
      df <- filter(df, as.numeric(unlist(df["Scenario"])) %in% df["Scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
      
      updateSelectInput(session, "subSel1", ops[1],
                        choices =  df[ops[1]])
      updateSelectInput(session, "subSel2", ops[2],
                        choices =  df[ops[2]]) 
    })
    # * * * * * * * * * * * * * * * * * * * * * * * *  * * * * 
    # * * * * * * * * * *    x L I N E   * * * * * * * * * * *   
    observeEvent(input$xAxisLine,{
      # TODO: should check what is selected in xAxis and from the remaining 2 vars show the second
      ops = c("Entries", "Years", "Locs", "Reps")
      sel_ops = c(input$xAxisLine, input$treatment)
      ops = ops[!(ops %in% sel_ops)]
      
      df <- rv$results_range_r
      df <- filter(df, as.numeric(unlist(df["Scenario"])) %in% df["Scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
      
      updateSelectInput(session, "subSel3", ops[1],
                        choices =  df[ops[1]])
      updateSelectInput(session, "subSel4", ops[2],
                        choices =  df[ops[2]]) 
    })
    # * * * * * * * * * *   T R E A T M E N T  * * * * * * * * * * *   
    observeEvent(input$treatment,{
      # TODO: should check what is selected in xAxis and from the remaining 2 vars show the second
      ops = c("Entries", "Years", "Locs", "Reps")
      sel_ops = c(input$xAxisLine, input$treatment)
      ops = ops[!(ops %in% sel_ops)]
      
      df <- rv$results_range_r
      df <- filter(df, as.numeric(unlist(df["Scenario"])) %in% df["Scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
      
      updateSelectInput(session, "subSel3", ops[1],
                        choices =  df[ops[1]])
      updateSelectInput(session, "subSel4", ops[2],
                        choices =  df[ops[2]]) 
    })
    
  }) # end of run ranges button  | NOTE : observers included so that they are triggered every time the Run button is pressed to update entries
  
  # Download Report
  output$download_all <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("summary_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Overview"
      )
      
      setColWidths(
        my_workbook,
        sheet = 1,
        cols = 1:4,
        widths = c(12, 12, 12, 12)
      )
      
      sumxGain <- meanGainSum(result = rv$results_all)
      
      writeData(
        my_workbook,
        sheet = 1,
        sumxGain,
        startRow = 4,
        startCol = 1,
        xy = NULL,
        colNames = TRUE,
        rowNames = TRUE,
        headerStyle = NULL,
        borders = c("none", "surrounding", "rows", "columns", "all"),
        borderColour = getOption("openxlsx.borderColour", "black"),
        borderStyle = getOption("openxlsx.borderStyle", "thin"),
        withFilter = FALSE,
        keepNA = FALSE,
        na.string = NULL,
        name = NULL,
        sep = ", "
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Gain",
        startRow = 4,
        startCol = 1
      )
      
      sumxTime <- meanGainSum(result = rv$results_allxTime)
      
      writeData(
        my_workbook,
        sheet = 1,
        sumxTime,
        startRow = 6 + nrow(sumxGain),
        startCol = 1, #3 + ncol(sumxGain),
        rowNames = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Gain per Year",
        startRow = 6 + nrow(sumxGain),
        startCol = 1
      )
      
      sumxCost <- meanGainSum(result = rv$results_allxCost)
      
      writeData(
        my_workbook,
        sheet = 1,
        sumxCost,
        startRow = 8 + nrow(sumxGain) + nrow(sumxTime),
        startCol = 1, #4 + 2*ncol(sumxTime),
        rowNames = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Gain per Cost",
        startRow = 8 + nrow(sumxGain) + nrow(sumxTime),
        startCol = 1
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontSize = 12,
          textDecoration = "bold"
        ),
        rows = c(4, 6 + nrow(sumxGain), 8 + nrow(sumxGain) + nrow(sumxTime)),
        cols = 1
      )
      
      
      writeData(
        my_workbook,
        sheet = 1,
        "Mean Gain Tables",
        startRow = 1,
        startCol = 1 #c(1, 3 + ncol(sumxGain))
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Overview Plots",
        startRow = 1,
        startCol = 4 + ncol(sumxGain)
      )
      
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontSize = 18,
          textDecoration = "bold"
        ),
        rows = c(1, 1),
        cols = c(1, 4 + ncol(sumxGain))
      )
      
      
      # Save plots as images in Excel file #          ---  NOT WORKING ON SHINY APPS SERVER  ---
      # print(plotScenarioGroup(rv$results_all))
      # insertPlot(my_workbook, 1, xy = c(3 + ncol(sumxGain), 4), height = 3.5, fileType = "png", units = "in")
      # #
      # print(plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)"))
      # insertPlot(my_workbook, 1, xy = c(3 + ncol(sumxGain), 24), height = 3.5, fileType = "png", units = "in")
      # #
      # print(plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)"))
      # insertPlot(my_workbook, 1, xy = c(3 + ncol(sumxGain), 44), height = 3.5, fileType = "png", units = "in")
      #
      # 
      p1 <- plotScenarioGroup(rv$results_all)
      p2 <- plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      p3 <- plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      #
      # method using ggsave
      ggsave("p1.png", plot = p1, scale = .8)  # Scale parameter resizes the object making text more legible
      ggsave("p2.png", plot = p2, scale = .8)
      ggsave("p3.png", plot = p3, scale = .8)
      insertImage(my_workbook, 1, "p1.png", height = 4, startCol = 3 + ncol(sumxGain), startRow = 4, units = "in")
      insertImage(my_workbook, 1, "p2.png", height = 4, startCol = 3 + ncol(sumxGain), startRow = 25, units = "in")
      insertImage(my_workbook, 1, "p3.png", height = 4, startCol = 3 + ncol(sumxGain), startRow = 46, units = "in")
      
      
      
      # save results of each scenario in different workbook tabs
      for (i in 1:tail(Scenarios,1))
      {
        addWorksheet(
          wb = my_workbook,
          sheetName = paste("Scenario", i)
        )

        setColWidths(
          my_workbook,
          1,
          cols = 1:4,
          widths = c(12, 12, 12, 12)
        )

        writeData(
          my_workbook,
          sheet = i+1,
          c(
            paste("Breeding Scenario", i),
            "Input Settings"
          ),
          startRow = 1,
          startCol = 1
        )

        addStyle(
          my_workbook,
          sheet = i+1,
          style = createStyle(
            fontSize = 18,
            textDecoration = "bold"
          ),
          rows = c(1, 2, 2, 8),
          cols = c(1, 1, 8, 1)
        )

        # mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
        mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, as.numeric(yti$varieties[1,2])), nrow = 1, ncol = 5)
        colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")

        writeData(
          my_workbook,
          sheet = i+1,
          mtx,
          startRow = 4,
          startCol = 1
        )

        writeData(
          my_workbook,
          sheet = i+1,
          c(
            "Yield Trials"
          ),
          startRow = 8,
          startCol = 1
        )

        # print(reactDT.list[[paste(i)]]) # 
        tmp_data = reactDT.list[[paste(i)]] # stages_current # TODO :: call to reactDT1$data
        colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')

        writeData(
          my_workbook,
          sheet = i+1,
          #yti$data,
          tmp_data,
          startRow = 10,
          startCol = 1
        )

        writeData(
          my_workbook,
          sheet = i+1,
          "Cost Summary Table",
          startRow = 2,
          startCol = 8
        )

        cost_summary <- cbind(totalYears(tmp_data, input$negen), totalLocs(tmp_data), totalPlots(tmp_data), totalLocsCost(tmp_data), totalPlotsCost(tmp_data), totalCost(tmp_data))
        colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')

        writeData(
          my_workbook,
          sheet = i+1,
          cost_summary,
          startRow = 4,
          startCol = 8
        )

      } # endof for loop over scenarios


      
      
      saveWorkbook(my_workbook, file)
    }
  )
  
  # Focus on about tab on start
  updateTabsetPanel(session = session, inputId = "my_tabs", selected = "About")
  
  
  # TV hide ALL tab 
  shiny::hideTab(inputId = "my_tabs", target = "ALL")
  # TV hide Gain per Cost plot with shinyjs package
  # hide("overviewTabxCost")
  
  # if (length(Scenarios) > 0)
  # {
  #   show("rangePlotEntriesReps1") # input[[paste0('rangePlots', i)]])   # NOT SHOWING HIDDEN!!!
  # }
  
} # endof server