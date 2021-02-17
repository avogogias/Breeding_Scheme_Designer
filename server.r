library(DT)
library(Rcpp)
library(RcppArmadillo)
library(ggplot2) 
library(shinyjs)
library(data.table)
library(dplyr)
library(plotly)
library(openxlsx)

sourceCpp("Engine.cpp")

server <- function(input, output, clientData, session) {
  
  # ***************************************************** #
  # ********************* VARIABLES ********************* #
  # ********************* --------- ********************* #
  # ***************************************************** #
  
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
  yt = cbind(stage,entries,years,locs,reps,error,h2)
  
  # per-session reactive values object to store all results of this user session
  rv <- reactiveValues(results_all = NULL, results_allxTime = NULL, results_allxCost = NULL, results_range = NULL)
  # defines a common reactive list to store all scenario input info (stages DT + other) to replace reactDT
  scenariosInput <- reactiveValues(stagesDT = list(), varG = list(), varGxL = list(), varGxY = list(), varieties = list()) # initially will store stages_current and updated accordingly
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
  totalCost <- function(scenarioDT = yti$data, costPerLoc = input$costPerLoc, costPerPlot = input$costPerPlot, costFixed = input$costFixed) {
    tc = totalLocsCost(scenarioDT, costPerLoc) + totalPlotsCost(scenarioDT, costPerPlot) + costFixed
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
               downloadButton(paste0("download_btn", i), "Download Report"),
               # cost table for this scenario
               DT::DTOutput(paste0('costDT', i)),
               # Start section with plots of ranges
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
               #
      )
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
                               varieties = input$varieties) 
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
  validVarieties <- function(scenarioDT = yti$data, varieties = input$varieties, min_entries = input$entries_range[1]) {
    entries = scenarioDT[,2]
    last_entries = tail(entries, 1)
    if (varieties > last_entries )
      return(FALSE)
    return(TRUE)
  }
  
  # function that returns vector of the incremental mean gain per stage calculated from runScenario() result matrix
  meanGain <- function(result = result) {
    # first calculate aggregated mean gain for each stage
    result <- round(apply(result, 1, mean), 2)
    return(result)
  } 
  
  # function that returns vector of the incremental mean gain per stage calculated from runScenario() result matrix
  meanGainInc <- function(result = result) {
    # first calculate aggregated mean gain for each stage
    result <- round(apply(result, 1, mean), 2)
    # replace accumulative gain with incremental gain per stage
    gain <- result
    if (length(gain)>1)
      for (i in 2:length(gain)) {result[i] <- round(gain[i]-gain[i-1], 2)} 
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
                                       scrollX = T, # horizontal slider
                                       ordering = F # suppressing sorting 
                                     ),
                                     class = "cell-border, compact, hover", 
                                     rownames = F, #TRUE,
                                     colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', toString(withMathJax('$$h^2$$'))), # '$$h_2$$'),  # 'h2'),
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
                                     scrollX = T, # horizontal slider
                                     ordering = F # suppressing sorting 
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
    varieties = isolate(input$varieties)
    
    # store settings for summary plot TODO : append column with mean genetic gain for each stage
    stages_current = data.frame(stages, entries, years, locs, reps, error, h2)

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
        shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
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
        
      # Create a new tab in the UI every time Run is pressed
      # UI input elements of all Scenario tabs are rendered
      output$mytabs = renderUI({
        createTab(scenarioID = tail(Scenarios,1))
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
  
      # Store results for different ranges of first stage entries
      rv$results_range = rbind(rv$results_range, runScenarioRange())
      #print(rv$results_range)
      
      # Calculate and Store Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      stages_current$meanxTime <- meanGainxTime(result, yti$data)
      stages_current$meanxCost <- meanGainxCost(result, yti$data) # display GG for $1000

      # Show 2nd Progress Bar for Plotting
      withProgress(message = 'Ploting results', value = 0, {
        
        # First save new plot in a variable before passing it to output x4
        rpEntries <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range))
        })
        # 
        incProgress(1/10, detail = paste("Plot of Entries"))
        #
        rpYears <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range), myX = "fs_years", myFilter = c("fs_reps", "fs_locs", "fs_entries"), myXl = "First Stage Years", title = "Gain by First Stage Years") 
        })
        #
        incProgress(1/10, detail = paste("Plot of Years"))
        #
        rpLocs <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range), myX = "fs_locs", myFilter = c("fs_years", "fs_reps", "fs_entries"), myXl = "First Stage Locs", title = "Gain by First Stage Locs") 
        })
        # 
        incProgress(1/10, detail = paste("Plot of Locs"))
        #
        rpReps <- renderPlot({
          plotMeanGrid(df = isolate(rv$results_range), myX = "fs_reps", myFilter = c("fs_years", "fs_locs", "fs_entries"), myXl = "First Stage Reps", title = "Gain by First Stage Reps") 
        })
        #
        incProgress(1/10, detail = paste("Plot of Reps"))
        #
        # *******************************
        # Save bubble plot in variable x6
        # *******************************
        rpEntriesYears <- renderPlotly({# renderPlot({
          # plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_locs"), myX = entries, myY = years, myXl = "First Stage Entries", myYl = "First Stage Years", title = "Gain by Entries by Years")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_locs"), myX = entries, myY = years, myXl = "First Stage Entries", myYl = "First Stage Years", title = "Gain by Entries by Years")
        }) 
        #
        incProgress(1/10, detail = paste("Plot of Entries by Years"))
        #
        rpEntriesLocs <- renderPlotly({# renderPlot({
          # plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_years"), myX = entries, myY = locs, myXl = "First Stage Entries", myYl = "First Stage Locs", title = "Gain by Entries by Locs")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_years"), myX = entries, myY = locs, myXl = "First Stage Entries", myYl = "First Stage Locs", title = "Gain by Entries by Locs")
        }) 
        #
        incProgress(1/10, detail = paste("Plot of Entries by Locs"))
        #
        rpEntriesReps <- renderPlotly({# renderPlot({
          # plotMeanGridBubble( myX = entries, myY = reps)
          plotMeanGridHeatmap( myX = entries, myY = reps)
        }) 
        #
        incProgress(1/10, detail = paste("Plot of Entries by Reps"))
        #
        rpYearsLocs <- renderPlotly({# renderPlot({
          #plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_entries"), myX = years, myY = locs, myXl = "First Stage Years", myYl = "First Stage Locs", title = "Gain by Locs by Years")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_reps", "fs_entries"), myX = years, myY = locs, myXl = "First Stage Years", myYl = "First Stage Locs", title = "Gain by Locs by Years")
        }) 
        #
        incProgress(1/10, detail = paste("Plot of Years by Locs"))
        #
        rpYearsReps <- renderPlotly({# renderPlot({
          #plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_locs", "fs_entries"), myX = years, myY = reps, myXl = "First Stage Years", myYl = "First Stage Reps", title = "Gain by Reps by Years")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_locs", "fs_entries"), myX = years, myY = reps, myXl = "First Stage Years", myYl = "First Stage Reps", title = "Gain by Reps by Years")
        }) 
        #
        incProgress(1/10, detail = paste("Plot of Years by Reps"))
        #
        rpLocsReps <- renderPlotly({# renderPlot({
          #plotMeanGridBubble(df = isolate(rv$results_range), myFilter = c("fs_years", "fs_entries"), myX = locs, myY = reps, myXl = "First Stage Locs", myYl = "First Stage Reps", title = "Gain by Locs by Reps")
          plotMeanGridHeatmap(df = isolate(rv$results_range), myFilter = c("fs_years", "fs_entries"), myX = locs, myY = reps, myXl = "First Stage Locs", myYl = "First Stage Reps", title = "Gain by Locs by Reps")
        }) 
        
        # Pass plots to output scenario tabs
        output[[paste0("cyPlot", tail(Scenarios,1))]] <- renderPlot({
          plotScenario(result)
        })
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
        #Sys.sleep(5)
        incProgress(1/10, detail = paste("Plot of Locs by Reps"))
      }) # endof 2nd progress bar
      
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
      
      # Add observer for selectInput drop down list
      i <- tail(Scenarios,1)
      observeEvent(input[[paste0('rangePlots', i)]], {
        # print(paste("Show/Hide plot: ", input[[paste0('rangePlots', i)]]))
        showPlot(input[[paste0('rangePlots', i)]], i) # TODO : write function that handles showing/hiding based on selectInput status
      })
      
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
        sheetName = "Gain"
      )
      
      setColWidths(
        my_workbook,
        1,
        cols = 1:4,
        widths = c(12, 12, 12, 12)
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Summary Tables",
        startRow = 1,
        startCol = 1
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontSize = 18,
          textDecoration = "bold"
        ),
        rows = 1,
        cols = 1
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
        startRow = 8 + nrow(sumxGain),
        startCol = 1, #3 + ncol(sumxGain),
        rowNames = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Gain per Year",
        startRow = 8 + nrow(sumxGain),
        startCol = 1
      )
      
      sumxCost <- meanGainSum(result = rv$results_allxCost)
      
      writeData(
        my_workbook,
        sheet = 1,
        sumxCost,
        startRow = 12 + nrow(sumxGain) + nrow(sumxTime),
        startCol = 1, #4 + 2*ncol(sumxTime),
        rowNames = TRUE
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Gain per Cost",
        startRow = 12 + nrow(sumxGain) + nrow(sumxTime),
        startCol = 1
      )
      
      addStyle(
        my_workbook,
        sheet = 1,
        style = createStyle(
          fontSize = 12,
          textDecoration = "bold"
        ),
        rows = c(4, 8 + nrow(sumxGain), 12 + nrow(sumxGain) + nrow(sumxTime)),
        cols = 1
      )
      
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