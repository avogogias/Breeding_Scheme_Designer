# Global settings for all DTs in senario tabs
sumset_DT = list( options = list(
  dom = 't', # only display the table, and nothing else
  # searching = F, # no search box
  # paginate = F,  # no num of pages
  # lengthChange = F, # no show entries
  scrollX = T, # horizontal slider
  ordering = F # suppressing sorting 
),
class = "cell-border, compact, hover", 
rownames = F, #TRUE,
colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000'), 
filter = "none",
escape = FALSE,
autoHideNavigation = TRUE,
selection = "none",
editable = list(target = "cell", disable = list(columns = c(0, 6, 10, 11, 12))),
server = TRUE) # server = F doesn't work with replaceData() cell editing

# Currently results_all updates only for scenarios 1-5
# Common lines of code between Scenario IDs
# cnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2')
#RANGE cnames = c('Stage', 'Min Entries', 'Max Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2')

# The following blocks of code are repeated for every run_btn index
# Ideally this control would take place recursively but building an
# output name dynamically using assign, doesn't seem to work.
if (tail(Scenarios,1) == 1)
{
  #assign(paste0("reactDT", tail(Scenarios,1)), reactiveValues(data = stages_current)) # WORKS BUT not useful
  # reactDT1 <- reactiveValues(data = stages_current) # PREV WORKING
  
  reactDT.list[['1']] <- stages_current
  # reactDT.list <<- c(reactDT.list, list(reactDT1$data)) # list(as.data.frame(reactDT1$data))) # append it in list of reactive values
  
  output[[paste0("stages_summary", tail(Scenarios,1))]] = DT::renderDT(reactDT.list[['1']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  #output$stages_summary1 = DT::renderDT(reactDT1$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy(paste0('stages_summary', tail(Scenarios,1)))

  # observeEvent(input[[paste0('stages_summary', tail(Scenarios,1), "_cell_edit")]], {
  observeEvent(input$stages_summary1_cell_edit, {
    #info = input[[paste0('stages_summary', tail(Scenarios,1), "_cell_edit")]]
    info = input$stages_summary1_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['1']][i, j] = DT::coerceValue(v, reactDT.list[['1']][i, j])

    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['1']], resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn1, {
    
  try(
    if (!validInput(reactDT.list[['1']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['1']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT.list[['1']][,2]),isolate(reactDT.list[['1']][,3]),
                            isolate(reactDT.list[['1']][,4]),isolate(reactDT.list[['1']][,5]),
                            isolate(reactDT.list[['1']][,6]),isolate(input$varieties))
      
      output$cyPlot1 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
      
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['1']][,11] <- stages_current$mean
      
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <- meanGainxTime(result, scenarioDT = reactDT.list[['1']])
      reactDT.list[['1']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['1']])
      reactDT.list[['1']][,13] <- stages_current$meanxCost
  
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(1)
      # rv$results_all <- rv$results_all[,rv$results_all[3,]!=1] # WORKS!
      #
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 1)
  
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Gain per Year ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(1, rv$results_allxTime)
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 1, scenarioDT = reactDT.list[['1']])
      #
      # Render grouped boxplots for Gain per Year
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   
      
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(1, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 1, scenarioDT = reactDT.list[['1']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
              plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    
      # reactDT.list[1] = list(reactDT.list[['1']]) # update glabal list storing the reactive values for each scenario
      # print(reactDT.list[['1']]) # prints the updated element of the list - OK
      # print(reactDT.list[['1']])
      
    }) #endof try()
  }) # endof update btn1

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['1']]))
    {
      reactDT.list[['1']][i,7] = updateH2(reactDT.list[['1']][i,]) # round(input$varG/(input$varG + input$varGxY/reactDT.list[['1']][i,3] + input$varGxL/(reactDT.list[['1']][i,3]*reactDT.list[['1']][i,4]) + reactDT.list[['1']][i,6]/(reactDT.list[['1']][i,3]*reactDT.list[['1']][i,4]*reactDT.list[['1']][i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['1']], input$negen), totalLocs(reactDT.list[['1']]), totalPlots(reactDT.list[['1']]), totalLocsCost(reactDT.list[['1']]), totalPlotsCost(reactDT.list[['1']]), totalCost(reactDT.list[['1']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F ) 
  
  
  
  # Download Report for scenario #1
  # 
  # Download Report
  output$download_btn1 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_01_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )
      
      
      tmp_data <- reactDT.list[['1']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )
      
      
      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['1']], input$negen), totalLocs(reactDT.list[['1']]), totalPlots(reactDT.list[['1']]), totalLocsCost(reactDT.list[['1']]), totalPlotsCost(reactDT.list[['1']]), totalCost(reactDT.list[['1']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )
  
  
  
  
  
  
  

} else if (tail(Scenarios,1) == 2)
{
  reactDT.list[['2']] <- stages_current # list(as.data.frame(reactDT1$data))) # append it in list of reactive values
  
  output$stages_summary2 = DT::renderDT(reactDT.list[['2']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary2')
  #
  observeEvent(input$stages_summary2_cell_edit, {
    info = input$stages_summary2_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['2']][i, j] = DT::coerceValue(v, reactDT.list[['2']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['2']], resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn2, {
    
  try(
    if (!validInput(reactDT.list[['2']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['2']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT.list[['2']][,2]),isolate(reactDT.list[['2']][,3]),
                            isolate(reactDT.list[['2']][,4]),isolate(reactDT.list[['2']][,5]),
                            isolate(reactDT.list[['2']][,6]),isolate(input$varieties))
  
      output$cyPlot2 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
  
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['2']][,11] <- stages_current$mean
      
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['2']])
      reactDT.list[['2']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['2']])
      reactDT.list[['2']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(2)
      
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 2)
  
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(2, rv$results_allxTime) 
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 2, scenarioDT = reactDT.list[['2']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab    
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(2, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 2, scenarioDT = reactDT.list[['2']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
              plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['2']]))
    {
      reactDT.list[['2']][i,7] = updateH2(reactDT.list[['2']][i,]) # round(input$varG/(input$varG + input$varGxY/reactDT.list[['2']][i,3] + input$varGxL/(reactDT.list[['2']][i,3]*reactDT.list[['2']][i,4]) + reactDT.list[['2']][i,6]/(reactDT.list[['2']][i,3]*reactDT.list[['2']][i,4]*reactDT.list[['2']][i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['2']], input$negen), totalLocs(reactDT.list[['2']]), totalPlots(reactDT.list[['2']]), totalLocsCost(reactDT.list[['2']]), totalPlotsCost(reactDT.list[['2']]), totalCost(reactDT.list[['2']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  # Download Report for scenario #2
  # 
  # Download Report
  output$download_btn2 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_02_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )

      tmp_data <- reactDT.list[['2']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['2']], input$negen), totalLocs(reactDT.list[['2']]), totalPlots(reactDT.list[['2']]), totalLocsCost(reactDT.list[['2']]), totalPlotsCost(reactDT.list[['2']]), totalCost(reactDT.list[['2']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )  
  
} else if (tail(Scenarios,1) == 3)
{
  reactDT.list[['3']] <- stages_current
  
  output$stages_summary3 = DT::renderDT(reactDT.list[['3']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary3')
  #
  observeEvent(input$stages_summary3_cell_edit, {
    info = input$stages_summary3_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['3']][i, j] = DT::coerceValue(v, reactDT.list[['3']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['3']], resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn3, {
    
  try(
    if (!validInput(reactDT.list[['3']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['3']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT.list[['3']][,2]),isolate(reactDT.list[['3']][,3]),
                            isolate(reactDT.list[['3']][,4]),isolate(reactDT.list[['3']][,5]),
                            isolate(reactDT.list[['3']][,6]),isolate(input$varieties))
      output$cyPlot3 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
  
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['3']][,11] <- stages_current$mean
  
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['3']])
      reactDT.list[['3']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['3']])
      reactDT.list[['3']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(3)
  
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 3)
  
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(3, rv$results_allxTime)
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 3, scenarioDT = reactDT.list[['3']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab    
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(3, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 3, scenarioDT = reactDT.list[['3']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
              plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['3']]))
    {
      reactDT.list[['3']][i,7] = updateH2(reactDT.list[['3']][i,]) # round(input$varG/(input$varG + input$varGxY/reactDT.list[['3']][i,3] + input$varGxL/(reactDT.list[['3']][i,3]*reactDT.list[['3']][i,4]) + reactDT.list[['3']][i,6]/(reactDT.list[['3']][i,3]*reactDT.list[['3']][i,4]*reactDT.list[['3']][i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['3']], input$negen), totalLocs(reactDT.list[['3']]), totalPlots(reactDT.list[['3']]), totalLocsCost(reactDT.list[['3']]), totalPlotsCost(reactDT.list[['3']]), totalCost(reactDT.list[['3']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  

  # Download Report for scenario #3
  # 
  # Download Report
  output$download_btn3 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_03_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )

      tmp_data <- reactDT.list[['3']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['3']], input$negen), totalLocs(reactDT.list[['3']]), totalPlots(reactDT.list[['3']]), totalLocsCost(reactDT.list[['3']]), totalPlotsCost(reactDT.list[['3']]), totalCost(reactDT.list[['3']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )  
  
} else if (tail(Scenarios,1) == 4)
{
  reactDT.list[['4']] <- stages_current
  
  output$stages_summary4 = DT::renderDT(reactDT.list[['4']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary4')
  #
  observeEvent(input$stages_summary4_cell_edit, {
    info = input$stages_summary4_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['4']][i, j] = DT::coerceValue(v, reactDT.list[['4']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['4']], resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn4, {
    
  try(
    if (!validInput(reactDT.list[['4']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['4']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT.list[['4']][,2]),isolate(reactDT.list[['4']][,3]),
                            isolate(reactDT.list[['4']][,4]),isolate(reactDT.list[['4']][,5]),
                            isolate(reactDT.list[['4']][,6]),isolate(input$varieties))
      output$cyPlot4 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
  
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['4']][,11] <- stages_current$mean
  
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['4']])
      reactDT.list[['4']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['4']])
      reactDT.list[['4']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(4)
  
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 4)
  
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(4, rv$results_allxTime) 
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 4, scenarioDT = reactDT.list[['4']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab    
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(4, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 4, scenarioDT = reactDT.list[['4']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
              plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['4']]))
    {
      reactDT.list[['4']][i,7] = updateH2(reactDT.list[['4']][i,]) # round(input$varG/(input$varG + input$varGxY/reactDT.list[['4']][i,3] + input$varGxL/(reactDT.list[['4']][i,3]*reactDT.list[['4']][i,4]) + reactDT.list[['4']][i,6]/(reactDT.list[['4']][i,3]*reactDT.list[['4']][i,4]*reactDT.list[['4']][i,5])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['4']], input$negen), totalLocs(reactDT.list[['4']]), totalPlots(reactDT.list[['4']]), totalLocsCost(reactDT.list[['4']]), totalPlotsCost(reactDT.list[['4']]), totalCost(reactDT.list[['4']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  

  # Download Report for scenario #4
  # 
  # Download Report
  output$download_btn4 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_04_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )
      
      tmp_data <- reactDT.list[['4']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['4']], input$negen), totalLocs(reactDT.list[['4']]), totalPlots(reactDT.list[['4']]), totalLocsCost(reactDT.list[['4']]), totalPlotsCost(reactDT.list[['4']]), totalCost(reactDT.list[['4']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )    
  
  
} else if (tail(Scenarios,1) == 5)
{
  reactDT.list[['5']] <- stages_current
  
  output$stages_summary5 = DT::renderDT(reactDT.list[['5']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary5')
  #
  observeEvent(input$stages_summary5_cell_edit, {
    info = input$stages_summary5_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['5']][i, j] = DT::coerceValue(v, reactDT.list[['5']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['5']], resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn5, {
    
  try(
    if (!validInput(reactDT.list[['5']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['5']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT.list[['5']][,2]),isolate(reactDT.list[['5']][,3]),
                            isolate(reactDT.list[['5']][,4]),isolate(reactDT.list[['5']][,5]),
                            isolate(reactDT.list[['5']][,6]),isolate(input$varieties))
      output$cyPlot5 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
  
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['5']][,11] <- stages_current$mean
  
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['5']])
      reactDT.list[['5']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['5']])
      reactDT.list[['5']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(5)
  
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 5)
  
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
  
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(5, rv$results_allxTime)
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 5, scenarioDT = reactDT.list[['5']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(5, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 5, scenarioDT = reactDT.list[['5']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
              plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['5']]))
    {
      reactDT.list[['5']][i,7] = updateH2(reactDT.list[['5']][i,]) # round(input$varG/(input$varG + input$varGxY/reactDT.list[['5']][i,3] + input$varGxL/(reactDT.list[['5']][i,3]*reactDT.list[['5']][i,4]) + reactDT.list[['5']][i,6]/(reactDT.list[['5']][i,3]*reactDT.list[['5']][i,4]*reactDT.list[['5']][i,5])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['5']], input$negen), totalLocs(reactDT.list[['5']]), totalPlots(reactDT.list[['5']]), totalLocsCost(reactDT.list[['5']]), totalPlotsCost(reactDT.list[['5']]), totalCost(reactDT.list[['5']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  # Download Report for scenario #5
  # 
  # Download Report
  output$download_btn5 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_05_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )

      tmp_data <- reactDT.list[['5']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )

      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['5']], input$negen), totalLocs(reactDT.list[['5']]), totalPlots(reactDT.list[['5']]), totalLocsCost(reactDT.list[['5']]), totalPlotsCost(reactDT.list[['5']]), totalCost(reactDT.list[['5']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )  
  
} else if (tail(Scenarios,1) == 6)
{
  reactDT.list[['6']] <- stages_current
  
  output$stages_summary6 = DT::renderDT(reactDT.list[['6']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary6')
  #
  observeEvent(input$stages_summary6_cell_edit, {
    info = input$stages_summary6_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['6']][i, j] = DT::coerceValue(v, reactDT.list[['6']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['6']], resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn6, {
    
  try(
    if (!validInput(reactDT.list[['6']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['6']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                           isolate(reactDT.list[['6']][,2]),isolate(reactDT.list[['6']][,3]),
                           isolate(reactDT.list[['6']][,4]),isolate(reactDT.list[['6']][,5]),
                           isolate(reactDT.list[['6']][,6]),isolate(input$varieties))
      output$cyPlot6 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
      
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['6']][,11] <- stages_current$mean    
      
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['6']])
      reactDT.list[['6']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['6']])
      reactDT.list[['6']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(6)
      
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 6)
      
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(6, rv$results_allxTime)
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 6, scenarioDT = reactDT.list[['6']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(6, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 6, scenarioDT = reactDT.list[['6']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
        plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['6']]))
    {
      reactDT.list[['6']][i,7] = updateH2(reactDT.list[['6']][i,]) # round(input$varG/(input$varG + input$varGxY/reactDT.list[['6']][i,3] + input$varGxL/(reactDT.list[['6']][i,3]*reactDT.list[['6']][i,4]) + reactDT.list[['6']][i,6]/(reactDT.list[['6']][i,3]*reactDT.list[['6']][i,4]*reactDT.list[['6']][i,6])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['6']], input$negen), totalLocs(reactDT.list[['6']]), totalPlots(reactDT.list[['6']]), totalLocsCost(reactDT.list[['6']]), totalPlotsCost(reactDT.list[['6']]), totalCost(reactDT.list[['6']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )

  

  # Download Report for scenario #6
  # 
  # Download Report
  output$download_btn6 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_06_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )

      tmp_data <- reactDT.list[['6']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )

      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['6']], input$negen), totalLocs(reactDT.list[['6']]), totalPlots(reactDT.list[['6']]), totalLocsCost(reactDT.list[['6']]), totalPlotsCost(reactDT.list[['6']]), totalCost(reactDT.list[['6']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )
  
    
} else if (tail(Scenarios,1) == 7)
{
  reactDT.list[['7']] <- stages_current
  
  output$stages_summary7 = DT::renderDT(reactDT.list[['7']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary7')
  #
  observeEvent(input$stages_summary7_cell_edit, {
    info = input$stages_summary7_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['7']][i, j] = DT::coerceValue(v, reactDT.list[['7']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['7']], resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn7, {
    
  try(
    if (!validInput(reactDT.list[['7']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['7']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                           isolate(reactDT.list[['7']][,2]),isolate(reactDT.list[['7']][,3]),
                           isolate(reactDT.list[['7']][,4]),isolate(reactDT.list[['7']][,5]),
                           isolate(reactDT.list[['7']][,6]),isolate(input$varieties))
      output$cyPlot7 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
      
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['7']][,11] <- stages_current$mean 
      
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['7']])
      reactDT.list[['7']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['7']])
      reactDT.list[['7']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(7)
      
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 7)
      
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(7, rv$results_allxTime)
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 7, scenarioDT = reactDT.list[['7']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(7, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 7, scenarioDT = reactDT.list[['7']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
        plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['7']]))
    {
      reactDT.list[['7']][i,7] = updateH2(reactDT.list[['7']][i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['7']], input$negen), totalLocs(reactDT.list[['7']]), totalPlots(reactDT.list[['7']]), totalLocsCost(reactDT.list[['7']]), totalPlotsCost(reactDT.list[['7']]), totalCost(reactDT.list[['7']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  
  # Download Report for scenario #7
  # 
  # Download Report
  output$download_btn7 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_07_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )

      tmp_data <- reactDT.list[['7']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['7']], input$negen), totalLocs(reactDT.list[['7']]), totalPlots(reactDT.list[['7']]), totalLocsCost(reactDT.list[['7']]), totalPlotsCost(reactDT.list[['7']]), totalCost(reactDT.list[['7']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )
  
  
} else if (tail(Scenarios,1) == 8)
{
  reactDT.list[['8']] <- stages_current
  
  output$stages_summary8 = DT::renderDT(reactDT.list[['8']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary8')
  #
  observeEvent(input$stages_summary8_cell_edit, {
    info = input$stages_summary8_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['8']][i, j] = DT::coerceValue(v, reactDT.list[['8']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['8']], resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn8, {
    
  try(
    if (!validInput(reactDT.list[['8']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['8']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                           isolate(reactDT.list[['8']][,2]),isolate(reactDT.list[['8']][,3]),
                           isolate(reactDT.list[['8']][,4]),isolate(reactDT.list[['8']][,5]),
                           isolate(reactDT.list[['8']][,6]),isolate(input$varieties))
      output$cyPlot8 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
      
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['8']][,11] <- stages_current$mean   
      
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['8']])
      reactDT.list[['8']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['8']])
      reactDT.list[['8']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(8)
      
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 8)
      
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(8, rv$results_allxTime)
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 8, scenarioDT = reactDT.list[['8']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(8, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 8, scenarioDT = reactDT.list[['8']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
        plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['8']]))
    {
      reactDT.list[['8']][i,7] = updateH2(reactDT.list[['8']][i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['8']], input$negen), totalLocs(reactDT.list[['8']]), totalPlots(reactDT.list[['8']]), totalLocsCost(reactDT.list[['8']]), totalPlotsCost(reactDT.list[['8']]), totalCost(reactDT.list[['8']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  
  # Download Report for scenario #8
  # 
  # Download Report
  output$download_btn8 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_08_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )

      tmp_data <- reactDT.list[['8']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['8']], input$negen), totalLocs(reactDT.list[['8']]), totalPlots(reactDT.list[['8']]), totalLocsCost(reactDT.list[['8']]), totalPlotsCost(reactDT.list[['8']]), totalCost(reactDT.list[['8']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )
  
  
} else if (tail(Scenarios,1) == 9)
{
  reactDT.list[['9']] <- stages_current
  
  output$stages_summary9 = DT::renderDT(reactDT.list[['9']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary9')
  #
  observeEvent(input$stages_summary9_cell_edit, {
    info = input$stages_summary9_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['9']][i, j] = DT::coerceValue(v, reactDT.list[['9']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['9']], resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn9, {
    
  try(
    if (!validInput(reactDT.list[['9']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['9']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                           isolate(reactDT.list[['9']][,2]),isolate(reactDT.list[['9']][,3]),
                           isolate(reactDT.list[['9']][,4]),isolate(reactDT.list[['9']][,5]),
                           isolate(reactDT.list[['9']][,6]),isolate(input$varieties))
      output$cyPlot9 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
      
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['9']][,11] <- stages_current$mean 
      
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['9']])
      reactDT.list[['9']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['9']])
      reactDT.list[['9']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(9)
      
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 9)
      
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(9, rv$results_allxTime)
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 9, scenarioDT = reactDT.list[['9']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(9, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 9, scenarioDT = reactDT.list[['9']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
        plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['9']]))
    {
      reactDT.list[['9']][i,7] = updateH2(reactDT.list[['9']][i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['9']], input$negen), totalLocs(reactDT.list[['9']]), totalPlots(reactDT.list[['9']]), totalLocsCost(reactDT.list[['9']]), totalPlotsCost(reactDT.list[['9']]), totalCost(reactDT.list[['9']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  
  
  # Download Report for scenario #9
  # 
  # Download Report
  output$download_btn9 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_09_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )
      
      tmp_data <- reactDT.list[['9']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['9']], input$negen), totalLocs(reactDT.list[['9']]), totalPlots(reactDT.list[['9']]), totalLocsCost(reactDT.list[['9']]), totalPlotsCost(reactDT.list[['9']]), totalCost(reactDT.list[['9']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )
  
  
} else if (tail(Scenarios,1) == 10)
{
  reactDT.list[['10']] <- stages_current
  
  output$stages_summary10 = DT::renderDT(reactDT.list[['10']], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary10')
  #
  observeEvent(input$stages_summary10_cell_edit, {
    info = input$stages_summary10_cell_edit
    i = info$row
    j = info$col + 1 # required when rownames = F in DT
    v = info$value
    str(info)
    # Character string needs to be coerced to same type as target value. Here as.integer()
    reactDT.list[['10']][i, j] = DT::coerceValue(v, reactDT.list[['10']][i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT.list[['10']], resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn10, {
    
  try(
    if (!validInput(reactDT.list[['10']])) # (is.unsorted(rev(entries))) # 
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
      stop("Invalid input: entries should not increase in later stages.")
    }
    else if (!validVarieties(reactDT.list[['10']]))
    {
      # TODO pop-up message and handle exception
      shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
      stop("Invalid input: varieties should be less than entries in last stage.")
    }
    else
    {
      result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                           isolate(reactDT.list[['10']][,2]),isolate(reactDT.list[['10']][,3]),
                           isolate(reactDT.list[['10']][,4]),isolate(reactDT.list[['10']][,5]),
                           isolate(reactDT.list[['10']][,6]),isolate(input$varieties))
      output$cyPlot10 <- renderPlot({
        plotScenario(result)
      })   # end of renderPlot
      
      # Update Mean Genetic Gain for each stage in summary table
      stages_current$mean <- meanGain(result)
      reactDT.list[['10']][,11] <- stages_current$mean
      
      # Update Mean Genetic Gain x Time for each stage in summary table
      stages_current$meanxTime <-meanGainxTime(result, scenarioDT = reactDT.list[['10']])
      reactDT.list[['10']][,12] <- stages_current$meanxTime
      
      # Update Mean Genetic Gain x Cost for each stage in summary table
      stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[['10']])
      reactDT.list[['10']][,13] <- stages_current$meanxCost
      
      # Update results_all entries
      # First remove previous run entries
      rv$results_all = removeScenarioResult(10)
      
      # Then add to matrix
      rv$results_all = storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = 10)
      
      # Render Group Boxplot with updated entries
      output$overviewTab <- renderPlot({
        plotScenarioGroup(rv$results_all)
      })   # end of renderPlot for Overview tab
      
      
      #------------ Overview Plot x Time --------------#
      #------------------------------------------------#
      # Update results_allxTime entries
      # First remove previous run entries
      rv$results_allxTime = removeScenarioResult(10, rv$results_allxTime)
      #
      # Store all results conditioned by Time in rv
      rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 10, scenarioDT = reactDT.list[['10']])
      #
      # Render grouped boxplots for all scenario results conditioned by Time (i.e. Total Years)
      output$overviewTabxTime <- renderPlot({
        plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
      })   # end of renderPlot for Overview tab
      
      #------------ Overview Gain per Cost ------------#
      #------------------------------------------------#
      # First remove previous run entries
      rv$results_allxCost = removeScenarioResult(10, rv$results_allxCost)
      # Store all Gain results conditioned by Cost
      rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 10, scenarioDT = reactDT.list[['10']])
      #
      # Render grouped boxplots for Gain per Cost
      output$overviewTabxCost <- renderPlot({
        plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
      })  
    }) #endof try()
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[['10']]))
    {
      reactDT.list[['10']][i,7] = updateH2(reactDT.list[['10']][i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT.list[['10']], input$negen), totalLocs(reactDT.list[['10']]), totalPlots(reactDT.list[['10']]), totalLocsCost(reactDT.list[['10']]), totalPlotsCost(reactDT.list[['10']]), totalCost(reactDT.list[['10']])), 
                                                               options = sumset_DT$options,
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  
  
  # Download Report for scenario #10
  # 
  # Download Report
  output$download_btn10 <- downloadHandler(
    filename = function() {
      # return(paste0("report_", Sys.Date(), ".csv")) # csv version
      return(paste0("scenario_10_report_", Sys.Date(), ".xlsx"))
    },
    content = function(file) {
      #write.csv(rv$results_all, file, row.names = FALSE) # csv version
      my_workbook <- createWorkbook()
      
      addWorksheet(
        wb = my_workbook,
        sheetName = "Summary"
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
        c(
          "Breeding Scenario",
          "Input Settings"
        ),
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
        rows = c(1, 2, 2, 8),
        cols = c(1, 1, 8, 1)
      )
      
      mtx <- matrix(c(input$varG, input$varGxL, input$varGxY, input$negen, input$varieties), nrow = 1, ncol = 5)
      colnames(mtx) <- c("Genetic Variance", "GxL(Y)", "GxY", "Multiplication Time(Y)", "Selected Parents")
      
      writeData(
        my_workbook,
        sheet = 1,
        mtx,
        startRow = 4,
        startCol = 1
      )
      
      writeData(
        my_workbook,
        sheet = 1,
        c(
          "Yield Trials"
        ),
        startRow = 8,
        startCol = 1
      )

      tmp_data <- reactDT.list[['10']]
      colnames(tmp_data) = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000')
      
      writeData(
        my_workbook,
        sheet = 1,
        #yti$data,
        tmp_data,
        startRow = 10,
        startCol = 1
      )

      writeData(
        my_workbook,
        sheet = 1,
        "Cost Summary Table",
        startRow = 2,
        startCol = 8
      )
      
      #cost_summary <- cbind(totalYears(yti$data), totalLocs(yti$data), totalPlots(yti$data), totalLocsCost(yti$data), totalPlotsCost(yti$data), totalCost(yti$data))
      cost_summary <- cbind(totalYears(reactDT.list[['10']], input$negen), totalLocs(reactDT.list[['10']]), totalPlots(reactDT.list[['10']]), totalLocsCost(reactDT.list[['10']]), totalPlotsCost(reactDT.list[['10']]), totalCost(reactDT.list[['10']]))
      colnames(cost_summary) <- c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost')
      
      writeData(
        my_workbook,
        sheet = 1,
        cost_summary,
        startRow = 4,
        startCol = 8
      )
      
      saveWorkbook(my_workbook, file)
    }
  )
  
} #endof if loop


#print(reactDT.list) #[[1]])

  
