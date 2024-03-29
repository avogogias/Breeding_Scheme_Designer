# Currently results_all updates only for scenarios 1-5
# Common lines of code between Scenario IDs
#PREV cnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', 'h2')
cnames = c('Stage', 'Min Entries', 'Max Entries', 'Years', 'Locs', 'Reps', 'Plot Error', 'h2')
output[[paste0("cyPlot", tail(Scenarios,1))]] <- nplot


# The following blocks of code are repeated for every run_btn index
# Ideally this control would take place recursively but building an
# output name dynamically using assign, doesn't seem to work.
if (tail(Scenarios,1) == 1)
{
  #assign(paste0("reactDT", tail(Scenarios,1)), reactiveValues(data = stages_current)) # WORKS BUT not useful
  reactDT1 <- reactiveValues(data = stages_current)
  output[[paste0("stages_summary", tail(Scenarios,1))]] = DT::renderDT(reactDT1$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT1$data[i, j] = DT::coerceValue(v, reactDT1$data[i, j])

    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT1$data, resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn1, {
    
    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                          isolate(reactDT1$data[,2]),isolate(reactDT1$data[,3]),
                          isolate(reactDT1$data[,4]),isolate(reactDT1$data[,5]),
                          isolate(reactDT1$data[,6]),isolate(input$varieties))
    
    output$cyPlot1 <- renderPlot({
      plotScenario(result)
    })   # end of renderPlot

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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 1, scenarioDT = reactDT1$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 1, scenarioDT = reactDT1$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
            plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn1

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT1$data))
    {
      reactDT1$data[i,7] = updateH2(reactDT1$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT1$data[i,3] + input$varGxL/(reactDT1$data[i,3]*reactDT1$data[i,4]) + reactDT1$data[i,6]/(reactDT1$data[i,3]*reactDT1$data[i,4]*reactDT1$data[i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT1$data, input$negen), totalLocs(reactDT1$data), totalPlots(reactDT1$data), totalLocsCost(reactDT1$data), totalPlotsCost(reactDT1$data), totalCost(reactDT1$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )  

} else if (tail(Scenarios,1) == 2)
{
  # output$cyPlot2 <- nplot
  reactDT2 <- reactiveValues(data = stages_current)
  output$stages_summary2 = DT::renderDT(reactDT2$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT2$data[i, j] = DT::coerceValue(v, reactDT2$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT2$data, resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn2, {
    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                          isolate(reactDT2$data[,2]),isolate(reactDT2$data[,3]),
                          isolate(reactDT2$data[,4]),isolate(reactDT2$data[,5]),
                          isolate(reactDT2$data[,6]),isolate(input$varieties))

    output$cyPlot2 <- renderPlot({

      plotScenario(result)
    })   # end of renderPlot

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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 2, scenarioDT = reactDT2$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 2, scenarioDT = reactDT2$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
            plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT2$data))
    {
      reactDT2$data[i,7] = updateH2(reactDT2$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT2$data[i,3] + input$varGxL/(reactDT2$data[i,3]*reactDT2$data[i,4]) + reactDT2$data[i,6]/(reactDT2$data[i,3]*reactDT2$data[i,4]*reactDT2$data[i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT2$data, input$negen), totalLocs(reactDT2$data), totalPlots(reactDT2$data), totalLocsCost(reactDT2$data), totalPlotsCost(reactDT2$data), totalCost(reactDT2$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
} else if (tail(Scenarios,1) == 3)
{
  # output$cyPlot3 <- nplot
  reactDT3 <- reactiveValues(data = stages_current)
  output$stages_summary3 = DT::renderDT(reactDT3$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT3$data[i, j] = DT::coerceValue(v, reactDT3$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT3$data, resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn3, {

    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                          isolate(reactDT3$data[,2]),isolate(reactDT3$data[,3]),
                          isolate(reactDT3$data[,4]),isolate(reactDT3$data[,5]),
                          isolate(reactDT3$data[,6]),isolate(input$varieties))
    output$cyPlot3 <- renderPlot({

      plotScenario(result)
    })   # end of renderPlot


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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 3, scenarioDT = reactDT3$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 3, scenarioDT = reactDT3$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
            plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT3$data))
    {
      reactDT3$data[i,7] = updateH2(reactDT3$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT3$data[i,3] + input$varGxL/(reactDT3$data[i,3]*reactDT3$data[i,4]) + reactDT3$data[i,6]/(reactDT3$data[i,3]*reactDT3$data[i,4]*reactDT3$data[i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT3$data, input$negen), totalLocs(reactDT3$data), totalPlots(reactDT3$data), totalLocsCost(reactDT3$data), totalPlotsCost(reactDT3$data), totalCost(reactDT3$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
} else if (tail(Scenarios,1) == 4)
{
  # output$cyPlot4 <- nplot
  reactDT4 <- reactiveValues(data = stages_current)
  output$stages_summary4 = DT::renderDT(reactDT4$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT4$data[i, j] = DT::coerceValue(v, reactDT4$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT4$data, resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn4, {

    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                          isolate(reactDT4$data[,2]),isolate(reactDT4$data[,3]),
                          isolate(reactDT4$data[,4]),isolate(reactDT4$data[,5]),
                          isolate(reactDT4$data[,6]),isolate(input$varieties))
    output$cyPlot4 <- renderPlot({

      plotScenario(result)
    })   # end of renderPlot



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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 4, scenarioDT = reactDT4$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 4, scenarioDT = reactDT4$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
            plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  

  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT4$data))
    {
      reactDT4$data[i,7] = updateH2(reactDT4$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT4$data[i,3] + input$varGxL/(reactDT4$data[i,3]*reactDT4$data[i,4]) + reactDT4$data[i,6]/(reactDT4$data[i,3]*reactDT4$data[i,4]*reactDT4$data[i,5])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT4$data, input$negen), totalLocs(reactDT4$data), totalPlots(reactDT4$data), totalLocsCost(reactDT4$data), totalPlotsCost(reactDT4$data), totalCost(reactDT4$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
  
} else if (tail(Scenarios,1) == 5)
{
  # output$cyPlot5 <- nplot
  reactDT5 <- reactiveValues(data = stages_current)
  output$stages_summary5 = DT::renderDT(reactDT5$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT5$data[i, j] = DT::coerceValue(v, reactDT5$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT5$data, resetPaging = FALSE)  # important
  })

  # Execute runScenario() for the current settings
  observeEvent(input$update_btn5, {

    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                          isolate(reactDT5$data[,2]),isolate(reactDT5$data[,3]),
                          isolate(reactDT5$data[,4]),isolate(reactDT5$data[,5]),
                          isolate(reactDT5$data[,6]),isolate(input$varieties))
    output$cyPlot5 <- renderPlot({

      plotScenario(result)
    })   # end of renderPlot



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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 5, scenarioDT = reactDT5$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 5, scenarioDT = reactDT5$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
            plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT5$data))
    {
      reactDT5$data[i,7] = updateH2(reactDT5$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT5$data[i,3] + input$varGxL/(reactDT5$data[i,3]*reactDT5$data[i,4]) + reactDT5$data[i,6]/(reactDT5$data[i,3]*reactDT5$data[i,4]*reactDT5$data[i,5])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT5$data, input$negen), totalLocs(reactDT5$data), totalPlots(reactDT5$data), totalLocsCost(reactDT5$data), totalPlotsCost(reactDT5$data), totalCost(reactDT5$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
  
} else if (tail(Scenarios,1) == 6)
{
  # output$cyPlot5 <- nplot
  reactDT6 <- reactiveValues(data = stages_current)
  output$stages_summary6 = DT::renderDT(reactDT6$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT6$data[i, j] = DT::coerceValue(v, reactDT6$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT6$data, resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn6, {
    
    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                         isolate(reactDT6$data[,2]),isolate(reactDT6$data[,3]),
                         isolate(reactDT6$data[,4]),isolate(reactDT6$data[,5]),
                         isolate(reactDT6$data[,6]),isolate(input$varieties))
    output$cyPlot6 <- renderPlot({
      
      plotScenario(result)
    })   # end of renderPlot
    
    
    
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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 6, scenarioDT = reactDT6$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 6, scenarioDT = reactDT6$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
      plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT6$data))
    {
      reactDT6$data[i,7] = updateH2(reactDT6$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT6$data[i,3] + input$varGxL/(reactDT6$data[i,3]*reactDT6$data[i,4]) + reactDT6$data[i,6]/(reactDT6$data[i,3]*reactDT6$data[i,4]*reactDT6$data[i,6])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT6$data, input$negen), totalLocs(reactDT6$data), totalPlots(reactDT6$data), totalLocsCost(reactDT6$data), totalPlotsCost(reactDT6$data), totalCost(reactDT6$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
} else if (tail(Scenarios,1) == 7)
{
  # output$cyPlot5 <- nplot
  reactDT7 <- reactiveValues(data = stages_current)
  output$stages_summary7 = DT::renderDT(reactDT7$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT7$data[i, j] = DT::coerceValue(v, reactDT7$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT7$data, resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn7, {
    
    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                         isolate(reactDT7$data[,2]),isolate(reactDT7$data[,3]),
                         isolate(reactDT7$data[,4]),isolate(reactDT7$data[,5]),
                         isolate(reactDT7$data[,6]),isolate(input$varieties))
    output$cyPlot7 <- renderPlot({
      
      plotScenario(result)
    })   # end of renderPlot
    
    
    
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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 7, scenarioDT = reactDT7$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 7, scenarioDT = reactDT7$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
      plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT7$data))
    {
      reactDT7$data[i,7] = updateH2(reactDT7$data[i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT7$data, input$negen), totalLocs(reactDT7$data), totalPlots(reactDT7$data), totalLocsCost(reactDT7$data), totalPlotsCost(reactDT7$data), totalCost(reactDT7$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
} else if (tail(Scenarios,1) == 8)
{
  # output$cyPlot5 <- nplot
  reactDT8 <- reactiveValues(data = stages_current)
  output$stages_summary8 = DT::renderDT(reactDT8$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT8$data[i, j] = DT::coerceValue(v, reactDT8$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT8$data, resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn8, {
    
    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                         isolate(reactDT8$data[,2]),isolate(reactDT8$data[,3]),
                         isolate(reactDT8$data[,4]),isolate(reactDT8$data[,5]),
                         isolate(reactDT8$data[,6]),isolate(input$varieties))
    output$cyPlot8 <- renderPlot({
      
      plotScenario(result)
    })   # end of renderPlot
    
    
    
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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 8, scenarioDT = reactDT8$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 8, scenarioDT = reactDT8$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
      plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT8$data))
    {
      reactDT8$data[i,7] = updateH2(reactDT8$data[i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT8$data, input$negen), totalLocs(reactDT8$data), totalPlots(reactDT8$data), totalLocsCost(reactDT8$data), totalPlotsCost(reactDT8$data), totalCost(reactDT8$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
} else if (tail(Scenarios,1) == 9)
{
  # output$cyPlot5 <- nplot
  reactDT9 <- reactiveValues(data = stages_current)
  output$stages_summary9 = DT::renderDT(reactDT9$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT9$data[i, j] = DT::coerceValue(v, reactDT9$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT9$data, resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn9, {
    
    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                         isolate(reactDT9$data[,2]),isolate(reactDT9$data[,3]),
                         isolate(reactDT9$data[,4]),isolate(reactDT9$data[,5]),
                         isolate(reactDT9$data[,6]),isolate(input$varieties))
    output$cyPlot9 <- renderPlot({
      
      plotScenario(result)
    })   # end of renderPlot
    
    
    
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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 9, scenarioDT = reactDT9$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 9, scenarioDT = reactDT9$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
      plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT9$data))
    {
      reactDT9$data[i,7] = updateH2(reactDT9$data[i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT9$data, input$negen), totalLocs(reactDT9$data), totalPlots(reactDT9$data), totalLocsCost(reactDT9$data), totalPlotsCost(reactDT9$data), totalCost(reactDT9$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
} else if (tail(Scenarios,1) == 10)
{
  # output$cyPlot5 <- nplot
  reactDT10 <- reactiveValues(data = stages_current)
  output$stages_summary10 = DT::renderDT(reactDT10$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    reactDT10$data[i, j] = DT::coerceValue(v, reactDT10$data[i, j])
    # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
    replaceData(proxy, reactDT10$data, resetPaging = FALSE)  # important
  })
  
  # Execute runScenario() for the current settings
  observeEvent(input$update_btn10, {
    
    result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                         isolate(reactDT10$data[,2]),isolate(reactDT10$data[,3]),
                         isolate(reactDT10$data[,4]),isolate(reactDT10$data[,5]),
                         isolate(reactDT10$data[,6]),isolate(input$varieties))
    output$cyPlot10 <- renderPlot({
      
      plotScenario(result)
    })   # end of renderPlot
    
    
    
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
    rv$results_allxTime = storeScenarioResultxTime(result = result, results_all = rv$results_allxTime, scenarioID = 10, scenarioDT = reactDT10$data)
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
    rv$results_allxCost = storeScenarioResultxCost(result = result, results_all = rv$results_allxCost, scenarioID = 10, scenarioDT = reactDT10$data)
    #
    # Render grouped boxplots for Gain per Cost
    output$overviewTabxCost <- renderPlot({
      plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
    })  
    
  }) # endof update btn
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT10$data))
    {
      reactDT10$data[i,7] = updateH2(reactDT10$data[i,]) 
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(totalYears(reactDT10$data, input$negen), totalLocs(reactDT10$data), totalPlots(reactDT10$data), totalLocsCost(reactDT10$data), totalPlotsCost(reactDT10$data), totalCost(reactDT10$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
                                                               server = F )
} else
  assign(paste('output$cyPlot', sep = "", tail(Scenarios,1)), nplot) # DOESNOT WORK