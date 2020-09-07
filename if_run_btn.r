# Currently results_all updates only for scenarios 1-5
# Common lines of code between Scenario IDs
cnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', 'h2')
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

      boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot

    # Update results_all entries
    # First remove previous run entries
    rv$results_all <- rv$results_all[,rv$results_all[3,]!=1] # WORKS!

    # Then add to matrix
    for(i in 1:nrow(result)) # 1:tail(Scenarios,1)
    {
      rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = result[i,], Scenario = 1))
    }

    # Render Group Boxplot with updated entries
    output$overviewTab <- renderPlot({
      ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        geom_boxplot()+
        xlab("Stage")+
        ylab("Gain")+
        scale_fill_discrete(name="Scenario")+
        ggtitle("Comparison between stages across all scenarios")
    })   # end of renderPlot for Overview tab
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT1$data))
    {
      reactDT1$data[i,7] = updateH2(reactDT1$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT1$data[i,3] + input$varGxL/(reactDT1$data[i,3]*reactDT1$data[i,4]) + reactDT1$data[i,6]/(reactDT1$data[i,3]*reactDT1$data[i,4]*reactDT1$data[i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(sum(reactDT1$data[,3])+input$negen,sum(reactDT1$data[,3]*reactDT1$data[,4]), totalPlots(reactDT1$data)), 
                                   options = list(
                                     searching = F, # no search box
                                     paginate = F,  # no num of pages
                                     lengthChange = F, # no show entries
                                     scrollX = T # horizontal slider
                                   ),
                                   rownames = F,
                                   colnames = c('Total Years', 'Total Locs', 'Total Plots'),
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

      boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot

    # Update results_all entries
    # First remove previous run entries
    rv$results_all <- rv$results_all[,rv$results_all[3,]!=2] # WORKS!
    print("UPDATED RESULTS")
    #print(head(t(rv$results_all)))
    #print(tail(t(rv$results_all)))
    # Then add to matrix
    for(i in 1:nrow(result)) # 1:tail(Scenarios,1)
    {
      rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = result[i,], Scenario = 2))
    }

    # Render Group Boxplot with updated entries
    output$overviewTab <- renderPlot({
      ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        geom_boxplot()+
        xlab("Stage")+
        ylab("Gain")+
        scale_fill_discrete(name="Scenario")+
        ggtitle("Comparison between stages across all scenarios")
    })   # end of renderPlot for Overview tab

  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT2$data))
    {
      reactDT2$data[i,7] = updateH2(reactDT2$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT2$data[i,3] + input$varGxL/(reactDT2$data[i,3]*reactDT2$data[i,4]) + reactDT2$data[i,6]/(reactDT2$data[i,3]*reactDT2$data[i,4]*reactDT2$data[i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(sum(reactDT2$data[,3])+input$negen,sum(reactDT2$data[,3]*reactDT2$data[,4]), totalPlots(reactDT2$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots'),
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

      boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot


    # Update results_all entries
    # First remove previous run entries
    rv$results_all <- rv$results_all[,rv$results_all[3,]!=3] # WORKS!

    # Then add to matrix
    for(i in 1:nrow(result)) # 1:tail(Scenarios,1)
    {
      rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = result[i,], Scenario = 3))
    }

    # Render Group Boxplot with updated entries
    output$overviewTab <- renderPlot({
      ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        geom_boxplot()+
        xlab("Stage")+
        ylab("Gain")+
        scale_fill_discrete(name="Scenario")+
        ggtitle("Comparison between stages across all scenarios")
    })   # end of renderPlot for Overview tab

  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT3$data))
    {
      reactDT3$data[i,7] = updateH2(reactDT3$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT3$data[i,3] + input$varGxL/(reactDT3$data[i,3]*reactDT3$data[i,4]) + reactDT3$data[i,6]/(reactDT3$data[i,3]*reactDT3$data[i,4]*reactDT3$data[i,5])), 3)
    }
  })
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(sum(reactDT3$data[,3])+input$negen,sum(reactDT3$data[,3]*reactDT3$data[,4]), totalPlots(reactDT3$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots'),
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

      boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot



    # Update results_all entries
    # First remove previous run entries
    rv$results_all <- rv$results_all[,rv$results_all[3,]!=4] # WORKS!

    # Then add to matrix
    for(i in 1:nrow(result)) # 1:tail(Scenarios,1)
    {
      rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = result[i,], Scenario = 4))
    }

    # Render Group Boxplot with updated entries
    output$overviewTab <- renderPlot({
      ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        geom_boxplot()+
        xlab("Stage")+
        ylab("Gain")+
        scale_fill_discrete(name="Scenario")+
        ggtitle("Comparison between stages across all scenarios")
    })   # end of renderPlot for Overview tab

  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT4$data))
    {
      reactDT4$data[i,7] = updateH2(reactDT4$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT4$data[i,3] + input$varGxL/(reactDT4$data[i,3]*reactDT4$data[i,4]) + reactDT4$data[i,6]/(reactDT4$data[i,3]*reactDT4$data[i,4]*reactDT4$data[i,5])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(sum(reactDT4$data[,3])+input$negen,sum(reactDT4$data[,3]*reactDT4$data[,4]), totalPlots(reactDT4$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots'),
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

      boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot



    # Update results_all entries
    # First remove previous run entries
    rv$results_all <- rv$results_all[,rv$results_all[3,]!=5] # WORKS!
    print("UPDATED RESULTS")

    # Then add to matrix
    for(i in 1:nrow(result)) # 1:tail(Scenarios,1)
    {
      rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = result[i,], Scenario = 5))
    }

    # Render Group Boxplot with updated entries
    output$overviewTab <- renderPlot({
      ggplot(as.data.frame(t(rv$results_all)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
        geom_boxplot()+
        xlab("Stage")+
        ylab("Gain")+
        scale_fill_discrete(name="Scenario")+
        ggtitle("Comparison between stages across all scenarios")
    })   # end of renderPlot for Overview tab


  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT5$data))
    {
      reactDT5$data[i,7] = updateH2(reactDT5$data[i,]) # round(input$varG/(input$varG + input$varGxY/reactDT5$data[i,3] + input$varGxL/(reactDT5$data[i,3]*reactDT5$data[i,4]) + reactDT5$data[i,6]/(reactDT5$data[i,3]*reactDT5$data[i,4]*reactDT5$data[i,5])), 3)
    }
  })
  
  
  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", tail(Scenarios,1))]] = DT::renderDT(cbind(sum(reactDT5$data[,3])+input$negen,sum(reactDT5$data[,3]*reactDT5$data[,4]), totalPlots(reactDT5$data)), 
                                                               options = list(
                                                                 searching = F, # no search box
                                                                 paginate = F,  # no num of pages
                                                                 lengthChange = F, # no show entries
                                                                 scrollX = T # horizontal slider
                                                               ),
                                                               rownames = F,
                                                               colnames = c('Total Years', 'Total Locs', 'Total Plots'),
                                                               server = F )      
  
} else if (tail(Scenarios,1) == 6)
{
  #output$cyPlot6 <- nplot
  output$stages_summary6 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (tail(Scenarios,1) == 7)
{
  #output$cyPlot7 <- nplot
  output$stages_summary7 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (tail(Scenarios,1) == 8)
{
  #output$cyPlot8 <- nplot
  output$stages_summary8 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (tail(Scenarios,1) == 9)
{
  # output$cyPlot9 <- nplot
  output$stages_summary9 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (tail(Scenarios,1) == 10)
{
  # output$cyPlot10 <- nplot
  output$stages_summary10 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else
  assign(paste('output$cyPlot', sep = "", tail(Scenarios,1)), nplot) # DOESNOT WORK