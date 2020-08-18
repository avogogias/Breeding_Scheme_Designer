# The following blocks of code are repeated for every run_btn index
# Ideally this control would take place recursively but building an
# output name dynamically using assign, doesn't seem to work.
if (input$run_btn == 1)
{
  output$cyPlot1 <- nplot
  reactDT1 <- reactiveValues(data = stages_table)
  output$stages_summary1 = DT::renderDT(reactDT1$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
  # Update editable DT through a proxy DT on cell edit event
  proxy = dataTableProxy('stages_summary1')
  #
  observeEvent(input$stages_summary1_cell_edit, {
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
    output$cyPlot1 <- renderPlot({
      result1 = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT1$data[,2]),isolate(reactDT1$data[,3]),
                            isolate(reactDT1$data[,4]),isolate(reactDT1$data[,5]),
                            isolate(reactDT1$data[,6]),isolate(input$varieties))
      boxplot(t(result1),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot
  }) # endof update btn  
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT1$data))
    {
      reactDT1$data[i,7] = round(input$varG/(input$varG + input$varGxY/reactDT1$data[i,3] + input$varGxL/(reactDT1$data[i,3]*reactDT1$data[i,4]) + reactDT1$data[i,6]/(reactDT1$data[i,3]*reactDT1$data[i,4]*reactDT1$data[i,5])), 3)
      # print(paste("H2 for stage", i, "is", yti$data[i,7]))
    }
  })
  
} else if (input$run_btn == 2)
{
  output$cyPlot2 <- nplot
  reactDT2 <- reactiveValues(data = stages_table)
  output$stages_summary2 = DT::renderDT(reactDT2$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    output$cyPlot2 <- renderPlot({
      result2 = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT2$data[,2]),isolate(reactDT2$data[,3]),
                            isolate(reactDT2$data[,4]),isolate(reactDT2$data[,5]),
                            isolate(reactDT2$data[,6]),isolate(input$varieties))
      boxplot(t(result2),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot
  }) # endof update btn  
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT2$data))
    {
      reactDT2$data[i,7] = round(input$varG/(input$varG + input$varGxY/reactDT2$data[i,3] + input$varGxL/(reactDT2$data[i,3]*reactDT2$data[i,4]) + reactDT2$data[i,6]/(reactDT2$data[i,3]*reactDT2$data[i,4]*reactDT2$data[i,5])), 3)
      # print(paste("H2 for stage", i, "is", yti$data[i,7]))
    }
  })
} else if (input$run_btn == 3)
{
  output$cyPlot3 <- nplot
  reactDT3 <- reactiveValues(data = stages_table)
  output$stages_summary3 = DT::renderDT(reactDT3$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    output$cyPlot3 <- renderPlot({
      result3 = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT3$data[,2]),isolate(reactDT3$data[,3]),
                            isolate(reactDT3$data[,4]),isolate(reactDT3$data[,5]),
                            isolate(reactDT3$data[,6]),isolate(input$varieties))
      boxplot(t(result3),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot
  }) # endof update btn  
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT3$data))
    {
      reactDT3$data[i,7] = round(input$varG/(input$varG + input$varGxY/reactDT3$data[i,3] + input$varGxL/(reactDT3$data[i,3]*reactDT3$data[i,4]) + reactDT3$data[i,6]/(reactDT3$data[i,3]*reactDT3$data[i,4]*reactDT3$data[i,5])), 3)
      # print(paste("H2 for stage", i, "is", yti$data[i,7]))
    }
  })
} else if (input$run_btn == 4)
{
  output$cyPlot4 <- nplot
  reactDT4 <- reactiveValues(data = stages_table)
  output$stages_summary4 = DT::renderDT(reactDT4$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    output$cyPlot4 <- renderPlot({
      result4 = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT4$data[,2]),isolate(reactDT4$data[,3]),
                            isolate(reactDT4$data[,4]),isolate(reactDT4$data[,5]),
                            isolate(reactDT4$data[,6]),isolate(input$varieties))
      boxplot(t(result4),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot
  }) # endof update btn  
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT4$data))
    {
      reactDT4$data[i,7] = round(input$varG/(input$varG + input$varGxY/reactDT4$data[i,3] + input$varGxL/(reactDT4$data[i,3]*reactDT4$data[i,4]) + reactDT4$data[i,6]/(reactDT4$data[i,3]*reactDT4$data[i,4]*reactDT4$data[i,5])), 3)
      # print(paste("H2 for stage", i, "is", yti$data[i,7]))
    }
  })
} else if (input$run_btn == 5)
{
  output$cyPlot5 <- nplot
  reactDT5 <- reactiveValues(data = stages_table)
  output$stages_summary5 = DT::renderDT(reactDT5$data, options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
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
    output$cyPlot5 <- renderPlot({
      result5 = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                            isolate(reactDT5$data[,2]),isolate(reactDT5$data[,3]),
                            isolate(reactDT5$data[,4]),isolate(reactDT5$data[,5]),
                            isolate(reactDT5$data[,6]),isolate(input$varieties))
      boxplot(t(result5),xlab="Stage",ylab="Mean Genetic Value")
    })   # end of renderPlot
  }) # endof update btn  
  
  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT5$data))
    {
      reactDT5$data[i,7] = round(input$varG/(input$varG + input$varGxY/reactDT5$data[i,3] + input$varGxL/(reactDT5$data[i,3]*reactDT5$data[i,4]) + reactDT5$data[i,6]/(reactDT5$data[i,3]*reactDT5$data[i,4]*reactDT5$data[i,5])), 3)
      # print(paste("H2 for stage", i, "is", yti$data[i,7]))
    }
  })
} else if (input$run_btn == 6)
{
  output$cyPlot6 <- nplot
  output$stages_summary6 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (input$run_btn == 7)
{
  output$cyPlot7 <- nplot
  output$stages_summary7 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (input$run_btn == 8)
{
  output$cyPlot8 <- nplot
  output$stages_summary8 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (input$run_btn == 9)
{
  output$cyPlot9 <- nplot
  output$stages_summary9 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else if (input$run_btn == 10)
{
  output$cyPlot10 <- nplot
  output$stages_summary10 = DT::renderDT(sumset_DT[[1]], options = sumset_DT$options, class = sumset_DT$class, colnames = sumset_DT$colnames, editable = sumset_DT$editable, server = sumset_DT$server)
} else
  assign(paste('output$cyPlot', sep = "", input$run_btn), nplot) # DOESNOT WORK