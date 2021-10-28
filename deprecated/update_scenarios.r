# Currently results_all updates only for scenarios 1-5
# Common lines of code between Scenario IDs
cnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error', 'h2')
output[[paste0("cyPlot", tail(Scenarios,1))]] <- nplot

# TV store scenario input in the reactive data structure scenariosInput
scenariosInput$stagesDT[[tail(Scenarios, 1)]] <- stages_current
scenariosInput$varG[[tail(Scenarios, 1)]] <- varG 
scenariosInput$varGxL[[tail(Scenarios, 1)]] <- varGxL 
scenariosInput$varGxY[[tail(Scenarios, 1)]] <- varGxY 
scenariosInput$varieties[[tail(Scenarios, 1)]] <- varieties

output[[paste0("stages_summary", tail(Scenarios,1))]] = DT::renderDT(scenariosInput$stagesDT[[tail(Scenarios, 1)]], options = sumset_DT$options, class = sumset_DT$class, rownames = sumset_DT$rownames, colnames = cnames, editable = sumset_DT$editable, server = sumset_DT$server)
# proxy = dataTableProxy(paste0('stages_summary', tail(Scenarios,1)))
assign(paste0("proxy", tail(Scenarios,1)), dataTableProxy(paste0('stages_summary', tail(Scenarios,1))))

observeEvent(input[[paste0("stages_summary", tail(Scenarios,1), "_cell_edit")]], { #$stages_summary1_cell_edit, {
  info = input[[paste0('stages_summary', tail(Scenarios,1), "_cell_edit")]]
  #info = input$stages_summary1_cell_edit
  i = info$row
  j = info$col + 1 # required when rownames = F in DT
  v = info$value
  str(info)
  # Character string needs to be coerced to same type as target value. Here as.integer()
  scenariosInput$stagesDT[[tail(Scenarios, 1)]][i, j] = DT::coerceValue(v, scenariosInput$stagesDT[[tail(Scenarios, 1)]][i, j])
  #reactDT1$data[i, j] = DT::coerceValue(v, reactDT1$data[i, j])
  # Produces invalid JSON response when renderDT (server = F), because replaceData() calls reloadData()
  replaceData(proxy, scenariosInput$stagesDT[[tail(Scenarios, 1)]], resetPaging = FALSE)  # important 
  # replaceData(paste0("proxy", tail(Scenarios,1)), scenariosInput$stagesDT[[tail(Scenarios, 1)]], resetPaging = FALSE)  # important 
  
})

# Execute runScenario() for the current settings
#observeEvent(input$update_btn1, {
observeEvent(input[[paste0('update_btn', tail(Scenarios,1))]], { #$update_btn1, {
  result = runScenario(isolate(input$varG),isolate(input$varGxL),isolate(input$varGxY),
                       isolate(scenariosInput$stagesDT[[tail(Scenarios, 1)]][,2]),isolate(scenariosInput$stagesDT[[tail(Scenarios, 1)]][,3]),
                       isolate(scenariosInput$stagesDT[[tail(Scenarios, 1)]][,4]),isolate(scenariosInput$stagesDT[[tail(Scenarios, 1)]][,5]),
                       isolate(scenariosInput$stagesDT[[tail(Scenarios, 1)]][,6]),isolate(input$varieties))
  output[[paste0("cyPlot", tail(Scenarios,1))]] <- renderPlot({
    
    boxplot(t(result),xlab="Stage",ylab="Mean Genetic Value")
  })   # end of renderPlot
  
  # Update results_all entries
  # First remove previous run entries
  rv$results_all <- rv$results_all[,rv$results_all[3,]!=tail(Scenarios,1)] # WORKS!
  # Then add to matrix
  for(i in 1:nrow(result)) # 1:tail(Scenarios,1)
  {
    rv$results_all = cbind(rv$results_all, rbind(Stage = i, Value = result[i,], Scenario = tail(Scenarios,1)))
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
  # cell_edit for each scenario first needs to work for this loop to work
  # for (j in 1:tail(Scenarios, 1))
  # {
  #   for (i in 1:nrow(scenariosInput$stagesDT[[j]]))
  #   {
  #     scenariosInput$stagesDT[[j]][i,7] = round(input$varG/(input$varG + input$varGxY/scenariosInput$stagesDT[[j]][i,3] + input$varGxL/(scenariosInput$stagesDT[[j]][i,3]*scenariosInput$stagesDT[[j]][i,4]) + scenariosInput$stagesDT[[j]][i,6]/(scenariosInput$stagesDT[[j]][i,3]*scenariosInput$stagesDT[[j]][i,4]*scenariosInput$stagesDT[[j]][i,5])), 3)
  #     # print(paste("H2 for stage", i, "is", yti$data[i,7]))
  #   }  
  # }
  
  
  for (i in 1:nrow(scenariosInput$stagesDT[[tail(Scenarios, 1)]]))
  {
    scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,7] = updateH2(scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,]) #round(input$varG/(input$varG + input$varGxY/scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,3] + input$varGxL/(scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,3]*scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,4]) + scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,6]/(scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,3]*scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,4]*scenariosInput$stagesDT[[tail(Scenarios, 1)]][i,5])), 3)
    # print(paste("H2 for stage", i, "is", yti$data[i,7]))
  }
})

