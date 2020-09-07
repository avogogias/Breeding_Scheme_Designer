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
      rv[[stagesID]][i,7] = updateH2(rv[[stagesID]][i,]) # round(input$varG/(input$varG + input$varGxY/rv[[stagesID]][i,3] + input$varGxL/(rv[[stagesID]][i,3]*rv[[stagesID]][i,4]) + rv[[stagesID]][i,6]/(rv[[stagesID]][i,3]*rv[[stagesID]][i,4]*rv[[stagesID]][i,5])), 3)
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
