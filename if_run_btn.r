# Global settings for all DTs in scenario tabs
sumset_DT = list( options = list(
  dom = 't', # only display the table, and nothing else
  scrollX = T, # horizontal slider
  ordering = F # suppressing sorting
),
class = "cell-border, compact, hover",
rownames = F,
colnames = c('Stage', 'Entries', 'Years', 'Locs', 'Reps', 'Plot Error Variance', 'h2', 'Plot Cost($)', 'Loc Cost($)', 'Fixed Cost($)', 'Genetic Gain', 'Gain per Year', 'Gain per $1000'),
filter = "none",
escape = FALSE,
autoHideNavigation = TRUE,
selection = "none",
editable = list(target = "cell", disable = list(columns = c(0, 6, 10, 11, 12))),
server = TRUE) # server = F doesn't work with replaceData() cell editing

# Dynamic scenario handler - replaces hardcoded if-else blocks for scenarios 1-10+
# Uses local() to capture the current scenario ID in a closure for each observer
local({
  sc_id <- tail(Scenarios, 1)
  sc_key <- as.character(sc_id)

  # Store current stages data in reactive list
  reactDT.list[[sc_key]] <- stages_current

  # Render editable summary DT for this scenario
  output[[paste0("stages_summary", sc_id)]] <- DT::renderDT(
    reactDT.list[[sc_key]],
    options = sumset_DT$options,
    class = sumset_DT$class,
    rownames = sumset_DT$rownames,
    colnames = sumset_DT$colnames,
    editable = sumset_DT$editable,
    server = sumset_DT$server
  )

  # Update editable DT through a proxy DT on cell edit event
  proxy_sc <- dataTableProxy(paste0('stages_summary', sc_id))

  observeEvent(input[[paste0('stages_summary', sc_id, '_cell_edit')]], {
    info <- input[[paste0('stages_summary', sc_id, '_cell_edit')]]
    i <- info$row
    j <- info$col + 1  # required when rownames = F in DT
    v <- info$value
    str(info)
    reactDT.list[[sc_key]][i, j] <- DT::coerceValue(v, reactDT.list[[sc_key]][i, j])
    replaceData(proxy_sc, reactDT.list[[sc_key]], resetPaging = FALSE)
  })

  # Execute runScenario() when Update button is pressed for this scenario
  observeEvent(input[[paste0('update_btn', sc_id)]], {
    try(
      if (!validInput(reactDT.list[[sc_key]])) {
        shinyalert("Oops!", "The number of entries should not increase in next stages.", type = "error")
        stop("Invalid input: entries should not increase in later stages.")
      }
      else if (!validVarieties(reactDT.list[[sc_key]])) {
        shinyalert("Oops!", "The number of selected parents should be less than the number of entries in the last stage.", type = "error")
        stop("Invalid input: varieties should be less than entries in last stage.")
      }
      else {
        result <- runScenario(
          isolate(input$varG), isolate(input$varGxL), isolate(input$varGxY),
          isolate(reactDT.list[[sc_key]][, 2]), isolate(reactDT.list[[sc_key]][, 3]),
          isolate(reactDT.list[[sc_key]][, 4]), isolate(reactDT.list[[sc_key]][, 5]),
          isolate(reactDT.list[[sc_key]][, 6]), isolate(input$varieties)
        )

        # Render interactive scenario boxplot
        output[[paste0("cyPlot", sc_id)]] <- renderPlotly({
          plotScenario(result)
        })

        # Update Mean Genetic Gain for each stage in summary table
        stages_current$mean <- meanGain(result)
        reactDT.list[[sc_key]][, 11] <- stages_current$mean

        # Update Mean Genetic Gain x Time for each stage
        stages_current$meanxTime <- meanGainxTime(result, scenarioDT = reactDT.list[[sc_key]])
        reactDT.list[[sc_key]][, 12] <- stages_current$meanxTime

        # Update Mean Genetic Gain x Cost for each stage
        stages_current$meanxCost <- meanGainxCost(result, scenarioDT = reactDT.list[[sc_key]])
        reactDT.list[[sc_key]][, 13] <- stages_current$meanxCost

        # Update results_all: remove previous entries then add new
        rv$results_all <- removeScenarioResult(sc_id)
        rv$results_all <- storeScenarioResult(result = result, results_all = rv$results_all, scenarioID = sc_id)

        # Render Overview grouped boxplot
        output$overviewTab <- renderPlotly({
          plotScenarioGroup(rv$results_all)
        })

        #------------ Overview Gain per Year ------------#
        rv$results_allxTime <- removeScenarioResult(sc_id, rv$results_allxTime)
        rv$results_allxTime <- storeScenarioResultxTime(
          result = result, results_all = rv$results_allxTime,
          scenarioID = sc_id, scenarioDT = reactDT.list[[sc_key]]
        )
        output$overviewTabxTime <- renderPlotly({
          plotScenarioGroup(rv$results_allxTime, ylabel = "Gain per Year", gtitle = "Genetic Gain by Stage (Scaled by Time)")
        })

        #------------ Overview Gain per Cost ------------#
        rv$results_allxCost <- removeScenarioResult(sc_id, rv$results_allxCost)
        rv$results_allxCost <- storeScenarioResultxCost(
          result = result, results_all = rv$results_allxCost,
          scenarioID = sc_id, scenarioDT = reactDT.list[[sc_key]]
        )
        output$overviewTabxCost <- renderPlotly({
          plotScenarioGroup(rv$results_allxCost, ylabel = "Gain per Cost", gtitle = "Genetic Gain by Stage (Scaled by Cost)")
        })
      }
    ) # endof try()
  }) # endof update btn

  # Update H2 for every stage as soon as input data that affect H2 change
  observe({
    for (i in 1:nrow(reactDT.list[[sc_key]])) {
      reactDT.list[[sc_key]][i, 7] <- updateH2(reactDT.list[[sc_key]][i, ])
    }
  })

  # Update cost table as soon as input data that affect cost change
  output[[paste0("costDT", sc_id)]] <- DT::renderDT(
    cbind(
      totalYears(reactDT.list[[sc_key]], input$negen),
      totalLocs(reactDT.list[[sc_key]]),
      totalPlots(reactDT.list[[sc_key]]),
      totalLocsCost(reactDT.list[[sc_key]]),
      totalPlotsCost(reactDT.list[[sc_key]]),
      totalCost(reactDT.list[[sc_key]])
    ),
    options = sumset_DT$options,
    rownames = F,
    colnames = c('Total Years', 'Total Locs', 'Total Plots', 'Total Locs Cost', 'Total Plots Cost', 'Total Cost'),
    server = F
  )
})
