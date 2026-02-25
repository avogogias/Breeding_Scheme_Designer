# Test suite for Breeding Scheme Designer core functions
# Run with: testthat::test_file("tests/test_core_functions.r")
#
# These tests validate the core calculation functions, multi-scenario logic,
# chart generation, and data management independently of the Shiny server.

library(testthat)
library(ggplot2)
library(plotly)
library(dplyr)
library(data.table)
library(Rcpp)
library(RcppArmadillo)

# Compile C++ engine
sourceCpp("Engine.cpp")

# ============================================================
# Helper: recreate core functions from server.r for testing
# ============================================================

updateH2 <- function(stg, vG = 1, vGxY = 1, vGxL = 1) {
  h2 <- round(vG / (vG + vGxY / stg[3] + vGxL / (stg[3] * stg[4]) + stg[6] / (stg[3] * stg[4] * stg[5])), 3)
  return(h2)
}

totalYears <- function(scenarioDT, selfingYears = 4) {
  ty <- sum(scenarioDT[, 3]) + selfingYears
  return(ty)
}

totalLocs <- function(scenarioDT) {
  tl <- sum(scenarioDT[, 3] * scenarioDT[, 4])
  return(tl)
}

totalPlots <- function(scenarioDT) {
  tp <- 0
  for (i in 1:nrow(scenarioDT))
    tp <- tp + prod(scenarioDT[i, 2:5])
  return(tp)
}

stageLocs <- function(scenarioDT, stage = 1) {
  sl <- prod(scenarioDT[stage, 3:4])
  return(sl)
}

stagePlots <- function(scenarioDT, stage = 1) {
  sp <- prod(scenarioDT[stage, 2:5])
  return(sp)
}

stageLocsCost <- function(scenarioDT, stage = 1) {
  slc <- stageLocs(scenarioDT, stage) * scenarioDT[stage, 9]
  return(slc)
}

stagePlotsCost <- function(scenarioDT, stage = 1) {
  spc <- stagePlots(scenarioDT) * scenarioDT[stage, 8]
  return(spc)
}

stageCost <- function(scenarioDT, stage = 1) {
  stc <- stageLocsCost(scenarioDT, stage) + stagePlotsCost(scenarioDT, stage) + scenarioDT[stage, 10]
  return(stc)
}

totalLocsCost <- function(scenarioDT) {
  tlc <- 0
  for (i in 1:nrow(scenarioDT))
    tlc <- tlc + stageLocsCost(scenarioDT, i)
  return(tlc)
}

totalPlotsCost <- function(scenarioDT) {
  tpc <- 0
  for (i in 1:nrow(scenarioDT))
    tpc <- tpc + stagePlotsCost(scenarioDT, i)
  return(tpc)
}

totalCost <- function(scenarioDT) {
  tc <- 0
  for (i in 1:nrow(scenarioDT))
    tc <- tc + stageCost(scenarioDT, i)
  return(tc)
}

stageTotalYears <- function(scenarioDT, stage = 1, selfingYears = 4) {
  scy <- sum(scenarioDT[1:stage, 3]) + selfingYears
  return(scy)
}

gainTime <- function(scenarioDT, result, stage = 1, selfingYears = 4) {
  gt <- result[stage, ] / stageTotalYears(scenarioDT, stage, selfingYears)
  return(gt)
}

gainCost <- function(scenarioDT, result, stage = 1) {
  gc <- 0
  for (i in 1:stage) {
    gc <- gc + stagePlotsCost(scenarioDT, i) + stageLocsCost(scenarioDT, i) + scenarioDT[i, 10]
  }
  gc <- result[stage, ] / gc
  return(gc)
}

validInput <- function(scenarioDT) {
  entries <- scenarioDT[, 2]
  if (is.unsorted(rev(entries)))
    return(FALSE)
  return(TRUE)
}

validVarieties <- function(scenarioDT, varieties = 1) {
  entries <- scenarioDT[, 2]
  last_entries <- tail(entries, 1)
  if (varieties > last_entries)
    return(FALSE)
  return(TRUE)
}

meanGain <- function(result) {
  result <- round(apply(result, 1, mean), 3)
  return(result)
}

meanGainxTime <- function(result, scenarioDT, selfingYears = 4) {
  for (i in 1:nrow(result)) {
    result[i, ] <- gainTime(scenarioDT, result, i, selfingYears)
  }
  return(meanGain(result))
}

meanGainxCost <- function(result, scenarioDT) {
  for (i in 1:nrow(result)) {
    result[i, ] <- gainCost(scenarioDT, result, i) * 1000
  }
  return(meanGain(result))
}

storeScenarioResult <- function(result, results_all = NULL, scenarioID = 1) {
  for (i in 1:nrow(result)) {
    results_all <- cbind(results_all, rbind(Stage = i, Value = result[i, ], Scenario = scenarioID))
  }
  return(results_all)
}

storeScenarioResultxTime <- function(result, results_all = NULL, scenarioID = 1, scenarioDT, selfingYears = 4) {
  for (i in 1:nrow(result)) {
    results_all <- cbind(results_all, rbind(Stage = i, Value = gainTime(scenarioDT, result, i, selfingYears), Scenario = scenarioID))
  }
  return(results_all)
}

storeScenarioResultxCost <- function(result, results_all = NULL, scenarioID = 1, scenarioDT) {
  for (i in 1:nrow(result)) {
    results_all <- cbind(results_all, rbind(Stage = i, Value = gainCost(scenarioDT, result, i), Scenario = scenarioID))
  }
  return(results_all)
}

removeScenarioResult <- function(scenarioID, results_all) {
  if (is.null(results_all) || ncol(results_all) == 0) return(NULL)
  results_all <- results_all[, results_all[3, ] != scenarioID, drop = FALSE]
  if (ncol(results_all) == 0) return(NULL)
  return(results_all)
}

rangeGrain <- function(min_val, max_val, grain) {
  qrt <- NULL
  for (i in 1:length(min_val)) {
    if (min_val[i] < max_val[i] && grain > 1) {
      qrt <- c(qrt, round(seq(min_val[i], max_val[i], by = (max_val[i] - min_val[i]) / (grain - 1))))
    } else {
      qrt <- c(qrt, min(min_val[i], max_val[1]))
    }
  }
  return(qrt)
}

# Interactive plotly chart functions
plotScenario <- function(result) {
  df <- data.frame(
    Stage = factor(rep(1:nrow(result), each = ncol(result))),
    Value = as.vector(t(result))
  )
  p <- plot_ly(df, x = ~Stage, y = ~Value, type = "box",
               marker = list(color = 'rgba(31,119,180,0.7)'),
               line = list(color = 'rgba(31,119,180,1)')) %>%
    layout(
      xaxis = list(title = "Stage"),
      yaxis = list(title = "Mean Genetic Value"),
      showlegend = FALSE
    )
  return(p)
}

plotScenarioGroup <- function(results_all, ylabel = "Gain", gtitle = "Genetic Gain by Stage") {
  df <- as.data.frame(t(results_all))
  df$Stage <- factor(df$Stage)
  df$Scenario <- factor(df$Scenario)
  p <- plot_ly(df, x = ~Stage, y = ~Value, color = ~Scenario, type = "box") %>%
    layout(
      title = list(text = gtitle, font = list(size = 14)),
      xaxis = list(title = "Stage"),
      yaxis = list(title = ylabel),
      boxmode = "group",
      legend = list(title = list(text = "Scenario"))
    )
  return(p)
}

plotScenarioGroupStatic <- function(results_all, ylabel = "Gain", gtitle = "Genetic Gain by Stage") {
  ggplot(as.data.frame(t(results_all)), aes(x = factor(Stage), y = Value, fill = factor(Scenario))) +
    geom_boxplot() +
    xlab("Stage") +
    ylab(ylabel) +
    scale_fill_discrete(name = "Scenario") +
    ggtitle(gtitle) +
    theme(plot.title = element_text(size = 14, face = "bold"))
}

# ============================================================
# Default test data
# ============================================================

make_default_scenario <- function() {
  stage <- c(1, 2, 3)
  entries <- c(1000, 100, 10)
  years <- c(1, 1, 2)
  locs <- c(1, 4, 8)
  reps <- c(1, 2, 3)
  error <- c(1, 1, 1)
  h2 <- c(0.5, 0.5, 0.5)
  plotCost <- c(10, 10, 10)
  locCost <- c(1000, 1000, 1000)
  fixedCost <- c(1000, 1000, 1000)
  yt <- cbind(stage, entries, years, locs, reps, error, h2, plotCost, locCost, fixedCost)
  return(yt)
}


# ============================================================
# TEST: C++ Engine (runScenario)
# ============================================================

context("C++ Engine: runScenario")

test_that("runScenario returns correct dimensions", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1
  )
  expect_equal(nrow(result), 3)       # 3 stages
  expect_equal(ncol(result), 1000)    # default 1000 replicates
})

test_that("runScenario result values are numeric and finite", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1
  )
  expect_true(all(is.finite(result)))
})

test_that("runScenario mean values increase across stages", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1
  )
  means <- rowMeans(result)
  # Mean genetic gain should generally increase across stages
  expect_true(means[2] >= means[1])
  expect_true(means[3] >= means[2])
})

test_that("runScenario with different nRepeats changes output columns", {
  result50 <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 1),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 50
  )
  expect_equal(ncol(result50), 50)
})

test_that("runScenario with single stage works", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(100),
    years = c(1),
    locs = c(2),
    reps = c(1),
    error = c(1),
    varieties = 1,
    nRepeats = 100
  )
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 100)
})

test_that("runScenario with many stages works", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 500, 200, 100, 50, 10),
    years = c(1, 1, 1, 2, 2, 3),
    locs = c(1, 2, 3, 4, 6, 8),
    reps = c(1, 1, 2, 2, 3, 3),
    error = c(1, 1, 1, 1, 1, 1),
    varieties = 1,
    nRepeats = 100
  )
  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 100)
})

# ============================================================
# TEST: C++ Engine (runScenarioLite)
# ============================================================

context("C++ Engine: runScenarioLite")

test_that("runScenarioLite returns mean and sd columns", {
  result <- runScenarioLite(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1
  )
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)   # mean and sd
})

test_that("runScenarioLite sd values are non-negative", {
  result <- runScenarioLite(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1
  )
  expect_true(all(result[, 2] >= 0))
})


# ============================================================
# TEST: Heritability Calculation
# ============================================================

context("Heritability (H2) Calculation")

test_that("updateH2 returns value between 0 and 1", {
  yt <- make_default_scenario()
  for (i in 1:nrow(yt)) {
    h2 <- updateH2(yt[i, ])
    expect_true(h2 >= 0 && h2 <= 1)
  }
})

test_that("updateH2 increases with more reps/locs/years", {
  base <- c(1, 1000, 1, 1, 1, 1, 0, 10, 1000, 1000)
  h2_base <- updateH2(base)

  more_reps <- base
  more_reps[5] <- 5  # increase reps
  h2_reps <- updateH2(more_reps)
  expect_true(h2_reps >= h2_base)

  more_locs <- base
  more_locs[4] <- 5  # increase locs
  h2_locs <- updateH2(more_locs)
  expect_true(h2_locs >= h2_base)

  more_years <- base
  more_years[3] <- 5  # increase years
  h2_years <- updateH2(more_years)
  expect_true(h2_years >= h2_base)
})

test_that("updateH2 equals 0.5 for known case", {
  # When vG = 1, error = 1, years=1, locs=1, reps=1, vGxY=0, vGxL=0
  stg <- c(1, 1000, 1, 1, 1, 1, 0, 10, 1000, 1000)
  h2 <- updateH2(stg, vG = 1, vGxY = 0, vGxL = 0)
  expect_equal(h2, 0.5)
})


# ============================================================
# TEST: Cost Functions
# ============================================================

context("Cost Calculations")

test_that("totalYears sums stage years plus selfing years", {
  yt <- make_default_scenario()
  ty <- totalYears(yt, selfingYears = 4)
  expect_equal(ty, 1 + 1 + 2 + 4)  # sum of years + selfingYears
})

test_that("totalLocs correctly sums years*locs products", {
  yt <- make_default_scenario()
  tl <- totalLocs(yt)
  expect_equal(tl, 1 * 1 + 1 * 4 + 2 * 8)  # years*locs per stage
})

test_that("totalPlots correctly sums entry*year*loc*rep products", {
  yt <- make_default_scenario()
  tp <- totalPlots(yt)
  expected <- 1000 * 1 * 1 * 1 + 100 * 1 * 4 * 2 + 10 * 2 * 8 * 3
  expect_equal(tp, expected)
})

test_that("stageLocs returns correct value", {
  yt <- make_default_scenario()
  expect_equal(stageLocs(yt, 1), 1 * 1)    # years[1] * locs[1]
  expect_equal(stageLocs(yt, 2), 1 * 4)    # years[2] * locs[2]
  expect_equal(stageLocs(yt, 3), 2 * 8)    # years[3] * locs[3]
})

test_that("stagePlots returns correct value", {
  yt <- make_default_scenario()
  expect_equal(stagePlots(yt, 1), 1000 * 1 * 1 * 1)
  expect_equal(stagePlots(yt, 2), 1000 * 1 * 1 * 1)  # Note: stagePlots uses scenarioDT default for stage 1
})

test_that("totalCost is positive for valid scenarios", {
  yt <- make_default_scenario()
  tc <- totalCost(yt)
  expect_true(tc > 0)
})

test_that("stageTotalYears accumulates correctly", {
  yt <- make_default_scenario()
  expect_equal(stageTotalYears(yt, 1, 4), 1 + 4)
  expect_equal(stageTotalYears(yt, 2, 4), 1 + 1 + 4)
  expect_equal(stageTotalYears(yt, 3, 4), 1 + 1 + 2 + 4)
})


# ============================================================
# TEST: Validation Functions
# ============================================================

context("Input Validation")

test_that("validInput returns TRUE for decreasing entries", {
  yt <- make_default_scenario()  # entries = c(1000, 100, 10)
  expect_true(validInput(yt))
})

test_that("validInput returns FALSE for increasing entries", {
  yt <- make_default_scenario()
  yt[2, 2] <- 2000  # make second stage entries > first
  expect_false(validInput(yt))
})

test_that("validVarieties returns TRUE when varieties < last stage entries", {
  yt <- make_default_scenario()  # last stage entries = 10
  expect_true(validVarieties(yt, varieties = 1))
  expect_true(validVarieties(yt, varieties = 10))
})

test_that("validVarieties returns FALSE when varieties > last stage entries", {
  yt <- make_default_scenario()  # last stage entries = 10
  expect_false(validVarieties(yt, varieties = 11))
})


# ============================================================
# TEST: Gain Calculation Functions
# ============================================================

context("Genetic Gain Calculations")

test_that("meanGain returns correct length vector", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1
  )
  mg <- meanGain(result)
  expect_equal(length(mg), 3)
})

test_that("meanGain values are increasing", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1
  )
  mg <- meanGain(result)
  for (i in 2:length(mg)) {
    expect_true(mg[i] >= mg[i - 1])
  }
})

test_that("gainTime divides result by cumulative years", {
  yt <- make_default_scenario()
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )
  gt <- gainTime(yt, result, stage = 1, selfingYears = 4)
  expected <- result[1, ] / (1 + 4)  # stage 1 years + selfing
  expect_equal(gt, expected)
})


# ============================================================
# TEST: Multi-Scenario Storage
# ============================================================

context("Multi-Scenario Result Storage")

test_that("storeScenarioResult stores results correctly", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )
  ra <- storeScenarioResult(result, results_all = NULL, scenarioID = 1)

  # Should have 3 rows: Stage, Value, Scenario
  expect_equal(nrow(ra), 3)
  # Scenario IDs should all be 1
  expect_true(all(ra[3, ] == 1))
  # Stage numbers should be 1, 2, 3 (repeated for each replicate)
  expect_true(all(ra[1, ] %in% c(1, 2, 3)))
})

test_that("storeScenarioResult can accumulate multiple scenarios", {
  result1 <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )
  result2 <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(500, 50, 5),
    years = c(2, 2, 3),
    locs = c(2, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )

  ra <- storeScenarioResult(result1, results_all = NULL, scenarioID = 1)
  ra <- storeScenarioResult(result2, results_all = ra, scenarioID = 2)

  # Should have results from both scenarios
  expect_true(1 %in% ra[3, ])
  expect_true(2 %in% ra[3, ])
  # Total columns: 3 stages * 10 reps * 2 scenarios = 60
  expect_equal(ncol(ra), 60)
})

test_that("removeScenarioResult correctly removes a scenario", {
  result1 <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )
  result2 <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(500, 50, 5),
    years = c(1, 1, 1),
    locs = c(1, 2, 4),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )

  ra <- storeScenarioResult(result1, results_all = NULL, scenarioID = 1)
  ra <- storeScenarioResult(result2, results_all = ra, scenarioID = 2)

  # Remove scenario 1
  ra <- removeScenarioResult(1, ra)
  expect_false(1 %in% ra[3, ])
  expect_true(2 %in% ra[3, ])
  expect_equal(ncol(ra), 30)  # Only scenario 2 remains (3 stages * 10 reps)
})

test_that("removeScenarioResult handles empty result gracefully", {
  ra <- removeScenarioResult(1, NULL)
  expect_null(ra)
})

test_that("multiple scenarios can be stored and updated independently", {
  # Simulate running 5 scenarios
  results_all <- NULL
  for (sc_id in 1:5) {
    result <- runScenario(
      varG = 1, varGxL = 1, varGxY = 1,
      entries = c(1000, 100, 10),
      years = c(1, 1, 2),
      locs = c(1, 4, 8),
      reps = c(1, 2, 3),
      error = c(1, 1, 1),
      varieties = 1,
      nRepeats = 10
    )
    results_all <- storeScenarioResult(result, results_all = results_all, scenarioID = sc_id)
  }

  # All 5 scenarios should be present
  for (sc_id in 1:5) {
    expect_true(sc_id %in% results_all[3, ])
  }
  expect_equal(ncol(results_all), 150)  # 5 * 3 * 10

  # Update scenario 3 (remove then re-add)
  results_all <- removeScenarioResult(3, results_all)
  new_result <- runScenario(
    varG = 2, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )
  results_all <- storeScenarioResult(new_result, results_all = results_all, scenarioID = 3)

  # Scenario 3 should still be present (updated)
  expect_true(3 %in% results_all[3, ])
  expect_equal(ncol(results_all), 150)
})


# ============================================================
# TEST: Range Functions
# ============================================================

context("Range Grain Calculation")

test_that("rangeGrain generates correct number of samples", {
  rg <- rangeGrain(100, 1000, 5)
  expect_equal(length(rg), 5)
})

test_that("rangeGrain includes min and max", {
  rg <- rangeGrain(100, 1000, 5)
  expect_true(100 %in% rg)
  expect_true(1000 %in% rg)
})

test_that("rangeGrain with grain=1 returns min", {
  rg <- rangeGrain(100, 1000, 1)
  expect_equal(length(rg), 1)
  expect_equal(rg, 100)
})

test_that("rangeGrain with equal min max returns single value", {
  rg <- rangeGrain(500, 500, 5)
  expect_equal(length(rg), 1)
})


# ============================================================
# TEST: Interactive Chart Generation (plotly)
# ============================================================

context("Interactive Charts (Plotly)")

test_that("plotScenario returns a plotly object", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 50
  )
  p <- plotScenario(result)
  expect_true(inherits(p, "plotly"))
})

test_that("plotScenarioGroup returns a plotly object for single scenario", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 50
  )
  ra <- storeScenarioResult(result, results_all = NULL, scenarioID = 1)
  p <- plotScenarioGroup(ra)
  expect_true(inherits(p, "plotly"))
})

test_that("plotScenarioGroup handles multiple scenarios", {
  ra <- NULL
  for (sc_id in 1:3) {
    result <- runScenario(
      varG = 1, varGxL = 1, varGxY = 1,
      entries = c(1000, 100, 10),
      years = c(1, 1, 2),
      locs = c(1, 4, 8),
      reps = c(1, 2, 3),
      error = c(1, 1, 1),
      varieties = 1,
      nRepeats = 50
    )
    ra <- storeScenarioResult(result, results_all = ra, scenarioID = sc_id)
  }
  p <- plotScenarioGroup(ra)
  expect_true(inherits(p, "plotly"))
})

test_that("plotScenarioGroup works with custom labels", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 50
  )
  ra <- storeScenarioResult(result, results_all = NULL, scenarioID = 1)
  p <- plotScenarioGroup(ra, ylabel = "Gain per Year", gtitle = "Test Title")
  expect_true(inherits(p, "plotly"))
})

test_that("plotScenarioGroupStatic returns a ggplot object", {
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 50
  )
  ra <- storeScenarioResult(result, results_all = NULL, scenarioID = 1)
  p <- plotScenarioGroupStatic(ra)
  expect_true(inherits(p, "ggplot"))
})


# ============================================================
# TEST: Multi-Scenario with Time and Cost
# ============================================================

context("Multi-Scenario Time and Cost Scaling")

test_that("storeScenarioResultxTime correctly scales by time", {
  yt <- make_default_scenario()
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )
  ra <- storeScenarioResultxTime(result, results_all = NULL, scenarioID = 1, scenarioDT = yt, selfingYears = 4)
  expect_equal(nrow(ra), 3)
  expect_true(all(is.finite(ra[2, ])))  # Values should be finite
})

test_that("storeScenarioResultxCost correctly scales by cost", {
  yt <- make_default_scenario()
  result <- runScenario(
    varG = 1, varGxL = 1, varGxY = 1,
    entries = c(1000, 100, 10),
    years = c(1, 1, 2),
    locs = c(1, 4, 8),
    reps = c(1, 2, 3),
    error = c(1, 1, 1),
    varieties = 1,
    nRepeats = 10
  )
  ra <- storeScenarioResultxCost(result, results_all = NULL, scenarioID = 1, scenarioDT = yt)
  expect_equal(nrow(ra), 3)
  expect_true(all(is.finite(ra[2, ])))  # Values should be finite
})


# ============================================================
# TEST: Dynamic Scenario Registration (if_run_btn.r pattern)
# ============================================================

context("Dynamic Scenario Registration Pattern")

test_that("Scenario ID management works correctly", {
  Scenarios <- c()

  # Add first scenario
  if (length(Scenarios) == 0) {
    Scenarios <- c(1)
  } else {
    Scenarios <- c(Scenarios, tail(Scenarios, 1) + 1)
  }
  expect_equal(Scenarios, c(1))

  # Add second scenario
  Scenarios <- c(Scenarios, tail(Scenarios, 1) + 1)
  expect_equal(Scenarios, c(1, 2))

  # Add third scenario
  Scenarios <- c(Scenarios, tail(Scenarios, 1) + 1)
  expect_equal(Scenarios, c(1, 2, 3))

  expect_equal(tail(Scenarios, 1), 3)
})

test_that("reactDT.list can store multiple scenarios dynamically", {
  # Simulate reactDT.list as a regular list for testing
  reactDT_list <- list()
  yt <- make_default_scenario()

  for (sc_id in 1:20) {
    sc_key <- as.character(sc_id)
    # Each scenario can have different number of stages
    n_stages <- sample(2:6, 1)
    stages <- 1:n_stages
    entries <- sort(sample(10:1000, n_stages), decreasing = TRUE)
    years <- sample(1:3, n_stages, replace = TRUE)
    locs <- sample(1:8, n_stages, replace = TRUE)
    reps <- sample(1:3, n_stages, replace = TRUE)
    error <- rep(1, n_stages)
    h2 <- rep(0.5, n_stages)
    plotCost <- rep(10, n_stages)
    locCost <- rep(1000, n_stages)
    fixedCost <- rep(1000, n_stages)

    reactDT_list[[sc_key]] <- cbind(stages, entries, years, locs, reps, error, h2, plotCost, locCost, fixedCost)
  }

  # Verify all 20 scenarios stored
  expect_equal(length(reactDT_list), 20)

  # Verify each scenario is accessible
  for (sc_id in 1:20) {
    sc_key <- as.character(sc_id)
    expect_true(!is.null(reactDT_list[[sc_key]]))
    expect_true(nrow(reactDT_list[[sc_key]]) >= 2)
  }
})


# ============================================================
# TEST: End-to-end multi-scenario workflow
# ============================================================

context("End-to-end Multi-Scenario Workflow")

test_that("Complete workflow: create, run, compare, update, and delete scenarios", {
  Scenarios <- c()
  results_all <- NULL
  results_allxTime <- NULL
  results_allxCost <- NULL
  yt <- make_default_scenario()

  # Create 3 scenarios with different parameters
  configs <- list(
    list(entries = c(1000, 100, 10), years = c(1, 1, 2)),
    list(entries = c(500, 50, 5), years = c(2, 2, 3)),
    list(entries = c(2000, 200, 20), years = c(1, 1, 1))
  )

  for (sc_id in 1:3) {
    Scenarios <- c(Scenarios, sc_id)
    cfg <- configs[[sc_id]]
    result <- runScenario(
      varG = 1, varGxL = 1, varGxY = 1,
      entries = cfg$entries,
      years = cfg$years,
      locs = c(1, 4, 8),
      reps = c(1, 2, 3),
      error = c(1, 1, 1),
      varieties = 1,
      nRepeats = 50
    )

    results_all <- storeScenarioResult(result, results_all, sc_id)
    results_allxTime <- storeScenarioResultxTime(result, results_allxTime, sc_id, yt)
    results_allxCost <- storeScenarioResultxCost(result, results_allxCost, sc_id, yt)
  }

  # Verify all scenarios are present
  expect_equal(length(Scenarios), 3)
  expect_true(all(c(1, 2, 3) %in% results_all[3, ]))

  # Generate comparison charts
  p1 <- plotScenarioGroup(results_all)
  p2 <- plotScenarioGroup(results_allxTime, ylabel = "Gain per Year", gtitle = "Scaled by Time")
  p3 <- plotScenarioGroup(results_allxCost, ylabel = "Gain per Cost", gtitle = "Scaled by Cost")
  expect_true(inherits(p1, "plotly"))
  expect_true(inherits(p2, "plotly"))
  expect_true(inherits(p3, "plotly"))

  # Update scenario 2 with new parameters
  results_all <- removeScenarioResult(2, results_all)
  results_allxTime <- removeScenarioResult(2, results_allxTime)
  results_allxCost <- removeScenarioResult(2, results_allxCost)

  new_result <- runScenario(
    varG = 2, varGxL = 0.5, varGxY = 0.5,
    entries = c(500, 50, 5),
    years = c(3, 3, 4),
    locs = c(2, 4, 8),
    reps = c(2, 3, 4),
    error = c(0.5, 0.5, 0.5),
    varieties = 1,
    nRepeats = 50
  )
  results_all <- storeScenarioResult(new_result, results_all, 2)
  results_allxTime <- storeScenarioResultxTime(new_result, results_allxTime, 2, yt)
  results_allxCost <- storeScenarioResultxCost(new_result, results_allxCost, 2, yt)

  # Verify all scenarios still present after update
  expect_true(all(c(1, 2, 3) %in% results_all[3, ]))

  # Charts should still work after update
  p_updated <- plotScenarioGroup(results_all)
  expect_true(inherits(p_updated, "plotly"))
})
