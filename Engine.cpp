/*
 * Version 3 of "determinisitic" GUI backend
 * Reports mean of potential parents ("varieties") by stage
 * Full version provides means for all replicates
 * Lite version summarizes replicates using mean and std. dev.
 */

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// Reports results for every replicate
// [[Rcpp::export]]
arma::fmat runScenario(float varG, // Genetic variance
                       float varGxL, // Genotype-by-location(in year) variance
                       float varGxY, // Genotype-by-year variance
                       arma::uvec entries, // Number of entries per stage
                       arma::fvec years, // Number of evaluation years per stage
                       arma::fvec locs, // Number of locations per stage
                       arma::fvec reps, // Number of replications per location
                       arma::fvec error, // Plot error variance
                       arma::uword varieties, // Final number selected (varieties, parents, ...)
                       arma::uword nRepeats=1000){ // Number of replicates of simulation
  arma::uword nStages = entries.n_elem; // Total number of stages
  arma::fmat output(nStages, nRepeats); // Output matrix for variety means
  float w; // Weight for full error variance
  
  // Cycle through repeats
  for(arma::uword r=0; r<nRepeats; ++r){
    
    // Sample genetic values
    arma::fvec g(entries(0),arma::fill::randn); // Sample from standard normal
    g *= sqrt(varG); // Scale genetic effects to desired variance
    
    // Stage 1
    arma::fvec e(entries(0),arma::fill::randn); // Non-genetic effects (GxL, GxY, & error)
    w = varGxL/(locs(0)*years(0)) + 
      varGxY/years(0) + error(0)/(reps(0)*locs(0)*years(0)); // Variance of non-genetic effects
    e *= sqrt(w); // Scale non-genetic effects to desired variance
    arma::fvec p = g+e; // Phenotype
    
    // Stage 2+
    for(arma::uword s=1; s<nStages; ++s){
      // Select entries from previous stage
      arma::uvec rank = sort_index(p,"descend"); // Sort by phenotype
      rank = rank(arma::span(0,entries(s)-1)); // Determine best genotypes by phenotype
      g = g(rank); // Make selections
      output(s-1,r) = mean(g(arma::span(0,varieties-1))); // Report mean of varieties (if selected now)
      
      // Assign new phenotypes
      e.set_size(entries(s)); // Reduce non-genetic effects vector to current size
      e.randn(); // Sample new non-genetic effects
      w = varGxL/(locs(s)*years(s)) + 
        varGxY/years(s) + error(s)/(reps(s)*locs(s)*years(s)); // Variance of non-genetic effects
      e *= sqrt(w); // Scale non-genetic effects to desired variance
      p = g + e; // Phenotype
    }
    
    // Varieties
    arma::uvec rank = sort_index(p,"descend"); // Sort by phenotype
    rank = rank(arma::span(0,varieties-1)); // Determine best genotypes by phenotype
    output(nStages-1,r) = mean(g(rank)); // Report mean of varieties
  }
  
  return output;
}

// Reports mean and standard deviation for each stage
// [[Rcpp::export]]
arma::fmat runScenarioLite(float varG, // Genetic variance
                           float varGxL, // Genotype-by-location(in year) variance
                           float varGxY, // Genotype-by-year variance
                           arma::uvec entries, // Number of entries per stage
                           arma::fvec years, // Number of evaluation years per stage
                           arma::fvec locs, // Number of locations per stage
                           arma::fvec reps, // Number of replications per location
                           arma::fvec error, // Plot error variance
                           arma::uword varieties, // Final number selected (varieties, parents, ...)
                           arma::uword nRepeats=1000){ // Number of replicates of simulation
  arma::fmat tmp = runScenario(varG,varGxL,varGxY,entries,
                               years,locs,reps,error,varieties,
                               nRepeats);
  return join_rows(mean(tmp,1), // Mean by stage
                   stddev(tmp,0,1)); // Standard deviation by stage
}


/*** R

# R code snippet showing use and testing performance
# This snippet should be removed from production code 

# library("ggplot2")
# 
# system.time({
#   example1 = runScenario(varG=1,
#                          varGxL=1,
#                          varGxY=1,
#                          entries=c(1000,100,10),
#                          years=c(1,1,1),
#                          locs=c(1,4,8),
#                          reps=c(1,2,3),
#                          error=c(1,1,1),
#                          varieties=1)
# })
# boxplot(t(example1),
#         main="Example 1",
#         xlab="Stage",
#         ylab="Mean Genetic Value")
# 
# system.time({
#   example2 = runScenario(varG=1,
#                          varGxL=1,
#                          varGxY=1,
#                          entries=c(1000,100,10),
#                          years=c(1,1,2), # Second stage now uses 2 years
#                          locs=c(1,4,8),
#                          reps=c(1,2,3),
#                          error=c(1,1,1),
#                          varieties=1)
# })
# boxplot(t(example2),
#         main="Example 2",
#         xlab="Stage",
#         ylab="Mean Genetic Value")
# 
# # Speed test for summarizing full output in R 
# system.time({
#   tmp = runScenario(varG=1,
#                     varGxL=1,
#                     varGxY=1,
#                     entries=c(1000,100,10),
#                     years=c(1,1,1),
#                     locs=c(1,4,8),
#                     reps=c(1,2,3),
#                     error=c(1,1,1),
#                     varieties=1)
#   liteExampleR = cbind(rowMeans(tmp),apply(tmp,1,sd))
#   rm(tmp)
# })
# 
# # Speed test for lite version, summarize in C++
# system.time({
#   liteExampleC = runScenarioLite(varG=1,
#                                  varGxL=1,
#                                  varGxY=1,
#                                  entries=c(1000,100,10),
#                                  years=c(1,1,1),
#                                  locs=c(1,4,8),
#                                  reps=c(1,2,3),
#                                  error=c(1,1,1),
#                                  varieties=1)
# })
# # Create boxplot without whiskers
# # Uses measured sd to set boxplot quartiles
# d = rbind(liteExampleC[,1]+2*qnorm(0.75,sd=liteExampleC[,2]),
#           liteExampleC[,1],
#           liteExampleC[,1]+2*qnorm(0.25,sd=liteExampleC[,2]))
# boxplot(d,
#         main="Example 3",
#         xlab="Stage",
#         ylab="Mean Genetic Value",
#         whisklty=0, staplelty=0)
# 
# 
# # RANGE
# system.time({
#   varG=1
#   varGxL=1
#   varGxY=1
#   entries=c(1000,100,10,2)
#   years=c(1,1,2,1) # Second stage now uses 2 years
#   locs=c(1,4,8,1)
#   reps=c(1,2,3,1)
#   error=c(1,1,1,1)
#   varieties=1
#   
#   entries_range = seq(100, 1000, by = (1000-100)/4)
#   results_range = NULL
#   it = 0
#   for (i in entries_range) {
#     print(i)
#     it = it + 1
#     entries[1]=i
#     result = runScenarioLite(varG,
#                          varGxL,
#                          varGxY,
#                          entries,
#                          years, 
#                          locs,
#                          reps,
#                          error,
#                          varieties)
#     for(j in 1:nrow(result)) 
#     {
#       results_range = cbind(results_range, rbind(Scenario = 1,
#                                                  Iteration = it,
#                                                  Stage = j, # Input Start
#                                                  Entries = entries[j], 
#                                                  Years = years[j], 
#                                                  Locs = locs[j],
#                                                  Reps = reps[j],
#                                                  Error = error[j],
#                                                  Varieties = varieties,
#                                                  Mean = result[j,1],  # Output Start
#                                                  SD = result[j,2]))
#     }
#   }
# })
# ggplot(as.data.frame(t(results_range)),aes(x=factor(Stage),y=Mean,fill=factor(Iteration)))+
#   geom_boxplot()+
#   xlab("Stage")+
#   ylab("Gain")+
#   scale_fill_discrete(name="1st Stage Entries")+
#   ggtitle("Range of entries") + 
#   theme(plot.title = element_text(size = 14, face = "bold"))
# 
# system.time({
# 
#   library(ggplot2)
#   library(dplyr)
#   library(plotly)
# 
#   runScenarioRange <- function(min_entries = input$entries_range[1], max_entries = input$entries_range[2],
#                                min_years = input$years_range[1], max_years = input$years_range[2],
#                                min_locs = input$locs_range[1], max_locs = input$locs_range[2],
#                                min_reps = input$reps_range[1], max_reps = input$reps_range[2],
#                                grain = input$grain,
#                                scenarioDT = yti$data,
#                                varG = input$varG,
#                                varGxL = input$varGxL,
#                                varGxY = input$varGxY,
#                                varieties = input$varieties)
#     # results_range = rv$results_range)
#   {
#     print(scenarioDT)
#     stage = scenarioDT[,1]
#     entries = scenarioDT[,2]
#     years = scenarioDT[,3]
#     locs = scenarioDT[,4]
#     reps = scenarioDT[,5]
#     error = scenarioDT[,6]
#     it = 0 # counter of iterations between range min max
#     range_entries = rangeGrain(min_entries, max_entries, grain)
#     range_years = rangeGrain(min_years, max_years, grain)
#     range_locs = rangeGrain(min_locs, max_locs, grain)
#     range_reps = rangeGrain(min_reps, max_reps, grain)
#     #print(range_reps)
# 
#     rr = NULL
#     for (i in range_entries)
#     {
#       for (j in range_reps)
#       {
#         for (k in range_years)
#         {
#           for (l in range_locs)
#           {
#             it = it + 1
#             entries[1] = i # replace first stage entries with range_entries
#             reps[1] = j  # replace first stage reps with range_reps
#             years[1] = k
#             locs[1] = l
#             resultLite = runScenarioLite(varG,
#                                          varGxL,
#                                          varGxY,
#                                          entries,
#                                          years,
#                                          locs,
#                                          reps,
#                                          error,
#                                          varieties)
#             #print(resultLite) # WORKS
#             resultLite = as.data.frame(resultLite)             # convert to a df
#             colnames(resultLite) <- c("mean","sd")
#             # Create df with I/O data and bind this to rr from previous iterations
#             rr<-rbind(rr, cbind(scenario = tail(Scenarios,1), fs_entries = i, fs_reps = j, fs_years = k, fs_locs = l, it, stage, entries, years, locs, reps, error, resultLite))
# 
#           }
#         }
#       }
#     }
#     return(rr)
#   }
#   # function takes 2 vectors and returns a matrix with a grid between paired min max elements
#   rangeGrain <- function(min = input$range[1], max = input$range[2], grain = input$grain) {
#     qrt = NULL
#     for (i in 1:length(min)) {
#       if (min[i] < max[i] && grain>1) #  && min[i]>entries[i+1]
#       {
#         qrt = c(qrt, round(seq(min[i], max[i], by = (max[i]-min[i])/(grain-1))))
#       }
#       else qrt = c(qrt, max(min[i], max[1]))
#     }
#     return(qrt)
#   }
# 
#   stage = c(1,2,3)
#   entries = c(1000,100,10)
#   years = c(1,1,2)
#   locs = c(1,4,8)
#   reps = c(1,2,3)
#   error = c(1,1,1)
#   h2 = c(0.5,0.5,0.5) # this is a calculated value initialised here
#   yt = cbind(stage,entries,years,locs,reps,error,h2)
# 
#   df = runScenarioRange(scenarioDT = yt, min_entries = 100, max_entries = 1000,  min_years = 1, max_years = 5, min_locs = 1, max_locs = 5, min_reps = 1, max_reps = 10, grain = 5, varG = 1, varGxY = 1, varGxL = 1, varieties = 1)
#   re = as.matrix(df)
# 
#   df <- transform(df, stage = as.character(stage))
#   df <- filter(df, as.numeric(unlist(df["fs_years"])) %in% df["fs_years"][1,]) # filter rows not on the first occurrence (min) of myFilter
#   df <- filter(df, as.numeric(unlist(df["fs_locs"])) %in% df["fs_locs"][1,]) # filter rows not on the first occurrence (min) of myFilter
#   df <- filter(df, as.numeric(unlist(df["scenario"])) %in% df["scenario"][length(df[,1]),]) # filter rows which do not belong to the last scenario
#   df <- filter(df, as.numeric(unlist(df["stage"])) %in% df["stage"][1,]) # filter rows which do not belong to the first stage
#   df <- df %>%
#     mutate(text = paste0("x: ", entries, "\n", "y: ", reps, "\n", "Value: ",round(mean,2), "\n", "What else?"))
#  })
# 
# # Bubble Plot
# # ggplot(df, aes(x=entries, y=reps, color = stage))+
# #   #geom_errorbar(aes(x=entries, ymin = reps+mean-sd, ymax = reps+mean+sd, size=0.5, width = 0.2, alpha = 0.7)) +
# #   geom_point(aes(size = mean, alpha=1))+
# #   geom_point(aes(size = mean+sd, stroke = 1, alpha = 1/20))+ # SD margins shown as homocentric bubbles with lower opacity
# #   scale_x_continuous("First Stage Entries")+
# #   scale_y_continuous("First Stage Reps")+
# #   ggtitle("Gain for both Entries and Reps Ranges")
# 
# # Heatmap
# fig <- ggplot(df, aes(x=entries, y=reps))+
#   geom_tile(aes(fill = mean)) +
#   scale_x_continuous("First Stage Entries")+
#   scale_y_continuous("First Stage Reps")+
#   ggtitle("Gain for both Entries and Reps Ranges")
# 
# ggplotly(fig, tooltip="text")

*/

