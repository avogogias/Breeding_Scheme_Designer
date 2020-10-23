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
*/

