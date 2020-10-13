//TV Iterates over ranges of parameters calculating a grid of scenarios

// Version 2, reports mean of potential parents

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

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
  arma::uword nStages = entries.n_elem;
  arma::fmat output(nStages, nRepeats);
  float w; // Weight for full error variance
  
  // Cycle through repeats
  for(arma::uword r=0; r<nRepeats; ++r){
    // Sample genetic values
    arma::fvec g(entries(0),arma::fill::randn);
    g *= sqrt(varG);
    
    // Stage 1
    arma::fvec e(entries(0),arma::fill::randn);
    w = varGxL/(locs(0)*years(0)) + 
      varGxY/years(0) + error(0)/(reps(0)*locs(0)*years(0));
    e *= sqrt(w);
    arma::fvec p = g+e;
    
    // Stage 2+
    for(arma::uword s=1; s<nStages; ++s){
      // Select entries to advance
      arma::uvec rank = sort_index(p,"descend");
      rank = rank(arma::span(0,entries(s)-1));
      g = g(rank);
      output(s-1,r) = mean(g(arma::span(0,varieties-1)));
      
      // Phenotype
      e.set_size(entries(s));
      e.randn();
      w = varGxL/(locs(s)*years(s)) + 
        varGxY/years(s) + error(s)/(reps(s)*locs(s)*years(s));
      e *= sqrt(w);
      p = g + e;
    }
    // Varieties
    arma::uvec rank = sort_index(p,"descend");
    rank = rank(arma::span(0,varieties-1));
    output(nStages-1,r) = mean(g(rank));
  }
  return output;
}

/*** R
library(ggplot2)
system.time({
  entries_range = seq(100, 1000, by = (1000-100)/4)
  results_range = NULL
  for (i in entries_range) {
    print(i)
    result = runScenario(varG=1,
                         varGxL=1,
                         varGxY=1,
                         entries=c(i,i/2,i/100),
                         years=c(1,1,2), # Second stage now uses 2 years
                         locs=c(1,4,8),
                         reps=c(1,2,3),
                         error=c(1,1,1),
                         varieties=1)
    for(j in 1:nrow(result)) 
    {
      results_range = cbind(results_range, rbind(Stage = j, Value = result[j,], Scenario = i))
    }
  }
})
ggplot(as.data.frame(t(results_range)),aes(x=factor(Stage),y=Value,fill=factor(Scenario)))+
  geom_boxplot()+
  xlab("Stage")+
  ylab("Gain")+
  scale_fill_discrete(name="Entries")+
  ggtitle("Range of entries") + 
  theme(plot.title = element_text(size = 14, face = "bold"))
# boxplot(t(example1),
#         main="Example 1",
#         xlab="Stage",
#         ylab="Mean Genetic Value")

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

*/
