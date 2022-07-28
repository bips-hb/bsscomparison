#' Simulate a low dimensional block setting and use Best subset selection (BSS)
#' to find the best subset based on different time limits and subset sizes. 
#' 
#' Gurobi solver version >= 8.0 needed!
#' 
#' @param N number of observations
#' @param P number of variables
#' @param s number of non-zero coefficients
#' @param SNR signal-to-noise-ratio
#' @param RHO correlation strength between variables 
#' @param Sim_n number of simulations
#' @param max.k maximal subset size
#' @param Time.limits vector of different time limits for each subset size in
#' seconds
#' @param mc number of workers for parallel computation
#' @param c_type type of cluster (e.g. "MPI", "PSOCK")
#' 
# load necessary packages 
library(parallel)
library(snow)
library(Rmpi)
library(glmnet)
library(bestsubset)
library(tibble)
library(tidyverse)
library(mvtnorm)

# function to quit mpi if R crashes
.Last <- function(){
  if (is.loaded("mpi_initialize")){
    if (mpi.comm.size(1) > 0){
      print("Please use mpi.close.Rslaves() to close slaves.")
      mpi.close.Rslaves()
    }
    print("Please use mpi.quit() to quit R")
    .Call("mpi_finalize")
  }
} 



clusterExport(cl, c("N",
                    "P",
                    "s",
                    "SNR", 
                    "Sim_n",  
                    "max.k",
                    "Time.limits",
                    "block_builder"))

clusterEvalQ(cl, {
  library(glmnet)
  library(bestsubset)
  library(tibble)
  library(tidyverse)
  library(mvtnorm)
})


Loop_Sim_n <- parLapply(cl, 1:Sim_n, function(sim_n){
  
  Loop_time <- lapply(Time.limits, function(time_i){
    
    Loop_Snr <- lapply(SNR, function(snr){
      
      # create block correlation matrix
      Sigma <- block_builder(P,RHO, s)
      
      # set betas
      beta <- rep(0, P)
      # first s variables are non-zero coefficients
      beta[1:s] <- 1
      
      non_zero_indices <- which(beta != 0)
      
      # noise variance based on signal-to-noise ratio
      sigma <- sqrt( (t(beta) %*% Sigma %*% beta) / snr )
      
      
      #' number of observations
      n <- N 
      #' set seed for noise
      seed <- round(sim_n+snr*10000)
      try(set.seed(seed))
      e <- rnorm(n) %*% sigma 
      
      #' set.seed for multivariate normal distributed variables
      seed <- round(sim_n+snr*10000000)
      try(set.seed(seed))
      X <- mvtnorm::rmvnorm(n = n, mean = rep(0, ncol(Sigma)), sigma = Sigma)
      #' linear model
      X <- scale(X)
      Y <- X %*% beta + e 
      Y <- scale(Y, scale = FALSE)
      
      #' run BSS
      BSS <- bs(x=X, y=Y, intercept=FALSE, time.limit=time_i, k=1:max.k, verbose=F)
      
      #' calculate true positves (TP), false positves (FP), false negative (FN) 
      #' and performance measures F1 etc for the BSS results for different 
      #' subset sizes k
      BSS_results <- 
        lapply(1:max.k, function(x){
          TP <- sum(which(abs(BSS$beta[,x]) > 0.0000001) %in% non_zero_indices)
          FP <- x-TP
          FN <- s-TP
          F1 <- TP/(TP + 0.5*(FP+FN)) 
          Precision <- TP/(TP + FP)
          Accuracy <- TP/s
          
          #' combine in tibble dataframe
          tibble(
            method="BSS",
            alpha=NA,
            k=x,
            TP,
            FP,
            FN,
            F1,
            Precision,
            Accuracy,
            n,
            p=P,
            s,
            snr,
            status = BSS$status[x],
            time = time_i,
            sim.n = sim_n
          )
          
        })
      
      #' bind results for different k
      BSS_results <- 
        do.call(rbind, BSS_results)
      
      BSS_results
      
      
      
    })
    #' bind different SNR
    Loop_Snr <- do.call(rbind, Loop_Snr)
    Loop_Snr
    
    
  })
  #' bind different time limits
  Loop_time <- do.call(rbind, Loop_time)
  Loop_time
  
})
#' bind simulation runs
Loop_Sim_n <- do.call(rbind, Loop_Sim_n)

# save results
saveRDS(Loop_Sim_n, "./results/time_comparison_block_100_1000.RDS")

#' stop cluster
stopCluster(cl)
if(c_type=="MPI"){
  mpi.quit()
}