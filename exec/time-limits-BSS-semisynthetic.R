#' Simulate a semi-synthetic data setting from TCGA ovarian cancer samples 
#' and use Best subset selection (BSS) to find the best subset based on 
#' different time limits and subset sizes. TCGA Dataset is available in folder 
#' ./data
#' 
#' Gurobi solver version >= 8.1 needed!
#' Running simulation on a high performance cluster is strongly recommended!
#' 
#' @param N number of observations
#' @param subdata_size number of variables used from TCGA data
#' @param s number of non-zero coefficients
#' @param SNR signal-to-noise-ratio
#' @param Sim_n number of simulations
#' @param max.k maximal subset size
#' @param Time.limits vector of different time limits for each subset size in
#' seconds
#' @param mc number of workers for parallel computation
#' @param c_type type of cluster (e.g. "MPI", "PSOCK")

# load necessary packages 
library(parallel)
library(snow)
library(Rmpi) # if openMPI is used
library(glmnet)
library(bestsubset)
library(tibble)
library(tidyverse)

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


#' load TCGA data 
load("./data/tcgaExpression.RData")
#' transpose the dataset for rowise observations
tcgaExpression <- t(tcgaExpression)


clusterExport(cl, c("N",
                    "s",
                    "tcgaExpression",
                    "subdata_size",
                    "SNR", 
                    "Sim_n",  
                    "max.k",
                    "Time.limits"))

clusterEvalQ(cl, {
  library(glmnet)
  library(bestsubset)
  library(tibble)
  library(tidyverse)
})

# run simulation on cl different worker nodes
Loop_Sim_n <- parLapply(cl, 1:Sim_n, function(sim_n){
  
  Loop_time <- lapply(Time.limits, function(time_i){
    
    Loop_Snr <- lapply(SNR, function(snr){
      

      #' sample subdata of TCGA and set seed
      seed <- round(sim_n+snr*10000)
      try(set.seed(seed))
      TCGA_subdata <- 
        tcgaExpression[,sample(1:ncol(tcgaExpression), size = subdata_size)]
      
      #' scale variables
      TCGA_subdata <- scale(TCGA_subdata)
      
      #' generate correlation matrix of data; this will be needed to select the
      #' two most correlated variables. The eight highly correlated variabkles 
      #' with one of them are selected, too, to be true direct predictors 
      #' (non_zero_indices)
      Corr_tcga <- cor(TCGA_subdata)
      tmp_Corr_tcga <- Corr_tcga
      tmp_Corr_tcga[lower.tri(tmp_Corr_tcga)] <- 0
      diag(tmp_Corr_tcga) <- 0
      max_indices <-
        which(tmp_Corr_tcga == max(tmp_Corr_tcga), arr.ind = TRUE)
      non_zero_indices <-
        sort(
          c(max_indices[1],
            which(Corr_tcga[max_indices[1],] %in%
                    sort(Corr_tcga[max_indices[1],], decreasing = TRUE)[2:s]))
        )

    
      #' set zero and non-zero (=1) betas
      beta <- rep(0, ncol(TCGA_subdata))
      beta[non_zero_indices] <- 1
      
      #' set noise variance      
      sigma <- sqrt( (t(beta) %*% Corr_tcga %*% beta) / snr )

      #' number of obervations
      n <- N 
      
      #' seed for randomly drawn observations
      seed <- round(sim_n+snr*10000)
      try(set.seed(seed))
      n_sample <- sample(1:nrow(TCGA_subdata), n, replace = F)
      
      #' seed for noise
      seed <- round(sim_n+snr*10000000)
      try(set.seed(seed))
      e <- rnorm(n_sample) %*% sigma 
      
      #' linear model
      X <- TCGA_subdata[n_sample,]
      Y <- X %*% beta + e 
      Y <- scale(Y, scale = FALSE)
      
      #' run BSS
      BSS <- bs(x=X, y=Y, intercept=FALSE, time.limit=time_i, k=1:max.k, verbose=F)
      
      #' calculate true positves (TP), false positves (FP), false negative (FN) 
      #' and performance measures F1 etc for the BSS results for different 
      #' subset sizes k
      BSS_results <- 
        lapply(1:max.k, function(x){
          TP <- sum(which(abs(BSS$beta[,x]) > 0.00001) %in% non_zero_indices)
          FP <- x-TP
          FN <- s-TP
          F1 <- TP/(TP + 0.5*(FP+FN)) 
          Precision <- TP/(TP + FP)
          Accuracy <- TP/s
          
          #' create a tibble dataframe of performance
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
            p=subdata_size,
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
      
      #' return results
      BSS_results
      
      
      
    })
    #' bind for SNRs
    Loop_Snr <- do.call(rbind, Loop_Snr)
    Loop_Snr
    
    
  })
  #' bind for different times
  Loop_time <- do.call(rbind, Loop_time)
  Loop_time
  
})
#' bind different simulation runs
Loop_Sim_n <- do.call(rbind, Loop_Sim_n)

# save results
saveRDS(Loop_Sim_n, "./results/time_comparison_semisyn_100_594.RDS")

#' stop the cluster
stopCluster(cl)
if(c_type=="MPI"){
  mpi.quit()
}
