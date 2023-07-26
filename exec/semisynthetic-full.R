#' Simulate a semi-synthetic data setting from TCGA ovarian cancer samples 
#' and and apply BSS, FSS, Enet and Lasso to find the the true model
#' TCGA Dataset is available in folder ./data
#' 
#' Gurobi solver version >= 8.0 needed!
#' Running simulation on a high performance cluster is strongly recommended!

library(glmnet)
library(bestsubset)
library(tibble)
library(tidyverse)

#' load TCGA data 
load("./data/tcgaExpression.RData")
#' transpose the dataset for rowise observations
tcgaExpression <- t(tcgaExpression)

if (run_in_parallel) { 
  library(parallel)
  library(snow)
  library(Rmpi)
  
  # this function terminates the MPI connection if the (open)MPI cluster crashes
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
                    "s",
                    "tcgaExpression",
                    "subdata_size",
                    "SNR", 
                    "Sim_n", 
                    "Alpha",  
                    "max.k", 
                    "nLambda"))

clusterEvalQ(cl, {
  library(glmnet)
  library(bestsubset)
  library(tibble)
  library(tidyverse)
})


Loop_Sim_n <- parLapply(cl, 1:Sim_n, function(sim_n){

          Loop_Snr <- lapply(SNR, function(snr){
              
              #' seed for subdata
              seed <- round(sim_n+snr*10000)
              try(set.seed(seed))
              TCGA_subdata <- 
                tcgaExpression[,sample(1:ncol(tcgaExpression), size = subdata_size)]
              
              #' scale subdata
              TCGA_subdata <- scale(TCGA_subdata)
              #' generate correlation matrix of data; this will be needed to select the
              #' two most correlated variables. The 8 most highly correlated variables 
              #' with one of them are selected are considered to be true direct predictors 
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
              seed <- round(sim_n+snr*100000)
              try(set.seed(seed))
              n_sample <- sample(1:nrow(TCGA_subdata), n, replace = F)
              
              #' seed for noise
              seed <- round(sim_n+snr*100000000)
              try(set.seed(seed))
              e <- rnorm(n_sample) %*% sigma 
              
              #' linear model
              X <- TCGA_subdata[n_sample,]
              Y <- X %*% beta + e 
              Y <- scale(Y, scale = FALSE)
              
              # run BSS
              if (run_BSS) {
              BSS <- bs(x=X, y=Y, intercept=FALSE, time.limit=300, 
                        k=1:max.k, verbose=F)
              
              #' calculate true positves (TP), false positves (FP), false negative 
              #' (FN) and performance measures F1 etc for the BSS results for 
              #' different subset sizes k
              BSS_results <- 
                lapply(1:max.k, function(x){
                  TP <- sum(which(abs(BSS$beta[,x]) > 0.00001) %in% 
                              non_zero_indices)
                  FP <- x-TP
                  FN <- s-TP
                  F1 <- TP/(TP + 0.5*(FP+FN)) 
                  Precision <- TP/(TP + FP)
                  Accuracy <- TP/s
                  
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
                    sim.n = sim_n
                  )
                  
                })
               
              BSS_results <- 
                do.call(rbind, BSS_results)
              }
              
              #' run FSS
              FSS <- fs(x=X, y=Y, maxsteps= max.k, intercept=FALSE, verbose=F)
              
              #' calculate true positves (TP), false positves (FP), false negative 
              #' (FN) and performance measures F1 etc for the FSS results for 
              #' different subset sizes k
              FSS_results <- 
                lapply(1:max.k, function(x){
                  TP <- sum(which(abs(FSS$beta[,x+1]) > 0.00001) %in% non_zero_indices)
                  FP <- x-TP
                  FN <- s-TP
                  F1 <- TP/(TP + 0.5*(FP+FN)) 
                  Precision <- TP/(TP + FP)
                  Accuracy <- TP/s
                  
                  tibble(
                    method="FSS",
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
                    sim.n = sim_n
                  )
                  
                })
              
              FSS_results <- 
                do.call(rbind, FSS_results)
              
              
              #' run Enet
              
              #' for different alpha; alpha=1 is Lasso
              Enet_results <- lapply(Alpha, function(alpha){

                fit_enet <- glmnet(X, Y, alpha = alpha, nlambda = nLambda)
                
                #' since we are only interested in the subsets selected by Lasso 
                #' and Enet, we only need the zeros and non-zeros
                enet_betas <- as.numeric(apply(fit_enet$beta, 2, function(x){
                  sum(x != 0) 
                }))
                
                #' since some lambda values will select the same model we will 
                #' omit duplicates. Further, glmnet() starts with an empty model 
                #' an adds one by one variables via the "Lasso-Path", i.e. the 
                #' results are ordered  in an suberset-subset sense
                bigger_subset <- sapply(2:length(enet_betas), function(i){
                  enet_betas[i] > enet_betas[i-1]
                })
                
                unique_enet_subsets <- c(seq_along(enet_betas)[-1])[bigger_subset]
                
          
                #' calculate true positves (TP), false positves (FP), false negative 
                #' (FN) and performance measures F1 etc for Enet/Lasso results for 
                #' different subset sizes k
                enet_alpha <- lapply(unique_enet_subsets, function(x){
                  TP <- sum(which(fit_enet$beta[,x] != 0) %in% non_zero_indices)
                  FP <- sum(fit_enet$beta[,x] != 0)-TP
                  FN <- s-TP
                  F1 <- TP/(TP + 0.5*(FP+FN)) 
                  Precision <- TP/(TP + FP)
                  Accuracy <- TP/s
                  
                  tibble(
                    method = paste("Enet ", alpha, sep=""),
                    alpha=alpha,
                    k=sum(fit_enet$beta[,x] != 0),
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
                    status = NA,
                    sim.n = sim_n
                  )
                  
                  
                })
                
                enet_alpha <- do.call(rbind, enet_alpha)
              })


              Enet_results <- do.call(rbind, Enet_results)
              
              #' return the binded results of all methods
              rbind(BSS_results, FSS_results, Enet_results)
             
            
              
              
          })
          Loop_Snr <- do.call(rbind, Loop_Snr)
          Loop_Snr
 
  
})
#' bind all results to one dataframe
Loop_Sim_n <- do.call(rbind, Loop_Sim_n)

#' save results
saveRDS(Loop_Sim_n,
        paste("./results/SemiSyntheticSimulation_",
              subdata_size,
              "_",
              N,
              ".RDS",
              sep=""))

#' stop cluster
stopCluster(cl)
mpi.quit()


} else { 
  
  # START NO PARALLEL RUN
  # not recommended for Sim_n > 1
  
  Loop_Sim_n <- lapply(1:Sim_n, function(sim_n){
    
    Loop_Snr <- lapply(SNR, function(snr){
      
      #' seed for subdata
      seed <- round(sim_n+snr*10000)
      try(set.seed(seed))
      TCGA_subdata <- 
        tcgaExpression[,sample(1:ncol(tcgaExpression), size = subdata_size)]
      
      #' scale subdata
      TCGA_subdata <- scale(TCGA_subdata)
      #' generate correlation matrix of data; this will be needed to select the
      #' two most correlated variables. The 8 most highly correlated variables 
      #' with one of them are selected are considered to be true direct predictors 
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
      seed <- round(sim_n+snr*100000)
      try(set.seed(seed))
      n_sample <- sample(1:nrow(TCGA_subdata), n, replace = F)
      
      #' seed for noise
      seed <- round(sim_n+snr*1000000)
      try(set.seed(seed))
      e <- rnorm(n_sample) %*% sigma 
      
      #' linear model
      X <- TCGA_subdata[n_sample,]
      Y <- X %*% beta + e 
      Y <- scale(Y, scale = FALSE)
      
      # run BSS
      if (run_BSS) { 
      BSS <- bs(x=X, y=Y, intercept=FALSE, time.limit=300, 
                k=1:max.k, verbose=F)
      
      #' calculate true positves (TP), false positves (FP), false negative 
      #' (FN) and performance measures F1 etc for the BSS results for 
      #' different subset sizes k
      BSS_results <- 
        lapply(1:max.k, function(x){
          TP <- sum(which(abs(BSS$beta[,x]) > 0.00001) %in% 
                      non_zero_indices)
          FP <- x-TP
          FN <- s-TP
          F1 <- TP/(TP + 0.5*(FP+FN)) 
          Precision <- TP/(TP + FP)
          Accuracy <- TP/s
          
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
            sim.n = sim_n
          )
          
        })
      
      BSS_results <- 
        do.call(rbind, BSS_results)
      }
      
      #' run FSS
      FSS <- fs(x=X, y=Y, maxsteps= max.k, intercept=FALSE, verbose=F)
      
      #' calculate true positves (TP), false positves (FP), false negative 
      #' (FN) and performance measures F1 etc for the FSS results for 
      #' different subset sizes k
      FSS_results <- 
        lapply(1:max.k, function(x){
          TP <- sum(which(abs(FSS$beta[,x+1]) > 0.00001) %in% non_zero_indices)
          FP <- x-TP
          FN <- s-TP
          F1 <- TP/(TP + 0.5*(FP+FN)) 
          Precision <- TP/(TP + FP)
          Accuracy <- TP/s
          
          tibble(
            method="FSS",
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
            sim.n = sim_n
          )
          
        })
      
      FSS_results <- 
        do.call(rbind, FSS_results)
      
      
      #' run Enet
      
      #' for different alpha; alpha=1 is Lasso
      Enet_results <- lapply(Alpha, function(alpha){
        
        fit_enet <- glmnet(X, Y, alpha = alpha, nlambda = nLambda)
        
        #' since we are only interested in the subsets selected by Lasso 
        #' and Enet, we only need the zeros and non-zeros
        enet_betas <- as.numeric(apply(fit_enet$beta, 2, function(x){
          sum(x != 0) 
        }))
        
        #' since some lambda values will select the same model we will 
        #' omit duplicates. Further, glmnet() starts with an empty model 
        #' an adds one by one variables via the "Lasso-Path", i.e. the 
        #' results are ordered  in an suberset-subset sense
        bigger_subset <- sapply(2:length(enet_betas), function(i){
          enet_betas[i] > enet_betas[i-1]
        })
        
        unique_enet_subsets <- c(seq_along(enet_betas)[-1])[bigger_subset]
        
        
        #' calculate true positves (TP), false positves (FP), false negative 
        #' (FN) and performance measures F1 etc for Enet/Lasso results for 
        #' different subset sizes k
        enet_alpha <- lapply(unique_enet_subsets, function(x){
          TP <- sum(which(fit_enet$beta[,x] != 0) %in% non_zero_indices)
          FP <- sum(fit_enet$beta[,x] != 0)-TP
          FN <- s-TP
          F1 <- TP/(TP + 0.5*(FP+FN)) 
          Precision <- TP/(TP + FP)
          Accuracy <- TP/s
          
          tibble(
            method = paste("Enet ", alpha, sep=""),
            alpha=alpha,
            k=sum(fit_enet$beta[,x] != 0),
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
            status = NA,
            sim.n = sim_n
          )
          
          
        })
        
        enet_alpha <- do.call(rbind, enet_alpha)
      })
      
      
      Enet_results <- do.call(rbind, Enet_results)
      
      #' return the binded results of all methods
      rbind(BSS_results, FSS_results, Enet_results)
      
      
      
      
    })
    Loop_Snr <- do.call(rbind, Loop_Snr)
    Loop_Snr
    
    
  })
  #' bind all results to one dataframe
  Loop_Sim_n <- do.call(rbind, Loop_Sim_n)
  
  #' save results
  saveRDS(Loop_Sim_n,
          paste("./results/SemiSyntheticSimulation_",
                subdata_size,
                "_",
                N,
                ".RDS",
                sep=""))
}
