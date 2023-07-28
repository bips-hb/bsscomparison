# This script runs the simulation for the results of the stability selection 
# by Meinshausen & Bühlmann (2010).
# For Best Subset Selection the Gurobi solver is necessary.
# NOTE: Stability Selection is computational challenging especially when
# when performing subsampling for BSS.
# It is strongly recommended to run this simulation on a multicore computer or
# high performance cluster

# Load necessary packages for parallel computing
library( parallel)
library( snow)
if(clusterType == "MPI"){
  library( Rmpi)
}


# Load necessary packages to run the simulation
library(glmnet)
library(bestsubset)
library(tibble)
library(tidyverse)
library(mvtnorm)
library(lars)

# If the (open)MPI environment for parallel computing is used this function is
# necessary in case the run crashes
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

# function to generate a block correlation structure
block_builder <- 
  function(p, rho, size){
    #' function to generate a block correlation structure
    #' 
    #' Input
    #' p: number of variables
    #' rho: correlation strength within blocks
    #' size: block size
    #' 
    #' Output:  
    #' a block structured correlation matrix 
    #' 
    n.blocks <- p/size 
    out <- matrix(rep(0, p^2), ncol = p) 
    for(i in c(0:(n.blocks-1))){
      out[c(i*size+1):c(i*size+size), c(i*size+1):c(i*size+size)] <- rho
    }
    diag(out) <- 1
    return(out)
  }


# number of subsamples for the stability approach
B <- 100

# stability threshold (called pi_thr in the original paper by Meinshausen &
# Bühlmann; the authors suggest values between 0.6 and 0.9)
cutoff <- 0.6

# the per-family error rate
pfer <- 5

if(run_in_parallel){
  ## run in parallel
  cl <- makeCluster(mc, type=clusterType)
  
  clusterExport(cl, c("block_builder",
                      "runBSS",
                      "gurobiTime",
                      "N",
                      "P",
                      "s",
                      "CORR_TYPE",
                      "RHO",
                      "SNR",
                      "BETA_POSITION", 
                      "max.k",
                      "Alpha",
                      "B",
                      "Sim_n", 
                      "cutoff",
                      "pfer"))
  
  clusterEvalQ(cl, {
    library(glmnet)
    library(bestsubset)
    library(tibble)
    library(tidyverse)
    library(mvtnorm)
    library(bestridge)
    library(lars)
  })
  
  Loop_Beta_pos <- lapply(BETA_POSITION, function(beta_position){
    Loop_Corr_type <- lapply(CORR_TYPE, function(corr_type){
      Loop_Rho <- lapply(RHO, function(Rho){
        Loop_Snr <- lapply(SNR, function(snr){
          Loop_Sim_n <- parLapply(cl, 1:Sim_n, function(sim_n){
            
            
            # define corr structure
            if(corr_type == "toeplitz"){
              Sigma <- Rho^toeplitz(0:(P-1))
            }else if(corr_type == "block"){
              Sigma <- block_builder(P, Rho, s)
            }
            
            # define non-zero positions
            beta <- rep(0, P)
            if(beta_position == "spread"){
              beta[seq(1,P, P/s)] <- 1
            }else if(beta_position == "adjacent"){
              beta[1:s] <- 1
            }
            
            n <- N
            
            # get indices of non-zeros
            non_zero_indices <- which(beta != 0)
            
            # variance of error term
            sigma <- sqrt( (t(beta) %*% Sigma %*% beta) / snr )
            
            # error
            seed <- round(sim_n+snr*1000000)
            try(set.seed(seed))
            e <- rnorm(n) %*% sigma 
            
            # simulate covariates
            seed <- round(sim_n+Rho*1000)
            X <- mvtnorm::rmvnorm(n = n, mean = rep(0, ncol(Sigma)), sigma = Sigma)
            Y <- X %*% beta + e 
            Y <- scale(Y, scale = FALSE)
            
            # define max subset size of best subset selection based on
            # per-family error rate and stability thresohold (cutoff)
            # see paper of Meinshausen & Bühlmann (2010)
            q <- ceiling(sqrt(pfer*(2*cutoff-1)*P)) 
            
            
            
            # Run Stabiliyt selection on BSS?
            # loop over subsampling process
            if(runBSS == TRUE){
              Loop_BSS_B <- lapply(1:B, function(b){
                
                # number of samples in the subsamples based onMeinshausen & 
                # Bühlmann (2010)
                set.seed(b)
                n_sample <- sample(1:n, n/2)
                
                
                BSS_b <- 
                  bs(x=X[n_sample,], y = Y[n_sample], 
                     intercept = FALSE, 
                     time.limit = gurobiTime, 
                     k=1:min(max.k, q), 
                     verbose=F)
                
                BSS_b$beta != 0
                
              })
              
              prob_selction_BSS <- Reduce("+", Loop_BSS_B)/B
              
              # get "stable" coefficients
              estimated_non_zeros_BSS <- 
                which(apply(prob_selction_BSS, 1, function(i){any(i >= cutoff)}))
              
              TP <- sum(estimated_non_zeros_BSS %in% non_zero_indices)
              FP <- length(estimated_non_zeros_BSS)-TP
              FN <- s-TP
              F1 <- TP/(TP + 0.5*(FP+FN))
              Precision <- TP/(TP + FP)
              Accuracy <- TP/s
              
              
              
              BSS_results <- tibble(
                method="BSS_gurobi",
                alpha=NA,
                lambda = NA,
                RSS = NA,
                RSS_selected_betas = NA,
                k=length(estimated_non_zeros_BSS),
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
                rho=Rho,
                sim.n = sim_n,
                beta_position = beta_position,
                corr_type = corr_type,
                criterion = "Stability Selection",
                cutoff = cutoff,
                PFER = pfer
              )
              rm(estimated_non_zeros_BSS)
            }else{
              BSS_results <- NULL
            }
            
            
            # end BSS
            
            
            
            # FSS
            # loop over subsampling process
            Loop_FSS_B <- lapply(1:B, function(b){
              
              # number of samples in the subsamples based onMeinshausen & 
              # Bühlmann (2010)
              set.seed(b)
              n_sample <- sample(1:n, n/2)
              
              FSS_b <- 
                lars::lars(x=X[n_sample,], y=Y[n_sample], 
                           max.steps = min(max.k, q), type = "stepwise",
                           use.Gram=FALSE)
              
              maxk_selected <- unlist(FSS_b$actions)
              
              selection_matrix <- matrix(rep(0, P*max.k), ncol=max.k)
              
              for(i in seq_along(maxk_selected)){
                selection_matrix[maxk_selected[1:i],i] <- 1
              }
              
              selection_matrix
              
            })
            
            prob_selction_FSS <- Reduce("+", Loop_FSS_B)/B
            
            # get "stable" coefficients
            estimated_non_zeros_FSS <- 
              which(apply(prob_selction_FSS, 1, function(i){any(i >= cutoff)}))
            
            TP <- sum(estimated_non_zeros_FSS %in% non_zero_indices)
            FP <- length(estimated_non_zeros_FSS)-TP
            FN <- s-TP
            F1 <- TP/(TP + 0.5*(FP+FN))
            Precision <- TP/(TP + FP)
            Accuracy <- TP/s
            
            
            
            FSS_results <- tibble(
              method="FSS",
              alpha=NA,
              lambda = NA,
              RSS = NA,
              RSS_selected_betas = NA,
              k=length(estimated_non_zeros_FSS),
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
              rho=Rho,
              sim.n = sim_n,
              beta_position = beta_position,
              corr_type = corr_type,
              criterion = "Stability Selection",
              cutoff = cutoff,
              PFER = pfer
            )
            rm(estimated_non_zeros_FSS)
            
            
            
            ### begin Lasso/Enet
            # loop over alphas
            Enet_results <- lapply(Alpha, function(i){
              
              Enet <- glmnet(x=X, y=Y, alpha = i, 
                             nlambda = 1000, 
                             intercept = F)
              
              
              
              n_non_zeros <- apply(Enet$beta, 2, function(x){
                sum(x != 0)
              })
              
              
              lambda_indices <- which(n_non_zeros <= q)
              
              Enet_lambdas <- Enet$lambda[lambda_indices]
              
              # loop over subsampling process
              Loop_Enet_B <- lapply(1:B, function(b){
                
                # number of samples in the subsamples based onMeinshausen & 
                # Bühlmann (2010)
                set.seed(b)
                n_sample <- sample(1:n, n/2)
                
                Enet_b <- 
                  glmnet(x=X[n_sample,], y=Y[n_sample], alpha = i,
                         lambda = Enet_lambdas, 
                         intercept = F)
                
                Enet_b$beta != 0
                
                
              })
              
              prob_selction_Enet <- Reduce("+", Loop_Enet_B)/B
              
              # get "stable" coefficients
              estimated_non_zeros_Enet <- 
                which(apply(prob_selction_Enet, 1, function(i){any(i >= cutoff)}))
              
              TP <- sum(estimated_non_zeros_Enet %in% non_zero_indices)
              FP <- length(estimated_non_zeros_Enet)-TP
              FN <- s-TP
              F1 <- TP/(TP + 0.5*(FP+FN))
              Precision <- TP/(TP + FP)
              Accuracy <- TP/s
              
              
              
              tibble(
                method=paste("Enet", i, sep=" "),
                alpha=i,
                lambda = NA,
                RSS = NA,
                RSS_selected_betas = NA,
                k=TP+FP,
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
                rho=Rho,
                sim.n = sim_n,
                beta_position = beta_position,
                corr_type = corr_type,
                criterion = "Stability Selection",
                cutoff = cutoff,
                PFER = pfer
              )
              
            })
            
            Enet_results <- do.call(rbind, Enet_results)
            
            
            out <- rbind(
              BSS_results,
              FSS_results,
              Enet_results)
            
            
            out
          })
          Loop_Sim_n <- do.call(rbind, Loop_Sim_n)
          Loop_Sim_n 
          
        })
        Loop_Snr <- do.call(rbind, Loop_Snr)
        Loop_Snr
        
        
      })
      Loop_Rho <- do.call(rbind, Loop_Rho)
      
      
      if(N==1000){
        Dim <- "low"
      }else if(N==100){
        Dim <- "high"
      }
      
      Loop_Rho$method[Loop_Rho$method == "Enet 1"] <- 
        "Lasso"
      
      if(runBSS == TRUE){
        Loop_Rho$method <- factor(Loop_Rho$method,
                                  levels = 
                                    c("Enet 0.1",
                                      "Enet 0.2",
                                      "Enet 0.3",
                                      "Enet 0.4",
                                      "Enet 0.5", 
                                      "Enet 0.6",
                                      "Enet 0.7",
                                      "Enet 0.8",
                                      "Enet 0.9", 
                                      "Lasso",
                                      "FSS",
                                      "BSS"))
      }else{
        Loop_Rho$method <- factor(Loop_Rho$method,
                                  levels = 
                                    c("Enet 0.1",
                                      "Enet 0.2",
                                      "Enet 0.3",
                                      "Enet 0.4",
                                      "Enet 0.5", 
                                      "Enet 0.6",
                                      "Enet 0.7",
                                      "Enet 0.8",
                                      "Enet 0.9", 
                                      "Lasso",
                                      "FSS"
                                    ))
      }
      
      
      # save results
      saveRDS(Loop_Rho,
              paste("./results/Data_Results_Stability_Selection_",
                    corr_type, "_",
                    Dim, "_",
                    beta_position, 
                    ".RDS",
                    sep=""))
    })
    
  })
  
  rm(Loop_Beta_pos)
  
  stopCluster(cl)
  
  if(clusterType == "MPI"){
    mpi.quit()
  }
  
}else{
  
  ### run not in parallel

  Loop_Beta_pos <- lapply(BETA_POSITION, function(beta_position){
    Loop_Corr_type <- lapply(CORR_TYPE, function(corr_type){
      Loop_Rho <- lapply(RHO, function(Rho){
        Loop_Snr <- lapply(SNR, function(snr){
          Loop_Sim_n <- lapply(1:Sim_n, function(sim_n){
            
            
            # define corr structure
            if(corr_type == "toeplitz"){
              Sigma <- Rho^toeplitz(0:(P-1))
            }else if(corr_type == "block"){
              Sigma <- block_builder(P, Rho, s)
            }
            
            # define non-zero positions
            beta <- rep(0, P)
            if(beta_position == "spread"){
              beta[seq(1,P, P/s)] <- 1
            }else if(beta_position == "adjacent"){
              beta[1:s] <- 1
            }
            
            n <- N
            
            # get indices of non-zeros
            non_zero_indices <- which(beta != 0)
            
            # variance of error term
            sigma <- sqrt( (t(beta) %*% Sigma %*% beta) / snr )
            
            # error
            seed <- round(sim_n+snr*1000000)
            try(set.seed(seed))
            e <- rnorm(n) %*% sigma 
            
            # simulate covariates
            seed <- round(sim_n+Rho*1000)
            X <- mvtnorm::rmvnorm(n = n, mean = rep(0, ncol(Sigma)), sigma = Sigma)
            Y <- X %*% beta + e 
            Y <- scale(Y, scale = FALSE)
            
            # define max subset size of best subset selection based on
            # per-family error rate and stability thresohold (cutoff)
            # see paper of Meinshausen & Bühlmann (2010)
            q <- ceiling(sqrt(pfer*(2*cutoff-1)*P)) 
            
            
            
            # Run Stabiliyt selection on BSS
            # loop over subsampling process
            if(runBSS == TRUE){
              Loop_BSS_B <- lapply(1:B, function(b){
                
                # number of samples in the subsamples based onMeinshausen & 
                # Bühlmann (2010)
                set.seed(b)
                n_sample <- sample(1:n, n/2)
                
                
                BSS_b <- 
                  bs(x=X[n_sample,], y = Y[n_sample], 
                     intercept = FALSE, 
                     time.limit = gurobiTime, 
                     k=1:min(max.k, q), 
                     verbose=F)
                
                BSS_b$beta != 0
                
              })
              
              prob_selction_BSS <- Reduce("+", Loop_BSS_B)/B
              
              # get "stable" coefficients
              estimated_non_zeros_BSS <- 
                which(apply(prob_selction_BSS, 1, function(i){any(i >= cutoff)}))
              
              TP <- sum(estimated_non_zeros_BSS %in% non_zero_indices)
              FP <- length(estimated_non_zeros_BSS)-TP
              FN <- s-TP
              F1 <- TP/(TP + 0.5*(FP+FN))
              Precision <- TP/(TP + FP)
              Accuracy <- TP/s
              
              
              
              BSS_results <- tibble(
                method="BSS_gurobi",
                alpha=NA,
                lambda = NA,
                RSS = NA,
                RSS_selected_betas = NA,
                k=length(estimated_non_zeros_BSS),
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
                rho=Rho,
                sim.n = sim_n,
                beta_position = beta_position,
                corr_type = corr_type,
                criterion = "Stability Selection",
                cutoff = cutoff,
                PFER = pfer
              )
              rm(estimated_non_zeros_BSS)
            }else{
              BSS_results <- NULL
            }
            
            
            # end BSS
            
            
            
            # FSS
            # loop over subsampling process
            Loop_FSS_B <- lapply(1:B, function(b){
              
              # number of samples in the subsamples based onMeinshausen & 
              # Bühlmann (2010)
              set.seed(b)
              n_sample <- sample(1:n, n/2)
              
              FSS_b <- 
                lars::lars(x=X[n_sample,], y=Y[n_sample], 
                           max.steps = min(max.k, q), type = "stepwise",
                           use.Gram=FALSE)
              
              maxk_selected <- unlist(FSS_b$actions)
              
              selection_matrix <- matrix(rep(0, P*max.k), ncol=max.k)
              
              for(i in seq_along(maxk_selected)){
                selection_matrix[maxk_selected[1:i],i] <- 1
              }
              
              selection_matrix
              
            })
            
            prob_selction_FSS <- Reduce("+", Loop_FSS_B)/B
            
            # get "stable" coefficients
            estimated_non_zeros_FSS <- 
              which(apply(prob_selction_FSS, 1, function(i){any(i >= cutoff)}))
            
            TP <- sum(estimated_non_zeros_FSS %in% non_zero_indices)
            FP <- length(estimated_non_zeros_FSS)-TP
            FN <- s-TP
            F1 <- TP/(TP + 0.5*(FP+FN))
            Precision <- TP/(TP + FP)
            Accuracy <- TP/s
            
            
            
            FSS_results <- tibble(
              method="FSS",
              alpha=NA,
              lambda = NA,
              RSS = NA,
              RSS_selected_betas = NA,
              k=length(estimated_non_zeros_FSS),
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
              rho=Rho,
              sim.n = sim_n,
              beta_position = beta_position,
              corr_type = corr_type,
              criterion = "Stability Selection",
              cutoff = cutoff,
              PFER = pfer
            )
            rm(estimated_non_zeros_FSS)
            
            
            
            ### begin Lasso/Enet
            # loop over alphas
            Enet_results <- lapply(Alpha, function(i){
              
              Enet <- glmnet(x=X, y=Y, alpha = i, 
                             nlambda = 1000, 
                             intercept = F)
              
              
              
              n_non_zeros <- apply(Enet$beta, 2, function(x){
                sum(x != 0)
              })
              
              
              lambda_indices <- which(n_non_zeros <= q)
              
              Enet_lambdas <- Enet$lambda[lambda_indices]
              
              # loop over subsampling process
              Loop_Enet_B <- lapply(1:B, function(b){
                
                # number of samples in the subsamples based onMeinshausen & 
                # Bühlmann (2010)
                set.seed(b)
                n_sample <- sample(1:n, n/2)
                
                Enet_b <- 
                  glmnet(x=X[n_sample,], y=Y[n_sample], alpha = i,
                         lambda = Enet_lambdas, 
                         intercept = F)
                
                Enet_b$beta != 0
                
                
              })
              
              prob_selction_Enet <- Reduce("+", Loop_Enet_B)/B
              
              # get "stable" coefficients
              estimated_non_zeros_Enet <- 
                which(apply(prob_selction_Enet, 1, function(i){any(i >= cutoff)}))
              
              TP <- sum(estimated_non_zeros_Enet %in% non_zero_indices)
              FP <- length(estimated_non_zeros_Enet)-TP
              FN <- s-TP
              F1 <- TP/(TP + 0.5*(FP+FN))
              Precision <- TP/(TP + FP)
              Accuracy <- TP/s
              
              
              
              tibble(
                method=paste("Enet", i, sep=" "),
                alpha=i,
                lambda = NA,
                RSS = NA,
                RSS_selected_betas = NA,
                k=TP+FP,
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
                rho=Rho,
                sim.n = sim_n,
                beta_position = beta_position,
                corr_type = corr_type,
                criterion = "Stability Selection",
                cutoff = cutoff,
                PFER = pfer
              )
              
            })
            
            Enet_results <- do.call(rbind, Enet_results)
            
            
            out <- rbind(
              BSS_results,
              FSS_results,
              Enet_results)
            
            
            out
          })
          Loop_Sim_n <- do.call(rbind, Loop_Sim_n)
          Loop_Sim_n 
          
        })
        Loop_Snr <- do.call(rbind, Loop_Snr)
        Loop_Snr
        
        
      })
      Loop_Rho <- do.call(rbind, Loop_Rho)
      
      
      if(N==1000){
        Dim <- "low"
      }else if(N==100){
        Dim <- "high"
      }
      
      Loop_Rho$method[Loop_Rho$method == "Enet 1"] <- 
        "Lasso"
      
      if(runBSS == TRUE){
        Loop_Rho$method <- factor(Loop_Rho$method,
                                  levels = 
                                    c("Enet 0.1",
                                      "Enet 0.2",
                                      "Enet 0.3",
                                      "Enet 0.4",
                                      "Enet 0.5", 
                                      "Enet 0.6",
                                      "Enet 0.7",
                                      "Enet 0.8",
                                      "Enet 0.9", 
                                      "Lasso",
                                      "FSS",
                                      "BSS"))
      }else{
        Loop_Rho$method <- factor(Loop_Rho$method,
                                  levels = 
                                    c("Enet 0.1",
                                      "Enet 0.2",
                                      "Enet 0.3",
                                      "Enet 0.4",
                                      "Enet 0.5", 
                                      "Enet 0.6",
                                      "Enet 0.7",
                                      "Enet 0.8",
                                      "Enet 0.9", 
                                      "Lasso",
                                      "FSS"
                                    ))
      }
      
      
      # save results
      saveRDS(Loop_Rho,
              paste("./results/Data_Results_Stability_Selection_",
                    corr_type, "_",
                    Dim, "_",
                    beta_position, 
                    ".RDS",
                    sep=""))
    })
    
  })
  
  rm(Loop_Beta_pos)
  
  
  
}




