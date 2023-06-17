# This script runs the simulation for various tuning parameter/subset size 
# criteria: BIC, mBIC2 and HQC. 
# For Best Subset Selection the Gurobi solver is necessary.
# NOTE: This simulation is computational challenging even without BSS.
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
    n.blocks <- p/size 
    out <- matrix(rep(0, p^2), ncol = p) 
    for(i in c(0:(n.blocks-1))){
      out[c(i*size+1):c(i*size+size), c(i*size+1):c(i*size+size)] <- rho
    }
    diag(out) <- 1
    return(out)
  }

# set path for saving data
pfs <- "./data/"

cl <- makeCluster(mc, type=clusterType)

clusterExport(cl, c("block_builder",
                    "gurobiTime",
                    "runBSS",
                    "N",
                    "P",
                    "s",
                    "CORR_TYPE",
                    "RHO",
                    "SNR",
                    "BETA_POSITION", 
                    "max.k",
                    "Alpha",
                    "Sim_n"))

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

          # define dimension 
          if(N==1000){
            Dim <- "low"
            n <- 1000
            p <- 100
          }else if(N==100){
            Dim <- "high"
            n <- 100
            p <- 1000
          }
          
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
          

          ### begin BSS
          if(runBSS == TRUE){
            
            BSS <- bs(x=X, y=Y, intercept=FALSE,
                      time.limit=gurobiTime,
                      k=1:max.k, verbose=F)
            
            # loop over subsetsizes for results like true positives (TP), false
            # positves (FP), F1 etc
            BSS_results <-
              lapply(1:max.k, function(x){
                RSS <- sum((Y - X %*% BSS$beta[,x])^2)
                
                TP <- sum(which(abs(BSS$beta[,x]) > 0.00001) %in% non_zero_indices)
                FP <- x-TP
                FN <- s-TP
                F1 <- TP/(TP + 0.5*(FP+FN))
                Precision <- TP/(TP + FP)
                Accuracy <- TP/s
                
                tibble(
                  method="BSS",
                  alpha=NA,
                  k=x,
                  RSS = RSS,
                  RSS_selected_betas = RSS,
                  TP,
                  FP,
                  FN,
                  F1,
                  Precision,
                  Accuracy,
                  n,
                  p,
                  s,
                  snr,
                  corr_type = corr_type,
                  rho = Rho,
                  beta_position = beta_position,
                  beta_switch = NA,
                  status = BSS$status[x],
                  sim.n = sim_n,
                  cutoff = NA,
                  PFER = NA
                )
                
              })
            
            BSS_results <-
              do.call(rbind, BSS_results)
            
          }else{
            BSS_results <- NULL
          }
          
          # end BSS
          
          
          # begin FSS
          FSS <- fs(x=X, y=Y, maxsteps= max.k, intercept=FALSE, verbose=F)
          
          # loop over subsetsizes for results like true positives (TP), false
          # positves (FP), F1 etc
          FSS_results <- 
            lapply(1:max.k, function(x){
              RSS <- sum((Y - X %*% FSS$beta[,x+1])^2)
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
                RSS = RSS,
                RSS_selected_betas = RSS,
                TP,
                FP,
                FN,
                F1,
                Precision,
                Accuracy,
                n,
                p,
                s,
                snr,
                corr_type = corr_type,
                rho = Rho,
                beta_position = beta_position,
                status = NA,
                sim.n = sim_n,
                cutoff = NA,
                PFER = NA
              )
              
            })
          
          FSS_results <- 
            do.call(rbind, FSS_results)
          
          # end FSS
          
          
          
          ### begin Enet
          
          # loop over all alphas
          Enet_results <- lapply(Alpha, function(alpha){
        
            fit_enet <- glmnet(X, Y, alpha = alpha, nlambda = 1000)
            
            
            non_zero_betas <- 
              apply(fit_enet$beta, 2, function(x){as.numeric(x != 0)})
            
            beta_switches <- c(sapply(1:(ncol(non_zero_betas)-1), function(i){
              non_zero_diffs <- non_zero_betas[,i+1]-non_zero_betas[,i+1]
              sum(non_zero_diffs < 0)
            }), 0)
            
            # loop over lambdas for results like true positives (TP), false
            # positves (FP), F1 etc
            enet_alpha <- lapply(2:ncol(fit_enet$beta), function(x){
              
              Enet_betas_indices <- which(abs(fit_enet$beta[,x]) > 0.000001)
              
              TP <- sum(Enet_betas_indices %in% non_zero_indices)
              FP <- length(Enet_betas_indices) - TP
              FN <- s - TP
              F1 <- TP/(TP + 0.5*(FP+FN)) 
              Precision <- TP/(TP + FP)
              Accuracy <- TP/s
              
              RSS <- sum((Y - X %*% fit_enet$beta[,x])^2)
              
              
              if(length(Enet_betas_indices) < n){
                # calculate regression coefficients for selected
                # (see also "Least squares after model selection in high-
                # dimensional sparse models" by Belloni & Chernozhukov (2013) )
                
                betas_selected <-  
                  solve(t(X[, Enet_betas_indices]) %*% 
                          X[, Enet_betas_indices]) %*% 
                  t(X[, Enet_betas_indices]) %*% 
                  Y
                
                # RSS for selected variables
                RSS_selected_betas <- sum((Y - X[, Enet_betas_indices] %*% betas_selected)^2)
                
                if(alpha == 1){
                  
                  df <- length(Enet_betas_indices)
                  
                }else{
                  
                  df <- sum(diag(
                    X[, Enet_betas_indices] %*%
                      solve(
                        t(X[, Enet_betas_indices]) %*% 
                          X[, Enet_betas_indices] +
                          diag(fit_enet$lambda[x], length(Enet_betas_indices))
                      ) %*% 
                      t(X[, Enet_betas_indices])
                  ))
                  
                }
                
                
                
              }else{
                RSS_selected_betas <- NA
                df <- NA
              }
              
              tibble(
                method = paste("Enet ", alpha, sep=""),
                alpha=alpha,
                k=sum(abs(fit_enet$beta[,x]) > 0.0000001),
                RSS = RSS,
                RSS_selected_betas = RSS_selected_betas,
                df = df,
                TP,
                FP,
                FN,
                F1,
                Precision,
                Accuracy,
                n,
                p,
                s,
                snr,
                corr_type = corr_type,
                rho = Rho,
                beta_position = beta_position,
                status = NA,
                sim.n = sim_n,
                cutoff = NA,
                PFER = NA
              )
              
              
            })
            
            enet_alpha <- do.call(rbind, enet_alpha)
          })
          
          
          Enet_results <- do.call(rbind, Enet_results)
          ### end Enet
          
          
          
          
          bind_rows(
            BSS_results,
            FSS_results,
            Enet_results
            )
          
        })
        Loop_Sim_n <- do.call(rbind, Loop_Sim_n)
        
        results <- Loop_Sim_n
        
        # rename methods
        results$method[results$method == "Enet 1"] <- 
          "Lasso"
        
        results$method <- factor(results$method,
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
        
        # calculate BIC, mBIC2 and HQC
        results_BIC <- results %>% filter(k <= 50) %>%
          group_by(snr, rho, method, alpha, sim.n) %>%
          mutate(ll = 0.5 * ( - n * (log(2 * pi) + 1 - log(n) + log( RSS_selected_betas/n)))) %>%
          mutate(BIC = -2 * ll + log(n) * (k+1)) %>%
          mutate(min_BIC = min(BIC)) %>%
          ungroup() %>%
          filter(min_BIC == BIC) %>%
          mutate(criterion = "BIC") %>%
          select(method, alpha, RSS,  RSS_selected_betas, ll, k, TP, FP, FN, F1, 
                 Precision, Accuracy, n, p, s, snr, rho, sim.n, beta_position,
                 corr_type, criterion)
        
        results_HQC <- results %>% filter(k <= 50) %>%
          group_by(snr, rho, method, alpha, sim.n) %>%
          mutate(ll = 0.5 * ( - n * (log(2 * pi) + 1 - log(n) + log( RSS_selected_betas/n)))) %>%
          mutate(HQC = -2 * ll + 2 * (k)*log(log(n))) %>%
          mutate(min_HQC = min(HQC)) %>%
          ungroup() %>%
          filter(min_HQC == HQC) %>%
          mutate(criterion = "HQC") %>%
          select(method, alpha, RSS,  RSS_selected_betas, ll, k, TP, FP, FN, F1, 
                 Precision, Accuracy, n, p, s, snr, rho, sim.n, beta_position,
                 corr_type, criterion)
        
        results_mBIC2 <- results %>% filter(k <= 50) %>%
          group_by(snr, rho, method, sim.n) %>%
          mutate(ll = 0.5 * (- n * (log(2 * pi) + 1 - log(n) + log(RSS_selected_betas/n)))) %>%
          mutate(mBIC2 = -2 * ll + (k+1)*(log(n) + 2*log(p) - 2*log(4)) - 2*log(factorial(k+1))) %>%
          mutate(min_mBIC2 = min(mBIC2)) %>%
          ungroup() %>%
          filter(min_mBIC2 == mBIC2) %>%
          mutate(criterion = "mBIC2") %>%
          select(method, alpha, RSS, RSS_selected_betas, ll, k, TP, FP, FN, F1, 
                 Precision, Accuracy, n, p, s, snr, rho, sim.n, beta_position,
                 corr_type, criterion)
        
        rbind(results_BIC,
              results_HQC,
              results_mBIC2
        )
        
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
            paste(pfs, "Results_Selection_Criteria_",
                  corr_type, "_",
                  Dim, "_",
                  beta_position,
                  ".RDS",
                  sep=""))
  })
  
})



stopCluster(cl)

if(clusterType == "MPI"){
  mpi.quit()
}





