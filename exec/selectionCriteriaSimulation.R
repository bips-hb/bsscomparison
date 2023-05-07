# This script runs the simulation for various tuning paramter/subset size 
# criterua: BIC, mBIC2 and HQC. Various settings are possible.
# For Best Subset Selection the Gurobi solver is necessary.
# NOTE: Stability Selection is computational challanging especially when
# when performing subsampling for Stability Selection.
# It is strongly recommended to run  Best Subset Selection on a high perfomance 
# clusterin

# Load necessary packages for parallelisation
library( parallel)
library( snow)
library( Rmpi)

# Load necessary packages to run the simulation
library(glmnet)
library(bestsubset)
library(tibble)
library(tidyverse)
library(mvtnorm)
library(lars)

# If the (open)MPI enviroment for parallel computing is used this function is
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

# set the number of observations (N), variables (P) and non-zero coefficients (s)
N <- 100
P <- 1000
s <- 10


# set the time limit in sec for the Gurobi solver (Important: this time limit is
# used in every subsample!)
gurobiTime <- 180

# set corraltion structure ("block" and "toeplitz" are available) 
CORR_TYPE <- "block"

# set the signal to noise ratios
SNR <- c(0.05, 0.25, 0.42, 1.22, 2.07, 6)

# set the correlation between variables
RHO <- c(0.35, 0.7)

# set the position of the non-zeros ("adjacent" or "spread")
BETA_POSITION <- "adjacent"

# number of simulation runs
Sim_n <- 100

# number of maximum subset size for Best Subset Selection and Forward Stepwise
# Selection
max.k <- 15

# alpha Values for Enet, i.e. weighting of the ridge penalty part; 1 is Lasso
Alpha <- seq(0.1,1,0.1)

mc <- 105 #Optional: number of cores can be determined automatically

# Type of cluster ("PSOCK" and "MPI" available)  
clusterType <- "MPI"
cl <- makeCluster(mc, type=clusterType)

clusterExport(cl, c("block_builder",
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
                RSS_unbiased = RSS,
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
                sim.n = sim_n
              )

            })

          BSS_results <-
            do.call(rbind, BSS_results)
          
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
                RSS_unbiased = RSS,
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
                sim.n = sim_n
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
                betas_unbiased <-  
                  solve(t(X[, Enet_betas_indices]) %*% 
                          X[, Enet_betas_indices]) %*% 
                  t(X[, Enet_betas_indices]) %*% 
                  Y
                
                RSS_unbiased <- sum((Y - X[, Enet_betas_indices] %*% betas_unbiased)^2)
                
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
                RSS_unbiased <- NA
                df <- NA
              }
              
              tibble(
                method = paste("Enet ", alpha, sep=""),
                # method="BSS",
                
                alpha=alpha,
                # alpha=NA,
                
                k=sum(abs(fit_enet$beta[,x]) > 0.0000001),
                RSS = RSS,
                RSS_unbiased = RSS_unbiased,
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
                sim.n = sim_n
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
          mutate(ll = 0.5 * ( - n * (log(2 * pi) + 1 - log(n) + log( RSS_unbiased/n)))) %>%
          mutate(BIC = -2 * ll + log(n) * (k+1)) %>%
          mutate(min_BIC = min(BIC)) %>%
          ungroup() %>%
          filter(min_BIC == BIC) %>%
          mutate(criterion = "BIC") %>%
          select(method, alpha, RSS, RSS_unbiased,  ll, k, TP, FP, FN, F1, 
                 Precision, Accuracy, n, p, s, snr, rho, sim.n, beta_position,
                 corr_type, criterion)
        
        results_HQC <- results %>% filter(k <= 50) %>%
          group_by(snr, rho, method, alpha, sim.n) %>%
          mutate(ll = 0.5 * ( - n * (log(2 * pi) + 1 - log(n) + log( RSS_unbiased/n)))) %>%
          mutate(HQC = -2 * ll + 2 * (k)*log(log(n))) %>%
          mutate(min_HQC = min(HQC)) %>%
          ungroup() %>%
          filter(min_HQC == HQC) %>%
          mutate(criterion = "HQC") %>%
          select(method, alpha, RSS, RSS_unbiased,  ll, k, TP, FP, FN, F1, 
                 Precision, Accuracy, n, p, s, snr, rho, sim.n, beta_position,
                 corr_type, criterion)
        
        results_mBIC2 <- results %>% filter(k <= 50) %>%
          group_by(snr, rho, method, sim.n) %>%
          mutate(ll = 0.5 * (- n * (log(2 * pi) + 1 - log(n) + log(RSS_unbiased/n)))) %>%
          mutate(mBIC2 = -2 * ll + (k+1)*(log(n) + 2*log(p) - 2*log(4)) - 2*log(factorial(k+1))) %>%
          mutate(min_mBIC2 = min(mBIC2)) %>%
          ungroup() %>%
          filter(min_mBIC2 == mBIC2) %>%
          mutate(criterion = "mBIC2") %>%
          select(method, alpha, RSS, RSS_unbiased, ll, k, TP, FP, FN, F1, 
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
    Loop_Rho
    
    if(N==1000){
      Dim <- "low"
    }else if(N==100){
      Dim <- "high"
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





