#' simulation run based on semi-synthetic data for N=594 observations and p=100
#' variables. Note: this example script does not use the best subset selection 
#' since it will need the Gurobi solver. Using alternative packages like "leaps"
#' can not solve best subset selection with p=100 and subset sizes up to k=15 in
#' a feasible amount of time. We use the forward stepwise procedure from the 
#' bestsubset package of Tibshirani et al. (2021) (see masterscript)

library(glmnet)
library(bestsubset)
library(tibble)
library(tidyverse)

#' load the TCGA dataset
load("./data/tcgaExpression.RData") 
#' transpose the dataset for rowwise observations
tcgaExpression <- t(tcgaExpression)

# set-up a progress bar
pb <- progress::progress_bar$new(
  format = "semi-synthetic simulation [:bar] :percent eta: :eta",
  total = Sim_n + 1, clear = FALSE, width= 80, show_after = 0)
pb$tick()

#' run simulation and application of all methods despite BSS
results_Example_SemiSynthetic <- lapply(1:Sim_n, function(sim_n){

  #' inner Loop over different signal-to-noise values
  Loop_Snr <- lapply(SNR, function(snr){
    
    #' set a seed for selecting the TCGA variables randomly
    seed <- round(sim_n+snr*10000)
    try(set.seed(seed))
    TCGA_subdata <- 
      tcgaExpression[,sample(1:ncol(tcgaExpression), size = subdata_size)]
    
    #' scale all selectedd variables
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
                  sort(Corr_tcga[max_indices[1],], 
                       decreasing = TRUE)[2:s]
                )
          )
      )
    
    #' set zero and non-zero (=1) betas
    beta <- rep(0, ncol(TCGA_subdata))
    beta[non_zero_indices] <- 1
    
    #' set noise variance
    sigma <- sqrt( (t(beta) %*% Corr_tcga %*% beta) / snr )
    
    #' number of observations
    n <- N 
    #number of observations
    seed <- round(sim_n+snr*100000)
    try(set.seed(seed))
    n_sample <- sample(1:nrow(TCGA_subdata), n, replace = F)
    
    #' set seed before generating noise
    seed <- round(sim_n+snr*1000000)
    try(set.seed(seed))
    e <- rnorm(n_sample) %*% sigma 
    
    #' linear model
    X <- TCGA_subdata[n_sample,]
    Y <- X %*% beta + e 
    Y <- scale(Y, scale = FALSE)
    
    
    # run FSS
    FSS <- fs(x=X, y=Y, maxsteps= max.k, intercept=FALSE, verbose=F)
    
    # get results of FSS for each subset size k
    FSS_results <- 
      lapply(1:max.k, function(x){
        #' ture positives
        TP <- sum(which(abs(FSS$beta[,x+1]) > 0.00001) %in% non_zero_indices)
        #' false positives
        FP <- x-TP
        #' false negatives (s is the size of the true model)
        FN <- s-TP
        #' F1 score, Precision, Accuracy
        F1 <- TP/(TP + 0.5*(FP+FN)) 
        Precision <- TP/(TP + FP)
        Accuracy <- TP/s
        
        #' make tipple of results
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
          status = "solved", 
          sim.n = sim_n
        )
        
      })
    
    #' bind results to one tibble/dataframe
    FSS_results <- 
      do.call(rbind, FSS_results)
    
    
    
    #' run Enet over different alpha values and vor nLambda values; alpha=1 is 
    #' Lasso
    Enet_results <- lapply(Alpha, function(alpha){
      
      fit_enet <- glmnet(X, Y, alpha = alpha, nlambda = nLambda)
      
      #' since we are only interested in the subsets selected by Lasso and Enet,
      #' we only need the zeros and non-zeros
      enet_betas <- as.numeric(apply(fit_enet$beta, 2, function(x){
        sum(x != 0) 
      }))
      
      #' since some lambda values will select the same model we will omit
      #' duplicates. Further, glmnet() starts with an empty model an adds 
      #' one by one variables via the "Lasso-Path", i.e. the results are ordered
      #' in an suberset-subset sense
      bigger_subset <- sapply(2:length(enet_betas), function(i){
        enet_betas[i] > enet_betas[i-1]
      })
    
      unique_enet_subsets <- c(seq_along(enet_betas)[-1])[bigger_subset]
      
      #' calculate TP, FP, etc and F1 etc for the Enet results
      enet_alpha <- lapply(unique_enet_subsets, function(x){
        TP <- sum(which(fit_enet$beta[,x] != 0) %in% non_zero_indices)
        FP <- sum(fit_enet$beta[,x] != 0)-TP
        FN <- s-TP
        F1 <- TP/(TP + 0.5*(FP+FN)) 
        Precision <- TP/(TP + FP)
        Accuracy <- TP/s
        
        # tibble dataframe with results
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
      #' bind over lambdas
      enet_alpha <- do.call(rbind, enet_alpha)
    })
    
    #' bind over alphas
    Enet_results <- do.call(rbind, Enet_results)

    
    #' bind all results 
    rbind(FSS_results, 
          Enet_results)
  })
  
  
  #' bind all results over differnt signal-to-noise ratios
  Loop_Snr <- do.call(rbind, Loop_Snr)
  
  pb$tick() # update progressbar
  
  Loop_Snr
})

pb$terminate()

#' bind results over all simulation runs
results_Example_SemiSynthetic <- do.call(rbind, results_Example_SemiSynthetic)

#' save results
saveRDS(results_Example_SemiSynthetic,
        paste("./results/Results_No_BSS_SemiSynthetic",
              subdata_size,
              "_",
              N,
              ".RDS",
              sep=""))
