fs_wrapper <- function(data, job, instance, ...) { 
  # extract the parameters from job 
  pp <- job$prob.pars 
  ap <- job$algo.pars  
  
  res <- bestsubset::fs(instance$X, instance$y, maxsteps = ap$k, intercept = FALSE)
  
  # determine the models when selected on the basis of the AIC, BIC and AICc
  models_ic <- compute_information_criteria(res, instance$X, instance$y)
  
  ic <- data.frame(p = 1:pp$p, rank = 1:pp$p) %>% 
    mutate(BIC  = models_ic$BIC[rank], 
           AIC  = models_ic$AIC[rank],
           AICc = models_ic$AICc[rank]) %>% arrange(rank)
  
  output <- list(
    id = res$action, 
    ic = ic
  )
  
  res <- process_results(output, pp$p, method = "fs", alpha = NULL, pp$beta_type, pp$s)
  res$job.id <- job$job.id
  res
}

enet_wrapper <- function(data, job, instance, ...) { 
  # extract the parameters from job 
  pp <- job$prob.pars 
  ap <- job$algo.pars  
  
  a <- job$algo.pars$alpha
  r <- as.logical(job$algo.pars$rescaled)
  
  fit <- glmnet::glmnet(instance$X, instance$y, alpha = a, relax = r)
  
  # determine the models when selected on the basis of the AIC, BIC and AICc
  models_ic <- compute_information_criteria(fit, instance$X, instance$y)
  
  # determine the active sets for all lambdas
  active_sets <- fit$beta != 0
  
  # get for which lambda the exposure variable appears for the first time
  # in the active set
  rank <- apply(active_sets == TRUE, 1, which.max) - 1
  
  # in case there is no first appearance, i.e., there was no lambda
  # found for that particular variable, we set it to NA, so that we
  # can later change it to 0
  rank[rank == 0] <- NA
  
  ic <- data.frame(p = 1:pp$p, rank = rank) %>% 
    mutate(BIC  = models_ic$BIC[rank], 
           AIC  = models_ic$AIC[rank],
           AICc = models_ic$AICc[rank]) %>% arrange(rank)
  
  # the highest lambdas for which the variables appear for the first time
  lambda <- fit$lambda[rank]
  # in case no lambda was found for that exposure, highest_lambda is set to 0
  lambda[is.na(lambda)] <- 0
  
  rank <- rank[order(lambda, decreasing = TRUE)]
  lambda <- sort(lambda, decreasing = TRUE)
  
  id <- as.integer(gsub("[^0-9\\.]", "", names(rank)))
  
  output <- list(
    id = id, 
    rank = rank,
    lambda = lambda, 
    alpha = ap$alpha, 
    ic = ic
  )
  
  res <- process_results(output, pp$p, method = "enet", alpha = ap$alpha, pp$relax, pp$beta_type, pp$s)
  res$job.id <- job$job.id
  res
}

