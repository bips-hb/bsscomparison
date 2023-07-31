#' This script contains wrapper functions for the application of FSS, BSS and
#' Lasso/Enet
#' 
#' INPUT:
#' Each wrapper function gets the job parameters and the instances (i.e. the 
#' simulated data) to perform the specific variable selection method 
#' ("fs" = forward stepwise selection, "bs" = best subset selection, "enet" = Enet)
#'
#' OUTPUT: the results for eahc method (based o the function process_results)


fs_wrapper <- function(job, instance, ...) { 
  # extract the parameters from job 
  pp <- job$prob.pars 
  ap <- job$algo.pars  
  
  res <- bestsubset::fs(instance$X, instance$y, maxsteps = ap$k, intercept = FALSE)
  output <- list(
    id = res$action
  )
  
  res <- process_results(output, pp$p, method = "fs", alpha = NULL, pp$beta_type, pp$s)
  res$job.id <- job$job.id
  res
}

bs_wrapper <- function(job, instance, ...) { 
  # extract the parameters from job 
  pp <- job$prob.pars 
  ap <- job$algo.pars  
  
  range <- 1:ap$k
  fit <- bestsubset::bs(instance$X, instance$y, k = range, intercept = FALSE, 
                        time.limit = 5*60)
  # return list. each entry is for a different k, e.g., 0:20 
  # each entry contains the variables that were select for that 
  # k 
  output <- apply(fit$beta, 2, function(x) which(x != 0))
  
  res <- process_results(output, pp$p, method = "bs", alpha = NULL, pp$beta_type, pp$s)
  res$job.id <- job$job.id
  res$finished[1:length(fit$status)] <- fit$status
  res
}

enet_wrapper <- function(job, instance, ...) { 
  # extract the parameters from job 
  pp <- job$prob.pars 
  ap <- job$algo.pars  
  
  fit <- glmnet::glmnet(instance$X, instance$y, alpha = ap$alpha)
 
  # determine the active sets for all lambdas
  active_sets <- fit$beta != 0
  
  # get for which lambda the exposure variable appears for the first time
  # in the active set
  rank <- apply(active_sets == TRUE, 1, which.max) - 1
  
  # in case there is no first appearance, i.e., there was no lambda
  # found for that particular variable, we set it to NA, so that we
  # can later change it to 0
  rank[rank == 0] <- NA
  
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
    alpha = ap$alpha
  )
  
  res <- process_results(output, pp$p, method = "enet", alpha = ap$alpha, pp$beta_type, pp$s)
  res$job.id <- job$job.id
  res
}

