#' Compute Information Criterion
#' 
#' Computes various information criteria given a 
#' glmnet fit 
#' 
#' @param fit Results of the glmnet function
#' 
#' @return List with various information criteria
#' @references 
#' https://stackoverflow.com/questions/40920051/r-getting-aic-bic-likelihood-from-glmnet
#' https://community.jmp.com/t5/Discussions/getting-AIC-likelihood-from-model-coefficients/td-p/31056
#' 
#' On the rescaled E-net: 
#' https://stats.stackexchange.com/questions/326427/why-does-glmnet-use-naive-elastic-net-from-the-zou-hastie-original-paper
compute_information_criteria <- function(fit) { 
  # determine the log-likelihood for the models
  loglikelihood <- (fit$nulldev - deviance(fit)) / 2
  
  # number of variables
  k <- fit$df
  
  # number of observations
  n <- fit$nobs
  
  list(
    loglikelihood = loglikelihood, 
    k = k, 
    n = n,
    AIC = -2*loglikelihood + 2*k, 
    AICc = -2*loglikelihood + 2*k + 2*k*(k + 1)/(n - k - 1),
    BIC = -2*loglikelihood + k*log(n) 
  )
}