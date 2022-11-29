#' Compute RSS
#' 
#' Compute the residual sum of squares given a fit
#' 
#' @param fit Fitted model
#' @param X 
#' @param y
#' 
#' @return RSS
compute_RSS <- function(fit, X, y) { 
  yhat <- predict(fit, X)
  ymat <- matrix(rep(y, ncol(yhat)), ncol = ncol(yhat))
  colSums((yhat - ymat)^2)
}

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
compute_information_criteria <- function(fit, X, y) { 
  # determine the RSS
  RSS <- compute_RSS(fit, X, y)
  
  # number of variables
  k <- fit$df
  
  # number of observations
  if (class(fit)[1] == "fs") { 
    n <- length(fit$y) 
  } else { 
    n <- fit$nobs
  }
  
  LL <- n * log(RSS/n)
  
  list(
    RSS = RSS, 
    k = k, 
    n = n,
    AIC = LL + 2*k, 
    AICc = LL + 2*k + 2*k*(k + 1)/(n - k - 1),
    BIC = LL + k*log(n) 
  )
}


#' Selected Variables
#' 
#' Returns the variables that are selected (i.e., their beta-values
#' are non-zero) for the min. of the AIC, BIC and AICc
selected_variables_information_criterion <- function(fit, X, y) { 
  ic <- compute_information_criteria(fit, X, y) 
  
  # get the variables that are non-zero for the lowest value of ...
  
  # AIC: 
  i <- which(ic$AIC == min(ic$AIC))
  beta_aic <- fit$beta[, i]
  
  # AICc:
  i <- which(ic$AICc == min(ic$AICc))
  beta_aicc <- fit$beta[, i]
  
  # BIC:
  i <- which(ic$BIC == min(ic$BIC))
  beta_bic <- fit$beta[, i]
  
  list(
    BIC  = beta_bic != 0, 
    AIC  = beta_aic != 0, 
    AICc = beta_aicc != 0 
  )
}