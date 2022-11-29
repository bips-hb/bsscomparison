library(simsham)
library(HDeconometrics)

instance <- simsham(n = 10, p = 20)
ap <- list(alpha = 0.6)

# https://stackoverflow.com/questions/40920051/r-getting-aic-bic-likelihood-from-glmnet
# https://community.jmp.com/t5/Discussions/getting-AIC-likelihood-from-model-coefficients/td-p/31056

s = bestsubset::fs(instance$X, instance$y, maxsteps = 10, intercept = FALSE)
# coef(s)
# s.supp = apply(fs.beta != 0, 2, which)


# y_hat = predict.fs(s, X)
# 
# y_rep <- matrix(rep(y, 12), ncol = 12)
# RSS = colSums((y_hat - y_rep)^2)
# n = 10
# MLL = -n/2 * log(RSS) - (n / 2)*log(pi/2) - n/2



# i = 40
# lambda <- fit$lambda[i]
# beta <- fit$beta[, i]
# alpha = ap$alpha
# 
# compute_loglikelihood_enet <- function(X, y, beta, lambda, alpha) { 
#     sum((y - X %*% beta)^2) + lambda * (alpha * sum(abs(beta)) + (1 - alpha)/2 * sum(beta^2))
# }
# 
# compute_loglikelihood_enet(X, y, beta, lambda, alpha)
# 
# f <- compute_information_criteria(fit)
# f$loglikelihood[i]
# f$BIC[i]
# 
# ic = ic.glmnet(instance$X, instance$y)
# ic$ic.range[i]

# fit$nulldev
# fit$dev.ratio
# 
# tLL <- fit$nulldev - deviance(fit)
# k <- fit$df
# n <- fit$nobs
# AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
# AICc
# 
# BIC<-log(n)*k - tLL
# BIC

fit <- glmnet::glmnet(instance$X, instance$y, alpha = ap$alpha, relax = F, intercept = FALSE)
X <- instance$X
y <- instance$y



ic <- ic.glmnet(X, y)

f = compute_information_criteria(fit, X, y)



# determine the active sets for all lambdas
active_sets <- fit$beta != 0

# get for which lambda the exposure variable appears for the first time
# in the active set
rank <- apply(active_sets == TRUE, 1, which.max) - 1



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


