library(simsham)
library(HDeconometrics)

instance <- simsham()
ap <- list(alpha = 0.6)

# https://stackoverflow.com/questions/40920051/r-getting-aic-bic-likelihood-from-glmnet
# https://community.jmp.com/t5/Discussions/getting-AIC-likelihood-from-model-coefficients/td-p/31056


fit <- glmnet::glmnet(instance$X, instance$y, alpha = ap$alpha, relax = F)
X <- instance$X
y <- instance$y
i = 40
lambda <- fit$lambda[i]
beta <- fit$beta[, i]
alpha = ap$alpha

compute_loglikelihood_enet <- function(X, y, beta, lambda, alpha) { 
    sum((y - X %*% beta)^2) + lambda * (alpha * sum(abs(beta)) + (1 - alpha)/2 * sum(beta^2))
}

compute_loglikelihood_enet(X, y, beta, lambda, alpha)

f <- compute_information_criteria(fit)
f$loglikelihood[i]
f$BIC[i]

ic = ic.glmnet(instance$X, instance$y)
ic$ic.range[i]

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
