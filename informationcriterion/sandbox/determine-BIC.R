library(simsham)
library(HDeconometrics)

instance <- simsham()
ap <- list(alpha = 0.5)

# https://stackoverflow.com/questions/40920051/r-getting-aic-bic-likelihood-from-glmnet
# https://community.jmp.com/t5/Discussions/getting-AIC-likelihood-from-model-coefficients/td-p/31056


fit <- glmnet::glmnet(instance$X, instance$y, alpha = ap$alpha)
ic = ic.glmnet(instance$X, instance$y)
fit$nulldev
fit$dev.ratio
