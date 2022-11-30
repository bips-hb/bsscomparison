# Script containing the plotting functions
library(ggplot2)
library(readr)
library(dplyr)
library(simsham)
library(latex2exp)
library( purrrlyr)

source("exec/load-results.R")
source("exec/plot.R")

# create all correlation matrix plots with betas
df <- as_tibble(data.frame(
  expand.grid(
    p = c(30),
    s = c(5, 10),
    type = c("toeplitz", "block"),
    betatype = c("first", "spread"),
    blocksize = c(5, 10)
  ))
) %>%
  rowwise() %>%
  mutate(
    filename = sprintf("figures/%s_%s_p%d_s%d_blocksize%d.pdf",type, betatype, p,s,blocksize)
  )

# create plots
df %>% rowwise() %>%
  mutate(done = {
    if (type == "toeplitz") {
      corrmat <-  simsham::corrmat_toeplitz(p = p, rho = .5)
    } else {
      corrmat <-  simsham::corrmat_block(p = p, blocksize = blocksize, rho = .5)
    }
    if (betatype == "first") {
      betas <- simsham::beta_first_s_covariates(p, s)
    } else {
      betas <- simsham::beta_spread_covariates(p, s,indices = seq(1,p,by=s))
    }
    plot_corrmat2(corrmat, betas = betas, legend = T)
    ggsave(filename, width = 4, height = 3)})


### create result plots

scores <- data.frame(score = c("F1", "AIC", "BIC", "AICc")) %>% 
  mutate(
    filename = sprintf("results/final-results-%s.tsv", score)
  )

for (i in 1:nrow(scores)) { 
   
  results <- readr::read_rds(scores$filename[i])
  
  source("parameter-settings.R")
  
  sim_param <- sim_param %>% 
    mutate(
      filename = sprintf("figures/%s-%s-%s-%s-%g-%g.pdf", scores$score[i], dimensionality, corr_type, beta_type, rho, snr)
    )
  
  for (j in 1:nrow(sim_param)) { 
    sp <- sim_param[j, ] 
    score <- scores$score[i]
    p <- plot_results(results, sp$n, sp$p, sp$s, sp$dimensionality,
                      sp$corr_type, sp$rho, sp$beta_type, sp$snr, 
                      title = "", 
                      ylim = c(0, 1), 
                      score = scores$score[i])
    ggsave(sp$filename, p, width = 5, height = 2)
  }
}
# 
# # create filenames
# s_f1 <- sim_param %>% mutate(score = "F1")
# s_bic <- sim_param %>% mutate(score = "BIC")
# s_aic <- sim_param %>% mutate(score = "AIC")
# s_aicc <- sim_param %>% mutate(score = "AICc")
# 
# s <- full_join(s_f1, s_bic)
# s <- full_join(s, s_aic) 
# sim_param <- full_join(s, s_aicc) 
# 
# sim_param <- sim_param %>% 
#   mutate(
#     filename = sprintf("figures/%s-%s-%s-%s-%g-%g.pdf", score, dimensionality, corr_type, beta_type, rho, snr)
#   )
# 
# for (i in 1:nrow(sim_param)) { 
#   sp <- sim_param[i, ] 
#   score <- sp$score
#    p <- plot_results(results, sp$n, sp$p, sp$s, sp$dimensionality,
#                   sp$corr_type, sp$rho, sp$beta_type, sp$snr, 
#                   title = "", 
#                   ylim = c(0, 1), 
#                   score = "F1")
#   ggsave(sp$filename, p, width = 5, height = 2)
# }













### create comparison plots between the different selection methods (F1, AIC, BIC, AICc)

results <- readr::read_rds("results/comparing-different-selection-methods.rds")

source("parameter-settings.R")
  
sim_param <- sim_param %>% 
  mutate(
    filename = sprintf("figures/IC-%s-%s-%s-%g-%g.pdf", dimensionality, corr_type, beta_type, rho, snr)
)
  
for (i in 1:nrow(sim_param)) {
  sp <- sim_param[i,]
  
  p <- plot_comparison(
    results,
    sp$n,
    sp$p,
    sp$s,
    sp$dimensionality,
    sp$corr_type,
    sp$rho,
    sp$beta_type,
    sp$snr,
    title = "",
    ylim = c(NA,NA)
  )
  
  ggsave(sp$filename, p, width = 6, height = 4)
}

# 
# # create filenames
# s_f1 <- sim_param %>% mutate(score = "F1")
# s_bic <- sim_param %>% mutate(score = "BIC")
# s_aic <- sim_param %>% mutate(score = "AIC")
# s_aicc <- sim_param %>% mutate(score = "AICc")
# 
# s <- full_join(s_f1, s_bic)
# s <- full_join(s, s_aic) 
# sim_param <- full_join(s, s_aicc) 
# 
# sim_param <- sim_param %>% 
#   mutate(
#     filename = sprintf("figures/%s-%s-%s-%s-%g-%g.pdf", score, dimensionality, corr_type, beta_type, rho, snr)
#   )
# 
# for (i in 1:nrow(sim_param)) { 
#   sp <- sim_param[i, ] 
#   score <- sp$score
#    p <- plot_results(results, sp$n, sp$p, sp$s, sp$dimensionality,
#                   sp$corr_type, sp$rho, sp$beta_type, sp$snr, 
#                   title = "", 
#                   ylim = c(0, 1), 
#                   score = "F1")
#   ggsave(sp$filename, p, width = 5, height = 2)
# }


