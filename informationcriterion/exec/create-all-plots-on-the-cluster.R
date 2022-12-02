
### load the data ----
results <- readr::read_rds("results/best.rds")

### turn alphas into numeric
results$alpha <- as.numeric(results$alpha)

### add the lasso explicitly
results_enet1 <- results %>% filter(algorithm == "enet", alpha == 1)
results_enet1$alpha <- as.numeric(results_enet1$alpha)
results_enet1$algorithm <- "lasso"

results <- rbind(results, results_enet1)

### add simulation id
results$simulation_id <-
  results %>% group_indices(n, p, s, dimensionality, corr_type, rho, beta_type, snr)


update_row <- function(r, selection_method = "F1") { 
  r$algorithm <- sprintf("%s_%s", r$algorithm, selection_method)
  r$score <- r[[selection_method]]
  r %>% select(-c(F1, BIC, AIC, AICc))
}

res <- tibble()
for (i in 1:nrow(results)) { 
  row <- results[i, ]
  for (selection_method in c("F1", "AIC", "AICc", "BIC")) { 
    res <<- rbind(res, update_row(row, selection_method = selection_method))
  }
}


### add nice labels for the algorithms
get_name <- function(algorithm) {
  switch(
    algorithm,
    "enet_F1" = "e-net (oracle)",
    "enet_AIC" = "e-net (AIC)",
    "enet_BIC" = "e-net (BIC)",
    "enet_AICc" = "e-net (AICc)",
    "lasso_F1" = "lasso (oracle)",
    "lasso_AIC" = "lasso (AIC)",
    "lasso_BIC" = "lasso (BIC)",
    "lasso_AICc" = "lasso (AICc)",
    "fs_F1" = "forward stepwise (oracle)",
    "fs_AIC" = "forward stepwise (AIC)",
    "fs_BIC" = "forward stepwise (BIC)",
    "fs_AICc" = "forward stepwise (AICc)"
  )
}

res$algorithm_label <-
  sapply(res$algorithm, function(algorithm)
    get_name(algorithm))

#plot_comparison(res, ylim = c(.1, .3))

res <- ungroup(res)

readr::write_rds(res, "results/comparing-different-selection-methods.rds", compress = "gz")


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
