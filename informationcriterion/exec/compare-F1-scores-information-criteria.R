library(dplyr)
library(readr)
library(tidyr)

res <- readr::read_rds("results/raw-results.rds")

get_score <- function(table, selection_method = c("F1", "BIC", "AIC", "AICc")) {
  switch(selection_method[1], 
         "F1"   = temp <- table %>% filter(F1 == max(F1, na.rm = TRUE)), 
         "BIC"  = temp <- table %>% filter(BIC == min(BIC, na.rm = TRUE)), 
         "AIC"  = temp <- table %>% filter(AIC == min(AIC, na.rm = TRUE)), 
         "AICc" = temp <- table %>% filter(AICc == min(AICc, na.rm = TRUE))
  )
  return(mean(temp$F1, na.rm = TRUE))
  # return(list(
  #   selection_method = selection_method[1],
  #   F1 = mean(temp$F1, na.rm = TRUE),
  #   F2 = mean(temp$F2, na.rm = TRUE),
  #   MCC = mean(temp$MCC, na.rm = TRUE)
  # ))
}

best <- res %>% rowwise() %>% 
  mutate(F1 = get_score(result, selection_method = "F1"), 
         BIC = get_score(result, selection_method = "BIC"),
         AIC = get_score(result, selection_method = "AIC"),
         AICc = get_score(result, selection_method = "AICc"))

readr::write_rds(best, "results/best.rds")




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


ggplot(data = res) + 
  geom_boxplot(mapping = aes(x = algorithm_label, y = score)) + 
  ylab("F1 score") + 
  xlab("") + 
  ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  geom_vline(xintercept = 4.5, linetype = "dotted") +
  geom_vline(xintercept = 8.5, linetype = "dotted") 






