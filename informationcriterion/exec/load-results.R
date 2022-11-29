library(readr)
library(dplyr)

scores <- data.frame(score = c("F1", "AIC", "BIC", "AICc")) %>% 
  mutate(
    filename_in = sprintf("results/best-%s.tsv", score),
    filename_out = sprintf("results/final-results-%s.tsv", score)
    )

# go over all possible scores
for (i in 1:nrow(scores)) { 
  ### load the data ----
  results <- readr::read_tsv(scores$filename_in[i])
  
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
  
  ### get the best result over all alphas for the e-net
  results_enet <- results %>%
    filter(algorithm == "enet") %>%
    group_by(simulation_id, alpha) %>%
    mutate(temp_id = 1:n()) %>%
    ungroup() %>%
    group_by(simulation_id, temp_id) 
  
  if (scores$score[i] == "F1") { 
    results_enet <- results_enet %>% filter(score == max(score))
  } else { 
    results_enet <- results_enet %>% filter(score == min(score)) 
  }
  
  results_enet <- results_enet %>%
    slice(1) %>%
    ungroup()
  
  results_enet <- results_enet %>%
    select(-temp_id)
  
  # combine the new enet results with the other results
  results <- rbind(results %>% filter(algorithm != "enet"),
                   results_enet)
  
  ### add nice labels for the algorithms
  get_name <- function(algorithm) {
    switch(
      algorithm,
      "bs" = "best subset",
      "fs" = "forward stepwise",
      "lasso" = "lasso",
      "enet" = "e-net",
      "enet_bs_hybrid" = "hybrid"
    )
  }
  
  results$algorithm_label <-
    sapply(results$algorithm, function(algorithm)
      get_name(algorithm))
  
  readr::write_rds(results, scores$filename_out[i], compress = "gz")
}