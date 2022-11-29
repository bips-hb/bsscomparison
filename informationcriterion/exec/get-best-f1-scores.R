library(dplyr)
library(readr)

res <- readr::read_rds("results/raw-results.rds")

get_extreme <- function(l, type = c("max", "min")) { 
  id = which(is.nan(l))
  
  if (length(id) != 0) { 
    l[id] <- 0 
  }
  
  if (type[1] == "max") { 
    max(l, na.rm = T)
  } else { 
    min(l, na.rm = T)
  }
}


# get the best F1-score 
summary <- res %>% rowwise() %>% 
  mutate(score = get_extreme(result$F1, type = "max")) 

summary <- summary %>% select(-result)
readr::write_tsv(summary, "results/best-f1.tsv")

# get the best BIC-score 
summary <- res %>% rowwise() %>% 
  mutate(score = get_extreme(result$BIC, type = "min")) 

summary <- summary %>% select(-result)
readr::write_tsv(summary, "results/best-BIC.tsv")


# get the best AIC-score 
summary <- res %>% rowwise() %>% 
  mutate(score = get_extreme(result$AIC, type = "min")) 

summary <- summary %>% select(-result)
readr::write_tsv(summary, "results/best-AIC.tsv")

# get the best AIC-score 
summary <- res %>% rowwise() %>% 
  mutate(score = get_extreme(result$AICc, type = "min")) 

summary <- summary %>% select(-result)
readr::write_tsv(summary, "results/best-AICc.tsv")
