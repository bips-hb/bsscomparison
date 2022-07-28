library(dplyr)
library(readr)

res <- readr::read_rds("results/raw-results.rds")

get_max <- function(l) { 
  id = which(is.nan(l))
  
  if (length(id) != 0) { 
    l[id] <- 0 
  }

  max(l, na.rm = T)
}

did_finish <- function(status) { 
  if (is.na(status)) { 
    return(NA) 
  }
  
  if ("TIME_LIMIT" %in% status) { 
    return(FALSE)
  } else { 
    return(TRUE)
  }
}

summary <- res %>% rowwise() %>% 
  mutate(F1 = get_max(result$F1), 
         finished = did_finish(result$finished)) 

summary <- summary %>% select(-result)
readr::write_tsv(summary, "results/best-f1-scores.tsv")
