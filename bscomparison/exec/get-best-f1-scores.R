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

summary <- NULL
for(i in 1:nrow(res)){
  
  result_i <- res[[i,14]]
  result_i$F1[is.nan(result_i$F1)] <- 0
  max_F1_i <- get_max(result_i$F1)
  row_max_F1_i <- min(which((result_i$F1)==max_F1_i))
  finished_i = did_finish(result_i$finished[row_max_F1_i])
  
  summary <- 
    bind_rows(summary,
              tibble(res[i,-14],
                     F1 = max_F1_i,
                     finished = finished_i))
}

readr::write_tsv(summary, "results/best-f1-scores.tsv")
