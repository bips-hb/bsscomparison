#' simple functions to extract the best F1 results of the simulation study and 
#' the certifications and write a summary file of these results

library(dplyr)
library(readr)

# load raw results
res <- readr::read_rds("results/raw-results.rds")

#' get max F1 score
#' INPUT:
#' l: vector of numerics (e.g. F1 scores)
#' OUTPUT:
#' max value (numeric)
 
get_max <- function(l) { 
  id = which(is.nan(l))
  
  if (length(id) != 0) { 
    l[id] <- 0 
  }

  max(l, na.rm = T)
}

#' function for checking if time limit has been reached
#' INPUT:
#' status: vector of strings
#' 
#' OUTPUT: 
#' TRUE/FALSE 
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

# loop for creating a summary of the results
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

# write summary as tsv
readr::write_tsv(summary, "results/best-f1-scores.tsv")
