#' download-intermediate-results-medium-high.R 
#'
#' Script that downloads the intermediate results 
#' needed to generate the plots for medium and 
#' high dimensional results. 

# check first whether file was already downloaded
if (! file.exists("results/raw_results_high_spread_block.RDS")) { 
  
  url <- "https://www.bips-institut.de/fileadmin/downloads/BestSubsetResults.zip" 
  
  cat(sprintf("Start downloading results file for medium and high dimensional settings...\n"))
  cat(sprintf("NOTE: this can take a considerable amount of time (~ 1.5 Gb)\n\n"))
  download.file(url, "results/BestSubsetResults.zip", method = "auto", quiet = FALSE, mode = "w",
                cacheOK = TRUE,
                extra = getOption("download.file.extra"),
                headers = NULL)
  success("Zip file downloaded successfully\n")
  cat(sprintf("Unzip file...\n"))
  
  unzip("results/BestSubsetResults.zip", exdir = "./results/")
  
  cat(sprintf("Unzipped file\n"))
  cat(sprintf("DONE Download\n"))
  
} else { 
  cat(sprintf("File has already been downloaded, see 'results/intermediate-results.RDS'\n")) 
}


