#' download-intermediate-results-medium-high.R 
#'
#' Script that downloads the intermediate results 
#' needed to generate the plots for medium and 
#' high dimensional results. 

# check first whether file was already downloaded
if (! file.exists("results/raw_results_high_spread_block.RDS")) { 

  url <- "https://zenodo.org/record/8139859/files/BestSubsetResults.zip?download=1"
  
  cat(sprintf("Start downloading results file for medium and high dimensional settings...\n"))
  cat(sprintf("NOTE: this can take a considerable amount of time (~ 1.5 Gb)\n\n"))
  cat(sprintf(" \n\n"))
  cat(sprintf(crayon::bgRed("IMPORTANT: If the download fails because of an timeout error \n")))
  cat(sprintf(crayon::bgRed("please set a higher timout limit (e.g. options(timeout=1000) ) \n")))
  
  download.file(url, "results/BestSubsetResults.zip", method = "auto", quiet = FALSE, mode = "wb",
                cacheOK = TRUE,
                extra = getOption("download.file.extra"),
                headers = NULL)
  success("Zip file downloaded successfully\n")
  cat(sprintf("Unzip file...\n"))
  
  unzip("results/BestSubsetResults.zip", exdir = "./results/")
  
  cat(sprintf("Unzipped file\n"))
  
  # Move files to ./results
  files_to_move <- list.files("./results/BestSubsetResults/")
  file.copy(from = paste("./results/BestSubsetResults/", files_to_move, sep=""), 
            to = paste("./results/", files_to_move, sep=""))
  
  cat(sprintf("Deleting not neccessary files"))
  # delete unpacked original directory and zip-file -- must add recursive = TRUE
  unlink("./results/BestSubsetResults.zip", recursive = TRUE)
  unlink("./results/__MACOSX/", recursive = TRUE)
  unlink("./results/BestSubsetResults", recursive = TRUE)
  
  cat(sprintf("DONE Download\n"))
  
  
  
} else { 
  cat(sprintf("File has already been downloaded, see 'results/intermediate-results.RDS'\n")) 
}


