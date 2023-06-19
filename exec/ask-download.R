cat("\n\nFor generating medium and high dimensional plots, one needs to download\n")
cat("the intermediate results from the BIPS website. This will be done automatically\n\n")

answered <- FALSE
while (!answered) { 
  
  # ask whether to run the simulation with different time limits
  time_study <- menu(title = "Do you want to download the intermediate results? Required for creating the related plots.\nYou can download the files manually under\nhttps://www.bips-institut.de/fileadmin/downloads/BestSubsetResults.zip \nand safe the file in ./results", 
                     choices = c("yes", 
                                 "no",
                                 "downloaded manually",
                                 "exit"))
  
  switch(as.character(time_study), 
         
         "1" = {
           create_medium_high_dimensional_plots <- TRUE
           source("exec/download-intermediateresults-medium-high.R")
           answered <- TRUE
         }, 
         
         "2" = {
           cat("--> Will not generate plots for medium and high dimensional settings\n")
           create_medium_high_dimensional_plots <- FALSE
           answered <- TRUE
         }, 
         
         "3" = {
           cat("--> I have downloaded the results and saved in ./results to generate plots for medium and high dimensional settings\n")
           create_medium_high_dimensional_plots <- TRUE
           answered <- TRUE
         }, 
         
         "4" = stop_quietly())
}
