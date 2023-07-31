cat("\n\nFor generating medium and high dimensional plots, one needs to download\n")
cat("the intermediate results. Please follow instructions and see README.\n\n")

answered <- FALSE
while (!answered) { 
  
  # ask whether to run the simulation with different time limits
  time_study <- menu(title = "Do you want to download the intermediate results? Required for creating the related plots.\nYou can download the files manually under\nhttps://zenodo.org/record/8139859/files/BestSubsetResults.zip?download=1 \nand safe and unzip the file to ./results", 
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
           cat("--> I have downloaded the results and extracted to ./results to generate plots for medium and high dimensional settings\n")
           create_medium_high_dimensional_plots <- TRUE
           answered <- TRUE
         }, 
         
         "4" = stop_quietly())
}
