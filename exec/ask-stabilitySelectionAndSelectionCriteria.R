#' Ask the user whether he/she wants to rerun the the stability 
#' selection/selection criteria simulations or examples

# keeps track whether answered. Otherwise repeats the question
answered <- FALSE
while (!answered) { 
  
  # ask whether to run the semisynthetic simulations
  selectionSimu <- menu(title = "Run the stability selection & selection criteria simulation study...", 
                        choices = c("Run an example without BSS", 
                                    "Run complete simulation without BSS", 
                                    "Run complete simulation with BSS (Gurobi is needed!)", 
                                    "Do not run an (example) simulation",
                                    "exit"))
  
  switch(as.character(selectionSimu), 
         
         "1" = {
           cat("--> Running example for stability selection & selection criteria without BSS\n")
           run_selection_example <- TRUE
           
           cat("Please enter the number of simulation runs for the example (1 to 100).\n ")
           cat("We highly reccomend 1 due to computational costs.\n")
           
           Sim_n <- readline("Number of simulation runs: ")
           if(Sim_n > 100 | Sim_n < 1 | !is.integer(Sim_n)){
             Sim_n <- 1
           }
           
           runBSS <- FALSE
           
           run_selection_complete <- FALSE
           
           answered <- TRUE
         }, 
         
         "2" = {
           yes <- askYesNo("Running a full simulation for stability selection & selection criteria without BSS (requires a powerful computer). Want to continue?")
           if (yes) { 
             cat("--> running all settings without BSS\n")
             run_selection_complete <- TRUE
             runBSS <- FALSE
             
             yes_parallel <- 
               askYesNo("Run in parallel?")
             if (yes_parallel) { 
               run_in_parallel <- TRUE 
             } else { 
               run_in_parallel <- FALSE 
             }
             
             run_selection_example <- FALSE
             answered <- TRUE
           } else { 
             cat("\n")
             answered <- FALSE
           }
         }, 
         
         "3" = {
           yes <- askYesNo("Running a full simulation for stability selection & selection criteria with BSS (requires a Gurobi license and a powerful computer). Want to continue?")
           if (yes) { 
             run_selection_complete <- TRUE
             runBSS <- TRUE
             
             yes_parallel <- 
               askYesNo("Run in parallel?")
             if (yes_parallel) { 
               run_in_parallel <- TRUE 
             } else { 
               run_in_parallel <- FALSE 
             }
             
             run_selection_example <- FALSE
             answered <- TRUE
           } else { 
             cat("\n")
             answered <- FALSE
           }
         },
         
         "4" = { 
           cat("--> Will not run the (example) simulation\n")
           run_selection_example <- FALSE
           run_selection_complete <- FALSE
           runBSS <- FALSE
           run_in_parallel <- FALSE
           answered <- TRUE
         }, 
         
         "5" = stop_quietly(), 
         
         "0" = stop_quietly())
}

