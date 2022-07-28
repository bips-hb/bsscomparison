#' ask-synthetic-study.R
#' 
#' Ask the user whether he/she wants to rerun the 
#' synthetic data simulations.  

# keeps track whether answered. Otherwise repeats the question
answered <- FALSE
while (!answered) { 
  
  # ask whether to run the semisynthetic simulations
  synthetic <- menu(title = "Run the synthetic simulation study...", 
                        choices = c("run example (1 repetition) without BSS (recommended)", 
                                    "run example (1 repetition) with BSS (requires Gurobi)", 
                                    "run full study without BSS", 
                                    "run full study with BSS", 
                                    "do not run the synthetic simulation study",
                                    "exit"))
  
  switch(as.character(synthetic), 
         
         "1" = {
           cat("--> Running example without BSS\n")
           run_synthetic_study <- TRUE
           run_example <- TRUE
           run_BSS <- FALSE
           answered <- TRUE
         }, 
         
         "2" = {
           cat("--> Running example with BSS\n")
           run_synthetic_study <- TRUE
           run_example <- TRUE
           run_BSS <- TRUE
           answered <- TRUE
         }, 
         
         "3" = {
           cat("Running full study without BSS\n")
           run_synthetic_study <- TRUE
           run_example <- FALSE
           run_BSS <- TRUE
           answered <- TRUE
         },
         
         "4" = { 
           yes <- askYesNo("Running full study with BSS. This requires a Gurobi license and a powerful computer. Want to continue?")
           
           if (yes) { 
             run_synthetic_study <- TRUE
             run_example <- FALSE
             run_BSS <- TRUE
             run_in_parallel <- TRUE
             answered <- TRUE
           } else { 
             cat("\n")
             answered <- FALSE
           }
         },
         
         "5" = { 
           cat("--> Will not run the synthetic simulation study\n")
           run_synthetic_study <- FALSE
           run_example <- FALSE
           run_BSS <- FALSE
           run_in_parallel <- FALSE
           answered <- TRUE
         }, 
         
         "6" = stop_quietly(), 
         
         "0" = stop_quietly())
}

