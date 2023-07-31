#' ask-synthetic-study.R
#' 
#' Ask the user whether he/she wants to rerun the 
#' synthetic data simulations.  

# keeps track whether answered. Otherwise repeats the question 
answered <- FALSE
while (!answered) { 
  
  # ask whether to run the semisynthetic simulations
  synthetic <- menu(title = "Run the synthetic simulation study?", 
                        choices = c("Yes (Gurobi and a HPC is mandatory).", 
                                    "No, do not run the synthetic simulation study",
                                    "Exit"))
  
  switch(as.character(synthetic), 
         
         "1" = { 
           yes <- askYesNo("Running full study. This requires a Gurobi license and a powerful computer. Want to continue?")
           
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
         
         "2" = { 
           cat("--> Will not run the synthetic simulation study\n")
           run_synthetic_study <- FALSE
           run_example <- FALSE
           run_BSS <- FALSE
           run_in_parallel <- FALSE
           answered <- TRUE
         }, 
         
         "3" = stop_quietly(), 
         
         "0" = stop_quietly())
}

