#' ask-semisynthetic-study.R
#' 
#' Ask the user whether he/she wants to rerun the 
#' semi-synthetic data simulations.  

# keeps track whether answered. Otherwise repeats the question
answered <- FALSE
while (!answered) { 
  
  # ask whether to run the semisynthetic simulations
  semisynthetic <- menu(title = "Run the semi-synthetic simulation study...", 
                        choices = c("example of low-dimensional setting without BSS application (recommended)", 
                                    "example of high-dimensional setting without BSS application (recommended)", 
                                    "complete low-dimensional setting including application of BSS (requires Gurobi)", 
                                    "complete high-dimensional setting including application of BSS (requires Gurobi)",
                                    "do not run the semi-synthetic simulation study",
                                    "exit"))
  
  switch(as.character(semisynthetic), 
         
         "1" = {
           cat("--> Running semi-synthetic low-dimensional example and without BSS\n")
           run_example <- TRUE
           run_semisynthetic_simulations_low_dimensional <- TRUE
           run_semisynthetic_simulations_high_dimensional <- FALSE
           run_BSS <- FALSE
           run_in_parallel <- FALSE
           answered <- TRUE
         }, 
         
         "2" = {
           yes <- askYesNo("Running high-dimensional settings is not recommended for a single machine. Want to continue?")
           if (yes) { 
             cat("--> running all settings without BSS\n")
             run_example <- TRUE
             run_semisynthetic_simulations_low_dimensional <- FALSE 
             run_semisynthetic_simulations_high_dimensional <- TRUE
             run_BSS <- FALSE
             run_in_parallel <- FALSE
             answered <- TRUE
           } else { 
             cat("\n")
             answered <- FALSE
           }
         }, 
         
         "3" = {
           yes <- askYesNo("Running high-dimensional settings with BSS requires a Gurobi license and a powerful computer. Want to continue?")
           if (yes) { 
             run_example <- TRUE
             run_semisynthetic_simulations_low_dimensional <- TRUE
             run_semisynthetic_simulations_high_dimensional <- FALSE
             run_BSS <- TRUE
             
             yes_parallel <- 
               askYesNo("Run in parallel?")
             
             if (yes_parallel) { 
                run_in_parallel <- TRUE 
             } else { 
                run_in_parallel <- FALSE 
             }
             answered <- TRUE
           } else { 
             cat("\n")
             answered <- FALSE
           }
         },
         
         "4" = { 
           yes <- askYesNo("Running high-dimensional settings with BSS requires a Gurobi license and a powerful computer. Want to continue?")
           
           if (yes) { 
            run_example <- TRUE
            run_semisynthetic_simulations_low_dimensional <- FALSE
            run_semisynthetic_simulations_high_dimensional <- TRUE
            run_BSS <- TRUE
           
            yes_parallel <- 
              askYesNo("Run in parallel?")
           
            if (yes_parallel) { 
              run_in_parallel <- TRUE 
            } else { 
              run_in_parallel <- FALSE 
            }
            answered <- TRUE
          } else { 
            cat("\n")
            answered <- FALSE
          }
         },
         
         "5" = { 
           cat("--> Will not run the semi-synthetic simulation study\n")
           run_example <- FALSE
           run_semisynthetic_simulations_low_dimensional <- FALSE
           run_semisynthetic_simulations_high_dimensional <- FALSE
           run_BSS <- FALSE
           run_in_parallel <- FALSE
           answered <- TRUE
         }, 
         
         "6" = stop_quietly(), 
  
         "0" = stop_quietly())
}

