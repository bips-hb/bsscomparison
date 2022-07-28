
answered <- FALSE
while (!answered) { 
  
  if (synthetic_time) { 
    title = "Run the synthetic data study with different time limits for the BSS (Requires Gurobi and takes a long time)" 
  } else { 
    title = "Run the semi-synthetic data study with different time limits for the BSS (Requires Gurobi and takes a long time)" 
  }
  
  # ask whether to run the simulation with different time limits
  time_study <- menu(title = title, 
                         choices = c("yes", 
                                     "no", 
                                     "exit"))
  
  switch(as.character(time_study), 
         
         "1" = {
           cat("--> Running study with different time limits\n")
           run_time_study <- TRUE
           yes_parallel <- 
             askYesNo("Run in parallel?")
           
           if (yes_parallel) { 
             run_in_parallel <- TRUE 
           } else { 
             run_in_parallel <- FALSE 
           }
           answered <- TRUE
         }, 
         
         "2" = {
           cat("--> Not running study with different time limits\n")
           run_time_study <- FALSE
           answered <- TRUE
         }, 
         
         "3" = stop_quietly(), 
         
         "4" = stop_quietly())
}
