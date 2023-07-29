#' set-up.R
#'
#' Performs the set up for the masterscript. 
#' It checks whether the working directory is 
#' correct and all the required packages are 
#' installed (see also install.R)
#'   
#' In the end, all the required libraries 
#' are loaded.  

#' running this script requires the specification of a number 
#' of global variables, which are set in the beginning 
#' of the masterscript.R. Here we check whether they were 
#' indeed set before

# check whether working directory is the folder of the masterscript.R
if (basename(getwd()) != "bsscomparison") {  
  stop(sprintf("Your current working directory %s is wrong. You should start the script while being in the 'bsscomparison' folder.", getwd()))
}
cat("Working directory is correct", green("\u2713\n"))

# check whether the required packages are installed. 
# If some packages are not installed, the user is 
# asks if he/she wants to install them. 

packages <- c("ggpubr", "glmnet", "tibble", "tidyverse", 
              "devtools", "batchtools", "bestsubset", "readr",  
              "simsham", "fitsham", "progress", "caret", "Matrix",
              "reshape2", "latex2exp", "purrrlyr", "mvtnorm", "lars") 

# in case one does not want to run the example, the gurobi
# package is needed
if (run_BSS) { 
  packages <- c(packages, "gurobi")  # Installing gurobi is not straight forward
}

# packages for parallel computing are needed
if (run_in_parallel) { 
  packages <- c(packages, c("parallel", "snow", "Rmpi"))
}

cat(sprintf("Packages installed...\n"))
source("exec/install.R")

# after checking that all the required packages are installed, load 
# the packages
for (package in packages) { 
  library(package, character.only = TRUE) 
}
