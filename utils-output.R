#' utils-output.R
#' 
#' Loads the crayon package and introduces 
#' some basic functions for generating the 
#' intermediate output

# create functions to show whether something worked or failed: 
success <- function(text) { cat(sprintf("  %s %s", crayon::green("\u2713"), text)) }
failed <- function(text) { cat(sprintf("  %s %s", crayon::red("\u2717"), text)) }

#' check whether package crayon is installed and load it
packages <- "crayon"
source("exec/install.R")
library(crayon)

# Give warnings or notes 
note <- function(text) { 
  cat(crayon::magenta(sprintf(text))) 
}

# outputs the parameters 
output_parameter_settings <- function(title = "Parameter settings", ...) { 
  param <- list(...) 
  
  cat(sprintf("\n%s\n", title))
  #sapply(param, function(p)
  n_params <- length(param)
  for (i in 1:n_params) { 
    cat( c( crayon::blue(sprintf("  %s:\t ", names(param)[i])), param[[i]], sprintf("\n"))) 
  }
  cat("\n")
}


# function to exit the code without any error message etc. 
# used when the user opt to exit the program by themselves
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

