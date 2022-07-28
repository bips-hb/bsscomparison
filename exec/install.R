#' install.R
#' 
#' Script that checks whether the required packages 
#' are installed. If not, the user is asked whether 
#' he/she wants to install the missing package or not. 
#' If he/she declines, the script halts.
#' 
#' Packages from GitHub: 
#'
#' 'bestsubset' package of Tibshirani et al. (2021)
#' Hash of last commit: b52d3ce511f642494f74bf30a18e3812d90d0b3b
#' 
#' 'simsham': package of our simualtion setup to generate 
#' the synthetic datasets 
#' Hash of last commit: 74c48e5d53c08d2fe1421d7db553d02fe1ec1299
#' 
#' 'fitsham': package to apply Lasso, Enet, BSS and FSS to the synthetic datasets
#' Hash of last commit: 745cb63ce1ae372a23737efbaaaa7b7326b1138c

# go over each package individually 
for (package in packages) { 
  
  # check whether the package is installed
  path <- system.file(package = package)
  
  # if it is not installed...
  if (path == "") {
    
    if (interactive()) { 
      yes <- askYesNo(sprintf("Package %s needed to run masterscript.R. Do you want to install the package?", package))
    } else { 
      yes <- TRUE 
    }
    
    # hack needed to make sure the user can fill in the input
    #if (FALSE) { break } 
    
    if (yes) { 
      # install the package
      switch(package, 
             "bestsubset" = {  
                note("Installing bestsubset. This can take a while!\n")
                devtools::install_github(repo = "ryantibs/best-subset", subdir="bestsubset", force = TRUE)
               }, 
             "simsham"    = devtools::install_github(repo = "https://github.com/bips-hb/simsham", force = TRUE), 
             "fitsham"    = devtools::install_github(repo = "https://github.com/bips-hb/fitsham", force = TRUE), 
             install.packages(package)
      )
    } else { # if the package is not there and the user does not want to install it: 
      stop(sprintf("Running masterscript.R requires package %s", package)) 
    }
  } else { 
    success(sprintf("%s is already installed\n", package)) 
  }
}