#' Masterscript for the paper "When choosing the best subset is not the best 
#' choice" by Hanke, M., Dijkstra, L., Foraita, R. and Didelez, V. (2022)
#' 
#' masterscript.R replicates all the results and plots presented in our paper.
#' Some steps are programmed in different scripts to make it easier to read.
#' 
#' The R packages needed can be easily installed by running 'source(set-up.R)'
#' 
#' IMPORTANT: Best Subset Selection (BSS) is a NP-hard problem and requires
#' specialized software to solve, see Berstimas et al. (2016, DOI: 10.1214/15-AOS1388).
#' In order to solve the optimization problem, commercial solver 
#' software 'Gurobi 8.0' (or higher) is required. In addition, due to the 
#' high number of parameter settings, it is not recommended to run the simulations 
#' on a single machine. It took a high-performance cluster with 25 nodes, 
#' each with 12 core Intel Xeon processors, close to 50 days (!). 
#' 
#' In order to still make it possible to run the code without a Gurobi license, 
#' we stored the intermediate/raw results. We provide an example simulation script 
#' for the semi-synthetic dataset, in which the BSS is left out.
#' 
#' General structure of the directory
#' ----------------------------------
#' 
#' - all scripts for generating the results are in the subfolder "exec/"
#' - all plots are saved in the subfolder "plots/"
#' - The TCGA dataset is saved in the subfolder "data"
#' - Raw results of the low-dimensional settings are in the subfolder "results/"
#' - Raw results for different time limits are in the subfolder "results/"  
#' - all other raw results can be downloaded by running 
#'   source("download-intermediate-results-medium-high.R") or manually from
#'   https://www.bips-institut.de/fileadmin/downloads/BestSubsetResults.zip
#'   (The raw results are .rds files and need to be put into "results/" before
#'   analyzing/plotting)
#' 
#' Table of contents of masterscript.R
#' -----------------------------------
#' 
#' 0. Set-up 
#' 
#' I. Simulation study for semi-synthetic data 
#'    1. An reproducible example without BSS for low-dimensional setting
#'    2. Complete simulation including application of BSS 
#' 
#' II. Simulation study for synthetic data including application of BSS and its 
#'     competitors
#' 
#' III. Applying BSS with different time limits to different data scenarios
#'      1. Synthetic data setting
#'      2. Semi-synthetic data setting
#' 
#' IV. Generate plots for performance of methods
#'     1. Synthetic data setting
#'     2. Semi-synthetic data setting
#'     
#' V. Generate plots for synthetic and a semi-synthetic data for different time
#'     limits when applying the Gurobi solver
#'     
#' VI.: Simulation for Selection Criteria
#' 
#' VII.: Plots for Selection Criteria
#'
#' #############################################################################
#'                              0. SET-UP
#' #############################################################################

# load some functions for generating output
cat("Loading package crayon...\n")
source("utils-output.R")
cat("DONE loading package crayon\n")

# initialize the global variables
variables <- c("run_semisynthetic_simulations", 
               "run_semisynthetic_simulations_high_dimensional", 
               "run_example",
               "run_in_parallel",
               "generate_medium_high_dimensional_plots")

for (variable in variables) {
  eval( sprintf("%s <- FALSE", variable)  )
}

#' #############################################################################
#'                 I. SEMI-SYNTHETIC SIMULATION STUDY 
#' #############################################################################
#' 
#' The following code simulates semi-synthetic data with previous parameter
#' settings for a low-dimensional scenario and applies Lasso, Forward Stepwise 
#' Selection and Enet. The user is ask if BSS should be applied, too, which 
#' needs Gurobi 8.1 or higher. Using alternative methods/packages is not 
#' feasible even in our low-dimensional setting. 
#' The simulation without BSS takes ~30 minutes on a MacBook Pro (Intel i5) and 
#' replicates the reported results for all methods despite BSS. At the end of 
#' this chapter we compare the results of this example with the original results.

source("exec/ask-semisynthetic-study.R")

# make sure that the required packages for the user's selection are there
cat("Setting up...\n")
source("set-up.R")
cat("DONE Setting up\n\n")

if (run_semisynthetic_simulations_low_dimensional || run_semisynthetic_simulations_high_dimensional) { 
  
  # Parameters for the semi-synthetic simulation
  if (run_semisynthetic_simulations_low_dimensional) { 
    N <- c(594)  # number of observations 
    #' number of randomly drawn variables of the TCGA dataset; we used in our 
    #' simulation seeds by applying set.seed() before drawing the subdata
    subdata_size <- 100
  }
  
  if (run_semisynthetic_simulations_high_dimensional) { 
    N <- c(100)  # number of observations 
    subdata_size <- 1000
  }
  
  s <- 10                    # number of true direct predictors
  SNR <- c(0.42, 1.22, 3.52) # signal-to-noise ratios
  Sim_n <- 100               # number of simulation runs per SNR value 
  nLambda <- 1000            # number of Lambdas for Elastic net/LASSO
  max.k <- 15                # maximal subset size for FSS (and BSS if run_example = FALSE)
  # Alpha values for Elastic net; Alpha = 1 is the Lasso
  Alpha <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  
  output_parameter_settings(title = "Parameters for semi-synthetic simulation:", 
                            N = N, s = s, SNR = SNR, Sim_n = Sim_n, nLambda = nLambda, 
                            max.k = max.k, Alpha = Alpha, subdata_size = subdata_size)
  
  # run the simulation
  if (run_example) { 
    cat(sprintf("Running semi-synthetic simulations (without BSS)...\n"))
    source("exec/semisynthetic-example.R")  # an example without BSS
  } else {  
    cat(sprintf("Running semi-synthetic simulations...\n"))
    source("exec/semisynthetic-full.R")     # full analysis with BSS 
  }

  # if the example was used, we here compare the results of the example 
  # with the full analysis. NOTE: the parameter settings set earlier must be 
  # the default 
  if (run_example) { 
    cat("\nRunning comparison between data simulated now and data used in the paper using dplyr::all_equal\n")
    # get the results from the paper
    semisynthetic_full <- readRDS("results/raw_results_SemiSyntheticSimulation_100_594.RDS")
  
    # filtered for SNR levels of the previous example simulation; BSS results are removed
    semisynthetic_full <- semisynthetic_full %>% 
      filter(snr %in% SNR & method != "BSS") %>%
      select(method, alpha, k, TP, FP, FN, F1, Precision, Accuracy)
  
    # get the results of the example run
    semisynthetic_example <- results_Example_SemiSynthetic %>% 
      filter(snr %in% SNR & method != "BSS") %>%
      select(method, alpha, k, TP, FP, FN, F1, Precision, Accuracy)
  
    #' all_equal() of the dplyr package is needed to compare two data frames
    res <- (dplyr::all_equal(semisynthetic_full, semisynthetic_example))
    if (is.logical(res) && res) { 
      success("datasets are equal\n") 
    } else { 
      failed("datasets are not equal\n")
      cat(sprintf("Output dplyr::all_equal --> %s\n", res))
    }
    cat("DONE comparison\n")
  }
  
  cat(sprintf("DONE running semi-synthetic simulations...\n\n"))
}


#' #############################################################################
#'      II. Simulation study for synthetic data including application of 
#'                         BSS and its competitors         
#' #############################################################################

#'             !!! Not recommended to run on a single machine !!!
#'                  !!! Gurobi 8.0 or higher is needed !!!!

source("exec/ask-synthetic-study.R")

if (run_synthetic_study) { 
  # store the current working directory 
  current_wd <- getwd() 
  
  cat(sprintf("temporarily shifts working directory to folder 'bscomparison/'...\n"))
  setwd(paste(current_wd, "/bscomparison", sep = ""))
  
  cat(sprintf("Starts synthetic simulation study...\n"))
  source("run.R") 
  cat(sprintf("DONE synthetic simulation study...\n"))
  cat(sprintf("Shifts back to original working directory\n"))
  setwd(current_wd)
  cat("Result of the synthetic simulation study can be found in the folder 'bscomparison/'\n")
}


#' #############################################################################
#'        III. Applying BSS with different time limits to different 
#'                            data scenarios                     
#' #############################################################################

#'             !!! Not recommended to run on a single machine !!!
#'                  !!! Gurobi 8.0 or higher is needed !!!!

#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'                        1. Synthetic data setting
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

run_BSS <- TRUE

synthetic_time <- TRUE
source("exec/ask-time-study.R")

if (run_time_study) { 
  source("set-up.R")
  
  ### low dimensional block setting with correlation 0.7 and signal-to-noise ratio
  ### 0.42 was used for synthetic data
  
  # load function to generate a block correlation structure
  source("exec/function-block-builder.R")
  
  # parameter setting
  P <- 1000      # number of variables
  N <- 100       # number of observations
  s <- 10        # number of true predictors
  SNR <- c(0.42) # signal-to-noise ratio(s)
  RHO <- 0.7     # correlation
  Sim_n <- 100   # number of simulations
  max.k <- 15    # maximal size of subset size
  # different time limits for the Gurobi solver in seconds
  Time.limits <- c(10, 60, 180, 600, 3600)
  
  if (run_in_parallel) { 
    # number of workers 
    mc <- 105 # Optional: number of cores can be determined automatically
    # make a cluster - default is "MPI"
    cl <- makeCluster(mc, type="MPI")
  }
  
  source("exec/time-limits-BSS-synthetic.R")
}

#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'                    2. Semi-synthetic data setting
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' low dimensional semi-synthtic data setting with signal-to-noise ratio 1.22

run_BSS <- TRUE
synthetic_time <- FALSE

source("exec/ask-time-study.R")
if (run_time_study) { 
  ### low dimensional block setting with correlation 0.7 and signal-to-noise ratio
  ### 0.42 was used for synthetic data
  
  # load function to generate a block correlation structure
  source("exec/function-block-builder.R")
  
  # parameter setting
  subdata_size <- 1000 # number of variables
  N <- 594             # number of observations
  s <- 10              # number of true predictors
  SNR <- c(1.22)       # signal-to-noise ratio(s)
  RHO <- 0.7           # correlation
  Sim_n <- 100         # number of simulations
  max.k <- 15          # maximal size of subset size
  # different time limits for the Gurobi solver in seconds
  Time.limits <- c(10, 60, 180, 600, 3600)
  
  if (run_in_parallel) { 
    # number of workers 
    mc <- 105 # Optional: number of cores can be determined automatically
    # make a cluster - default is "MPI"
    cl <- makeCluster(mc, type="MPI")
  }
  
  source("exec/time-limits-BSS-semisynthetic.R")
}

#' #############################################################################
#' #############################################################################
#'                              IV. Generate plots 
#' #############################################################################
#' #############################################################################

cat(sprintf("Generating plots..."))

#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'                        1. Synthetic data setting
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Select the coorelation structure ("block", "toeplitz" or "independent")
CORR <- c("block","toeplitz", "independent")

#' Select position of non-zero betas ("spread" = equally distributed; "first" =
#' adjacent) 
BETA <- c("spread", "first")

#' Select dimension of problem ("low" means p=100 & n=1000; "medium" means p=500
#' & n=500; "high" means p=1000 & n=100). For this example we use only "low"
#' data sets of the medium high dimenstional problems are rather big
#' Next, you ill be asked if you want to download the raw results for medium and
#' hig dimensional settings. Alternatively you can download the results manually
#' from
#' https://www.bips-institut.de/fileadmin/downloads/BestSubsetResults.zip

source("exec/ask-download.R")

if (create_medium_high_dimensional_plots) { 
  DIM <- c("low", "medium", "high")
} else { # default
  DIM <- c("low")
}

#' Different SNR values can be chosen for the Performance of BSS based on
#' different subset sizes k:
SNR_BSS_k <- c(0.05, 0.09, 0.14, 0.25, 0.42, 0.71, 1.22, 2.07, 3.52, 6)

#' start script for plotting best possible F1-scores and BSS results based on 
#' subset size k for synthetic data
source("exec/generate-plots-synthetic.R")
#' plots are saved in subfolder ./plots


#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'                        2. Semi-synthetic data setting
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' start script for plotting best possible F1-scores and BSS results based on 
#' subset size k for semi-synthetic data
source("exec/generate-plots-semisynthetic.R")
#' plots are saved in subfolder ./plots


#' #############################################################################
#' #############################################################################
#'    V.: Generate plots for synthetic and a semi-synthetic data for             
#'          different time limits when applying the Gurobi solver
#' #############################################################################
#' #############################################################################

#' start script to plot BSS performance for different time limits and different
#' subset sizes
source("exec/generate-plots-BSS-time-limits.R")

cat(sprintf("DONE generating plots..."))



cat(sprintf("\n \n Star Section for Selection Criteria and Stability Selectio"))

cat(sprintf("\nDefault is not to run the simulation but generate the Figure 9"))
cat(sprintf("i.e. a high-dimensional block setting with rho=0.7"))
#' #############################################################################
#' #############################################################################
#'                      VI.: Simulation for Selection Criteria
#' #############################################################################
#' #############################################################################

#' This part will run the simulation for different selection criteria (BIC, 
#' mBIC, HQC and stability selection). 
#' It is important to have Gurobi installed to run BSS but you can can the 
#' without BSS by setting runBSS <- FALSE. 
#' NOTE: even without BSS this simulation runs rather long due to the many 
#' different parameter combinations and subsampling process of Stability 
#' Selection. We suggest to use a multicore processor or HPC to apply parallel
#' computing or reduce the number of simualtion runs and/or parameter 
#' combinations. Alternatively, by setting runCriteriaSimu <- FALSE you can omit 
#' thesimulation and only generate the plots using our raw data under ./results
#' by running the script of next section (VII)

#' Run simulation?
runCriteriaSimu <- FALSE

#' run BSS?
runBSS <- FALSE

#' ---- Settings for simulation ----

# set the number of observations (N), variables (P) and non-zero coefficients (s)
N <- 100
P <- 1000
s <- 10

# set the time limit in sec for the Gurobi solver (Important: this time limit is
# also used in every subsample of Stability Selection!)
gurobiTime <- 180

# set corraltion structure ("block" and "toeplitz" are available) 
CORR_TYPE <- "block"

# set the signal to noise ratios
SNR <- c(0.05, 0.25, 0.42, 1.22, 2.07, 6)

# set the correlation between variables
RHO <- c(0.7)

# set the position of the non-zeros ("adjacent" or "spread")
BETA_POSITION <- "adjacent"

# number of simulation runs
Sim_n <- 100

# number of maximum subset size for Best Subset Selection and Forward Stepwise
# Selection
max.k <- 15

# alpha Values for Enet, i.e. weighting of the ridge penalty part; 1 is Lasso
Alpha <- seq(0.1,1,0.1)

# Type of cluster ("PSOCK"/"SOCK" and "MPI" available); if you are NOT working
# on a HPC with (open)MPI we suggest to use "SOCK"
clusterType <- "MPI"

# number of workers for parallel computation (should not exceed the number of 
# (physical) cores)
mc <- 100 

if(runCriteriaSimu == TRUE){
  #' run stability selection
  source("exec/stabilitySelectionSimulation.R")
  #' run BIC, mBIC2 and HQC
  source("exec/selectionCriteriaSimulation.R")
}


#' #############################################################################
#' #############################################################################
#'                     VII.: Plots for Selection Criteria
#' #############################################################################
#' #############################################################################

#' Script for generating the plots of the selection criteria based on the 
#' simulation results of the previous section or on the raw data from ./results
#' Plots are saved in subfolder ./plots

#' Settings to plot. The default is for the plot of Figure 9 of our paper

methods <- c("Enet 0.1", "Enet 0.5", "Enet 0.9", "Lasso", "FSS", "BSS")
corr_struc <- "block"
Dim <- "high"
beta_position <- "adjacent"
rho <- 0.7


source("exec/generate-plots-selectionCriteria.R")

cat(sprintf("\n \nEnd of masterscript.R"))