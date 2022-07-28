library(batchtools)
library(dplyr)
library(readr)
library(simsham)
library(bestsubset)
library(glmnet)
library(caret)

options(batchtools.verbose = TRUE)
options(stringsAsFactors = FALSE)

### packages and files to load
packages = c("dplyr", "readr", "simsham", "bestsubset", "glmnet", "caret")
source = c("problems.R", "process-results.R", "algorithms.R", "parameter-settings.R")

### number of replications for each parameter setting
if (run_example) { 
  repls <- 1 
} else { 
  repls <- 100
}

### Setting up the repository 
start_from_scratch <- TRUE # if true, removes all repository and creates a new one

reg_name <- "bscomparison"
reg_dir <- sprintf("%s/registries/%s", getwd(), reg_name)

if (start_from_scratch) { 
  dir.create("registries", showWarnings = FALSE)
  unlink(reg_dir, recursive = TRUE)
  reg <- makeExperimentRegistry(file.dir = reg_dir, packages = packages, source = source)      
} else { 
  reg <- loadRegistry(file.dir = reg_dir, writeable = TRUE)
}

### add problems
addProblem(name = "sim_data", fun = simulator_wrapper, seed = 1) 

### add algorithms 
addAlgorithm(name = "fs", fun = fs_wrapper) 
addAlgorithm(name = "enet", fun = enet_wrapper) 
if (run_BSS) { 
  addAlgorithm(name = "bs", fun = bs_wrapper) 
}


### add the experiments

# parameters for the simulation
prob_design <- list(sim_data = sim_param)

# parameters for the methods
if (run_BSS) { 
  algo_design <- list(
    bs = expand.grid(k = 15),
    fs = expand.grid(k = 15), 
    enet = expand.grid(alpha = seq(.1, 1, by = .1))
  )
} else { 
  algo_design <- list(
    fs = expand.grid(k = 15), 
    enet = expand.grid(alpha = seq(.1, 1, by = .1))
  ) 
}

addExperiments(prob_design, algo_design, repls = repls)

### submit 
ids <- findNotStarted()

if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotStarted()
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              memory = 80000, walltime = 10*24*3600,
                              max.concurrent.jobs = 200))
} else {
  submitJobs(ids = ids)
}

waitForJobs()

### collect results 
res <- reduceResultsDataTable(fun = unwrap) 

### combine the results with the parameters for the job
pars = unwrap(getJobPars())
tab = ijoin(pars, res)

readr::write_rds(tab, "results/raw-results.rds", compress = "gz")

# post-process the results
source("exec/get-best-f1-scores.R")

# create the plots
source("create-plots.R")
