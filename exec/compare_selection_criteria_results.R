#' Script for comparing the results of an example of stability selection and
#' different selection criteria
#' 
#' NOTE: BSS has been omitted due to the mandatory Gurobi Solver and its 
#' computational costs. Please run selectionCriteriaSimulation.R and
#' stabilitySelectionSimulation.R for BSS 

# load the example data of stability selection
test_data_ss <- 
  readRDS("./results/ExampleData_Results_No_BSS_Stability_Selection_block_high_adjacent.RDS")

# load the example data of stability selection
test_data_sc <- 
  readRDS("./results/ExampleData_Results_No_BSS_Selection_Criteria_block_high_adjacent.RDS")

# combine data
test_data <- 
  bind_rows(test_data_sc,
            test_data_ss)

# remove the single data sets
rm(test_data_ss)
rm(test_data_sc)

# determine if the test data is low or high-dimensional
N <- unique(test_data$n)
if(N == 100){
  Dim <- "high"
}else{
  Dim <- "low"
}

# determine correlation structure
Corr <- unique(test_data$corr_type)

# determine non-zero betas
Beta <- unique(test_data$beta_position)

# load the corresponding results of th original simulation for comparison:
# different selection criteria
val_data_sc <- 
  readRDS(
    paste(
      "./results/Results_Selection_Criteria_",
  Corr, "_", 
  Dim, "_",
  Beta, ".RDS",
  sep=""))

# stability selection
val_data_ss <- 
  readRDS(
    paste(
      "./results/Results_Stability_Selection_",
      Corr, "_", 
      Dim, "_",
      Beta, ".RDS",
      sep=""))

# combine
val_data <- 
  bind_rows(val_data_sc,
            val_data_ss)

rm(val_data_sc)
rm(val_data_ss)

# keep only settings and simulation runs which have been used in the example
val_data_reduced <-
  val_data %>% 
  filter(method %in% unique(test_data$method) & 
           snr %in% unique(test_data$snr) & 
           sim.n %in% unique(test_data$sim.n) & 
           rho %in% unique(test_data$rho))

rm(val_data)

# keep only variables of the original simulation results
test_data <- 
  test_data %>% 
  select(names(val_data_reduced))

# calculate the proportion of identical results
prop.identical <- sum(
  sapply(1:nrow(test_data),
         function(x){
           
           nrow(
             merge(
               test_data[1,c("method", "alpha", "RSS", "F1", "Precision", 
                             "Accuracy","snr","criterion" )],
               val_data_reduced[,c("method", "alpha", "RSS", "F1", "Precision", 
                                   "Accuracy", "snr", "criterion" )])
             ) == 1
           
         })
  )/nrow(test_data)


if (prop.identical == 1) { 
  cat("SUCCESS: Example datasets are equal to original simulation results\n") 
} else { 
  cat("FAILURE: Example datasets are not equal to original simulation results\n")
  cat(paste("Only", prop.identical*100, "% of results are identical \n", res))
}
cat("DONE comparison\n")
