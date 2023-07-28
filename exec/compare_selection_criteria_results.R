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

test_data <- 
  bind_rows(test_data_sc,
            test_data_ss)

rm(test_data_ss)
rm(test_data_sc)


N <- unique(test_data$n)
if(N == 100){
  Dim <- "high"
}else{
  Dim <- "low"
}

Corr <- unique(test_data$corr_type)

Beta <- unique(test_data$beta_position)

val_data_sc <- 
  readRDS(
    paste(
      "./results/Results_Selection_Criteria_",
  Corr, "_", 
  Dim, "_",
  Beta, ".RDS",
  sep=""))

val_data_ss <- 
  readRDS(
    paste(
      "./results/Results_Stability_Selection_",
      Corr, "_", 
      Dim, "_",
      Beta, ".RDS",
      sep=""))

val_data <- 
  bind_rows(val_data_sc,
            val_data_ss)

rm(val_data_sc)
rm(val_data_ss)

val_data_reduced <-
  val_data %>% 
  filter(method %in% unique(test_data$method) & 
           snr %in% unique(test_data$snr) & 
           sim.n %in% unique(test_data$sim.n) & 
           rho %in% unique(test_data$rho))

rm(val_data)

test_data <- 
  test_data %>% 
  select(names(val_data_reduced))

names(val_data_reduced) == names(test_data) 


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
