# in case one wants to run only a few simulations
parameter_setting_for_testing <- FALSE

if (parameter_setting_for_testing) { 
  
  dimension_param <- dplyr::tibble(
    n = c(100),
    p = c(100),
    s = c(10),
    dimensionality = c("low")
  )
  
  SNR <- c(.05)
  
  sim_param <- dplyr::as_tibble(
    expand.grid(
      corr_type = c("block"),
      rho = c(.7),
      beta_type = c("spread"),
      snr = SNR
    )
  )
  
} else { 
  
  dimension_param <- dplyr::tibble(
    n = c(1000, 500, 100),
    p = c(100, 500, 1000),
    s = c(10, 10, 10),
    dimensionality = c("low", "medium", "high")
  )
  
  
  # signal to noise
  SNR <- c(.05, .09, .14, .25, .42, .71, 1.22, 2.07, 3.52, 6.00)
  
  sim_param <- dplyr::as_tibble(
    expand.grid(
      corr_type = c("toeplitz", "block"),
      rho = c(.35, .7),
      beta_type = c("first", "spread"),
      snr = SNR
    )
  )
  
}

# all parameters for the independent case
temp_param <- dplyr::as_tibble(
  expand.grid(
    corr_type = c("independent"),
    rho = c(0),
    beta_type = c("first"),
    snr = SNR
  )
)

# combine the two
sim_param <- dplyr::bind_rows(sim_param, temp_param)

# combine it with the dimension parameters 
sim_param <- merge(dimension_param, sim_param)


