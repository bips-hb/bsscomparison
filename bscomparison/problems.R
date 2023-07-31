#' A wrapper function for simulating the data
#' 
#' INPUT: 
#' n: number of observations
#' p: number 
#' s: number of non-zeros
#' dimensiontality: chr ("low", "medium" or "high")
#' corr_type: correlation structure (""independent", "toeplitz" or "block"")
#' beta_type: chr ("first" or "spread")
#' snr: signal to noise ratio (numeric: >0)
#' 
#' OUTPUT:
#' list of simualted data, i.e. X (covariates with multivariate normal 
#' distribution) and y (response with normal distribution) 


simulator_wrapper <-
  function(n, p, s, dimensionality, corr_type, rho, beta_type, snr, ...) {

    corrmat <- switch(
      corr_type,
      "independent" = simsham::corrmat_identity(p),
      "toeplitz" = simsham::corrmat_toeplitz(p, rho),
      "block" = simsham::corrmat_block(p, rho, s)
    )
    
    beta <- switch(
      beta_type,
      "first" = simsham::beta_first_s_covariates(p, s),
      "spread" = simsham::beta_spread_covariates(p, s, indices = seq(1, (s *
                                                                           s), by = s))
    )
    
    data <-
      simsham::simsham(n,
                       p,
                       s,
                       corrmat = corrmat,
                       beta = beta,
                       snr = snr)
  }