simulator_wrapper <-
  function(data, job, n, p, s, dimensionality, corr_type, rho, beta_type, snr, ...) {

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