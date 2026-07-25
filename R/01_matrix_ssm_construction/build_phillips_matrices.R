

################################################################################
#
# Phillips Curve 2D Matrix Builders
#
################################################################################

#' Build the H Matrix (State Transition)
#' @description 2x2 Matrix mapping states: rho_t = [Trend_t, Cycle_t]'
build_H_philips <- function(model_params) {
  phi_val <- if(!is.null(model_params$phi)) as.numeric(model_params$phi) else 0.0
  
  # Row 1: Trend(t) = 1*Trend(t-1) + 0*Cycle(t-1)
  # Row 2: Cycle(t) = 0*Trend(t-1) + phi*Cycle(t-1)
  H <- matrix(c(1,       0,
                0, phi_val), nrow = 2, ncol = 2, byrow = TRUE)
  return(H)
}

#' Build the G Matrix (Factor Loadings)
#' @description 2x2 Matrix mapping states to observed [CPI_inflation, SPF_5y]'
build_G_philips <- function() {
  # Row 1 (Observed Inflation): Trend + Cycle
  # Row 2 (SPF Long-term Forecast): Trend ONLY
  G <- matrix(c(1, 1,
                1, 0), nrow = 2, ncol = 2, byrow = TRUE)
  return(G)
}

#' Build the M Matrix (Measurement Noise Scaling with Hardcoded Floor)
#' @description Pinning actual CPI measurement error to clear structural identification bounds.
build_M_philips <- function(model_params, Y_data) {
  ny <- ncol(Y_data)
  
  # Extract estimated SPF observation error scale
  sig_spf <- if(!is.null(model_params$sigma_spf)) as.numeric(model_params$sigma_spf) else 0.5
  sig_cpi <- if(!is.null(model_params$sigma_cpi)) as.numeric(model_params$sigma_cpi) else 0.5
  
  
  # Position 1: Observed inflation (Hardcoded baseline floor to isolate cycle process shocks)
  # Position 2: SPF Expectations (Estimated dynamically by the optimizer)
  sigmas <- c(sig_cpi, sig_spf)
  
  M <- diag(sigmas, nrow = ny, ncol = ny)
  colnames(M) <- rownames(M) <- c("sigma_cpi_fixed", "sigma_spf")
  return(M)
}

#' Build the N Matrix (Process Noise Scaling)
#' @description 2x2 Diagonal standard deviation matrix for shocks hitting Trend and Cycle states.
build_N_philips <- function(model_params, default_sig_xi = 0.01) {
  sig_trend <- if(!is.null(model_params$xi_trend)) as.numeric(model_params$xi_trend) else default_sig_xi
  sig_cycle <- if(!is.null(model_params$xi_cycle)) as.numeric(model_params$xi_cycle) else default_sig_xi
  
  N <- diag(c(sig_trend, sig_cycle), nrow = 2, ncol = 2)
  return(N)
}

#' Build the nu_t Matrix (Exogenous State Impact)
#' @description Direct structural demand and cost-push impacts shifting the cyclical state.
build_nu_t_philips <- function(model_params, X_data) {
  beta_y   <- as.numeric(model_params$beta_y)
  psi_lop  <- as.numeric(model_params$psi_lop)
  
  gdp_gap  <- as.numeric(X_data[, "gdp_gap"])
  lop_gap  <- as.numeric(X_data[, "lop_gap"])
  
  # Combined economic pressure shifting cyclical inflation
  cyc_drift <- (beta_y * gdp_gap) + (psi_lop * lop_gap)
  
  # Row t structural returns: [0 impact for Trend, cyc_drift impact for Cycle]
  return(cbind(rep(0, nrow(X_data)), cyc_drift))
}

#' Build the mu_t Matrix (Measurement Intercept)
#' @description Kept as an explicit 0-placeholder matrix to keep structural filter pipelines aligned.
build_mu_t_philips <- function(model_params, X_data) {
  return(matrix(0, nrow = nrow(X_data), ncol = 2))
}






#' Initialize the Phillips Curve State-Space Model Blueprint (Strict 2D Version)
#' @export
initialize_my_philips_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # 1. ENFORCE STRICT SELECTION: Validate all active structural coefficients
  required_params <- c("beta_y", "psi_lop", "phi", 
                       "xi_trend", "xi_cycle", "sigma_spf")
  
  missing_idx <- !(required_params %in% names(parameter_guesses))
  if (any(missing_idx)) {
    stop(paste("SSM PHILLIPS INIZIALIZATION ERROR: Missing parameters:", 
               paste(required_params[missing_idx], collapse = ", ")))
  }
  
  # 2. Build the structural optimization transformation constraints
  manifest <- list(
    # Exogenous Demand and Cost Shifters (Rule 0: Unconstrained Linear space)
    beta_y    = list(val = parameter_guesses$beta_y,  rule = 1),
    psi_lop   = list(val = parameter_guesses$psi_lop, rule = 0),
    
    # State Persistence Over Time (Rule 2: Logit transformation bounded safely between 0 and 1)
    phi       = list(val = parameter_guesses$phi,
                     rule = 3,       
                     low  = 0.01,   
                     high = 0.7),
    
    xi_trend           = list(val = parameter_guesses$xi_trend, 
                              rule = 3,       
                              low  = 0.01,   
                              high = 0.1
    ),
    xi_cycle           = list(val = parameter_guesses$xi_cycle, rule = 1),
    
    # Measurement Scale Variation (Rule 1: Exponentiated to map strictly > 0)
    sigma_spf = list(val = parameter_guesses$sigma_spf, rule = 3,
                     low = 0.001, # practically 0 -> avoid the =0 trap
                     high = 4),
    sigma_cpi = list(val = parameter_guesses$sigma_cpi, rule = 3,
                     low  = 0.1,   
                     high = 4)
  )
  
  # 3. Assemble complete structural framework capsule
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), 
      X = as.matrix(X_data)
    ),
    manifest = manifest,
    builders = list(
      mu_t   = build_mu_t_philips,
      nu_t   = build_nu_t_philips,
      H      = build_H_philips,
      G      = build_G_philips,
      M      = build_M_philips,
      N      = build_N_philips
    ),
    name = "philips",
    # Balanced initial guesses: Trend inflation assumes a ~2.0% structural target, cycle starts dead at 0%
    rho_guess   = c(2.0, 0.0), 
    # Clean 2x2 Identity mapping bounds for initial state spatial propagation
    sigma_guess = c(1, 0, 0, 1) 
  )
  
  return(ssm)
}
