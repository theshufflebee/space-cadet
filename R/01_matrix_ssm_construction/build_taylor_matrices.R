
################################################################################
#
# Taylor Rule / Policy Rate Model Forecast
#
################################################################################


# ==============================================================================
# --- Matrices ---
# ==============================================================================

#' Build the H Matrix (State Transition)
#' @param model_params A named list containing "rho_tp" for the cyclical term premium.
#' @return A 3 x 3 matrix defining the transition dynamics.
build_H_taylor <- function(model_params) {
  # rho_tp determines the persistence of the cyclical term premium component
  rho_tp <- if(!is.null(model_params$rho_tp)) as.numeric(model_params$rho_tp) else 0.9
  
  H <- diag(3)      # Natural rate random walk // Trend term premium random walk [cite: 146, 159]
  H[3,3] <- rho_tp  # Cyclical term premium AR(1) [cite: 159]
  
  return(H)
}

#' Build the G Matrix (Factor Loadings)
#' @param model_params A named list containing the smoothing parameter "phi".
#' @return A 2 x 3 matrix of factor loadings.
build_G_taylor <- function(model_params) {
  # phi is the interest rate smoothing parameter
  phi_val <- if(!is.null(model_params$phi)) as.numeric(model_params$phi) else 0.8
  
  G <- matrix(0, 2, 3)
  
  # Row 1: Policy Rate loads on natural rate i_t [cite: 134, 138]
  G[1,1] <- (1 - phi_val)
  
  # Row 2: Forward Rate loads on i_t + trend TP + cyclical TP [cite: 149, 158]
  G[2,1] <- 1
  G[2,2] <- 1
  G[2,3] <- 1
  
  return(G)
}

#' Build the M Matrix (Measurement Noise Standard Deviations)
#' @param model_params A named list containing "sigma_policy" and optionally "sigma_fwd".
#' @return A 2 x 2 diagonal matrix of standard deviations.
build_M_taylor <- function(model_params, Y_data) {
  
  ny <- ncol(Y_data)
  
  s_policy <- as.numeric(model_params$sigma_policy)
  
  if ("sigma_fwd" %in% names(model_params) && !is.null(model_params$sigma_fwd)) {
    s_fwd <- as.numeric(model_params$sigma_fwd)
  } else {
    s_fwd <- 1e-4  # Small floor to preserve matrix inversion stability
  }
  
  M <- matrix(0, ny, ny)
  M[1, 1] <- s_policy  # Standard deviation of Taylor Rule residual [cite: 134]
  M[2, 2] <- s_fwd     # Standard deviation of Forward Rate measurement
  
  return(M)
}

#' Build the N Matrix (Process Noise Standard Deviations)
build_N_taylor <- function(model_params, default_sig_trend = 0.1, default_sig_cycle = 0.5) {
  xi_i        <- if(!is.null(model_params$xi_i)) as.numeric(model_params$xi_i) else default_sig_trend
  xi_tp_trend <- if(!is.null(model_params$xi_tp_bar)) as.numeric(model_params$xi_tp_bar) else default_sig_trend
  xi_tp_cycl  <- if(!is.null(model_params$xi_tp_cycl)) as.numeric(model_params$xi_tp_cycl) else default_sig_cycle
  
  N <- diag(c(xi_i, xi_tp_trend, xi_tp_cycl), 3, 3)
  return(N)
}

#' Build the External Data Vector (mu_t)
#' @description Computes the purely exogenous parts of the Taylor rule. 
build_mu_t_taylor <- function(model_params, X_data) {
  gamma_pi <- as.numeric(model_params$gamma_pi)
  gamma_y  <- as.numeric(model_params$gamma_y)
  phi      <- as.numeric(model_params$phi)
  
  gdp_g <- as.numeric(X_data[, "gdp_gap"])
  inf_g <- as.numeric(X_data[, "inf_gap"])
  
  # Pure Taylor Rule Exogenous Component [cite: 134, 138]
  ex_comp <- ((1 - phi) * (gamma_y * gdp_g + gamma_pi * inf_g))
  
  # Row 1: Policy Rate Intercept, Row 2: Forward Rate Intercept (0)
  return(cbind(ex_comp, rep(0, nrow(X_data))))
}

#' Build the Autoregressive Measurement Matrix (ar_mat)
#' @description Constructs a 2x2 matrix that scales lagged observables.
build_AR_matrix <- function(model_params, Y_data) {
  phi <- as.numeric(model_params$phi)
  
  # Must be 2x2 to match the dimension of Y_t (Policy Rate, Forward Rate) 
  ar_mat <- matrix(0, nrow = 2, ncol = 2)
  ar_mat[1, 1] <- phi  # Map smoothing parameter exclusively to the policy rate [cite: 138]
  
  return(ar_mat)
}

#' Build the Nu_t Matrix (Exogenous State Drift)
#' @description Returns a matrix of zeros matching the dataset length, 
#' as the structural state transitions follow pure un-drifted stochastic processes.
#' @param model_params A named list of economic parameters (unused).
#' @param X_data The exogenous data matrix used to extract timeline length.
#' @return A T x 3 matrix filled entirely with zeros.
build_nu_t_taylor <- function(model_params, X_data) {
  
  T_len <- nrow(X_data)
  
  # Structural state dimension is exactly 3 (i_bar, TP_bar, TP_tilde)
  nu_mat <- matrix(0, nrow = T_len, ncol = 3)
  
  return(nu_mat)
}



#' Initialize Taylor Rule State-Space Model
#'
#' @description
#' Bundles data, parameters, and builders for Model 3 as defined in 0_technical_note.pdf[cite: 130].
#' Variables include the natural rate (i_bar), term premium trend (TP_bar), 
#' and term premium cycle (TP_tilde).
initialize_taylor_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # 1. Define Required Parameters for Taylor Rule and Term Premium [cite: 134, 138, 159]
  required_params <- c("gamma_pi", "gamma_y", "phi", "rho_tp", 
                       "sigma_policy", "xi_i", "xi_tp_bar", "xi_tp_cycl")
  
  if (!all(required_params %in% names(parameter_guesses))) {
    stop("SSM TAYLOR INIZIALIZATION ERROR: Missing parameters:")
  }
  
  # 2. Build the Manifest
  # Rule 1 = Exponential (>0), Rule 3 = Bounded Logistic (low to high)
  manifest <- list(
    gamma_pi     = list(val = parameter_guesses$gamma_pi, rule = 0),
    gamma_y      = list(val = parameter_guesses$gamma_y,  rule = 0),
    
    # Smoothing bounded strictly below 1 for dynamic stability
    phi          = list(val  = parameter_guesses$phi, 
                        rule = 3,
                        low = 0.6,
                        high = 0.99
    ),
    rho_tp       = list(val = parameter_guesses$rho_tp,  rule = 3, low = 0.01, high = 0.99),
    
    # Measurement Noise
    sigma_policy = list(val = parameter_guesses$sigma_policy, rule = 1),
    
    # State Innovation Noise 
    xi_i         = list(val = parameter_guesses$xi_i,         rule = 3,
                        low = 0.01,
                        high = 0.2),
    xi_tp_bar    = list(val = parameter_guesses$xi_tp_bar,    rule = 3,
                        low = 0.01,
                        high = 0.5),
    xi_tp_cycl   = list(val = parameter_guesses$xi_tp_cycl,   rule = 3,
                        low = 0.001,
                        high = 0.5
                        )
  )
  
  # Handle Optional SPF Anchors [cite: 165]
  if ("sigma_spf3m" %in% names(parameter_guesses)) {
    manifest$sigma_spf3m <- list(val = parameter_guesses$sigma_spf3m, rule = 1)
  }
  if ("sigma_spf12m" %in% names(parameter_guesses)) {
    manifest$sigma_spf12m <- list(val = parameter_guesses$sigma_spf12m, rule = 1)
  }
  
  # 3. Bundle Blueprint
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), # Column 1: Policy Rate, Column 2: Forward Rate 
      X = as.matrix(X_data)  # Column 1: inf_gap, Column 2: gdp_gap 
    ),
    manifest = manifest,
    builders = list(
      mu_t   = build_mu_t_taylor, 
      nu_t   = build_nu_t_taylor,  # Added to prevent NULL errors in log-likelihood loops
      H      = build_H_taylor,   
      G      = build_G_taylor,   
      M      = build_M_taylor,  
      N      = build_N_taylor,
      ar_mat = build_AR_matrix
    ),
    name = "taylor",
    rho_guess = parameter_guesses$state_init,
    sigma_guess = parameter_guesses$sigma_init
  )
  
  return(ssm)
}