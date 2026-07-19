
#' Build the H Matrix (State Transition)
#'
#' @description
#' Defines the 2*2 transition matrix H mapping the 2D state vector 
#' rho_t = [u*_t, u^c_t]'. The trend follows a random walk, and the 
#' cycle follows an AR(1) process governed by phi.
build_H <- function(model_params) {
  phi_val <- if(!is.null(model_params$phi)) as.numeric(model_params$phi) else 0.0
  
  # Row 1: Trend(t) = 1*Trend(t-1) + 0*Cycle(t-1)
  # Row 2: Cycle(t) = 0*Trend(t-1) + phi*Cycle(t-1)
  H <- matrix(c(1,       0,
                0, phi_val), nrow = 2, ncol = 2, byrow = TRUE)
  return(H)
}


#' Build the G Matrix (Factor Loadings)
#'
#' @description
#' Constructs the 2*2 mapping matrix G that relates the 2D state vector
#' rho_t = [u*_t, u^c_t]' to the observed measurements y_t = [u_t, SPF_t]'.
build_G <- function() {
  # Row 1 (Observed Unemployment): Loads 1-to-1 on Trend AND Cycle
  # Row 2 (SPF 5Y Forecast): Loads 1-to-1 ONLY on Trend
  G <- matrix(c(1, 1,
                1, 0), nrow = 2, ncol = 2, byrow = TRUE)
  return(G)
}


build_M <- function(model_params, Y_data) {
  if (is.null(model_params$sigma_spf_5y_unemp) ||is.null(model_params$sigma_unemp)) {
    warning("Missing measurement parameters in model_params!")
  }
  
  M <- diag(c(as.numeric(model_params$sigma_unemp), as.numeric(model_params$sigma_spf_5y_unemp)), 
            nrow = ncol(Y_data), ncol = ncol(Y_data))
  return(M)
}


#' Build the N Matrix (Process Noise Scaling)
#'
#' @description
#' Constructs the 2*2 diagonal standard deviation matrix N for the structural 
#' state shocks hitting the Trend (xi_trend) and the Cycle (xi_cycle).
build_N <- function(model_params) {
  if (is.null(model_params$xi_trend) || is.null(model_params$xi_cycle)) {
    warning("Missing process noise parameters in build_N!")
  }
  
  N <- diag(c(as.numeric(model_params$xi_trend), as.numeric(model_params$xi_cycle)), 
            nrow = 2, ncol = 2)
  return(N)
}


#' Build the nu_t Matrix (Exogenous State Impact)
#'
#' @description
#' Constructs the T*2 exogenous intercept matrix for the transition equations.
#' The Okun's Law GDP gap betas shift the cyclical state equation directly.
build_nu_t <- function(model_params, X) {
  beta_names <- grep("^beta", names(model_params), value = TRUE)
  betas      <- unlist(model_params[beta_names])
  
  # Structural cyclical impact of GDP gaps (T x 1 vector)
  cyc_drift <- X %*% matrix(betas, ncol = 1)
  
  # Row t returns: [0 for Trend, cyc_drift for Cycle]
  return(cbind(rep(0, nrow(X)), cyc_drift))
}


#' Build mu_t matrix (Measurement Intercept)
build_mu_t <- function(model_params, X) {
  # Returns a T x 2 matrix filled entirely with zeros
  return(matrix(0, nrow = nrow(X), ncol = 2))
}




















################################################################################
#
# OKUN SSM OBJECT
#
################################################################################

#' Initialize the Okun State-Space Blueprint (Strict 2D Form)
#'
#' @description
#' This function constructs the "Blueprint" or "SSM Object" required for Kalman filtering 
#' and parameter optimization. Every parameter must be explicitly passed in the guess list.
#'
#' @export
initialize_my_okun_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # 1. ENFORCE STRICT PROTOCOL: All 8 required structural parameters must be present
  required_params <- c("beta1", "beta2", "beta3", "phi", 
                       "xi_trend", "xi_cycle", 
                       "sigma_spf_5y_unemp")
  
  missing_idx <- !(required_params %in% names(parameter_guesses))
  if (any(missing_idx)) {
    stop(paste("SSM OKUN INIZIALIZATION ERROR: Missing parameters:", 
               paste(required_params[missing_idx], collapse = ", ")))
  }
  
  # 2. Build the parameter manifest mapping rules cleanly
  # Rule 0: Unconstrained Linear
  # Rule 1: exponential strictly positive
  # rule 2: logit between 0 and 1
  # rule 3: custom constraint
  manifest <- list(
    # Okun's Law Betas (Rule 0: Unconstrained Linear)
    beta1              = list(val = parameter_guesses$beta1, rule = 0),
    beta2              = list(val = parameter_guesses$beta2, rule = 0),
    beta3              = list(val = parameter_guesses$beta3, rule = 0),
    
    # Cyclical Persistence
    phi                = list(val = parameter_guesses$phi,
                              rule = 3,       
                              low  = 0.5,  
                              high = 0.9 
    ),
    
    # Process Noise Standard Deviations (Rule 1: Exponentiated to stay > 0)
    xi_trend           = list(val = parameter_guesses$xi_trend, 
                              rule = 3,       
                              low  = 0.005,   
                              high = 0.025    
    ),
    xi_cycle           = list(val = parameter_guesses$xi_cycle, rule = 2),
    
    # Measurement Noise Standard Deviations (Rule 1: Exponentiated to stay > 0)
    sigma_spf_5y_unemp = list(val = parameter_guesses$sigma_spf_5y_unemp,
                              rule = 1   
    ),
    sigma_unemp       = list(val = parameter_guesses$sigma_unemp,
                             rule = 1
  ))
  
  # 3. Assemble structural blueprint bundle for the core filter
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), 
      X = as.matrix(X_data)
    ),
    manifest = manifest,
    builders = list(
      mu_t = build_mu_t,
      nu_t = build_nu_t,
      H    = build_H,
      G    = build_G,
      M    = build_M,
      N    = build_N
    ),
    name = "okun",
    # Initial state vector guesses: [Trend, Cycle]
    rho_guess   = parameter_guesses$state_init,
    # 2x2 Identity matrix flattened out to scale initial state variances safely
    sigma_guess = parameter_guesses$sigma_init 
  )
  
  return(ssm)
}
