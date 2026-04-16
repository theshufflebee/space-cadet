################################################################################
#
# OKUN MATRICES AND SSM OBJECT
#
################################################################################

# mu_t Builder (Okun's Law)
build_mu_t <- function(model_params, X) {
  # model_params is now a named list/vector
  # Dynamically find all parameters starting with "beta"
  beta_names <- grep("^beta", names(model_params), value = TRUE)
  betas <- unlist(model_params[beta_names])
  
  impact <- X %*% matrix(betas, ncol = 1)
  
  # Return T x 2 matrix
  cbind(impact, rep(0, nrow(X)))
}

# H Builder (Persistence)
build_H <- function(model_params) {
  # Explicitly look for "phi", default to 1 (Random Walk) if not found
  phi_val <- if(!is.null(model_params$phi)) model_params$phi else 1.0
  return(matrix(phi_val, 1, 1))
}

# G: Observation matrix (Loadings)
build_G <- function() {
  matrix(c(1, 1), nrow = 2, ncol = 1)
}

#' Measurement Noise Matrix (M) Builder
#' @param model_params Named list of estimated parameters
build_M <- function(model_params) {
  
  # 1. Dynamically identify all measurement noise parameters
  # This looks for any parameter starting with "sigma_" 
  # but excludes the state innovation "sigma_state_innov" (or similar)
  p_names <- grep("^sigma_", names(model_params), value = TRUE)
  p_names <- p_names[!p_names %in% c("sigma_state_innov", "sigma_n")]
  
  # 2. Extract values
  # We use the names to ensure we maintain the correct order
  sigmas <- unlist(model_params[p_names])
  ny <- length(sigmas)
  
  # 3. Create diagonal matrix of variances (SD squared)
  # diag() handles the dimensions based on the length of sigmas
  M <- diag(sigmas^2, nrow = ny, ncol = ny)
  
  # Optional: Set row/colnames for easier debugging
  colnames(M) <- rownames(M) <- p_names
  
  return(M)
}

# N: Process noise (Innovation of Natural Rate)

build_N <- function(model_params) {
  # Check if a specific state sigma exists, otherwise use your fixed 0.0001
  sig_n <- if(!is.null(model_params$sigma_state_innov)) model_params$sigma_state_innov else sqrt(0.0001)
  matrix(sig_n^2, 1, 1)
}



#' Initialize the Okun State-Space Blueprint
#'
#' @param Y_data Matrix/DataFrame of observations (e.g., [unemp, spf])
#' @param X_data Matrix of exogenous regressors (e.g., lagged GDP gaps)
#'
#' @return A structured list (SSM Object) used for optimization and filtering
#' @export
initialize_my_okun_ssm <- function(Y_data, X_data) {
  
  # 1. Define the Manifest
  # This acts as the "Single Source of Truth" for your parameters
  # Rules: 0 = Linear, 1 = Exponential (>0), 2 = Logit (0 to 1)
  manifest <- list(
    # Okun's Law Betas (Unconstrained)
    beta1              = list(val = -0.10, rule = 0),
    beta2              = list(val = -0.05, rule = 0),
    beta3              = list(val = -0.02, rule = 0),
    
    # State Persistence (Bounded for stability)
    # phi                = list(val = 0.98,  rule = 2),
    
    # Measurement Noise Standard Deviations (Must be positive)
    # Names match: "sigma_" + colnames(Y_data)
    sigma_unemp_rate   = list(val = 0.01,  rule = 1),
    sigma_spf_5y_unemp = list(val = 0.005, rule = 1),
    
    # Process Noise (Standard deviation of the Natural Rate innovation)
    sigma_state_innov  = list(val = 0.001, rule = 1)
  )
  
  # 2. Bundle the builders and data into the blueprint
  # These are the standalone functions you defined earlier
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), 
      X = as.matrix(X_data)
    ),
    manifest = manifest,
    builders = list(
      mu_t = build_mu_t,
      H    = build_H,
      G    = build_G,
      M    = build_M,
      N    = build_N
    )
  )
  
  return(ssm)
}
