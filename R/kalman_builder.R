# ==============================================================================
# Create factory functions for all matrices
# ==============================================================================
# The matrix that transitions the State equation
# Currently if the phi parameter was specified in the params selection it will
# add it, else it is as random walk.


#' Transition Matrix (H) Factory
#' Factory Function that creates a function
#' This Function Creates the H matrix of for the Kalmann filter, which gouverns
#' the persistance of the state. It can be either an AR(1) Process (default) or
#' a Randowm walk when there is no persistance parameter to estimate and the
#' matrix defaults to 1.
#' 
#' @param name give the AR(1) parameter a name, defaults to phi
#' @param random_walk If set to true, the phi parameter is set to 1 for a random walk
#' 
#' @return returns a list with 2 objects, one is the matrix function used for the optimizer
#' and the parameter rule that translates between optimizer and the model, as the AR(1) param
#' has to be between ]0;1[
#' 
#' @seealso [loglik_ssm()]
#' @export 
H_matrix_factory <- function(name = "phi", random_walk = FALSE) {
  
  # This is a list containing the dictionary and the starting guess
  manifest <- list(
    name    = if(random_walk) character(0) else name,
    rule    = if(random_walk) numeric(0) else setNames(2, name), # 2 = Logistic
    default = if(random_walk) list() else setNames(0.95, name)
  )
  
  # Builder: this is the function we pass into the likelyhood
  # it checks if the parameter name, which we define in the factory function exists
  # in the model parameters that the optimizer is estimating
  # if it dies it passes the phi value into the matrix it creates, else it
  # defaults to the random walk
  builder <- function(model_params) {
    # Logic: if phi exists in params -> use it, else it's a Random Walk -> phi=1
    phi_val <- if(!is.null(model_params[[name]])) model_params[[name]] else 1.0
    return(matrix(phi_val, 1, 1))
  }
  
  # Return them as a nested list
  return(list(
    manifest = manifest,
    builder   = builder
  ))
}


#' Measurement Link Matrix (G) Factory
#' 
#' Factory Function that creates a function
#' Creates the matrix linking the hidden state (e.g., natural rate) to the observed variables.
#' It creates a list containing the manifest (parameter specifications) and a
#' function according to matrix specifications for the optimizer.
#'
#' @param Y_data Matrix/DataFrame of observed variables used to determine dimensions.
#' @param loadings Numeric vector. Fixed weights for the state's impact on observations. Defaults to 1s.
#' @param nr Integer. Number of hidden states, defaults to 1.
#'
#' @return A list containing \code{manifest} (empty for fixed loadings) and \code{builder}.
#' 
#' @seealso [ssm_optimizer()]
#' 
#' @export
G_matrix_factory <- function(Y_data, loadings = NULL, nr = 1) {
  ny <- ncol(Y_data)
  
  # Default to 1s if no specific loadings are provided
  if (is.null(loadings)) {
    loadings <- rep(1, ny)
  }
  
  # Do the loadings match the amount of measurement variables
  if (length(loadings) != ny) {
    stop(sprintf("G-Matrix Error: Loadings length (%d) does not match Y columns (%d)", 
                 length(loadings), ny))
  }
  
  # manifest would be used if i want to estimate loadings, currently empty
  manifest <- list(
    name    = character(0),
    rule    = numeric(0),
    default = list()
  )
  
  # Builder of the function that builds the matrix in the optimizer
  builder <- function(model_params) {
    # model_params currently useless, but if i want an estimation its already here
    
    # Create the ny x nr matrix using the provided loadings
    G <- matrix(loadings, nrow = ny, ncol = nr)
    return(G)
  }
  
  return(list(manifest = manifest, builder = builder))
}


#' Measurement Noise Matrix (M) Factory
#' 
#' Factory Function that creates a function
#' Creates a diagonal covariance matrix for observation errors. The parameter specs
#' are restricted to be positive in the manifest (parameter specs)
#'
#' @param Y_data Matrix/DataFrame of observations to determine the number of variances.
#' @param prefix Character. Prefix for the variance parameters, defaults to "sigma_".
#'
#' @return A list containing \code{manifest} (Rule 1: Exponential to be non negative) and \code{builder}.
#' @export
M_matrix_factory <- function(Y_data, prefix = "sigma_") {
  col_names <- colnames(Y_data)
  ny        <- length(col_names)
  p_names   <- paste0(prefix, col_names)
  
  # These are parameter mappings and restrictions >0 so rule 1
  manifest <- list(
    names   = p_names,
    rules   = setNames(rep(1, ny), p_names),
    # Defaulting to 0.005 (a standard starting guess for unemployment volatility)
    default = setNames(as.list(rep(0.005, ny)), p_names)
  )
  
  # builder function
  builder <- function(model_params) {
    # Dynamically extract the values the optimizer chose for these specific names
    sigmas <- unlist(model_params[p_names])
    
    # Build the diagonal matrix M (ny x ny)
    # The Kalman Filter uses this to weight the 'certainty' of each observation
    M <- diag(sigmas, nrow = ny, ncol = ny)
    
    return(M)
  }
  
  return(list(manifest = manifest, builder = builder))
}

#' Exogenous Component Matrix (mu_t) Factory
#' 
#' Factory Function that creates a function.
#' Creates a time-varying matrix of exogenous signals. Can do this using exogenous inputs.
#'
#' @param X_data Matrix of exogenous regressors
#' @param Y_data Matrix of observations to determine time and variable dimensions.
#' @param param_prefix Character. Prefix for the coefficients, defaults to "beta".
#' @param intercept Bool. Whether to include an estimated intercept.
#' @param impact_cols Integer vector. Indices of observation variables influenced by the signal.
#'
#' @return A list containing \code{manifest} (Rule 0: Linear) and \code{builder}.
#' @export
mu_t_matrix_factory <- function(X_data,
                          Y_data,
                          param_prefix = "beta",
                          intercept = FALSE,
                          impact_cols = c(1)) {
  
  T_len <- nrow(Y_data)
  ny    <- ncol(Y_data)
  nx    <- ncol(X_data)
  
  # Generate Names
  p_names <- if(intercept) paste0(param_prefix, 0:nx) else paste0(param_prefix, 1:nx)
  
  # build manifest wher parameters and transform rules and defaults are mapped
  # Rule 0 = Plain (No transformation needed for betas)
  manifest <- list(
    names   = p_names,
    rules   = setNames(rep(0, length(p_names)), p_names),
    default = setNames(as.list(rep(-0.1, length(p_names))), p_names)
  )
  
  # Builder
  builder <- function(model_params) {
    
    # Extract only the betas needed for this component
    current_betas <- unlist(model_params[p_names])
    
    # Handle the Intercept logic
    X_mat <- if(intercept) cbind(1, X_data) else X_data
    
    # Signal = X * Beta
    # Result is a T_len x 1 vector
    signal <- X_mat %*% matrix(current_betas)
    
    # Map the signal into the mu_t matrix (T_len x ny)
    # Usually, Okun only hits the first column (unemployment)
    mu_matrix <- matrix(0, nrow = T_len, ncol = ny)
    for (col in impact_cols) { 
      mu_matrix[, col] <- as.vector(signal) 
    }
    
    return(mu_matrix)
  }
  
  return(list(manifest = manifest, builder = builder))
}

# ==============================================================================
# parameter mapping for keeping them positive if needed
# ==============================================================================


#' Map Optimizer Parameters to Model Parameters
#' 
#' Transforms unconstrained values from the optimizer (that gives random values)
#' into model-appropriate values according to parameter restictions using specified
#' rules (e.g., exponential for variance or non negative parameters, or logistic
#' for persistence that are between 0 and 1). These are then transformed and plugged
#' into the matrix functions
#'
#' @param theta Numeric vector. Unconstrained parameters from the optimizer.
#' @param spec List. Contains parameter names and their transformation rules.
#'
#' @return A named list of transformed model parameters.
#' 
#' @seealso [model2param_gen()]
#' 
#' @export
param2model_gen <- function(theta, spec) {
  out <- list()
  for (i in seq_along(spec$names)) {
    name <- spec$names[i]
    rule <- spec$rules[name] # Look up the rule (0, 1, or 2)
    val  <- theta[i]
    
    out[[name]] <- switch(as.character(rule),
                          "1" = exp(val),                    # Exponential (Variances)
                          "2" = 1 / (1 + exp(-val)),         # Logistic (AR Phi)
                          val                                # Default / Rule 0 (Betas)
    )
  }
  return(out)
}


#' Map Model Parameters to Optimizer Parameters
#' 
#' Inverse of \code{param2model_gen}. Maps model values back to unconstrained
#' space for the optimizer.
#'
#' @param model_list Named list of model parameters.
#' @param spec List. Transformation rules.
#'
#' @return A numeric vector of unconstrained parameters.
#' 
#' @seealso [param2model_gen()]
#' 
#' @export
model2param_gen <- function(model_list, spec) {
  theta <- numeric(length(spec$names))
  for (i in seq_along(spec$names)) {
    name <- spec$names[i]
    rule <- spec$rules[name]
    val  <- model_list[[name]]
    
    theta[i] <- switch(as.character(rule),
                       "1" = log(val),
                       "2" = log(val / (1 - val)),        # Logit inverse
                       val
    )
  }
  return(theta)
}

# ==============================================================================
# Log lik function made to be customizable for SSMs
# ==============================================================================


#' State-Space Log-Likelihood Function
#' 
#' Calculates the negative log-likelihood of a State-Space model given current parameters.
#' Integrates parameter mapping, matrix building, and Kalman filtering.
#'
#' @param theta Numeric vector of parameters furnished by optimizer
#' @param Y Observed data matrix.
#' @param model_spec Parameter transformation dictionary containing restrictions
#' @param mu_t_builder Builder function for the exogenous component matrix.
#' @param G_builder Builder function  for the measurement link matrix.
#' @param H_builder Builder function  for the transition matrix.
#' @param M_builder Builder function  for the measurement noise matrix.
#'
#' @return Numeric. The negative log-likelihood.
#' @export
loglik_ssm <- function(theta,
                       Y,
                       model_spec,
                       mu_t_builder,
                       G_builder,
                       H_builder,
                       M_builder,
                       return_full_res = FALSE) {
  
  # Map Parameters so the ones specified can't be positive
  model <- param2model_gen(theta, model_spec)
  
  # Build exogenous mu_t, still a work in progress,
  # it takes the parameters theta which are the numbers selected by optimizer
  # and returns the matrices for the full estimation (for each t)
  # EXECUTE the builders with the current 'model' parameters
  mu_t <- mu_t_builder(model)
  
  # build the Matrix linking state euation and measurement equation
  G    <- G_builder(model)
  
  # Transition: Handles Random Walk or AR(1)
  H    <- H_builder(model)
  
  # build the matrix representing the shocks on the measurement variables
  M    <- M_builder(model)  # Fixed: Added (model)
  
  # Build the rest of the model matrices
  
  # intercept for the state equation
  T_len <- nrow(Y) # for nu_t
  nu_t  <- matrix(0, T_len, 1)
  

  # the shocks in the state euqation
  N     <- matrix(0.0001, 1, 1) # Fixed Smoothness
  
  # Run filter with given specs
  res <- kalman_filter(Y_t = Y, nu_t = nu_t, H = H, N = N, 
                       mu_t = mu_t, G = G, M = M, 
                       Sigma_0 = matrix(10), rho_0 = Y[1, 2])
  
  # returns the negative loglik, so the optimizer can run the whol thing again
  # wuth newly selected parameters
  
  if (return_full_res) {
    return(res) # Returns the whole list (states, lik, etc.)
  } else {
    return(-sum(res$loglik.vector)) # Returns the scalar for the optimizer
  }
}



# ==============================================================================
# wrapper function for the optimizer to run one estimation
# ==============================================================================

# goal is to wrap this into a rollinge stimation wrap to get parameters for
# the forecast

#' Hybrid Optimizer Wrapper
#' 
#' Refines model estimates using a multi-method optimization loop (Nelder-Mead and nlminb).
#' Useful for avoiding local minima in complex likelihood surfaces.
#'
#' @param nb_loop Integer. Number of full refinement cycles.
#' @param theta_start Numeric vector. Initial parameter guesses.
#' @param Y Observed data.
#' @param X Exogenous data.
#' @param mu_t_builder,H_builder,G_builder,M_builder Matrix builder functions.
#' @param model_spec Parameter mapping dictionary.
#' @param optim_methods Character vector of optimization methods to cycle through.
#'
#' @return A list containing the optimized \code{params} (transformed) and the raw \code{theta}.
#' @export
ssm_optimizer_wrapper <- function(nb_loop = 3,
                                  theta_start,
                                  Y = Y,
                                  X = X,
                                  mu_t_builder,
                                  H_builder,
                                  G_builder,
                                  M_builder,
                                  model_spec,
                                  optim_methods = c("Nelder-Mead", "nlminb")) {
  
  # Theta is the parameter to optimize wrt
  theta <- theta_start
  
  message("Starting Hybrid Optimization Loop...\n")
  
  # --- LOOP---
  for(i in 1:nb_loop) {
    
    # have 3 estimations, eeach time taking the previous esttimates
    message("\n--- Cycle", i, "of", nb_loop, "---\n")
    
    for(method_name in optim_methods) {
      # secondly estimate over two methods, one very robust the other very fast
      
      # Run the Optimizer
      res_opt <- optimx::optimx(
        par            = theta, 
        fn             = loglik_ssm,         
        Y              = Y,                  
        model_spec     = model_spec, 
        mu_t_builder   = mu_t_builder,
        H_builder      = H_builder,
        G_builder      = G_builder,
        M_builder      = M_builder,
        method         = method_name,
        control        = list(maximize = FALSE, trace = 0)
      )
      
      # Update the estimate: Take the coefficients from the current best result
      # We extract only the parameter columns 
      theta <- as.numeric(res_opt[1, 1:length(theta)])
      
      # Live Status
      message(sprintf("  [%s] -> NegLogLik: %.4f\n", method_name, res_opt$value))
    }
  }
  
  # --- FINAL OUTPUT ---
  # Unpack the last refined theta_est back into human-readable parameters
  final_params <- param2model_gen(theta, model_spec)
  
  message("\nFinal Refined Parameters:\n")
  print(final_params)
  
  return(list(
    params = final_params, 
    theta  = theta
  ))
  
}
