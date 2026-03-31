# ==============================================================================
# Create factory functions for all matrices
# ==============================================================================
# The matrix that transitions the State equation
# Currently if the phi parameter was specified in the params selection it will
# add it, else it is as random walk.


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


# currently also just a normal function
# This matrix links the state to the measurement equation
# currently just as many 1s as there are measurement variables
# a ny x 1 matrix where ny is the number of measurement variables

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


# The mu_t matrix with exogenous components
# currently only true factory function
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
loglik_ssm <- function(theta,
                       Y,
                       model_spec,
                       mu_t_builder,
                       G_builder,
                       H_builder,
                       M_builder) {
  
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
  
  return(-sum(res$loglik.vector))
}



# ==============================================================================
# wrapper function for the optimizer to run one estimation
# ==============================================================================

# goal is to wrap this into a rollinge stimation wrap to get parameters for
# the forecast

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
  
  return(final_params)
  
}
