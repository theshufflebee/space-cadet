
# ==============================================================================
# parameter mapping for keeping them positive if needed
# ==============================================================================

#' Map Optimizer Parameters to Model Parameters
#' @param theta Numeric vector. Unconstrained parameters from the optimizer.
#' @param ssm The SSM Object containing the manifest.
param2model_gen <- function(theta, ssm) {
  spec <- ssm$manifest
  p_names <- names(spec)
  out <- list()
  
  for (i in seq_along(p_names)) {
    name <- p_names[i]
    rule <- spec[[name]]$rule
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
#' @param model_list Named list of model parameters (economic scale).
#' @param ssm The SSM Object containing the manifest.
model2param_gen <- function(model_list, ssm) {
  spec <- ssm$manifest
  p_names <- names(spec)
  theta <- numeric(length(p_names))
  
  for (i in seq_along(p_names)) {
    name <- p_names[i]
    rule <- spec[[name]]$rule
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
#' @param theta Numeric vector of parameters furnished by optimizer.
#' @param ssm The fully initialized SSM object.
#' @param return_full_res Boolean. Return filter details or just scalar likelihood.
loglik_ssm <- function(theta, ssm, return_full_res = FALSE) {
  
  # 1. Map Parameters (Optimizer Space -> Economic Space)
  model_params <- param2model_gen(theta, ssm)
  
  # 2. Execute builders using the named model_params
  # Note: Builders now use the ssm$data internal references
  mu_t <- ssm$builders$mu_t(model_params, ssm$data$X)
  G    <- ssm$builders$G()
  H    <- ssm$builders$H(model_params)
  M    <- ssm$builders$M(model_params)
  N    <- ssm$builders$N(model_params) # Dynamically calculated now
  
  # 3. Setup static components
  T_len <- nrow(ssm$data$Y)
  nu_t  <- matrix(0, T_len, 1)
  
  # 4. Run Kalman Filter
  # We use the Y data stored inside the object
  res <- kalman_filter(
    Y_t     = ssm$data$Y, 
    nu_t    = nu_t, 
    H       = H, 
    N       = N, 
    mu_t    = mu_t, 
    G       = G, 
    M       = M, 
    Sigma_0 = matrix(10, 1, 1), 
    rho_0   = ssm$data$Y[1, 2] # Starting at first SPF value
  )
  
  # 5. Output handling
  if (return_full_res) {
    return(res) 
  } else {
    # Optimizer needs a single scalar to minimize
    return(-sum(res$loglik.vector)) 
  }
}



# ==============================================================================
# wrapper function for the optimizer to run one estimation
# ==============================================================================
#' Multi-Step State-Space Model Optimizer Wrapper
#'
#' @param ssm The fully initialized SSM blueprint object.
#' @param methods Character vector of methods (e.g., c("Nelder-Mead", "BFGS")).
#' @param iters Number of times to cycle through the methods list.
#' @param start_par Optional: unconstrained parameters from a previous run (warm start).
#'
#' @return A list containing the optimized parameters (economic scale), the final fit object, and the unconstrained theta.
ssm_optimizer_wrapper <- function(ssm, 
                                  methods = c("Nelder-Mead", "BFGS"), 
                                  iters = 2, 
                                  start_par = NULL) {
  
  # 1. Prepare Initial Parameters
  if (is.null(start_par)) {
    # If no warm start, use manifest defaults
    init_theta_econ <- sapply(ssm$manifest, function(x) x$val)
    current_par_opt <- model2param_gen(init_theta_econ, ssm)
  } else {
    current_par_opt <- start_par
  }
  
  n_par <- length(current_par_opt)
  message("Starting Multi-Step Optimization...")
  
  # 2. Run Optimization Loop
  # Cycles through each method for the specified number of iterations
  for (i in 1:iters) {
    for (m in methods) {
      fit <- optimx::optimx(
        par     = current_par_opt,
        fn      = loglik_ssm,
        ssm     = ssm,
        method  = m,
        control = list(
          maximize = FALSE, 
          maxit    = 1000,  
          itnmax   = 1000    
        )
      )
      
      # Update current_par_opt for the next step in the loop
      current_par_opt <- as.numeric(fit[1, 1:n_par])
      print(current_par_opt)
    }
  }
  
  # 3. Extract and Transform Results
  # Final mapping back to Economic Space (Named List)
  final_params_econ <- param2model_gen(current_par_opt, ssm)
  
  message("Optimization Done")
  
  return(list(
    params = final_params_econ, # Economic scale (Named List)
    theta  = current_par_opt,   # Unconstrained scale (Vector for next warm start)
    fit_summary = fit,          # The last optimx result object
    ssm = ssm
  ))
}


#===============================================================================
#
#===============================================================================

#' Rolling Okun SSM Estimation
#' 
#' Generate Rolling SSM Parameters for Forecasting
#' 
#' Estimates model parameters over an expanding window up to a specified end date.
#' Returns a list of parameters to be used in a separate forecasting step.
#' 
#' We have T = 1 as the start of the data, T=T is the end of the data,
#' T=t as the current date up until we have the data availabe for the pseudo forecast
#' and t+h would then be the forecast horizon.
#' What we do here is just get the parameter estimation at time t.
#' 
#' At each step we have to reestimate the HP filter to get the gaps. This is done via a burn in
#' where we start the estimation at the first observation in the dataframe. the dataframe is cut to the burn in start in the preparation
#' this assures there is no starting point bias.
#' 
#' @param data Raw merged dataframe containing log_gdp, unemp_rate, and spf_5y_unemp
#' @param forecast_start The first date to generate a forecast/parameter set for
#' @param forecast_end End date of rolling window (optional)
#' @param val_T1 The start date for the Kalman Filter (e.g., 2015-01-01)
new_get_params_okun_ssm <- function(data,
                                    forecast_start,
                                    forecast_end = NULL,
                                    date_col = "quarter",
                                    val_T1 = "2015-01-01") {
  
  # 1. Format Dates
  data[[date_col]] <- as.yearqtr(data[[date_col]])
  start_q <- as.yearqtr(as.Date(forecast_start))
  val_T1 <- as.yearqtr(as.Date(val_T1))
  
  if(is.null(forecast_end)) {
    end_q <- max(data[[date_col]], na.rm = TRUE)
  } else {
    end_q <- as.yearqtr(as.Date(forecast_end))
  }
  
  # Define the sequence of "Vantage Points" (Today)
  forecast_dates <- data[[date_col]][data[[date_col]] >= start_q & data[[date_col]] <= end_q] 
  
  comp <- list()
  
  # Initial setup for the first warm start
  # We initialize with NULL so the wrapper uses manifest defaults for the first run
  current_theta <- NULL 
  
  message(sprintf("Rolling Estimation: %s to %s", start_q, end_q))
  
  for (i in seq_along(forecast_dates)) {
    target_date <- forecast_dates[i]
    message("\n--- Vantage Point: ", target_date, " ---")
    
    # 2. Slice Data available "Today"
    data_t <- data[data[[date_col]] <= target_date, ]
    
    # 3. Process Exogenous (HP Filter on full available history)
    valid_gdp_indices <- which(!is.na(data_t$log_gdp))
    if (length(valid_gdp_indices) < 5) {
      message("Skipping ", target_date, ": Not enough GDP data points.")
      next
    }
    
    first_obs_idx <- valid_gdp_indices[1]
    gdp_series    <- data_t$log_gdp[first_obs_idx:nrow(data_t)]
    
    hp_res    <- mFilter::hpfilter(gdp_series, freq = 1600)
    gdp_cycle <- as.numeric(hp_res$cycle)
    
    # Align cycle back to the time-series and create lags
    processed_data <- data_t
    processed_data$gap_l0 <- NA_real_
    processed_data$gap_l0[first_obs_idx:nrow(data_t)] <- gdp_cycle
    
    processed_data <- processed_data %>%
      mutate(
        gap_l1 = dplyr::lag(gap_l0, 1),
        gap_l2 = dplyr::lag(gap_l0, 2)
      ) %>%
      # 4. Filter for Estimation Window
      filter(quarter >= val_T1) %>%
      filter(complete.cases(unemp_rate, spf_5y_unemp, gap_l0, gap_l1, gap_l2))
    
    if (nrow(processed_data) < 5) {
      message("Skipping ", target_date, ": No valid overlapping observations.")
      next
    }
    
    # 5. Build Matrices
    Y_final <- as.matrix(processed_data[, c("unemp_rate", "spf_5y_unemp")])
    X_final <- as.matrix(processed_data[, c("gap_l0", "gap_l1", "gap_l2")])
    
    message(sprintf("Estimation range: %s to %s (%d obs)", 
                    min(processed_data$quarter), max(processed_data$quarter), nrow(processed_data)))
    
    # 6. Initialize Blueprint
    my_ssm_model <- initialize_my_okun_ssm(Y_final, X_final)
    
    # 7. Optimization: Multi-step wrapper with Warm Start
    # We use Nelder-Mead and BFGS to ensure global search and local precision
    opt_results <- ssm_optimizer_wrapper(
      ssm       = my_ssm_model, 
      methods   = c("Nelder-Mead", "BFGS"), 
      iters     = 2, 
      start_par = current_theta
    )
    
    # Update current_theta for the next vantage point (Warm Start)
    current_theta <- opt_results$theta
    
    # 8. Final Extraction of States
    final_states <- loglik_ssm(current_theta, my_ssm_model, return_full_res = TRUE)
    
    # Store results
    comp[[as.character(target_date)]] <- list(
      target_date = target_date,
      params      = opt_results$params,
      states      = final_states
    )
    
    cat("Likelihood: ", -final_states$loglik, "Parameters", current_theta)
    cat(sprintf("[%d/%d] Estimated: %s\n", i, length(forecast_dates), as.character(target_date)))
  }
  
  return(comp)
}






