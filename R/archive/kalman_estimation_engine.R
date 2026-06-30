

# ==============================================================================
# Log lik function made to be customizable for SSMs
# ==============================================================================


#' State-Space Log-Likelihood Function
#'
#' @description
#' This function serves as the primary objective function for the SSM optimization. 
#' It bridges the unconstrained optimizer space and the State-Space model by mapping 
#' parameters, invoking matrix builders, and executing the Kalman Filter.
#'
#' @param theta Numeric vector. Unconstrained parameters provided by the optimizer.
#' @param ssm A fully initialized SSM object (blueprint) containing data, 
#'   parameter manifest, and matrix builder functions.
#' @param return_full_res Logical. If \code{TRUE}, returns the full output of 
#'   the Kalman Filter (states, covariances, etc.). If \code{FALSE} (default), 
#'   returns the negative log-likelihood scalar for optimization.
#' @param diffuse_prior Logical. If \code{TRUE}, initializes the state variance 
#'   with a large value (10), representing high uncertainty.
#' @param init_guess_state Numeric. Optional manual starting value for the 
#'   latent state \eqn{\rho_0}. Defaults to the first value of the anchor 
#'   variable (SPF forecast) in the observation matrix, which is in the second column
#'
#' @details
#' The function follows a strict execution pipeline:
#' \enumerate{
#'   \item \bold{Mapping:} Transforms \code{theta} to economic scale using \code{param2model_gen}.
#'   \item \bold{Matrix Building:} Calls the builder functions stored in \code{ssm$builders} 
#'     to generate \eqn{\mu_t, G, H, M,} and \eqn{N}.
#'   \item \bold{Filtering:} Runs the Kalman Filter recursion to calculate prediction 
#'     errors and the log-likelihood vector.
#' }
#' 
#' For optimization, the function returns the \emph{negative} log-likelihood because 
#' standard R optimizers (like \code{optimx}) are minimizers.
#'
#' @return If \code{return_full_res = FALSE}, a numeric scalar. If \code{TRUE}, 
#'   a list containing filtered states, covariances, and the mapped parameters.
#'
#' @export
loglik_ssm <- function(theta,
                       ssm,
                       return_full_res = FALSE,
                       rho_guess = 0.1,
                       set_silent = FALSE) {
  
  # Initial State Vector (rho_0)
  # Ensure it is a 3x1 column vector for Model 3
  
  rho_init <- matrix(ssm$rho_guess, nrow = length(ssm$rho_guess), ncol = 1)

  # Determine Number of States (nr = 3 for Model 3, others 1)
  nr <- nrow(rho_init) 
  
  # Initial Covariance Matrix (Sigma_0)
  # Uses a diagonal structure to represent initial uncertainty
  sig_init <- diag(as.numeric(ssm$sigma_guess), nr)

  # Map Parameters (Optimizer Space -> Economic Space)
  model_params <- param2model_gen(theta, ssm)
  
  # Build the matrices with the parameters to create the loglikelihood
  # The builders themselves are saved in the ssm object. They are functions as
  # they take in the parameters from the optimizer and calculate it
  mu_t <- ssm$builders$mu_t(model_params, ssm$data$X)
  # G    <- ssm$builders$G() # G is not static anymore need to remove before running other models
  H    <- ssm$builders$H(model_params)
  M    <- ssm$builders$M(model_params)
  N    <- ssm$builders$N(model_params) # Dynamically calculated now
  
  args_expected <- names(formals(ssm$builders$G))
  
  if ("model_params" %in% args_expected) {
    G <- ssm$builders$G(model_params)
  } else {
    G <- ssm$builders$G()
  }
  
  # Setup static components, 
  T_len <- nrow(ssm$data$Y)
  nu_t  <- matrix(0, T_len, nr) # we have no interceptr so its just 0's -> added to nr because needed number of states
  
  # Run Kalman Filter with the matrices
  # The Y data is from the ssm object the X data is already in mu_t
  # we also add initial guesses for the state and give a prior
  res <- kalman_filter(
    Y_t     = ssm$data$Y, 
    nu_t    = nu_t, 
    H       = H, 
    N       = N, 
    mu_t    = mu_t, 
    G       = G, 
    M       = M, 
    Sigma_0 = sig_init, # Prior on variance (certainty of guess)
    rho_0   = rho_init # default is the second value of the Y column or the anchor
  )
  
  # Output handling
  # The optimizer only needs the loglikelihood, therefore we need that as a return
  # If we want to run a normal estimatiion with parameters we'd like to see the full
  # return, we specify this and we get state and all other variables
  
  
  cat(sprintf("\rLog Likelihood: %.4f", -sum(res$loglik.vector)))
  flush.console()
  
  if (return_full_res) {
    res$param_debugs <- model_params
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
                                  methods = c("Nelder-Mead", "bobyqa", "BFGS"), 
                                  iters = 2, 
                                  start_par = NULL,
                                  set_silent = TRUE) {
  
  # Prepare Initial Parameters
  if (is.null(start_par)) {
    # If no warm start, use manifest defaults
    # sapply here simplifies the nested list -> selects val from each element in the list
    init_theta_econ <- sapply(ssm$manifest, function(x) x$val) 
    
    current_par_opt <- model2param_gen(init_theta_econ, ssm)
    
    
    if(!set_silent){
      param_string <- paste(names(init_theta_econ), "=", round(init_theta_econ, 4), collapse = ", ")
      message("Debug: Economic Params (Initial): ", param_string)
      
      opt_string <- paste(names(current_par_opt), "=", round(current_par_opt, 4), collapse = ", ")
      message("Debug: Optimizer Params (Theta): ", opt_string)
      
    }
    
    
    

    
  } else {
    current_par_opt <- start_par
    
    if(!set_silent) {
      message("\n--- Transformed Optimizer Space (Theta) ---")
      # This creates a "Name: Value" pair on each new line
      formatted_list <- paste0(names(current_par_opt), ": ", round(current_par_opt, 4), collapse = "\n")
      message(formatted_list)
    }
    

  }
  
  n_par <- length(current_par_opt)
  
  
  
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
          all.methods = FALSE, # Run them in order
          follow.on = TRUE,    # Method 2 starts where Method 1 ends
          dowarn = FALSE, 
          maximize = FALSE,
          itnmax = 1000,  # For bobyqa/optimx
          maxit = 1000,
          reltol = 1e-4,  # Stop if relative improvement is less than this
          abstol = 1e-4  # Stop if absolute improvement is less than this
        )
      )
    }
      
      # Update current_par_opt for the next step in the loop
      proposed_par <- as.numeric(fit[1, 1:n_par])
      
      
      if(!set_silent){
        formatted_theta_opt <- paste0(
          sprintf("  %-20s : %.4f", names(proposed_par), proposed_par), 
          collapse = "\n")
        
        #Message for Debugging of parameters
        cat("\n", rep("=", 45), "\n", sep = "")
        cat("ESTIMATED OPTIMIZER PARAMETERS (Economic Space)\n")
        cat(rep("-", 45), "\n", sep = "")
        cat(formatted_theta_opt, "\n")
        cat(rep("-", 45), "\n")
        print(proposed_par)
        
      }
      

      
      #Validation Check: Ensure the optimizer didn't return NA or NaN
      if (any(is.na(proposed_par)) || any(is.infinite(proposed_par))) {
        
        cat("\n!!! WARNING: Optimizer failed to converge (NA/Inf detected) !!!\n")
        formatted_theta <- paste0(
          sprintf("  %-20s : %.4f", names(proposed_par), proposed_par), 
          collapse = "\n"
        )
        cat(formatted_theta, "\n")
        cat(rep("-", 45), "\n")

      } else {
        
        # 2. Success: Update current_par_opt for the next vintage
        current_par_opt <- proposed_par
        
        formatted_theta <- paste0(
          sprintf("  %-20s : %.4f", names(current_par_opt), current_par_opt), 
          collapse = "\n"
        )
        cat(formatted_theta, "\n")
        cat(rep("-", 45), "\n")
    }
  }
  
  
  
  # 3. Extract and Transform Results
  # Final mapping back to Economic Space (Named List)
  final_params_econ <- param2model_gen(current_par_opt, ssm)
  
  # 1. Format the parameter names and values into a clean list
  # This calculates the width needed to align the ":" signs
  formatted_params <- paste0(
    sprintf("  %-20s : %.6f", names(final_params_econ), final_params_econ), 
    collapse = "\n"
  )
  
  # 2. Construct the full message
  cat("\n", rep("=", 45), "\n", sep = "")
  cat("ESTIMATED ECONOMETRIC PARAMETERS (Economic Space)\n")
  cat(rep("-", 45), "\n", sep = "")
  cat(formatted_params, "\n")
  cat(rep("=", 45), "\n", sep = "")
  message("Optimization successful. Results stored.")
  
  return(list(
    params = final_params_econ, # Economic scale (Named List)
    theta  = current_par_opt,   # Unconstrained scale (Vector for next warm start)
    fit_summary = fit,          # The last optimx result object
    ssm = ssm
  ))
}














#===============================================================================
# Rolling Estimation Functions
#===============================================================================

# Okun I can still implement the HP filter function

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
rolling_est_okun_old_ssm <- function(data,
                                     forecast_start,
                                     forecast_end = NULL,
                                     date_col = "quarter",
                                     val_T1 = "1991-01-01") {
  
  # Assure correct format
  data[[date_col]] <- as.yearqtr(data[[date_col]])
  
  # Select the starting quarter
  start_q <- as.yearqtr(as.Date(forecast_start))
  
  # Sleect T = 0
  val_T1 <- as.yearqtr(as.Date(val_T1))
  
  # Either estimate until last obs or estimate until a given date
  if(is.null(forecast_end)) {
    end_q <- max(data[[date_col]], na.rm = TRUE)
  } else {
    end_q <- as.yearqtr(as.Date(forecast_end))
  }
  
  # Define the sequence of "Vantage Points" (The end of the period on which we estimate)
  # Is the today (end of information set) for the forecast
  forecast_dates <- data[[date_col]][data[[date_col]] >= start_q & data[[date_col]] <= end_q] 
  
  comp <- list()
  
  # Initial setup for the first warm start
  # We initialize with NULL so the wrapper uses manifest defaults for the first run
  current_theta <- NULL 
  
  message(sprintf("Rolling Estimation: %s to %s", start_q, end_q))
  
  # This always estimates from val_T1 to the forecast vantage point -> today
  # run parameter estimation on the full information set
  for (i in seq_along(forecast_dates)) {
    target_date <- forecast_dates[i]
    message("\n--- Vantage Point: ", target_date, " ---")
    
    # Slice Data available "Today"
    data_t <- data[data[[date_col]] <= target_date, ]
    
    # Process Exogenous Data
    # HP Filter is estimated inclduing the burn in period before the start of the information set
    
    # Error if there ar not enough valid gdp obs 
    valid_gdp_indices <- which(!is.na(data_t$log_gdp))
    if (length(valid_gdp_indices) < 5) {
      message("Skipping ", target_date, ": Not enough GDP data points.")
      next
    }
    
    # select GDP series
    first_obs_idx <- valid_gdp_indices[1]
    gdp_series    <- data_t$log_gdp[first_obs_idx:nrow(data_t)]
    
    # Run filter
    hp_res    <- mFilter::hpfilter(gdp_series, freq = 1600)
    gdp_cycle <- as.numeric(hp_res$cycle) * 100
    
    # Align cycle back to the time-series and create lags
    processed_data <- data_t
    processed_data$gap_l0 <- NA_real_
    processed_data$gap_l0[first_obs_idx:nrow(data_t)] <- gdp_cycle
    
    processed_data <- processed_data %>%
      mutate(
        gap_l1 = dplyr::lag(gap_l0, 1),
        gap_l2 = dplyr::lag(gap_l0, 2)
      ) %>%
      # Filter for Estimation Window
      filter(quarter >= val_T1) %>%
      filter(complete.cases(unemp_rate, gap_l0, gap_l1, gap_l2))
    
    if (nrow(processed_data) < 5) {
      message("Skipping ", target_date, ": No valid overlapping observations.")
      next
    }
    
    # Build Data Matrices
    Y_final <- as.matrix(processed_data[, c("unemp_rate", "spf_5y_unemp")])
    X_final <- as.matrix(processed_data[, c("gap_l0", "gap_l1", "gap_l2")])
    
    message(sprintf("Estimation range: %s to %s (%d obs)", 
                    min(processed_data$quarter), max(processed_data$quarter), nrow(processed_data)))
    
    # Initialize Blueprint
    my_ssm_model <- initialize_my_okun_ssm(Y_final,
                                           X_final,
                                           parameter_guesses = okun_parameter_guess)
    
    # Optimization: Multiple estimations with Warm Start
    # We use Nelder-Mead and BFGS that are very different for robustnes
    # each previous result becomes guess for the next
    opt_results <- ssm_optimizer_wrapper_okun(
      ssm       = my_ssm_model, 
      methods   = c("Nelder-Mead", "BFGS"), 
      iters     = 2, 
      start_par = current_theta
    )
    
    # Update current_theta for the next vantage point (Warm Start)
    current_theta <- opt_results$theta
    
    if(any(is.na(current_theta))) {
      message("Warning: Optimizer crashed. Resetting to manifest defaults.")
      current_theta <- NULL
    }
    
    
    # 8. Final Extraction of States
    final_states <- loglik_ssm_okun(current_theta, my_ssm_model, return_full_res = TRUE)
    
    # Store results
    comp[[as.character(target_date)]] <- list(
      target_date = target_date,
      params      = opt_results$params,
      states      = final_states
    )
    
    cat("Likelihood: ", -final_states$loglik, "Parameters", current_theta)
    cat(sprintf("\n [%d/%d] Estimated: %s\n", i, length(forecast_dates), as.character(target_date)))
  }
  
  return(comp)
}


