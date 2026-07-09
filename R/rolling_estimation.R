

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
rolling_est_okun_ssm <- function(data,
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
    
    Y_matrix <- data_t %>%
      select(all_of(c("quarter", "unemp_rate", "spf_5y_unemp")))
    
    # Process Exogenous Data
    # HP Filter is estimated inclduing the burn in period before the start of the information set
    
    # Error if there ar not enough valid gdp obs 
    valid_gdp_indices <- which(!is.na(data_t$log_gdp))
    
    # select GDP series
    first_obs_idx <- valid_gdp_indices[1]
    
    
    processed_data <- get_hp_gap(data = data_t, 
                                 gdp_forecast_data = gdp_forecasts_arima,
                                 vantage_q         = target_date,
                                 return_type = "history")
    
    processed_data <- processed_data %>%
      rename(gap_l0 = gap) %>% # Map output back to your expected lag-base handle cleanly
      mutate(
        gap_l0 = gap_l0 * 100,
        gap_l1 = dplyr::lag(gap_l0, 1),
        gap_l2 = dplyr::lag(gap_l0, 2)
      ) %>%
      filter(quarter >= val_T1) %>%
      left_join(Y_matrix, by = "quarter") %>%
      # Ensure unemp_rate checks against the real column name inside complete.cases
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
    opt_results <- ssm_optimizer_wrapper_core(
      methods = estimation_settings$okun$methods,
      iters =  estimation_settings$okun$iters,
      ssm       = my_ssm_model, 
      start_par = current_theta
    )
    
    # Update current_theta for the next vantage point (Warm Start)
    current_theta <- opt_results$theta
    
    if(any(is.na(current_theta))) {
      message("Warning: Optimizer crashed. Resetting to manifest defaults.")
      current_theta <- NULL
    }
    
    
    # 8. Final Extraction of States
    final_states <- loglik_ssm_core(current_theta,
                                    my_ssm_model,
                                    return_full_res = TRUE)
    
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




# ==============================================================================
# Rolling Estimation Loop PHILLIPS
# ==============================================================================

rolling_est_philips_ssm <- function(data,
                                    forecast_start,
                                    forecast_end = NULL,
                                    date_col = "quarter",
                                    val_T1 = "1990-01-01",
                                    break_warm_start = TRUE
) {
  
  # Assure correct format
  data[[date_col]] <- as.yearqtr(data[[date_col]])
  
  # Select the starting quarter
  start_q <- as.yearqtr(forecast_start)
  
  # Sleect T = 0
  val_T1 <- as.yearqtr(val_T1)
  
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
    
    data_t <- as.data.frame(build_data_matrix_philips(data = master_philips,
                                                      vantage_quarter = target_date,
                                                      T_0 = val_T1))
    
    # Slice Data available "Today"
    data_t <- data_t[data_t[[date_col]] <= target_date, ]
    
    
    
    # Build Data Matrices
    Y_final <- as.matrix(data_t[, c("log_inflation_diff", "5y_cpi_forecast")])
    X_final <- as.matrix(data_t[, c("gdp_gap", "lop_gap")])
    
    message(sprintf("Estimation range: %s to %s (%d obs)", 
                    min(data_t$quarter), max(data_t$quarter), nrow(data_t)))
    
    # Initialize Blueprint
    my_ssm_model <- initialize_my_philips_ssm(Y_final,
                                              X_final,
                                              parameter_guesses = philips_parameter_guess)
    
    
    message("SSM PHILIPS INIT SUCCESSFUL")
    
    # Optimization: Multiple estimations with Warm Start
    # We use Nelder-Mead and BFGS that are very different for robustnes
    # each previous result becomes guess for the next
    
    print(current_theta)
    
    opt_results <- ssm_optimizer_wrapper_core(
      methods = estimation_settings$phillips$methods,
      iters =  estimation_settings$phillips$iters,
      ssm       = my_ssm_model, 
      start_par = current_theta
    )
    
    # Update current_theta for the next vantage point (Warm Start)
    current_theta <- opt_results$theta
    
    final_states <- loglik_ssm_core(current_theta,
                                    my_ssm_model,
                                    return_full_res = TRUE)
    
    # Store results safely
    comp[[as.character(target_date)]] <- list(
      target_date = target_date,
      params      = opt_results$params,
      states      = final_states
    )
    
    # Safe console tracking printout
    if(is.list(final_states)) {
      cat("Likelihood: ", -final_states$loglik, "Parameters: ", current_theta)
    } else {
      cat("Likelihood: SAFEGARD HIT (Penalty Value:", final_states, ") Parameters: ", current_theta)
    }
    cat(sprintf("\n [%d/%d] Estimated: %s\n", i, length(forecast_dates), as.character(target_date)))
    
    if(break_warm_start){
      current_theta <- NULL
    }
    
  }
  
  return(comp)
}





rolling_est_taylor_ssm <- function(data,
                                   forecast_start,
                                   forecast_end = NULL,
                                   date_col = "quarter",
                                   val_T1 = "1991-01-01") {
  
  # Assure correct format
  
  data <- as.data.frame(data)
  
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
    valid_inf_indices <- which(!is.na(data_t$log_cpi))
    if (length(valid_inf_indices) < 5) {
      message("Stopping: Not enough CPI data points.")
      stop()
    }
    
    # select GDP series
    first_obs_idx <- valid_inf_indices[1]
    inf_series    <- data_t$log_cpi[first_obs_idx:nrow(data_t)]
    
    gdp_gap_data <- get_hp_gap(data = data_t,
                               gdp_forecast_data = gdp_forecasts_arima, # forecast data
                               vantage_q = target_date
    )
    gdp_gap_data$gap <- gdp_gap_data$gap * 100
    
    
    inf_gap_data <- data_t %>%
      select(all_of(c("quarter", "inf_gap")))
    
    
    
    
    
    processed_data <- data_t %>%
      select(quarter, saron_libor_splice, forward_rate, yoy_inf) %>%
      left_join(gdp_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
      left_join(inf_gap_data %>% select(quarter, inf_gap), by = "quarter") %>%
      filter(quarter >= as.yearqtr(val_T1)) %>%
      arrange(quarter) %>%
      filter(complete.cases(gdp_gap, inf_gap))
    
    
    if (nrow(processed_data) < 20) { # 20 is a safe minimum for a 3-state SSM
      message(sprintf("Skipping %s: Insufficient observations after filtering (%d obs)", 
                      target_date, nrow(processed_data)))
      next # Skip to the next quarter in the rolling loop
    }
    
    
    # Build Data Matrices
    Y_final <- as.matrix(processed_data[, c("saron_libor_splice", "forward_rate")])
    X_final <- as.matrix(processed_data[, c("gdp_gap", "inf_gap")])
    
    message(sprintf("Estimation range: %s to %s (%d obs)", 
                    min(processed_data$quarter), max(processed_data$quarter), nrow(processed_data)))
    
    # Initialize Blueprint
    my_ssm_model <- initialize_taylor_ssm(Y_data = Y_final,
                                          X_data =X_final,
                                          parameter_guesses = snb_rate_parameter_guess)
    
    # Optimization: Multiple estimations with Warm Start
    # We use Nelder-Mead and BFGS that are very different for robustnes
    # each previous result becomes guess for the next
    opt_results <- ssm_optimizer_wrapper_core(
      methods = estimation_settings$taylor$methods,
      iters =  estimation_settings$taylor$iters,
      ssm = my_ssm_model,
      start_par = current_theta
    )
    
    # Update current_theta for the next vantage point (Warm Start)
    current_theta <- opt_results$theta
    
    if(any(is.na(current_theta))) {
      message("Warning: Optimizer crashed. Resetting to manifest defaults.")
      current_theta <- NULL
    }
    
    
    # 8. Final Extraction of States
    final_states <- loglik_ssm_core(current_theta,
                                    my_ssm_model,
                                    return_full_res = TRUE)
    
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

