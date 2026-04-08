################################################################################
#
# These functions are used to create forecast
#
################################################################################

# Pseudocode

#' Generate Rolling SSM Parameters for Forecasting
#' 
#' Estimates model parameters over an expanding window up to a specified end date.
#' Returns a list of parameters to be used in a separate forecasting step.
get_ssm_forecast_parameters <- function(data,
                                        y_cols,
                                        x_cols,
                                        date_col = "quarter",
                                        all_builder_functions,
                                        spec,
                                        all_defaults,
                                        forecast_start,
                                        forecast_end = NULL) {
  
  # 1. Force the data's date column to yearqtr
  date_vector <- as.yearqtr(data[[date_col]])
  
  # 2. Force start_q safely (The "Double Conversion")
  # We use as.Date to handle the string, then yearqtr to handle the class
  start_q <- as.yearqtr(as.Date(forecast_start))
  
  if(is.null(forecast_end)) {
    end_q <- max(date_vector, na.rm = TRUE)
    message(sprintf("No end date provided. Defaulting to last available: %s", as.character(end_q)))
  } else {
    # Same safety here
    end_q <- as.yearqtr(as.Date(forecast_end))
  }
  
  # 3. CRITICAL: Use which() or !is.na to avoid generating NA vectors
  # This ensures forecast_dates only contains actual matches
  mask <- which(date_vector >= start_q & date_vector <= end_q)
  forecast_dates <- date_vector[mask]
  
  # Debugging: If this is still 0, the dates just don't overlap
  if(length(forecast_dates) == 0) {
    stop(sprintf("No dates found between %s and %s in the dataset!", 
                 as.character(start_q), as.character(end_q)))
  }
  
  print("Confirmed Forecast Dates:")
  print(forecast_dates)
  print(start_q)
  print(end_q)
  
  # select the dates over whicht to estinate parameters
  #forecast_dates <- date_vector[date_vector >= start_q & date_vector <= end_q]

  comp <- list()
  
  #initialize initial guess
  current_theta <- model2param_gen(all_defaults, spec)
  
  message(sprintf("Running rolling estimation from %s to %s...", 
                  as.character(start_q), as.character(end_q)))
  
  # loop over all forecasting dates
  for (i in seq_along(forecast_dates)) {
    target_date <- forecast_dates[i]
    
    # select subset of data
    data_t   <- data[date_vector <= target_date, ]
    Y_select <- as.matrix(data_t[, y_cols])
    X_select <- as.matrix(data_t[, x_cols])
    
    # re initualize the builders as the time dimension changes
    curr_okun_fact  <- mu_t_matrix_factory(X_select, Y_select, intercept = FALSE)
    curr_trans_fact <- H_matrix_factory(random_walk = TRUE)
    curr_link_fact  <- G_matrix_factory(Y_select)
    curr_noise_fact <- M_matrix_factory(Y_select)
    
    # Run the wrapper to find parameters
    # reusing parameters from previous try so only 2 loops
    output <- ssm_optimizer_wrapper(
      nb_loop      = 2, 
      theta_start  = current_theta,
      Y            = Y_select,
      X            = X_select,
      model_spec   = spec,
      mu_t_builder = curr_okun_fact$builder,
      H_builder    = curr_trans_fact$builder,
      G_builder    = curr_link_fact$builder,
      M_builder    = curr_noise_fact$builder
    )
    
    # result storage in the list
    # Store the human-readable parameters and the raw theta for next loop

    # update theta for next loop
    current_theta <- output$theta
    
    #-------------------------------

    # 
    # Call the function again with return_full_res = TRUE
    # so we get the full output with the optimal parameters
    # this is so we can for example extract the natural rate later for forecasting
    final_output <- loglik_ssm(theta = current_theta, Y = Y_select, 
                               model_spec = spec,
                               mu_t_builder = curr_okun_fact$builder,
                               H_builder    = curr_trans_fact$builder,
                               G_builder    = curr_link_fact$builder,
                               M_builder    = curr_noise_fact$builder,
                               return_full_res = TRUE)
    
    comp[[as.character(target_date)]] <- list(
      params = output$params,   # parameters
      states = final_output     # The full Kalman filter object containing all data of the optimal parameter run
    )
    
    #----------------------------------------
    
    
    cat(sprintf("[%d/%d] Estimated: %s\n", i, length(forecast_dates), as.character(target_date)))
  }
  
  return(comp)
}


# FOrecast





