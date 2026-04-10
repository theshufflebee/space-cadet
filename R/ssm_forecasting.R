################################################################################
#
# These functions are used to create forecast
#
################################################################################

# Pseudocode
library(mFilter)

#' Generate Rolling SSM Parameters for Forecasting
#' 
#' Estimates model parameters over an expanding window up to a specified end date.
#' Returns a list of parameters to be used in a separate forecasting step.
#' 
#' We have T = 1 as the start of the data, T=T is the end of the data,
#' T=t as the current date up until we have the data availabe for the pseudo forecast
#' and t+h would then be the forecast horizon.
#' What we do here is just get the parameter estimation at time t
#' 
#' @param data The Dataframe that contains full data$
#' @param y_cols The Name of the Y column(s) in the data dataframe
#' @param x_col The column that contains the data for exogenous variales
#' @param date_col the column name of the date column
#' @param spec the okun model specification
#' @param all_defaults the default guesses
#' @param forecast_start The date where the forecast stats, limit of the available information set
#' @param val_T1 the beginning of the forecast information set
#' 
#' 
get_ssm_forecast_parameters <- function(data,
                                        y_cols,
                                        x_col,
                                        date_col = "quarter",
                                        all_builder_functions,
                                        spec,
                                        all_defaults,
                                        forecast_start,
                                        val_T1 = "2015-01-01", # The beginning of the dataset we use to forecast
                                        forecast_end = NULL) {
  
  # Force the data's date column to yearqtr to prevent format issues
  data[[date_col]] <- as.yearqtr(data[[date_col]])
  
  # create date_vector
  full_date_vector      <- data[[date_col]]
  
  # Start of the pseudo forecast
  start_q <- as.yearqtr(as.Date(forecast_start))
  
  
  # First observation of the full sample
  val_T1 <- as.yearqtr(as.Date(val_T1))
  
  # If there is no forecast date end specified do as far as possible
  # else go until specifed date
  if(is.null(forecast_end)) {
    end_q <- max(full_date_vector, na.rm = TRUE)
  } else {
    end_q <- as.yearqtr(as.Date(forecast_end))
  }
  
  message("estimation between", start_q, "and", end_q)
  
  forecast_dates <- full_date_vector[full_date_vector >= start_q & full_date_vector <= end_q]
  
  comp <- list()
  current_theta <- model2param_gen(all_defaults, spec)
  
  message(sprintf("Running rolling estimation from %s to %s...", 
                  as.character(start_q), as.character(end_q)))
  
  # loop over all forecasting dates
  for (i in seq_along(forecast_dates)) {
    target_date <- forecast_dates[i]
    
    message("target date: ", target_date) # This is today
    
    # select subset of data available at the time
    data_t <- data[full_date_vector <= target_date, ] #here only take the data available at t
    
    # Select the first non NA observation of the Exogenous / GDP variable
    first_obs_idx <- which(!is.na(data_t[[x_col]]))[1]
    gdp_series    <- data_t[[x_col]][first_obs_idx:nrow(data_t)]
    
    # C. Apply HP Filter (mFilter package)
    # freq = 1600 is standard for quarterly data
    hp_res <- hpfilter(gdp_series, freq = 1600)
    
    # D. Align Cycle back to vintage dataframe
    data_t$y_gap <- NA
    data_t$y_gap[first_obs_idx:nrow(data_t)] <- as.numeric(hp_res$cycle)
    print(data_t$quarter)
    
    data_estimation <- data_t %>%
      mutate(
        y_gap_l0 = as.numeric(y_gap),
        y_gap_l1 = as.numeric(dplyr::lag(y_gap, 1)),
        y_gap_l2 = as.numeric(dplyr::lag(y_gap, 2))
      ) %>%
      filter(!!sym(date_col) >= val_T1)
    
    
    Y_select <- as.matrix(data_estimation[, y_cols])
    X_select <- as.matrix(data_estimation[, c("y_gap_l0", "y_gap_l1", "y_gap_l2")])
    
    print(head(X_select))
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




