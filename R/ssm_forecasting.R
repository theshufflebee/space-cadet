################################################################################
#
# These functions are used to create forecast
#
################################################################################


#' Function to predict the random walk ssm with exogenous betas
#' 
#' @param who_start the last state observation from the estimation
#' @param betas the estimated beta coefficients as dataframe or matrix
#' @param exog_features exogenous features corresponding to betas as matrix or dataframe
#' 
#' This function takes the last state variable and adds the exogenous component
#' built from using a matrix multiplication of exogenous features and betas and
#' adding the state.
#' 
#' gdp features is a data frame where each row is a time in h and each column is a lag
#' 
predict_ssm_path_rw <- function(rho_start, betas, exog_features) {
  cyclical_impact <- as.matrix(exog_features) %*% matrix(betas)
  return(as.numeric(rho_start + cyclical_impact))
}


#' This function runs the actual forecast for the okun model
#' 
#' @param params_df a dataframe containing all necessary parameters for forecsting
#' and the final state estimate
#' @param date_col the name of the date column in the data_df
#' @param exog_var_col name of the exogenous fata (gdp) column
#' @param forecast_h forecast horizon, how many quarters ahead we should forecast
#' @param Y_data_df the dataframe containing dependent Y observed data
#' @param target_variable name of the dependent variable that we want to forecast
#' @param X_data Independent regressors added to the data
#' @param gdp_gap_forecasts External forecasts that are used as a forecast in the external regressors
#' 
#' @returns A dataframe whith the forecats of the selected target variable. The fdiagonal contains the
#' true observed variables, the values below are the forecats at the column date for the rowdate
#' 
#' The goal of the function is a forecast of the unemployment rate using an Okun model,
#' where the gdp gap acts as an external regressor. This function uses as input a parameter df
#' which contains estimated up to a certain horizon for a a series of dates. then the function loops over all these dates
#' and runs a pseudo out of sample forecast.
#' 
#' The difficulty in this function is the handling of the external regressors. At each date of the pseudo forecast,
#' (vantage point) we have to rerun the HP filter to get the cycles we'd have had at that time. To reliably
#' forecast the GDP gap we also use an forecast for the next h periods of gdp that we fuse together with the observed gdp
#' and then run the filter. We also use a burn in for the HP filter. This means that the estimation window starts in 2015 for this model
#' we start calculating the HP filter in 2010. 
#' 
#' @seealso [get_ssm_forecast_parameters()]
#' @seealso [predict_ssm_path_rw()]
forecast_okun_ssm <- function(params_df,
                              date_col = "quarter",
                              exog_var_col = "log_gdp",
                              forecast_h = 8,
                              Y_data_df = Y_okun,
                              target_variable = "unemp_rate",
                              X_data = X_okun,
                              gdp_gap_forecasts = gdp_gap_forecasts) {
  
  parameters <- params_df %>%
    mutate(quarter = as.yearqtr(quarter))
  
  # Select dates that will be forecast
  dates <- parameters %>%
    pull(all_of(date_col))# dates are all dates where we forecast
  
  # Since we will forecast beyond known dates we extend the dataframe
  extended_rows <- seq(min(dates), by = 0.25, length.out = length(dates) + forecast_h)
  
  # Build the matrix for storage
  # Dim: number of forecast dates + h rows and number of forecast dates columns
  # forecast dates are all the dates we make a forecast from
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(extended_rows)
  colnames(eval_mat) <- as.character(dates)
  
  # The dataframe with the actual data
  actual_unemp_df <- Y_data_df %>%
    filter(quarter %in% dates) %>%
    arrange(quarter)
  
  # loop over each diagonal value and fill them in with the true values
  for(i in seq_along(dates)) {
    date_str <- as.character(dates[i])
    eval_mat[date_str, i] <- actual_unemp_df[[target_variable]][i]
  }
  
  # Loop over dates to build forecasts
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] # select start date
    
    # Select parameters / state for this vintage
    current_params <- parameters[i, ]
    rho_T <- current_params$natural_rate
    betas <- c(current_params$beta1, current_params$beta2, current_params$beta3)
    
    # get 8 quarters after "today" the origin
    look_ahead_dates <- seq(forecast_origin + 0.25, by = 0.25, length.out = forecast_h)
    
    # get the historical true data known today
    history_gdp <- X_data %>%
      filter(quarter <= forecast_origin) %>% 
      select(quarter, all_of(exog_var_col))
    
    future_gdp <- gdp_gap_forecasts %>%
      filter(quarter > forecast_origin) %>% 
      slice(1:forecast_h) %>%
      select(quarter, all_of(exog_var_col))
    
    combined_gdp <- bind_rows(history_gdp, future_gdp) %>%
      arrange(quarter) %>%
      filter(!is.na(.data[[exog_var_col]]))
    
    # Take HP filter over the fpast and predicted future gdp
    hp_res <- hpfilter(combined_gdp[[exog_var_col]], freq = 1600)
    combined_gdp$y_gap <- as.numeric(hp_res$cycle)
    
    # Create the 3 Lag Columns
    gdp_features_wide <- combined_gdp %>%
      mutate(
        gdp_gap  = y_gap,
        gap_lag1 = dplyr::lag(y_gap, 1),
        gap_lag2 = dplyr::lag(y_gap, 2)
      ) %>%
      # Select only the horizon we want to forecast
      filter(quarter > forecast_origin) %>%
      arrange(quarter) %>%
      select(gdp_gap, gap_lag1, gap_lag2)
    
    
    # get forecast
    if (nrow(gdp_features_wide) > 0) {
      h_available <- nrow(gdp_features_wide)
      # currently here for end of horizon where we have less than 8 known dates
      # later we use the gdp forecasts and can run that easily
      
      # Use Random walk ssm prediction function
      path <- predict_ssm_path_rw(rho_T, betas, gdp_features_wide)
      
      # Add to matrix
      target_rows <- as.character(look_ahead_dates[1:h_available])
      eval_mat[target_rows, i] <- path
    }
  }
  
  eval_df <- as.data.frame(eval_mat)
  
  # Final Result
  eval_df <- as.data.frame(eval_mat)
  
  return(eval_df)
  
}
