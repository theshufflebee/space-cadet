################################################################################
#
# This script estimates the parameters for the Okun model and runs the forecast
#
################################################################################


# ==============================================================================


# ==============================================================================
# DATA PREPARATION: HP Filter, Lagging, and Final Alignment
# ==============================================================================

# Start by creating merged dataframe
# (Contains 'quarter', 'log_gdp', 'unemp_rate', 'spf_5y_unemp')
df_okun_merged <- X_okun %>%
  filter(quarter >= hp_filter_burn_in) %>%
  left_join(Y_okun, by = "quarter")

#selectrange
non_na_range <- which(complete.cases(df_okun_merged[,colnames(X_okun)]))

# Determine the available datarange
first_valid <- min(non_na_range)
last_valid <- max(non_na_range)

# message("save estimation range: ", first_valid, "to", last_valid)

# Select available data range
df_okun_final <- df_okun_merged[first_valid:last_valid, ]


# --- Run estimation ---
results <- new_get_params_okun_ssm(df_okun_merged, forecast_start = "2024-07-01")


okun_params_df_new <- extract_params_df(results)

# View the result
print(head(okun_params_df_new))



# ==============================================================================
# CURREN WORK IN PROGRESS HERE

# currently forecasts is a vector, will eventually be a dataframe with each col a vantage point

forecast_okun_ssm <- function(params_df,                  
                              date_col = "quarter",
                              exog_var_col = "log_gdp",
                              forecast_h = 8,
                              Y_data_df = Y_okun,
                              target_variable = "unemp_rate",
                              X_data = X_okun,
                              gdp_gap_forecasts_input = gdp_gap_forecasts) { # Renamed argument
  
  # Clean prepare params_df
  parameters <- params_df %>%
    mutate(quarter = as.yearqtr(!!sym(date_col)))
  
  dates <- parameters %>% pull(!!sym(date_col))
  
  # Setup Eval Matrix
  extended_rows <- seq(min(dates), by = 0.25, length.out = length(dates) + forecast_h)
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(extended_rows)
  colnames(eval_mat) <- as.character(dates)
  
  # Align Actuals -> true ons in diag
  actual_unemp_df <- Y_data_df %>%
    filter(quarter %in% dates) %>%
    arrange(quarter)
  
  for(i in seq_along(dates)) {
    date_str <- as.character(dates[i])
    eval_mat[date_str, i] <- actual_unemp_df[[target_variable]][i]
  }
  
  #Main Forecast Loop -> loop over dates and each time
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    
    # Select params for this vintage
    current_params <- parameters[i, ]
    # Check if column is named natural_rate or natural_rate_final from your previous step
    rho_T <- current_params$natural_rate 
    betas <- c(current_params$beta1, current_params$beta2, current_params$beta3)
    
    # 5. Build the GDP path (History + External Forecasts)
    history_gdp <- X_data %>%
      filter(quarter <= forecast_origin) %>% 
      select(quarter, all_of(exog_var_col))
    
    future_gdp <- gdp_gap_forecasts_input %>%
      filter(quarter > forecast_origin) %>% 
      slice(1:forecast_h) %>%
      select(quarter, all_of(exog_var_col))
    
    combined_gdp <- bind_rows(history_gdp, future_gdp) %>%
      arrange(quarter) %>%
      filter(!is.na(!!sym(exog_var_col)))
    
    # Re-calculate HP Cycle (The Pseudo-Real-Time approach)iincluding forecasts
    hp_res <- mFilter::hpfilter(combined_gdp[[exog_var_col]], freq = 1600)
    combined_gdp$y_gap <- as.numeric(hp_res$cycle)
    
    # Create Lags and extract only the forecast horizon
    gdp_features_wide <- combined_gdp %>%
      mutate(
        gdp_gap  = y_gap,
        gap_lag1 = dplyr::lag(y_gap, 1),
        gap_lag2 = dplyr::lag(y_gap, 2)
      ) %>%
      filter(quarter > forecast_origin) %>%
      arrange(quarter) %>%
      select(gdp_gap, gap_lag1, gap_lag2)
    
    # Generate paths / forecasts and store
    if (nrow(gdp_features_wide) > 0) {
      h_available <- nrow(gdp_features_wide)
      
      # Predict ssm is simple matrix opertaion
      path <- predict_ssm_path_rw(rho_T, betas, gdp_features_wide)
      
      look_ahead_dates <- seq(forecast_origin + 0.25, by = 0.25, length.out = h_available)
      target_rows <- as.character(look_ahead_dates)
      eval_mat[target_rows, i] <- path
    }
  }
  
  return(as.data.frame(eval_mat))
}




gdp_gap_forecasts <- df_okun_final[c("quarter", "log_gdp")]



forecast_okun_df <- forecast_okun_ssm(params_df = okun_params_df_new, Y_data_df = Y_okun)
