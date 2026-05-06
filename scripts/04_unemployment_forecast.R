################################################################################
#
# This script estimates the parameters for the Okun model and runs the forecast
#
################################################################################


# ==============================================================================
# DATA PREPARATION: HP Filter, Lagging, and Final Alignment
# ==============================================================================

# Start by creating merged dataframe
# (Contains 'quarter', 'log_gdp', 'unemp_rate', 'spf_5y_unemp')
df_okun_merged <- X_okun %>%
  filter(quarter >= hp_filter_burn_in) %>%
  left_join(Y_okun, by = "quarter")

# Select the Estimation range
non_na_range <- which(complete.cases(df_okun_merged[,colnames(X_okun)]))

# Determine the available datarange, as the X data can't be NA
first_valid <- min(non_na_range)
last_valid <- max(non_na_range)

# Get range of X Data
# This does not check in the Y column as the Kalman filter can handle NA in these
# Positions
first_date <- df_okun_merged$quarter[min(non_na_range)]
last_date  <- df_okun_merged$quarter[max(non_na_range)]

# Print the readable range
message("Save estimation range: ", first_date, " to ", last_date)

# Select available data range
df_okun_final <- df_okun_merged[first_valid:last_valid, ]

# get the the external forecasts for the output gap. This is currently just the true observations
# will later be replaced with true forecasts
gdp_gap_forecasts <- df_okun_final[c("quarter", "log_gdp")]


# ==============================================================================
# Estimate Parameters
# ==============================================================================


if(run_estimation){
  message("Starting Estimation of Model 1: Unemployment...")
  # The parameters are estimate in a rolling scheme, i.e from the T = 0 to each date
  # from first valid to last valid
  params_okun <- rolling_est_okun_ssm(df_okun_merged, forecast_start = forecast_starting_date)
  
  # Extract the parameters from the results and put them in the correctly formated dataframe
  params_okun_df <- extract_params_df(params_okun)
  
  write_csv(params_okun_df, output_save_paths$params$rolling_param_est_okun)
  
} else {
  message("run_estimation set to FASE. Loading Unemployment Model Parameters from Disk...")
  params_okun_df <- read_csv(output_save_paths$params$rolling_param_est_okun)
  
}




# ==============================================================================
# Create the Actual Forecast
# ==============================================================================

# Use the estimated parameters from the last step to forecast with the forecasted output gaps
forecast_okun_df <- forecast_okun_ssm(params_df = params_okun_df, Y_data_df = Y_okun)

# save the results
write_csv(forecast_okun_df, output_save_paths$forecasts$forecast_df_okun)

forecast_okun_df <- read_csv(output_save_paths$forecasts$forecast_df_okun)

# Turn the Dataframe into a square where we know the true values
# (Normal DF has h rows more than columns, we drop them by dropping dates without observations)
last_origin <- ncol(forecast_okun_df)

okun_eval_square <- forecast_okun_df[1:last_origin, 1:last_origin]

# CheckThe number of rows should now equal the number of columns
dim(okun_eval_square)




