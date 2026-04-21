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

# The parameters are estimate in a rolling scheme, i.e from the T = 0 to each date
# from first valid to last valid
results <- rolling_est_okun_ssm(df_okun_merged, forecast_start = "2022-07-01")

# Extract the parameters from the results and put them in the correctly formated dataframe
okun_params_df_new <- extract_params_df(results)

# View the result
print(head(okun_params_df_new))
write_csv(okun_params_df_new, "output/okun_forecast_parameters.csv")


# ==============================================================================
# Create the Actual Forecast
# ==============================================================================

# Use the estimated parameters from the last step to forecast with the forecasted output gaps
forecast_okun_df <- forecast_okun_ssm(params_df = okun_params_df_new, Y_data_df = Y_okun)

# save the results
write_csv(forecast_okun_df, "output/forecast_df.csv")

# Turn the Dataframe into a square where we know the true values
# (Normal DF has h rows more than columns, we drop them by dropping dates without observations)
last_origin <- as.yearqtr(tail(colnames(forecast_okun_df), 1))

# turn the data frame into a quadratic matrix for estimation
okun_eval_square <- forecast_okun_df[as.yearqtr(rownames(forecast_okun_df)) <= last_origin, ]

# CheckThe number of rows should now equal the number of columns
dim(okun_eval_square)
