
################################################################################
#
# OKUN FORECATSTS
#
################################################################################

# Use the estimated parameters from the last step to forecast with the forecasted output gaps
Y_okun <- master_okun %>%
  select(quarter, unemp_rate)

X_okun <- master_okun %>%
  select(quarter, log_gdp)

forecast_okun_df <- forecast_okun_ssm(params_df = params_okun_df,
                                      Y_data_df = Y_okun,
                                      X_data = X_okun,
                                      gdp_gap_forecasts_input = gdp_forecasts_arima
)

# save the results
write_csv(forecast_okun_df, output_save_paths$forecasts$forecast_df_okun)

forecast_okun_df <- read_csv(output_save_paths$forecasts$forecast_df_okun, 
                             show_col_types = FALSE)

# Turn the Dataframe into a square where we know the true values
# (Normal DF has h rows more than columns, we drop them by dropping dates without observations)
last_origin <- ncol(forecast_okun_df)

okun_eval_square <- forecast_okun_df[1:last_origin, 1:last_origin]

# CheckThe number of rows should now equal the number of columns
dim(okun_eval_square)


################################################################################
# 
# PHILLIPS FORECASTS
#
################################################################################

# Run the Forecasts based on the Params df
philips_forecast_df <- forecast_philips_ssm(phillips_parmas_df,                  
                                            date_col = "quarter",
                                            master_df = master_philips,
                                            forecast_h = 8,
                                            exogenous_gdp_forecast_data = gdp_forecasts_arima)

# Save the Forecasts
write_csv(as.data.frame(philips_forecast_df), output_save_paths$forecasts$forecast_df_philips)
philips_forecast_df <- read_csv(output_save_paths$forecasts$forecast_df_philips)


last_origin <- ncol(philips_forecast_df)

philips_eval_square <- philips_forecast_df[1:last_origin, 1:last_origin]

# CheckThe number of rows should now equal the number of columns
dim(philips_eval_square)


# Prepare for use as exog forecast inputs and also Fcst tests
colnames(philips_eval_square) <- format(as.yearqtr(colnames(philips_eval_square)), "%YQ%q")

rownames(philips_eval_square) <- format(as.yearqtr(colnames(philips_eval_square)), "%YQ%q")

fcst_df_inf <- philips_eval_square %>%
  rownames_to_column(var = "date") %>%
  as.data.frame()


# Format the date column to expected format
fcst_df_inf$date <- format(zoo::as.yearqtr(fcst_df_inf$date), format = "%YQ%q")

################################################################################
# 
# TAYLOR FORECASTS
#
################################################################################

# Load Arima FOrecasts
inf_arima_fcst_df <- read_csv(output_save_paths$forecasts$forecast_df_inf_arima) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)


final_inflation_vintages <- merge_inflation_forecast_vintages(
  arima_df = inf_arima_fcst_df, # Your baseline output
  ssm_df   = fcst_df_inf        # Your state space data starting in 2010
)


# Run the Forecast
# ------------------------------------------------------------------------------

taylor_forecast_df <- forecast_taylor_ssm(params_df = taylor_params_df,
                                          date_col = "quarter",
                                          master_df = master_taylor,
                                          forecast_h = 8,
                                          exogenous_gdp_forecast_data = gdp_forecasts_arima,
                                          exogenous_inf_forecast_data = final_inflation_vintages,
                                          hp_inf_gap = FALSE,
                                          inf_target = 1)


# Save the Forecast df (and reload)
write_csv(as.data.frame(taylor_forecast_df), output_save_paths$forecasts$forecast_df_taylor)
taylor_forecast_df <- read_csv(output_save_paths$forecasts$forecast_df_taylor, 
                               show_col_types = FALSE)

# --- Format df for the Evaluation --- 
# Make the Dataframe a square
last_origin <- ncol(taylor_forecast_df)

taylor_eval_square <- taylor_forecast_df[1:last_origin, 1:last_origin]

# Check: the number of rows should now equal the number of columns
dim(taylor_eval_square)



