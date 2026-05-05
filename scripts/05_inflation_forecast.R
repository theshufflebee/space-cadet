################################################################################
# 
# RUN FORECASTS FOR MODEL 2 / INFLATIONS
#
################################################################################





if(run_estimation) {
  
  message("Starting Estimation of Model 2: Inflation...")
  
  # Run the Rolling estimation
  params_philips <- rolling_est_philips_ssm(data = master_philips,
                                            forecast_start = forecast_starting_date
  )
  
  #Turn Estimation output into a df
  philips_params_df <- extract_params_df(params_philips)
  
  # Save the df
  write_csv(philips_params_df, here("output/parameter_estimation/philips_params.csv"))
  
} else {
  message("run_estimation set to FASE. Loading Inflation Model Parameters from Disk...")
  philips_params_df <- read_csv(here("output/parameter_estimation/philips_params.csv"))
}


# Run the Forecasts based on the Params df
forecast_df <- forecast_phillips_ssm(philips_params_df,                  
                                  date_col = "quarter",
                                  master_df = master_philips,
                                  forecast_h = 8,
                                  gdp_forecast_data = master_philips)

# Save the Forecasts
write_csv(forecast_df, here("output/forecasts/inflation_forecasts.csv"))

