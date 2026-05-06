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
  params_philips_df <- extract_params_df(params_philips)
  
  # Save the df
  write_csv(params_philips_df, output_save_paths$params$rolling_param_est_philips)
  
} else {
  message("run_estimation set to FASE. Loading Inflation Model Parameters from Disk...")
  params_philips_df <- read_csv(output_save_paths$params$rolling_param_est_philips)
}


# Run the Forecasts based on the Params df
philips_forecast_df <- forecast_phillips_ssm(params_philips_df,                  
                                  date_col = "quarter",
                                  master_df = master_philips,
                                  forecast_h = 8,
                                  gdp_forecast_data = master_philips)

# Save the Forecasts
write_csv(philips_forecast_df, output_save_paths$forecasts$forecast_df_philips)


last_origin <- ncol(philips_forecast_df)

philips_eval_square <- philips_forecast_df[1:last_origin, 1:last_origin]

# CheckThe number of rows should now equal the number of columns
dim(philips_eval_square)

