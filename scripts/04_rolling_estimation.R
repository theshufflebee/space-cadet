################################################################################
#
# DATA LOADING
#
################################################################################
#stop("STOPPING BEFORE ESTIMATION")


master_okun <- read_csv(data_save_paths$processed$okun_master_csv, 
                        show_col_types = FALSE)



master_philips <- read_csv(data_save_paths$processed$philips_master_csv, 
                           show_col_types = FALSE)

master_philips <- master_philips %>%
  mutate(quarter = as.yearqtr(quarter))

master_taylor <- read_csv(data_save_paths$processed$taylor_master_csv, 
                          show_col_types = FALSE)


gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                show_col_types = FALSE) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)

################################################################################
#
# OKUN ESTIMATION
#
################################################################################

if(run_estimation){
  message("Starting Estimation of Model 1: Unemployment...")

  params_okun <- rolling_est_okun_ssm(master_okun,
                                      forecast_start = forecast_starting_date)
  
  params_okun_df <- extract_params_df(params_okun)
  
  write_csv(params_okun_df, output_save_paths$params$rolling_param_est_okun)
  
} else {
  message("run_estimation set to FALSE. Loading Unemployment Model Parameters from Disk...")
  params_okun_df <- read_csv(output_save_paths$params$rolling_param_est_okun, 
                             show_col_types = FALSE)
}


################################################################################
# 
# PHILLIPS ESTIMATION
#
################################################################################

# Phillips model Data availability check -> earliest start is currently 2004 Q1
if (forecast_starting_date < as.yearqtr("2004 Q4")) {
  
  forecast_starting_date_philips <- as.yearqtr("2004 Q4")
  message("FORECAST STARTING DATE FOR PHILLIPS MODEL SET TO Q1 2010, DUE TO DATA AVAILABILITY")
} else {forecast_starting_date_philips <- forecast_starting_date }


if(run_estimation) {
  
  message("Starting Estimation of Model 2: Inflation...")
  
  params_phillips <- rolling_est_philips_ssm(data = master_philips,
                                             forecast_start = forecast_starting_date_philips,
                                             
  )
  
  phillips_parmas_df <- extract_params_df(params_phillips, extract_fitted_obs = TRUE)
  
  write_csv(phillips_parmas_df, output_save_paths$params$rolling_param_est_philips)
  
} else {
  message("run_estimation set to FALSE. Loading Inflation Model Parameters from Disk...")
  phillips_parmas_df <- read_csv(output_save_paths$params$rolling_param_est_philips, 
                                 show_col_types = FALSE)
}



################################################################################
#
# TAYLOR MODEL
#
################################################################################


if(run_estimation){
  
  message("Starting Estimation of Model 3: Policy Rate...")
  
  taylor_param_est <- rolling_est_taylor_ssm(data = master_taylor,
                                             forecast_start = forecast_starting_date)
  
  taylor_params_df_df <- extract_params_df(taylor_param_est, extract_fitted_obs = TRUE)
  
  write_csv(taylor_params_df_df, output_save_paths$params$rolling_param_est_taylor)
  
}else{
  
  taylor_params_df <- read_csv(output_save_paths$params$rolling_param_est_taylor, 
                               show_col_types = FALSE)
}