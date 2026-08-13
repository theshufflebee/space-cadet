################################################################################
#
# RUN ROLLING ESTIMATIION
#
################################################################################
#stop("STOPPING BEFORE ESTIMATION")
library(here)
source(here("scripts", "00a_install_dependencies.R"))
source(here("config.R"))

# ==============================================================================
# Load Data
# ==============================================================================

# --- Load Master Dataframes ---

master_okun <- read_csv(data_save_paths$processed$okun_master_csv, 
                        show_col_types = FALSE)

master_philips <- read_csv(data_save_paths$processed$philips_master_csv, 
                           show_col_types = FALSE)

master_philips <- master_philips %>%
  mutate(quarter = as.yearqtr(quarter))

master_taylor <- read_csv(data_save_paths$processed$taylor_master_csv, 
                          show_col_types = FALSE)

# ==============================================================================
# AUXILIARY FORECASTS
# ==============================================================================

# --- GDP FORECASTS ---
if (run_rolling_estimation || !file.exists(output_save_paths$forecasts$forecast_df_gdp_arima)) {
  
  # Generate true log GDP levels using the updated arima loop wrapper
  gdp_forecasts_arima <- gdp_forecast_wrapper(
    fcast_start = forecast_starting_date,
    data        = master_quarterly,
    date_col    = "quarter",
    gdp_col     = "log_gdp",
    horizon     = h
  )
  
  # Save the pure log GDP level matrix layout
  write_csv(gdp_forecasts_arima, output_save_paths$forecasts$forecast_df_gdp_arima)
  message("GDP FORECATSTS SAVED SUCCESSFULLY")
  
} else {
  # Read from disk and dynamically format matrix structures back into expected shapes
  message("LOADING GDP FORECATSTS...")
  
  gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima) %>%
    mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
    rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)
}




last_origin <- ncol(gdp_forecasts_arima)

gdp_eval_square <- gdp_forecasts_arima[1:last_origin, 1:last_origin]

dim(gdp_eval_square)

fcst_df_gdp <- gdp_eval_square %>%
  mutate(date = format(zoo::as.yearqtr(date), "%YQ%q")) %>%
  rename_with(~ format(zoo::as.yearqtr(.x), "%YQ%q"), .cols = -date)



# --- INFLATION FORECASTS ---
if (run_rolling_estimation || !file.exists(output_save_paths$forecasts$forecast_df_inf_arima)) {
  
  inf_arima_fcst_df <- inflation_forecast_wrapper(forecast_starting_date,
                                                  fcast_end = NULL,
                                                  master_quarterly,
                                                  date_col = "quarter",
                                                  inf_col = "log_inflation_diff",
                                                  horizon = 8
  )
  
  write_csv(inf_arima_fcst_df, output_save_paths$forecasts$forecast_df_inf_arima)
  message("INF FORECATSTS SAVED SUCCESSFULLY")
  
} else {
  message("LOADING INF FORECATSTS...")
  
  inf_arima_fcst_df <- read_csv(output_save_paths$forecasts$forecast_df_inf_arima) %>%
    mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
    rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)
}


################################################################################


# ==============================================================================
# OKUN ESTIMATION
# ==============================================================================

if(run_rolling_estimation){
  message("Starting Estimation of Model 1: Unemployment...")

  params_okun <- rolling_est_okun_ssm(master_okun,
                                      forecast_start = forecast_starting_date)
  
  okun_params_df <- extract_params_df(params_okun)
  
  write_csv(okun_params_df, output_save_paths$params$rolling_param_est_okun)
  
} else {
  message("run_rolling_estimation set to FALSE.")
  
  if(LOAD_PARA_EST) {
    message("Loadig Unemployment Model Parameters from Parallel Estimation")
    okun_parmas_list <- ingest_parallel_results(TARGET_FOLDER_OKUN)
    
    okun_params_df <- extract_params_df(okun_parmas_list)
    rownames(okun_params_df) <- NULL
  } else {
    message("Loading Unemployment Model Parameters from save folder")
    okun_params_df <- read_csv(output_save_paths$params$rolling_param_est_okun, 
                               show_col_types = FALSE)
  }
  
}


# ==============================================================================
# PHILLIPS ESTIMATION
# ==============================================================================

# Phillips model Data availability check -> earliest start is currently 2004 Q1
if (forecast_starting_date < as.yearqtr("2004 Q4")) {
  
  forecast_starting_date_philips <- as.yearqtr("2004 Q4")
  message("FORECAST STARTING DATE FOR PHILLIPS MODEL SET TO Q1 2010, DUE TO DATA AVAILABILITY")
} else {forecast_starting_date_philips <- forecast_starting_date }


if(run_rolling_estimation) {
  
  message("Starting Estimation of Model 2: Inflation...")
  
  params_phillips <- rolling_est_philips_ssm(data = master_philips,
                                             forecast_start = forecast_starting_date_philips,
                                             
  )
  
  phillips_parmas_df <- extract_params_df(params_phillips, extract_fitted_obs = TRUE)
  
  write_csv(phillips_parmas_df, output_save_paths$params$rolling_param_est_philips)
  
} else {
  message("run_rolling_estimation set to FALSE.")
  if(LOAD_PARA_EST){
    message("Loadig Inflation Model Parameters from Parallel Estimation")
    phillips_parmas_list <- ingest_parallel_results(TARGET_FOLDER_PHILLIPS)
    phillips_parmas_df <- extract_params_df(phillips_parmas_list)
    rownames(phillips_parmas_df) <- NULL
    
  } else{
    message("Loading Inflatin Model Parameters from save folder")
    phillips_parmas_df <- read_csv(output_save_paths$params$rolling_param_est_philips, 
                                 show_col_types = FALSE)
  }
}

# ==============================================================================
# TAYLOR MODEL
# ==============================================================================

if(run_rolling_estimation){
  
  message("Starting Estimation of Model 3: Policy Rate...")
  
  taylor_param_est <- rolling_est_taylor_ssm(data = master_taylor,
                                             forecast_start = forecast_starting_date)
  
  taylor_params_df_df <- extract_params_df(taylor_param_est, extract_fitted_obs = TRUE)
  
  write_csv(taylor_params_df_df, output_save_paths$params$rolling_param_est_taylor)
  
}else{
  message("run_rolling_estimation set to FALSE.")
  if(LOAD_PARA_EST) {
    message("Loadig Policy Rate Model Parameters from Parallel Estimation")
    taylor_params_list <- ingest_parallel_results(TARGET_FOLDER_TAYLOR)
    
    taylor_params_df <- extract_params_df(taylor_params_list)
    rownames(taylor_params_df) <- NULL
    
  } else {
    message("Loading Policy Rate Model Parameters from save folder")
    
    taylor_params_df <- read_csv(output_save_paths$params$rolling_param_est_taylor, 
                                 show_col_types = FALSE)
  }
}