message("Starting SNB Policy Rate Forecast")


# Source Correct Kalmann Specs
source(here("R", "kalman_implementation_taylor.R"))

gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                show_col_types = FALSE) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)

master_taylor <- read_csv(data_save_paths$processed$taylor_master_csv, 
                          show_col_types = FALSE)


required_dataset <- "master_taylor"

# The Data Detector Guard
if (!exists(required_dataset)) {
  # Write the error cleanly to the standard error log channel
  writeLines(paste0("[ERROR] Critical data frame '", required_dataset, "' is missing from the environment.\n",
                    "        Please run your data loading/preparation script first."), stderr())
  
  # If running via CLI, terminate safely with an error exit code
  quit(status = 1) 
} else (writeLines("Taylor Data set already loaded. Continuing with estimation..."))


if (forecast_starting_date < as.yearqtr("2010 Q1")) {
  
  forecast_starting_date_philips_taylor <- as.yearqtr("2010 Q1")
  message("FORECAST STARTING DATE FOR TAYLOR MODEL SET TO Q1 2010, DUE TO DATA AVAILABILITY")
} 


# ==============================================================================
# Last Data Prep
# ==============================================================================

# --- Select the already Transformed data for the model

# 1. Select all target columns and drop quarters containing any missing data
clean_taylor_data <- master_taylor %>%
  select(all_of(c("saron_libor_splice", "forward_rate", "gdp_gap", "inf_gap"))) %>%
  drop_na(gdp_gap, inf_gap)

# 2. Split the unified clean dataset back into independent X and Y matrices
Y_data_taylor <- clean_taylor_data %>%
  select(saron_libor_splice, forward_rate)

X_data_taylor <- clean_taylor_data %>%
  select(gdp_gap, inf_gap)
  

# ==============================================================================
# Run the Full Rolling Forecast
# ==============================================================================

#  Run the Rolling Estimation
# ------------------------------------------------------------------------------
if(run_estimation){
  
  taylor_param_est <- rolling_est_taylor_ssm(data = master_taylor,
                                             forecast_start = forecast_starting_date)
  
  taylor_params <- extract_params_df(taylor_param_est, extract_fitted_obs = TRUE)
  
  write.csv(taylor_params, output_save_paths$params$rolling_param_est_taylor)
  
}else{
  
  taylor_params <- read_csv(output_save_paths$params$rolling_param_est_taylor, 
                            show_col_types = FALSE)
}


generate_param_latex_table(
  manifest_source = taylor_manifest_source,
  model_name      = "Taylor Rule",
  save_path       = output_save_paths$tables$taylor_param_table
)

rolling_natural_rate_taylor <- taylor_params %>%
  select(all_of(c("quarter", "natural_rate"))) %>%
  mutate(quarter = as.yearqtr(quarter))


# Run the Forecast
# ------------------------------------------------------------------------------

taylor_forecast_df <- forecast_taylor_ssm(params_df = taylor_params,
                                          date_col = "quarter",
                                          master_df = master_taylor,
                                          forecast_h = 8,
                                          exogenous_gdp_forecast_data = gdp_forecasts_arima,
                                          exogenous_inf_forecast_data = fcst_df_inf,
                                          hp_inf_gap = FALSE,
                                          inf_target = 2)


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




# ==============================================================================
# Estimate and Plot the Full Sample Fit
# ==============================================================================


# Run Parameter Estimation
# ------------------------------------------------------------------------------



# --- Initialize SSM
ssm_taylor <- initialize_taylor_ssm(Y_data = Y_data_taylor,
                                    X_data = X_data_taylor,
                                    manifest_source = taylor_manifest_source)


if(run_estimation & !fit_plot_last_est){
  
  # --- Estimate the full Model ---
  output_estim <- ssm_optimizer_wrapper_shadow(ssm = ssm_taylor)
  
  estimated_params_taylor <- output_estim$params
  
  } else {
    
    last_rolling_row_taylor <- taylor_params %>%
      slice_tail(n = 1)
    
    # 2. Extract and format the specific coefficients into a clean, named list
    last_estimated_params <- list(
      gamma_pi     = as.numeric(last_rolling_row_taylor$gamma_pi),
      gamma_y      = as.numeric(last_rolling_row_taylor$gamma_y),
      phi          = as.numeric(last_rolling_row_taylor$phi),
      rho_tp       = as.numeric(last_rolling_row_taylor$rho_tp),
      sigma_policy = as.numeric(last_rolling_row_taylor$sigma_policy),
      xi_i         = as.numeric(last_rolling_row_taylor$xi_i),
      xi_tp_bar    = as.numeric(last_rolling_row_taylor$xi_tp_bar),
      xi_tp_cycl   = as.numeric(last_rolling_row_taylor$xi_tp_cycl)
    )
    
    estimated_params_taylor <- last_estimated_params
    
    
  }
  

  

# --- Run the filter with the final parameters ---
final_res <- kalman_filter_taylor(
  Y_t = ssm_taylor$data$Y,
  nu_t = matrix(0, nrow(ssm_taylor$data$Y), 3), # Adjust nr if needed
  H = ssm_taylor$builders$H(estimated_params_taylor),
  N = ssm_taylor$builders$N(estimated_params_taylor),
  mu_t = ssm_taylor$builders$mu_t(estimated_params_taylor, ssm_taylor$data$X),
  G = ssm_taylor$builders$G(estimated_params_taylor),
  M = ssm_taylor$builders$M(estimated_params_taylor),
  ar_matrix = ssm_taylor$builders$ar_mat(estimated_params_taylor, ssm_taylor$data$Y),
  Sigma_0 = diag(0.1, 3), # Or your specific Sigma_0
  rho_0 = matrix(ssm_taylor$rho_guess, ncol=1)
)

# --- Extract the values for the plot ---
shadow_rate <- final_res$fitted_obs[, 1]
natural_rate_est <- final_res$r[, 1] # State 1
tp_trend <- final_res$r[, 2]
tp_cycle <- final_res$r[, 3]
fit_rate <- final_res$fitted_obs_t_t[, 1]
dates <- master_taylor$quarter # Assuming you have a date vector
snb_rate <-Y_data_taylor$saron_libor_splice
fw_rate <- Y_data_taylor$forward_rate
inflation_gap <- X_data_taylor$inf_gap
gdp_gap <- X_data_taylor$gdp_gap


n_filter <- length(fit_rate)

# Plot the Data - Trimmed to match the filtered state sequence length
# ==============================================================================
taylor_plot_data <- tibble(
  date          = as.yearqtr(tail(dates, n_filter)), 
  fitted_shadow = as.numeric(fit_rate),
  natural_interest_rate = as.numeric(natural_rate_est),
  tp_trend      = as.numeric(tp_trend),
  tp_cycle      = as.numeric(tp_cycle),
  observed_rate = as.numeric(tail(snb_rate, n_filter)),
  forward_rate  = as.numeric(tail(fw_rate, n_filter)),
  inflation_gap = as.numeric(tail(inflation_gap, n_filter)),
  gdp_gap       = as.numeric(tail(gdp_gap, n_filter))
)

taylor_plot_data <- taylor_plot_data %>%
  left_join(rolling_natural_rate_taylor, by = c("date" = "quarter"))

# Define structure arrays
taylor_top_cols    <- c("observed_rate" = "Observed LIBOR / SARON",
                        "fitted_shadow" = "Fitted (Shadow) Rate",
                        "natural_interest_rate" = "Natural Rate (i*)",
                        "natural_rate" = "Rolling Estimation Natural Rate"
)

taylor_bottom_cols <- c("forward_rate" = "Forward Rate",
                        "inflation_gap" = "Inflation Gap",
                        "gdp_gap" = "GDP Gap",
                        "tp_trend"      = "TP Trend",
                        "tp_cycle"      = "TP Cycle")

taylor_top_colors    <- c("Observed LIBOR / SARON" = "#2ecc71",
                          "Fitted (Shadow) Rate" = "#2980b9",
                          "Natural Rate (i*)" = "#e74c3c",
                          "Rolling Estimation Natural Rate" = "orange"
)

taylor_bottom_colors <- c("Forward Rate" = "#e67e22",
                          "Inflation Gap" = "#9b59b6",
                          "GDP Gap" = "#16a085",
                          "TP Trend" = "pink",
                          "TP Cycle" = "violet")

# Generate
snb_chart <- plot_state_space_fit(
  plot_df        = taylor_plot_data, # Tibble with all the Plotted Data
  title          = "SNB Policy Rate Fit",
  subtitle       = "Taylor Rule Specification: Constant Inflation Gap (Inflation -1)",
  top_metrics    = taylor_top_cols,
  bottom_metrics = taylor_bottom_cols,
  top_colors     = taylor_top_colors,
  bottom_colors  = taylor_bottom_colors,
  zlb_bounds     = c(-0.75, 0.00),
  save_path = output_save_paths$plots$fit_taylor
)
print(snb_chart)
