message("Starting SNB Policy Rate Forecast")
source(here("R", "kalman_taylor_specs.R"))


# Source Correct Kalmann Specs

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


# ==============================================================================
# Last Data Prep
# ==============================================================================

# --- Select the already Transformed data for the model

# From The Full dataset select the Data needed for the Rolling estimation of the FUll timeline
Y_data_taylor <- master_taylor %>%
  select(all_of(c("saron_libor_splice", "forward_rate")))

# Set inf_gap to hp_inf_gap if you want to estimate with the HP inf gap
X_data_taylor <- master_taylor %>%
  select(all_of(c("gdp_gap", "inf_gap")))
  

# ==============================================================================
# Run the Full Rolling Forecast
# ==============================================================================

#  Run the Rolling Estimation
# ------------------------------------------------------------------------------
if(run_estimation){
  
  taylor_param_est <- rolling_est_taylor_ssm(data = master_taylor,
                                             forecast_start = as.yearqtr(forecast_starting_date))
  
  taylor_params <- extract_params_df(taylor_param_est, extract_fitted_obs = TRUE)
  
  write.csv(taylor_params, output_save_paths$params$rolling_param_est_taylor)
  
}else{
  
  taylor_params <- read_csv(output_save_paths$params$rolling_param_est_taylor, 
                            show_col_types = FALSE)
}

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
                                    parameter_guesses =snb_rate_parameter_guess)


if(run_estimation) {
  
  
  # --- Estimate the full Model ---
  
  # Currently not working??
  output_estim <- ssm_optimizer_wrapper_shadow(ssm = ssm_taylor)
  
  
  
  # --- Run the filter with the final parameters ---
  final_res <- kalman_filter_taylor(
    Y_t = ssm_taylor$data$Y,
    nu_t = matrix(0, nrow(ssm_taylor$data$Y), 3), # Adjust nr if needed
    H = ssm_taylor$builders$H(output_estim$params),
    N = ssm_taylor$builders$N(output_estim$params),
    mu_t = ssm_taylor$builders$mu_t(output_estim$params, ssm_taylor$data$X),
    G = ssm_taylor$builders$G(output_estim$params),
    M = ssm_taylor$builders$M(output_estim$params),
    ar_matrix = ssm_taylor$builders$ar_mat(output_estim$params, ssm_taylor$data$Y),
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
  
  
  #Plot the Data
  # ==============================================================================
  taylor_plot_data <- tibble(
    date          = as.yearqtr(dates), # Convert yearqtr to Date format for smooth ggplot axes
    fitted_shadow = as.numeric(fit_rate),
    natural_interest_rate  = as.numeric(natural_rate_est),
    tp_trend      = as.numeric(tp_trend),
    tp_cycle      = as.numeric(tp_cycle),
    observed_rate = as.numeric(snb_rate),
    forward_rate  = as.numeric(fw_rate),
    inflation_gap = as.numeric(inflation_gap),
    gdp_gap       = as.numeric(gdp_gap)
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
  
  
  
  
  
}

