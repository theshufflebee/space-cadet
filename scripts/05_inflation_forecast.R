################################################################################
# 
# RUN FORECASTS FOR MODEL 2 / INFLATIONS
#
################################################################################

# Get Correct Kalman Specs
source(here("R", "kalman_implementation_philips.R"))


gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                show_col_types = FALSE) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)

master_philips <- read_csv(data_save_paths$processed$philips_master_csv, 
                           show_col_types = FALSE)

master_philips <- master_philips %>%
  mutate(quarter = as.yearqtr(quarter))

required_dataset <- "master_philips"

# The Data Detector Guard
if (!exists(required_dataset)) {
  # Write the error cleanly to the standard error log channel
  writeLines(paste0("[ERROR] Critical data frame '", required_dataset, "' is missing from the environment.\n",
                    "        Please run your data loading/preparation script first."), stderr())
  
  # If running via CLI, terminate safely with an error exit code
  quit(status = 1) 
} else (writeLines("Phillips Data set already loaded. Continuing with estimation..."))


if (forecast_starting_date < as.yearqtr("2010 Q1")) {
  
  forecast_starting_date_philips_taylor <- as.yearqtr("2010 Q1")
  message("FORECAST STARTING DATE FOR PHILLIPS MODEL SET TO Q1 2010, DUE TO DATA AVAILABILITY")
} 

# ==============================================================================
# Run Rolling Estimation and Create The forecast
# ==============================================================================
if(run_estimation) {
  
  message("Starting Estimation of Model 2: Inflation...")
  
  # Run the Rolling estimation
  params_philips <- rolling_est_philips_ssm(data = master_philips,
                                            forecast_start = forecast_starting_date_philips_taylor,
                                            
  )
  
  #Turn Estimation output into a df
  params_philips_df <- extract_params_df(params_philips, extract_fitted_obs = TRUE)
  
  # Save the df
  write_csv(params_philips_df, output_save_paths$params$rolling_param_est_philips)
  
} else {
  message("run_estimation set to FALSE. Loading Inflation Model Parameters from Disk...")
  params_philips_df <- read_csv(output_save_paths$params$rolling_param_est_philips, 
                                show_col_types = FALSE)
}


rolling_natural_rate_philips <- params_philips_df %>%
  select(all_of(c("quarter", "natural_rate"))) %>%
  mutate(quarter = as.yearqtr(quarter))




# Run the Forecasts based on the Params df
philips_forecast_df <- forecast_philips_ssm(params_philips_df,                  
                                  date_col = "quarter",
                                  master_df = master_philips,
                                  forecast_h = 8,
                                  exogenous_gdp_forecast_data = gdp_forecasts_arima)

# Save the Forecasts
write_csv(as.data.frame(philips_forecast_df), output_save_paths$forecasts$forecast_df_philips)


last_origin <- ncol(philips_forecast_df)

philips_eval_square <- philips_forecast_df[1:last_origin, 1:last_origin]

# CheckThe number of rows should now equal the number of columns
dim(philips_eval_square)


# Prepare for use as exog forecast inputs and also Fcst tests
colnames(philips_eval_square) <- format(as.yearqtr(colnames(philips_eval_square)), "%YQ%q")

rownames(philips_eval_square) <- format(as.yearqtr(colnames(philips_eval_square)), "%YQ%q")

fcst_df_inf <- philips_eval_square %>%
  as.data.frame() %>%
  rownames_to_column(var = "date")

# Format the date column to expected format
fcst_df_inf$date <- format(as.yearqtr(fcst_df_inf$date), "%YQ%q")



# ==============================================================================
# Extract Model Fit
# ==============================================================================

# Prepare Data
# ------------------------------------------------------------------------------
X_matrix_full <- build_data_matrix_philips(
  T_0             = "2005-01-01",  # Or your full sample start preference
  vantage_quarter = "2025 Q4",     # Up to latest realized history block
  data            = master_philips
)

# Extract Measurement vector Y (Inflation and SPF 5Y Expectation survey)
# Note: rows between 2005-2015 for 5y_cpi_forecast are allowed to be NA
Y_data_philips <- X_matrix_full %>%
  select(all_of(c("log_inflation_diff", "5y_cpi_forecast")))

# Extract Structural Determinants vector X (Gaps and Lags)
X_data_philips <- X_matrix_full %>%
  select(all_of(c("gdp_gap", "lop_gap")))


# Run The Estimation
# ------------------------------------------------------------------------------

ssm_philips <- initialize_my_philips_ssm(
  Y_data            = Y_data_philips,
  X_data            = X_data_philips,
  parameter_guesses = philips_parameter_guess # Your setup initial config list
)




if(run_estimation & !fit_plot_last_est){
  
  output_estim_philips <- ssm_optimizer_wrapper_philips(ssm = ssm_philips)
  chosen_params_philips <- output_estim_philips$params        # Choice A: MLE Optimizer
  
  
} else {
  
  message("USING LAST ROLLING ESTIMATE FOR FIT PLOT ANYWAY BECAUSE EITHER run_estimation IS SET TO FALSE OR LAST EST SET TO TRUE")
  
  last_rolling_row_philips <- params_philips_df %>%
    slice_tail(n = 1)
  
  # Format the coefficients into a structured list mapping your Phillips Curve parameterization
  last_estimated_params_philips <- list(
    beta_y    = as.numeric(last_rolling_row_philips$beta_y),
    psi_lop   = as.numeric(last_rolling_row_philips$psi_lop),
    phi       = as.numeric(last_rolling_row_philips$phi),
    sigma_cpi = as.numeric(last_rolling_row_philips$sigma_cpi),
    sigma_spf = as.numeric(last_rolling_row_philips$sigma_spf),
    xi_n      = as.numeric(last_rolling_row_philips$xi_n)
  )
  
  chosen_params_philips <- last_estimated_params_philips
}
  

rho_init <- matrix(ssm_philips$rho_guess, nrow = length(ssm_philips$rho_guess), ncol = 1)
nr <- nrow(rho_init)

T_len <- nrow(ssm_philips$data$Y)


# Toggle between Full Sample MLE optimization or the exact Last Rolling Vintage params:


final_res_philips <- kalman_filter_philips(
  Y_t       = ssm_philips$data$Y,
  # Dynamically scales the tracking matrix to match your 1-state setup
  nu_t      = matrix(0, T_len, nr), 
  H         = ssm_philips$builders$H(chosen_params_philips),
  N         = ssm_philips$builders$N(chosen_params_philips),
  mu_t      = ssm_philips$builders$mu_t(chosen_params_philips, ssm_philips$data$X),
  G         = ssm_philips$builders$G(),
  M         = ssm_philips$builders$M(chosen_params_philips),
  ar_matrix = ssm_philips$builders$ar_mat(chosen_params_philips, ssm_philips$data$Y),
  
  # Uses your exact initial variance guess from your ssm initialization object
  Sigma_0   = matrix(ssm_philips$sigma_guess),
  
  # Uses your exact initial state vector guess (1.00 = 1%)
  rho_0     = matrix(ssm_philips$rho_guess, ncol = 1)
)

# Extract specific quantitative tracks for plotting variables
# Adjust state array indexes [ , x] to exactly match your kalman_implementation_philips.R vector indices
dates_philips       <- X_matrix_full$quarter
observed_cpi_inf    <- Y_data_philips$log_inflation_diff
spf_expectations    <- Y_data_philips$`5y_cpi_forecast`

# Extract unobserved states from the filtered state array matrix ($r$)
latent_inflation_trend <- final_res_philips$r[, 1] # Assumes your State 1 is pi_bar_t
fitted_inflation_t_t   <- final_res_philips$fitted_obs_t_t[, 1] # Model's overall inflation fit

# Cycle Gaps from the exogenous data rows
gdp_gap_track <- X_data_philips$gdp_gap
lop_gap_track <- X_data_philips$lop_gap

# ==============================================================================
# 4. Map Elements and Render via our Generalized Dashboard Container
# ==============================================================================

# Construct the exact data payload table expected by our create_state_space_dashboard function
philips_plot_data <- tibble(
  date          = dates_philips, # Handled natively inside the dashboard container
  obs_cpi       = as.numeric(observed_cpi_inf),
  spf_survey    = as.numeric(spf_expectations),
  fitted_inf    = as.numeric(fitted_inflation_t_t),
  latent_trend  = as.numeric(latent_inflation_trend),
  gdp_gap       = as.numeric(gdp_gap_track),
  lop_gap       = as.numeric(lop_gap_track)
)

philips_plot_data <- philips_plot_data %>%
  left_join(rolling_natural_rate_philips, by = c("date" = "quarter"))

# Set up column-to-legend text properties arrays
philips_top_metrics <- c(
  "obs_cpi"      = "Observed CPI Inflation",
  "fitted_inf"   = "Model Fitted Inflation",
  "latent_trend" = "Unobserved Core Trend (pi_bar)",
  "spf_survey"   = "SPF 5-Year Expectation Survey",
  "natural_rate" = "Rolling Est Natural Rate"
)

philips_bottom_metrics <- c(
  "gdp_gap" = "Swiss Output Gap (GDP)",
  "lop_gap" = "Law of One Price (LOP) Gap"
)

# Define exact high-contrast academic HEX color mappings
philips_top_colors <- c(
  "Observed CPI Inflation"         = "#34495e", # Dark Slate
  "Model Fitted Inflation"         = "#2980b9", # Deep Blue
  "Unobserved Core Trend (pi_bar)" = "#e74c3c", # Core Trend Red
  "SPF 5-Year Expectation Survey"  = "#f1c40f",  # Gold Yellow
  "Rolling Est Natural Rate" = "green"
)

philips_bottom_colors <- c(
  "Swiss Output Gap (GDP)"         = "#16a085", # Teal
  "Law of One Price (LOP) Gap"     = "#9b59b6"  # Amethyst Purple
)


philips_fit_plot <- plot_state_space_fit(
  plot_df        = philips_plot_data,
  title          = "Swiss Phillips Curve State-Space Estimation",
  subtitle       = "Hybrid PC Specification: Latent Core Trend with Professional Survey Anchoring",
  top_metrics    = philips_top_metrics,
  bottom_metrics = philips_bottom_metrics,
  top_colors     = philips_top_colors,
  bottom_colors  = philips_bottom_colors,
  zlb_bounds     = NULL, # Set to NULL because inflation parameters are unconstrained by a ZLB floor
  y_label_top    = "Inflation Rate / Expectations (%)",
  y_label_bottom = "Macroeconomic Cycle Gaps (%)",
  save_path      = output_save_paths$plots$fit_philips
)

# Print to your RStudio Plot viewer window
print(philips_fit_plot)


