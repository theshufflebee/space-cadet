################################################################################
#
# This script estimates the parameters for the Okun model and runs the forecast
#
################################################################################

# Load correct Kalman Specs

# stop("STOPPING BEFORE OKUN ESTIMATION")

#source(here("R", "kalman_implementation_base.R"))
source(here("R", "kalman_implementation_okun.R"))


# Load Data
master_okun <- read_csv(data_save_paths$processed$okun_master_csv, 
                        show_col_types = FALSE)

gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                show_col_types = FALSE) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)

required_dataset <- "master_okun"

# The Data Detector Guard
if (!exists(required_dataset)) {
  # Write the error cleanly to the standard error log channel
  writeLines(paste0("[ERROR] Critical data frame '", required_dataset, "' is missing from the environment.\n",
                    "        Please run your data loading/preparation script first."), stderr())
  
  # If running via CLI, terminate safely with an error exit code
  quit(status = 1) 
} else (writeLines("Okun Data set already loaded. Continuing with estimation..."))


# ==============================================================================
# Estimation and Forecasting
# ==============================================================================



# ------------------------------------------------------------------------------
Y_data_okun <- master_okun %>%
  select(unemp_rate, spf_5y_unemp)


# X Df for full FIt Estimation
# ------------------------------------------------------------------------------
X_data_okun <- master_okun %>%
  select(gdp_gap, gdp_gap_lag_1, gdp_gap_lag_2)


# ----------------------------------------------
if(run_estimation){
  message("Starting Estimation of Model 1: Unemployment...")
  # The parameters are estimate in a rolling scheme, i.e from the T = 0 to each date
  # from first valid to last valid
  params_okun <- rolling_est_okun_ssm(master_okun,
                                      forecast_start = forecast_starting_date)
  
  # Extract the parameters from the results and put them in the correctly formated dataframe
  params_okun_df <- extract_params_df(params_okun)
  
  write_csv(params_okun_df, output_save_paths$params$rolling_param_est_okun)
  
} else {
  message("run_estimation set to FALSE. Loading Unemployment Model Parameters from Disk...")
  params_okun_df <- read_csv(output_save_paths$params$rolling_param_est_okun, 
                             show_col_types = FALSE)
  
}

rolling_natural_rate_okun <- params_okun_df %>%
  select(all_of(c("quarter", "natural_rate"))) %>%
  mutate(quarter = as.yearqtr(quarter))


generate_param_latex_table(
  manifest_source = okun_manifest_source,
  model_name      = "Okuns Law",
  save_path       = output_save_paths$tables$okun_param_table
)


# ==============================================================================
# Create the Actual Forecast
# ==============================================================================

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


# Y Df for full FIt Estimation
# ------------------------------------------------------------------------------
Y_data_okun <- master_okun %>%
  select(unemp_rate, spf_5y_unemp)


# X Df for full FIt Estimation
# ------------------------------------------------------------------------------
X_data_okun <- master_okun %>%
  select(gdp_gap, gdp_gap_lag_1, gdp_gap_lag_2)




# ==============================================================================
# Estimate Fit Of Okun SSM Model
# ==============================================================================

# Create the full manifest container object for the Okun setup
ssm_okun <- initialize_my_okun_ssm( # Update function name if it matches your initialize setup
  Y_data            = Y_data_okun,
  X_data            = X_data_okun,
  manifest_source = okun_manifest_source
)



# Get full model fit for fit plot
# ------------------------------------------------------------------------------
if(run_estimation & !fit_plot_last_est){
  
  # Run estimation
  output_estim_okun <- ssm_optimizer_wrapper_okun(ssm = ssm_okun)
  
  chosen_params_okun <- output_estim_okun$params        # Choice A: Full Sample MLE Fit
  
} else {
  
  message("USING LAST ROLLING ESTIMATE FOR FIT PLOT ANYWAY BECAUSE EITHER run_estimation IS SET TO FALSE OR LAST EST SET TO TRUE")
  
  last_rolling_row_okun <- params_okun_df %>%
    slice_tail(n = 1)
  
  # Format the coefficients into a structured list mapping your Okun parameterization
  last_estimated_params_okun <- list(
    beta1              = as.numeric(last_rolling_row_okun$beta1),
    beta2              = as.numeric(last_rolling_row_okun$beta2),
    beta3              = as.numeric(last_rolling_row_okun$beta3),
    phi                = as.numeric(last_rolling_row_okun$phi),
    sigma_unemp_rate   = as.numeric(last_rolling_row_okun$sigma_unemp_rate),
    sigma_spf_5y_unemp = as.numeric(last_rolling_row_okun$sigma_spf_5y_unemp),
    xi_n               = as.numeric(last_rolling_row_okun$xi_n)
  )
  
  chosen_params_okun <- last_estimated_params_okun     # Choice B: Locked Last Rolling Vintage
}
  



# Determine the exact latent dimension from your prior guesses vector (Length 1 for u_bar)
num_latent_states_okun <- length(ssm_okun$rho_guess)

# Run the fixed structural Kalman filter function with optimized inputs
final_res_okun <- kalman_filter_okun(
  Y_t       = ssm_okun$data$Y,
  nu_t      = matrix(0, nrow(ssm_okun$data$Y), num_latent_states_okun), 
  H         = ssm_okun$builders$H(chosen_params_okun),
  N         = ssm_okun$builders$N(chosen_params_okun),
  mu_t      = ssm_okun$builders$mu_t(chosen_params_okun, ssm_okun$data$X),
  G         = ssm_okun$builders$G(),
  M         = ssm_okun$builders$M(chosen_params_okun),
  ar_matrix = ssm_okun$builders$ar_mat(chosen_params_okun, ssm_okun$data$Y),
  Sigma_0   = matrix(ssm_okun$sigma_guess, nrow = num_latent_states_okun, ncol = num_latent_states_okun),
  rho_0     = matrix(ssm_okun$rho_guess, ncol = 1)
)

# Extract specific quantitative rows and tracks for the plotting array
dates_okun            <- master_okun$quarter
observed_unemp        <- Y_data_okun$unemp_rate
spf_unemp_expect      <- Y_data_okun$spf_5y_unemp

# Extract state tracks (Fitted observation overall profile vs. Latent Natural NAIRU trend u_bar)
fitted_unemp_t_t      <- final_res_okun$fitted_obs[, 1]
latent_nairu_trend    <- final_res_okun$r[, 1] # State 1 is natural rate

# Cyclical inputs for lower covariate tracking
output_gap_track      <- X_data_okun$gdp_gap




# Create Data Tibble and Visualization specs for Fit-Plot
#-------------------------------------------------------------------------------
# Construct the data payload expected by your create_state_space_dashboard function
okun_plot_data <- tibble(
  date          = as.yearqtr(dates_okun),
  obs_unemp     = as.numeric(observed_unemp),
  spf_survey    = as.numeric(spf_unemp_expect),
  fitted_unemp  = as.numeric(fitted_unemp_t_t),
  latent_trend  = as.numeric(latent_nairu_trend),
  gdp_gap       = as.numeric(output_gap_track)
)

okun_plot_data <- okun_plot_data %>%
  left_join(rolling_natural_rate_okun, by = c("date" = "quarter"))

# Set up column-to-legend text property maps
okun_top_metrics <- c(
  "obs_unemp"    = "Unemployment",
  "fitted_unemp" = "Fitted Unemployment",
  "latent_trend" = "Natural Rate",
  "spf_survey"   = "SPF 5-Year Unemp Mean",
  "natural_rate" = "Last State in Rolling Estimation"
)

okun_bottom_metrics <- c(
  "gdp_gap" = "Output Gap"
)

# Define clean, publication-ready HEX color codes matching your script aesthetics
okun_top_colors <- c(
  "Unemployment"         = "#34495e", # Dark Slate
  "Fitted Unemployment"     = "#2980b9", # Deep Blue
  "Natural Rate" = "#e74c3c", # Natural Rate Red
  "SPF 5-Year Unemp Mean"  = "#f1c40f",  # Gold Yellow
  "Last State in Rolling Estimation" = "green"
)

okun_bottom_colors <- c(
  "Output Gap" = "#16a085" # Teal
)

# Generate
# ------------------------------------------------------------------------------
okun_dashboard <- plot_state_space_fit(
  plot_df        =  okun_plot_data,
  title          = "Swiss Okun's Law State-Space Estimation",
  subtitle       = "Okun Specification: Latent Natural Rate (u_bar) with Long-Term Survey Anchoring",
  top_metrics    = okun_top_metrics,
  bottom_metrics =  okun_bottom_metrics,
  top_colors     = okun_top_colors,
  bottom_colors  =  okun_bottom_colors,
  zlb_bounds     = NULL, # Left NULL since this is unemployment
  y_label_top    = "Unemployment Rate / Expectations (%)",
  y_label_bottom = "Output Gap (%)",
  save_path      = output_save_paths$plots$fit_okun
)

# Print the resulting plot directly to the RStudio workspace graphics engine
print(okun_dashboard)
