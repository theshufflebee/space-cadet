message("Starting Visualizations")

# NOTE TO SELF: Build upd the function to work on all models
# Then build a wrapper that gives a complete SSM report for inspection

# Temporarily here so updating functions in it and reruning is easier

# Parameters over Time Plot

# ==============================================================================
#  Parameter Plots
# ==============================================================================


okun_param_plot <- plot_model_parameters(df = okun_params_df,
                                         title = "Okun's Law Model Recursive Parameter Estimates",
                                         save_path = output_save_paths$plots$params_okun)

philips_param_plot <- plot_model_parameters(df = phillips_parmas_df,
                                            title = "Philips Curve Model Recursive Parameter Estimates",
                                            save_path = output_save_paths$plots$params_philips)

taylor_param_plot <- plot_model_parameters(df = taylor_params_df,
                                            title = "Taylor Rule Model Recursive Parameter Estimates",
                                            save_path = output_save_paths$plots$params_taylor)


# ==============================================================================
#  AUX/GDP: Spaghetti Plots for Forecasts and Benchmarks
# ==============================================================================
creaFcstEval::spaghetti_plot(
  df          = Y_gdp_eval,
  fcst_df     = fcst_df_gdp,
  output_path = output_save_paths$plots$spaghetti_gdp
)

creaFcstEval::spaghetti_plot(
  df          = output_gap_full_series,
  fcst_df     = output_gap_forecasts,
  output_path = output_save_paths$plots$spaghetti_gdp_gap
)


error_plotting_wrapper(fcst_df_gdp, "GDP Gap ARIMA Model", save_path = output_save_paths$plots$errors_gdp)


# --- RW Benchmark Spaghetti Plot ---
gdp_rw_bench_wide <- creaFcstEval::bench_to_wide(result_gdp_rw$bench_long)
gdp_rw_bench_wide <- standardize_yq_seq(gdp_rw_bench_wide)
gdp_rw_bench_wide <- reduce_diagonal_matrix(mat = gdp_rw_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_gdp_eval,
  fcst_df     = gdp_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$gdp_rw
)

# --- AR1 Benchmark Spaghetti Plot ---
gdp_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_gdp_ar1$bench_long)
gdp_ar1_bench_wide <- standardize_yq_seq(gdp_ar1_bench_wide)
gdp_ar1_bench_wide <- reduce_diagonal_matrix(mat = gdp_ar1_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_gdp_eval,
  fcst_df     = gdp_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$gdp_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
gdp_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_gdp_auto_arma$bench_long)
gdp_auto_arma_bench_wide <- standardize_yq_seq(gdp_auto_arma_bench_wide)
gdp_auto_arma_bench_wide <- reduce_diagonal_matrix(mat = gdp_auto_arma_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_gdp_eval,
  fcst_df     = gdp_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$gdp_auto_arma
)


# ==============================================================================
#  AUX/LOP: Spaghetti Plots for Forecasts and Benchmarks
# ==============================================================================

# Currently no use, will be needed later
full_lop_gap <- build_data_matrix_philips(
  T_0             = val_T1_phillips,  # Or your full sample start preference
  vantage_quarter = "2026 Q1",     # Up to latest realized history block
  data            = master_philips
) %>%
  select(quarter,lop_gap, reer_eu_ppi)%>%
  rename(date = quarter,
         value = lop_gap,
         snb_reer = reer_eu_ppi)%>%
  mutate(
    date = format(zoo::as.yearqtr(date), "%YQ%q")
  ) %>%
  slice_head(n = -4)


creaFcstEval::spaghetti_plot(
  df          = Y_snb_reer,
  fcst_df     = fcst_df_lop,
  output_path = output_save_paths$plots$spaghetti_lop
)

creaFcstEval::spaghetti_plot(
  df          = lop_gap_full_series,
  fcst_df     = lop_forecasts,
  output_path = output_save_paths$plots$spaghetti_lop_gap
)


error_plotting_wrapper(fcst_df_lop, "SNB REER Forecast Error", save_path = output_save_paths$plots$errors_lop)
error_plotting_wrapper(lop_forecasts, "LOP Gap Forecast Error", save_path = output_save_paths$plots$errors_lop_gap)


###########################

# --- RW Benchmark Spaghetti Plot ---
lop_rw_bench_wide <- creaFcstEval::bench_to_wide(result_gdp_rw$bench_long)
lop_rw_bench_wide <- standardize_yq_seq(gdp_rw_bench_wide)
lop_rw_bench_wide <- reduce_diagonal_matrix(mat = lop_rw_bench_wide, max_h = 8)


creaFcstEval::spaghetti_plot(
  df          = Y_gdp_eval,
  fcst_df     = gdp_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$gdp_rw
)

# --- AR1 Benchmark Spaghetti Plot ---
gdp_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_gdp_ar1$bench_long)
gdp_ar1_bench_wide <- standardize_yq_seq(gdp_ar1_bench_wide)
gdp_ar1_bench_wide <- reduce_diagonal_matrix(mat = gdp_ar1_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_gdp_eval,
  fcst_df     = gdp_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$gdp_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
gdp_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_gdp_auto_arma$bench_long)
gdp_auto_arma_bench_wide <- standardize_yq_seq(gdp_auto_arma_bench_wide)
gdp_auto_arma_bench_wide <- reduce_diagonal_matrix(mat = gdp_auto_arma_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_gdp_eval,
  fcst_df     = gdp_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$gdp_auto_arma
)


# ==============================================================================
#  OKUN: Spaghetti Plots for Forecasts and Benchmarks
# ==============================================================================

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = fcst_df_unemp,
  output_path = output_save_paths$plots$spaghetti_okun
)

error_plotting_wrapper(fcst_df_unemp, "Error Plot Okun's Law Model", save_path = output_save_paths$plots$errors_okun)

# --- RW Benchmark Spaghetti Plot ---
okun_rw_bench_wide <- creaFcstEval::bench_to_wide(result_okun_rw$bench_long)
okun_rw_bench_wide <- standardize_yq_seq(okun_rw_bench_wide)
okun_rw_bench_wide <- reduce_diagonal_matrix(mat = okun_rw_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = okun_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$okun_rw
)

# --- AR1 Benchmark Spaghetti Plot ---
okun_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_okun_ar1$bench_long)
okun_ar1_bench_wide <- standardize_yq_seq(okun_ar1_bench_wide)
okun_ar1_bench_wide <- reduce_diagonal_matrix(mat = okun_ar1_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = okun_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$okun_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
okun_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_okun_auto_arma$bench_long)
okun_auto_arma_bench_wide <- standardize_yq_seq(okun_auto_arma_bench_wide)
okun_auto_arma_bench_wide <- reduce_diagonal_matrix(mat = okun_auto_arma_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = okun_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$okun_auto_arma
)



# ==============================================================================
#  PHILLIPS: Spaghetti Plots for Forecasts and Benchmarks
# ==============================================================================

# --- Forecast Spaghetti Plot ---
creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = fcst_df_inf,
  output_path = output_save_paths$plots$spaghetti_philips
)

error_plotting_wrapper(fcst_df_inf, "Error Plot Phillips Curve Model", save_path = output_save_paths$plots$errors_phillips)

# --- RW Benchmark Spaghetti Plot ---
phillips_rw_bench_wide <- creaFcstEval::bench_to_wide(result_philips_rw$bench_long)
phillips_rw_bench_wide <- standardize_yq_seq(phillips_rw_bench_wide)
phillips_rw_bench_wide <- reduce_diagonal_matrix(mat = phillips_rw_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = phillips_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$philips_rw
)

# --- AR1 Benchmark Spaghetti Plot ---
phillips_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_philips_ar1$bench_long)
phillips_ar1_bench_wide <- standardize_yq_seq(phillips_ar1_bench_wide)
phillips_ar1_bench_wide <- reduce_diagonal_matrix(mat = phillips_ar1_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = phillips_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$philips_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
phillips_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_philips_auto_arma$bench_long)
phillips_auto_arma_bench_wide <- standardize_yq_seq(phillips_auto_arma_bench_wide)
phillips_auto_arma_bench_wide <- reduce_diagonal_matrix(mat = phillips_auto_arma_bench_wide,  max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = phillips_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$philips_auto_arma
)


# ==============================================================================
#  TAYLOR: Spaghetti Plots for Forecasts and Benchmarks
# ==============================================================================

# --- Forecast Spaghetti Plot ---
creaFcstEval::spaghetti_plot(
  df              = Y_taylor,
  fcst_df         = fcst_df_policy_rate,
  output_path     = output_save_paths$plots$spaghetti_taylor
)

creaFcstEval::spaghetti_plot(
  df              = Y_taylor_rounded,
  fcst_df         = fcst_df_policy_rate_rounded,
  output_path     = output_save_paths$plots$spaghetti_taylor_rounded
)

error_plotting_wrapper(fcst_df_policy_rate, "Error Plot Taylor Rule Model", save_path = output_save_paths$plots$errors_taylor)

# --- RW Benchmark Spaghetti Plot ---
taylor_rw_bench_wide <- creaFcstEval::bench_to_wide(result_taylor_rw$bench_long)
taylor_rw_bench_wide <- standardize_yq_seq(taylor_rw_bench_wide)
taylor_rw_bench_wide <- reduce_diagonal_matrix(mat = taylor_rw_bench_wide, max_h = 8)


creaFcstEval::spaghetti_plot(
  df          = Y_taylor,
  fcst_df     = taylor_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$taylor_rw
)


# --- AR1 Benchmark Spaghetti Plot ---
taylor_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_taylor_ar1$bench_long)
taylor_ar1_bench_wide <- standardize_yq_seq(taylor_ar1_bench_wide)
taylor_ar1_bench_wide <- reduce_diagonal_matrix(mat = taylor_ar1_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_taylor,
  fcst_df     = taylor_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$taylor_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
taylor_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_taylor_auto_arma$bench_long)
taylor_auto_arma_bench_wide <- standardize_yq_seq(taylor_auto_arma_bench_wide)
taylor_auto_arma_bench_wide <- reduce_diagonal_matrix(mat = taylor_auto_arma_bench_wide, max_h = 8)

creaFcstEval::spaghetti_plot(
  df          = Y_taylor,
  fcst_df     = taylor_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$taylor_auto_arma
)


# ==============================================================================
#  OKUN FIT PLOT & PARAM TABLE
# ==============================================================================


# --- Load Master Df ---
master_okun <- read_csv(data_save_paths$processed$okun_master_csv, 
                        show_col_types = FALSE) %>%
  filter(quarter >= as.yearqtr(val_T1_okun))
  

# --- Load GDP Forecasts ---
gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                show_col_types = FALSE) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)


# Y Df for full FIt Estimation
# ------------------------------------------------------------------------------
Y_data_okun <- master_okun %>%
  select(unemp_rate, spf_5y_unemp)

# Subset X Data and slice off the last 2 rows
X_data_okun <- master_okun %>%
  select(gdp_gap, gdp_gap_lag_1, gdp_gap_lag_2)


# Create the full manifest container object for the Okun setup
ssm_okun <- initialize_my_okun_ssm( # Update function name if it matches your initialize setup
  Y_data            = Y_data_okun,
  X_data            = X_data_okun,
  parameter_guesses = okun_parameter_guess # Your configuration guess object
)

generate_param_latex_table(manifest_source = ssm_okun$manifest, 
                           model_name = "Okun's Law Model",
                           save_path = output_save_paths$tables$okun_param_table)


# ==============================================================================
# Estimate Fit Of Okun SSM Model
# ==============================================================================
# Run the loglik function to extract the fit




last_params_list_okun <- okun_params_df %>% 
  tail(1) %>% 
  select(-quarter, -natural_rate) %>% 
  as.list() %>% 
  lapply(unlist)


param_last_fit_okun <- model2param_gen(last_params_list_okun, ssm_okun)

# --- Get OKUN RES OF FINAL PARAMS ---
final_res_okun <- loglik_ssm_core(ssm =ssm_okun,
                                  theta = param_last_fit_okun,
                                  return_full_res = TRUE
)

# Extract specific rows for plotting
dates_okun            <- master_okun$quarter # Dates for X axis
observed_unemp        <- Y_data_okun$unemp_rate
spf_unemp_expect      <- Y_data_okun$spf_5y_unemp
fitted_unemp_t_t      <- final_res_okun$fitted.obs[, 1] # Fitted Y
latent_nairu_trend    <- final_res_okun$r[, 1] # First state is NAIRU
output_gap_track      <- X_data_okun$gdp_gap # Exogenous regressor -> Output gap


# Create Data Tibble expected by function
#-------------------------------------------------------------------------------
okun_plot_data <- tibble(
  date          = as.yearqtr(dates_okun),
  obs_unemp     = as.numeric(observed_unemp),
  spf_survey    = as.numeric(spf_unemp_expect),
  fitted_unemp  = as.numeric(fitted_unemp_t_t),
  latent_trend  = as.numeric(latent_nairu_trend),
  gdp_gap       = as.numeric(output_gap_track)
)

# --- Add Natural Rate ---

# Extract from param_df and join
rolling_natural_rate_okun <- okun_params_df %>%
  select(all_of(c("quarter", "natural_rate"))) %>%
  mutate(quarter = as.yearqtr(quarter))

okun_plot_data <- okun_plot_data %>%
  left_join(rolling_natural_rate_okun, by = c("date" = "quarter"))

# ============================================================================
# Plot Setting and Plotting
# ============================================================================

okun_top_metrics <- c(
  "fitted_unemp" = "Fitted Unemployment",
  "obs_unemp"    = "Observed Unemployment",
  
  "latent_trend" = "Nat. Unemp. Rate",
  "spf_survey"   = "SPF 5y Unemp",
  "natural_rate" = "Last State in Recursive Est"
)

okun_bottom_metrics <- c(
  "gdp_gap" = "Output Gap"
)

# HEX Color Codes
okun_top_colors <- c(
  "Observed Unemployment"   = "#34495e", # Dark Slate
  "Fitted Unemployment"     = "#2980b9", # Deep Blue
  "Nat. Unemp. Rate" = "#e74c3c", # Natural Rate Red
  "SPF 5y Unemp"  = "#f1c40f",  # Gold Yellow
  "Last State in Recursive Est" = "green"
)

okun_bottom_colors <- c(
  "Output Gap" = "#16a085" # Teal
)

# ============================================================================
# Generate Plot
# ============================================================================
okun_dashboard <- plot_state_space_fit(
  plot_df        =  okun_plot_data,
  title          = "Okun's Law Model",
  subtitle       = "Fit Plot for Full Sample Estimation",
  top_metrics    = okun_top_metrics,
  bottom_metrics =  okun_bottom_metrics,
  top_colors     = okun_top_colors,
  bottom_colors  =  okun_bottom_colors,
  zlb_bounds     = NULL, # Left NULL since this is unemployment
  y_label_top    = "Unemployment Rate (%)",
  y_label_bottom = "Output Gap (%)",
  save_path      = output_save_paths$plots$fit_okun
)

print(okun_dashboard)



# ==============================================================================
#  PHILLIPS FIT PLOT & PARAM TABLE
# ==============================================================================


# Prepare Data
# ------------------------------------------------------------------------------
phillips_data_matrix_full <- build_data_matrix_philips(
  T_0             = val_T1_phillips,  # Or your full sample start preference
  vantage_quarter = "2026 Q1",     # Up to latest realized history block
  data            = master_philips
)

# Extract Measurement vector Y (Inflation and SPF 5Y Expectation survey)
# Note: rows between 2005-2015 for 5y_cpi_forecast are allowed to be NA
Y_data_philips <- phillips_data_matrix_full %>%
  select(all_of(c("log_inflation_diff", "5y_cpi_forecast")))

# Extract Structural Determinants vector X (Gaps and Lags)
X_data_philips <- phillips_data_matrix_full %>%
  select(all_of(c("gdp_gap", "lop_gap")))


# Run The Estimation
# ------------------------------------------------------------------------------

ssm_phillips <- initialize_my_philips_ssm(
  Y_data            = Y_data_philips,
  X_data            = X_data_philips,
  parameter_guesses = philips_parameter_guess # Your setup initial config list
)

generate_param_latex_table(manifest_source = ssm_phillips$manifest, 
                           model_name = "Phillips Model",
                           save_path = output_save_paths$tables$philips_param_table)



last_params_list_phillips <- phillips_parmas_df %>% 
  tail(1) %>% 
  select(-quarter, -natural_rate) %>% # Drop tracking columns
  as.list() %>% 
  lapply(unlist)

param_last_fit_phillips <- model2param_gen(last_params_list_phillips, ssm_phillips)


final_res_philips <- loglik_ssm_core(ssm =ssm_phillips,
                                     theta = param_last_fit_phillips,
                                     return_full_res = TRUE
)


rolling_natural_rate_philips <- phillips_parmas_df %>%
  select(all_of(c("quarter", "natural_rate"))) %>%
  mutate(quarter = as.yearqtr(quarter))



# Run the global optimizer routine across the economic parameter bounds


# Extract specific quantitative tracks for plotting variables
# Adjust state array indexes [ , x] to exactly match your kalman_implementation_philips.R vector indices
dates_philips       <- phillips_data_matrix_full$quarter
observed_cpi_inf    <- Y_data_philips$log_inflation_diff
spf_expectations    <- Y_data_philips$`5y_cpi_forecast`

# Extract unobserved states from the filtered state array matrix ($r$)
latent_inflation_trend <- final_res_philips$r[, 1] # Assumes your State 1 is pi_bar_t
fitted_inflation_t_t   <- final_res_philips$fitted.obs[, 1] # Model's overall inflation fit

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
  "fitted_inf"   = "Fitted Inflation",
  "latent_trend" = "Natural Rate of Inflation",
  "spf_survey"   = "SPF 5y Inf Forecast",
  "natural_rate" = "Last State in Recursive Est"
)

philips_bottom_metrics <- c(
  "gdp_gap" = "Output Gap",
  "lop_gap" = "LOP Gap"
)

# Define exact high-contrast academic HEX color mappings
philips_top_colors <- c(
  "Observed CPI Inflation"         = "#34495e", # Dark Slate
  "Fitted Inflation"         = "#2980b9", # Deep Blue
  "Natural Rate of Inflation" = "#e74c3c", # Core Trend Red
  "SPF 5y Inf Forecast"  = "#f1c40f",  # Gold Yellow
  "Last State in Recursive Est" = "green"
)

philips_bottom_colors <- c(
  "Output Gap"         = "#16a085", # Teal
  "LOP Gap"     = "#9b59b6"  # Amethyst Purple
)




philips_fit_plot <- plot_state_space_fit(
  plot_df        = philips_plot_data,
  title          = "Phillips Curve Model",
  subtitle       = "Fit Plot for Full Sample Estimation",
  top_metrics    = philips_top_metrics,
  bottom_metrics = philips_bottom_metrics,
  top_colors     = philips_top_colors,
  bottom_colors  = philips_bottom_colors,
  zlb_bounds     = NULL, # Set to NULL because inflation parameters are unconstrained by a ZLB floor
  y_label_top    = "Inflation (%)",
  y_label_bottom = "Output and LOP Gaps (%)",
  save_path      = output_save_paths$plots$fit_philips
)

# Print to your RStudio Plot viewer window
print(philips_fit_plot)




# ==============================================================================
#  TAYLOR FIT PLOT & PARAM TABLE
# ==============================================================================

# --- Select the already Transformed data for the model

# From The Full dataset select the Data needed for the Rolling estimation of the FUll timeline
master_taylor <- read_csv(data_save_paths$processed$taylor_master_csv, 
                          show_col_types = FALSE) %>%
  filter(quarter >= as.yearqtr(val_T1_taylor))

Y_data_taylor <- master_taylor %>%
  select(all_of(c("saron_libor_splice", "forward_rate")))%>%
  slice(4:n())

# Set inf_gap to hp_inf_gap if you want to estimate with the HP inf gap
X_data_taylor <- master_taylor %>%
  select(all_of(c("gdp_gap", "inf_gap")))%>%
  slice(4:n())

dates_df_taylor <- master_taylor %>%
  select(all_of(c("quarter")))%>%
  slice(4:n())



rolling_natural_rate_taylor <- taylor_params_df %>%
  select(all_of(c("quarter", "natural_rate"))) %>%
  mutate(quarter = as.yearqtr(quarter))



# ==============================================================================
# Estimate and Plot the Full Sample Fit
# ==============================================================================


# Run Parameter Estimation
# ------------------------------------------------------------------------------

# --- Initialize SSM
ssm_taylor <- initialize_taylor_ssm(Y_data = Y_data_taylor,
                                    X_data = X_data_taylor,
                                    parameter_guesses = snb_rate_parameter_guess)

generate_param_latex_table(manifest_source = ssm_taylor$manifest, 
                           model_name = "Taylor Rule Model",
                           save_path = output_save_paths$tables$taylor_param_table)

last_params_list_taylor <- taylor_params_df %>% 
  tail(1) %>% 
  select(-quarter, -natural_rate) %>% # Drop tracking columns
  as.list() %>% 
  lapply(unlist)

param_last_fit_taylor <- model2param_gen(last_params_list_taylor, ssm_taylor)


final_res_taylor <- loglik_ssm_core(ssm =ssm_taylor,
                                    theta = param_last_fit_taylor,
                                    return_full_res = TRUE
)
# --- Extract the values for the plot ---
shadow_rate <- final_res_taylor$fitted.obs[, 1]
natural_rate_est <- final_res_taylor$r[, 1] # State 1
tp_trend <- final_res_taylor$r[, 2]
tp_cycle <- final_res_taylor$r[, 3]
dates <- dates_df_taylor$quarter# Assuming you have a date vector
snb_rate <-Y_data_taylor$saron_libor_splice
fw_rate <- Y_data_taylor$forward_rate
inflation_gap <- X_data_taylor$inf_gap
gdp_gap <- X_data_taylor$gdp_gap


#Plot the Data
# ==============================================================================
taylor_plot_data <- tibble(
  date          = as.yearqtr(dates), # Convert yearqtr to Date format for smooth ggplot axes
  fitted_shadow = as.numeric(shadow_rate),
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
taylor_top_cols    <- c("forward_rate" = "Forward Rate",
                        "observed_rate" = "Observed Policy Rate",
                        "fitted_shadow" = "Fitted (Shadow) Rate",
                        "natural_interest_rate" = "Natural Interest Rate",
                        "natural_rate" = "Last State in Recursive Est"
)

taylor_bottom_cols <- c("inflation_gap" = "Inf. Gap",
                        "gdp_gap" = "GDP Gap",
                        "tp_trend"      = "TP Trend",
                        "tp_cycle"      = "TP Cycle")

taylor_top_colors    <- c("Observed Policy Rate" = "#2ecc71",
                          "Fitted (Shadow) Rate" = "#2980b9",
                          "Forward Rate" = "#e67e22",
                          "Natural Interest Rate" = "#e74c3c",
                          "Last State in Rolling Estimation" = "green"
)

taylor_bottom_colors <- c("Inflation Gap" = "#9b59b6",
                          "GDP Gap" = "#16a085",
                          "TP Trend" = "pink",
                          "TP Cycle" = "violet")

# Generate
snb_chart <- plot_state_space_fit(
  plot_df        = taylor_plot_data, # Tibble with all the Plotted Data
  title          = "Taylor Rule Model Fit",
  subtitle       = "Fit Plot for Full Sample Estimation",
  top_metrics    = taylor_top_cols,
  bottom_metrics = taylor_bottom_cols,
  top_colors     = taylor_top_colors,
  bottom_colors  = taylor_bottom_colors,
  zlb_bounds     = c(-0.75, 0.00),
  save_path = output_save_paths$plots$fit_taylor
)
print(snb_chart)


#===============================================================================
# Plot Live Forecasts
#===============================================================================


# Define your models and corresponding titles
fcst_inputs <- list(
  forecast_okun_df,
  philips_forecast_df_current,
  taylor_forecast_df,
  taylor_forecast_df_rounded
)

plot_titles <- c(
  "Unemployment Rate Forecast",
  "CPI Inflation Forecast)",
  "Policy Rate Forecast",
  "Rounded Policy Rate Forecast"
)

# Run and save the 2x2 plot (truncated from 2010 onwards)
combined_fig <- plot_combined_current_forecasts(
  forecast_list = fcst_inputs,
  titles        = plot_titles,
  start_year    = "2010 Q1",
  save_path     = output_save_paths$plots$current_forecasts, 
  width         = 12,
  height        = 8
)

print(combined_fig)

