################################################################################
# 
# CONFIG.R Set the estimation Settings
#
################################################################################

# Set the seed for all things random
set.seed(42)

# ==============================================================================
# Load functions
# ==============================================================================


# Loading Data Handling / Formatting Functions
source(here("R", "data_download.R")) # Data loading SNB
source(here("R", "utils.R")) # Diverse Utility functions
source(here("R", "model_input_preparation.R")) # Functions to prepare model Data Inputs
source(here("R", "latex_formatting.R"))
source(here("R", "02_parallel", "est_ssm_para.R"))
source(here("R", "latex_formatting.R"))



# General Kalman Filter Functions
source(here("R", "crea_fcst_eval_utils.R"))
source(here("R", "kalman_base.R"))


# Specific SSM Settings
source(here("R", "01_matrix_ssm_construction", "build_okun_matrices.R"))
source(here( "R", "01_matrix_ssm_construction", "build_phillips_matrices.R"))
source(here( "R", "01_matrix_ssm_construction", "build_taylor_matrices.R"))

# Specific Kalman filter for the taylor model
source(here("R", "kalman_filter_taylor.R"))

#Forecasting Functions
source(here("R", "forecasting_functions.R"))

# Visualizations
source(here("R", "visualizations.R"))

# ==============================================================================
# Parellel Estimation Folder settings
# ==============================================================================

LOAD_PARA_EST <- TRUE

# To analyze the estimations from the parallel estimations load these folders
TARGET_FOLDER_OKUN <- "final_run_okun_3" # Output destination folder: output/para/...
TARGET_FOLDER_PHILLIPS <- "final_run_phillips_3" # Output destination folder: output/para/...
TARGET_FOLDER_TAYLOR <- "final_run_taylor_3" # Output destination folder: output/para/...

# ==============================================================================
# Downloading and Data Prep Settings
# ==============================================================================

# -> If there is no data, API gets called even if set to false
do_api_call <- FALSE

if (do_api_call) {
  message("do_api_call set to TRUE. Redownloading all files")
}

# This section handles all paths with here. All paths are relative to the repo
# root. Further we handle external id's or link to download data from external
# sources. Finally we handle the date range for downloads and if we want to
# force a re-download of all data

# --- Set Paths / create folders ---
# ------------------------------------------------------------------------------

# Where transformed Data is saved
data_base_path <- here("data")

# Storage of RAW Data
raw_data_path  <- here("data", "raw")
if (!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)


# Data Keys for Downloads / API Calls
data_external_ids <- list(
  bfs_cpi        = "36483229",
  bfs_employment = "px-x-0602000000_101",
  bfs_unemployment = "36589267",
  #url_seco_gdp   = "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv",
  url_seco_gdp  = "https://scheduler.swissdatas.ch/scheduled/ch-seco-gdp.csv",
  cpi_asset_id = "36483229", # ID for CPI Download
  emp_asset_id = "px-x-0602000000_101", # ID From Package
  unemp_asset_id = "36453929", # Id from package Catalog
  ppi_asset_id =  "36532319",
  kof_data_key = "kof_consensus_forecast_mean"
  )


# Define start and end dates for SNB Data
from_date <- "1972-01"
to_date <- format(Sys.Date(), "%Y-%m")

# Data Save Paths
# ------------------------------------------------------------------------------
data_save_paths <- list(
  
  # --- Raw Data: Source files directly from APIs or URLs ---
  raw = list(
    ## SNB Data
    money_market_csv    = here(raw_data_path, "money_market.csv"),
    money_market_json   = here(raw_data_path, "money_market_metadata.json"),
    
    gov_bonds_csv       = here(raw_data_path, "gov_bonds.csv"),
    gov_bonds_json      = here(raw_data_path, "gov_bonds_metadata.json"),
    
    spot_gov_bonds_csv  = here(raw_data_path, "spot_gov_bonds.csv"),
    spot_gov_bonds_json = here(raw_data_path, "spot_gov_bonds_metadata.json"),
    
    reer_ppi_eu_csv     = here(raw_data_path, "reer_ppi_eu.csv"),
    reer_ppi_eu_json    = here(raw_data_path, "reer_ppi_eu_metadata.json"),
    
    ex_eur_av_csv       = here(raw_data_path, "ex_eur_av.csv"),
    ex_eur_av_json      = here(raw_data_path, "ex_eur_av_metadata.json"),
    
    ex_eur_eom_csv      = here(raw_data_path, "ex_eur_eom.csv"),
    ex_eur_eom_json     = here(raw_data_path, "ex_eur_eom_metadata.json"),
    
    snb_policy_csv     = here(raw_data_path, "snb_policy_rate.csv"),
    snb_policy_json    = here(raw_data_path, "snb_policy_rate_metadata.json"),
    
    ## KOF Data
    kof_master_csv      = here(raw_data_path, "kof_consensus_master.csv"),
    
    ## BFS Data
    cpi_series_xlsx     = here(raw_data_path, "cpi_series.xlsx"),
    employment_csv      = here(raw_data_path, "employment_data.csv"),
    unemployment_csv    = here(raw_data_path, "unemployment_canton.csv"),
    ppi_csv             = here(raw_data_path, "ppi_ch.xlsx"),
    
    ## SECO Data
    gdp_seco_csv        = here(raw_data_path, "gdp.csv"),
    
    ## EUROSTAT
    eu_ppi_csv         = here(raw_data_path, "eu_ppi_raw.csv")
    
  ),
  
  # --- Processed Data: Formatted and joined datasets ---
  processed = list(
    master_df_csv       = here(data_base_path, "master.csv"),
    okun_master_csv  = here(data_base_path, "okun_master.csv"),
    philips_master_csv  = here(data_base_path, "philips_master.csv"),
    taylor_master_csv  = here(data_base_path, "taylor_master.csv")
  )
)


# --- DATA NAMING ---
# ------------------------------------------------------------------------------

# KOF data series we need for this project
kof_mapping <- list(
  "5y_unemp_forecast"    = list(old_col = "ch.kof.consensus.q_qn_unemp_5y.mean",      seasonal_adj = TRUE),
  "5y_cpi_forecast"      = list(old_col = "ch.kof.consensus.q_qn_prices_5y.mean",     seasonal_adj = FALSE),
  "3m_interest_forecast"  = list(old_col = "ch.kof.consensus.q_qn_3minterest_3m.mean",  seasonal_adj = FALSE),
  "12m_interest_forecast" = list(old_col = "ch.kof.consensus.q_qn_3minterest_12m.mean", seasonal_adj = FALSE)
)

# names of all time series needed for the project
ts_names <- c(
  "saron_ts",
  "libor_ts",
  "gb_5y_ts",
  "gb_10y_ts", 
  "reer_eu_ppi_ts",
  "cpi_ts",
  "unemployment_ts", 
  "employment_ts",
  "gdp_ts",
  "kof_5y_unemp", 
  "kof_5y_cpi",
  "kof_3m_interest",
  "kof_12m_interest",
  "ex_av_ts",
  "ex_eom_ts", 
  "eu_ppi_ts",
  "ppi_ch_ts",
  "snb_policy_rate_ts")


# ==============================================================================
# Estimation Settings
# ==============================================================================

# This is the start of the fit estimation / forst observation
val_T1_okun <- "1991-01-01"
val_T1_phillips <- "1982-01-01"
val_T1_taylor <- "1991-01-01"


  
# --- Initial guesses for Okun ---
okun_parameter_guess <- list(
  beta1 = -0.1,
  beta2 = -0.1,
  beta3 = -0.1,
  phi = 0.6,
  sigma_unemp = 0.3,
  sigma_spf_5y_unemp = 0.3,
  xi_trend = 0.02,
  xi_cycle = 0.11,
  state_init = c(1, 0),
  sigma_init = c(1, 0, 0, 1)
)

# --- Initial Guesses for Philips Model ---
philips_parameter_guess <- list(
  
  # Parameters on exogenous variables
  beta_y = 0.1,
  psi_lop = -0.1,
  phi = 0.6,
  
  # sd on measurement variables
  sigma_spf = 0.2,
  sigma_cpi = 1, #currently non existant in matrices
  
  # state innovation
  xi_trend = 0.02,
  xi_cycle = 0.5,

  state_init = c(1.3, 0),
  sigma_init = c(1, 0, 0, 1)
)

# --- Initial guesses for Taylor Rule / SNB Policy Rate ---
snb_rate_parameter_guess <- list(
  # Taylor Rule Parameters (Unconstrained)
  gamma_pi            = 0.9,    # Inflation gap coefficient
  gamma_y             = 0.5,    # Output gap coefficient
  
  # Persistence Parameters (Rule 2: Logit 0 to 1)
  phi                 = 0.7,    # Interest rate smoothing (it = rho*i_t-1 + ...)
  rho_tp              = 0.6,    # Persistence of cyclical term premium component
  
  # Measurement Noise Standard Deviations (Rule 1: Exponential > 0)
  sigma_policy        = 0.1,   # Error in the shadow policy rate equation

  # State Innovation Standard Deviations (Rule 1: Exponential > 0)
  xi_i                = 0.1,  # Shock to the nominal natural rate (random walk)
  xi_tp_bar         = 0.05,  # Shock to the trend term premium (random walk)
  xi_tp_cycl          = 0.05,    # Shock to the cyclical term premium (AR1)
  
  state_init = c(6, 1, 0.1),
  sigma_init = c(1, 0, 0, 
                 0, 1, 0,
                 0, 0, 1)
)

# FOr model 2 the SNB data is released with a year delay -> more like 3q due to late release of all other data
SNB_REER_DELAY <- 3

# can build this into the the functions building the Phillips data matrix, as they
# just need to filter for NA values. 
# model_philips_burn_in <- "2000 Q1"



# ==============================================================================
# --- Output Save Path Settings ---
# ==============================================================================

# Select either the temp or the persisten folder
store_temp <- FALSE

# Only concerns the output folder

# Define the base output directory
# can store either in the persistent folder which should contain clean and proper outputs
# or temp to check what you have (example shorter estimations)
output_persistent <- here("output", "persistent")
output_temp <- here("output", "temp")

output_base <- if(store_temp) output_temp else output_persistent

# generate sub folders
invisible(lapply(c("plots", "parameter_estimation", "forecasts", "tables"), function(subfolder) {
  dir_path <- file.path(output_base, subfolder)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
}))


# Organize paths into a structured list 
# The Lists inside the List correspond to teh folders inside the output folder
# Naming: always first what then model last
output_save_paths <- list(
  params = list(
    rolling_param_est_okun    = here(output_base, "parameter_estimation/okun_params.csv"),
    rolling_param_est_philips = here(output_base, "parameter_estimation/philips_params.csv"),
    rolling_param_est_taylor = here(output_base, "parameter_estimation/taylor_params.csv")
  ),
  plots = list(
    fit_okun    = here(output_base, "plots/fit_okun_model.png"),
    fit_philips = here(output_base, "plots/fit_philips_model.png"),
    fit_taylor  = here(output_base, "plots/fit_taylor_model.png"),
    params_okun    = here(output_base, "plots/params_okun_model.png"),
    params_philips = here(output_base, "plots/params_philips_model.png"),
    params_taylor  = here(output_base, "plots/params_taylor_model.png"),
    spaghetti_okun      = here(output_base, "plots/spaghetti_okun.png"),
    spaghetti_philips      = here(output_base, "plots/spaghetti_philips.png"),
    spaghetti_taylor      = here(output_base, "plots/spaghetti_taylor.png"),
    spaghetti_taylor_rounded = here(output_base, "plots/spaghetti_taylor_rounded.png"),
    errors_okun = here(output_base, "plots/errors_okun.png"),
    errors_phillips = here(output_base, "plots/errors_phillips.png"),
    errors_taylor = here(output_base, "plots/errors_taylor.png"),
    
    bench_error_rw_okun = here(output_base, "plots/errors_bench_rw_okun.png"),
    bench_error_rw_phillips = here(output_base, "plots/errors_bench_rw_phillips.png"),
    bench_error_rw_taylor = here(output_base, "plots/errors_bench_rw_taylor.png"),
    
    bench_error_ar1_okun = here(output_base, "plots/errors_bench_ar1_okun.png"),
    bench_error_ar1_phillips = here(output_base, "plots/errors_bench_ar1_phillips.png"),
    bench_error_ar1_taylor = here(output_base, "plots/errors_bench_ar1_taylor.png"),
    
    bench_error_auto_arma_okun = here(output_base, "plots/errors_bench_auto_arma_okun.png"),
    bench_error_auto_arma_phillips = here(output_base, "plots/errors_bench_auto_arma_phillips.png"),
    bench_error_auto_arma_taylor = here(output_base, "plots/errors_bench_auto_arma_taylor.png"),
    
    benchmark = list(
      okun_rw = here(output_base, "plots/benchmark_spaghetti_okun_rw.png"),
      okun_ar1 = here(output_base, "plots/benchmark_spaghetti_okun_ar1.png"),
      okun_auto_arma = here(output_base, "plots/benchmark_spaghetti_okun_auto_arma.png"),
      
      philips_rw = here(output_base, "plots/benchmark_spaghetti_philips_rw.png"),
      philips_ar1 = here(output_base, "plots/benchmark_spaghetti_philips_ar1.png"),
      philips_auto_arma = here(output_base, "plots/benchmark_spaghetti_philips_auto_arma.png"),
      
      taylor_rw = here(output_base, "plots/benchmark_spaghetti_taylor_rw.png"),
      taylor_ar1 = here(output_base, "plots/benchmark_spaghetti_taylor_ar1.png"),
      taylor_auto_arma = here(output_base, "plots/benchmark_spaghetti_taylor_auto_arma.png")
    ),
    
    current_forecasts = list(
      okun_current_forecasts = here(output_base, "plots/okun_current_forecast.png"),
      phillips_current_forecasts = here(output_base, "plots/phillips_current_forecast.png"),
      taylor_current_forecasts = here(output_base, "plots/taylor_current_forecast.png"),
      taylor_rounded_current_forecasts = here(output_base, "plots/taylor_rounded_current_forecast.png")
    )
  ),
  forecasts = list(
    forecast_df_okun    = here(output_base, "forecasts/unemployment_forecasts.csv"),
    forecast_df_philips = here(output_base, "forecasts/inflation_forecasts.csv"),
    forecast_df_taylor = here(output_base, "forecasts/policy_rate_forecasts.csv"),
    forecast_df_taylor_rounded = here(output_base, "forecasts/policy_rate_forecasts_rounded.csv"),
    forecast_df_gdp_arima = here(output_base, "forecasts/gdp_arima_forecasts.csv"),
    forecast_df_inf_arima = here(output_base, "forecasts/inf_arima_forecasts.csv")
  ),
  
  tables = list(
    
    # RW Benchmark
    eval_rw_okun = here(output_base, "tables/okun_eval_rw_table.tex"),
    eval_rw_philips = here(output_base, "tables/philips_eval_rw_table.tex"),
    eval_rw_taylor = here(output_base, "tables/taylor_eval_rw_table.tex"),
    
    # AR1 Benchmark
    eval_ar1_okun = here(output_base, "tables/okun_eval_ar1_table.tex"),
    eval_ar1_philips = here(output_base, "tables/philips_eval_ar1_table.tex"),
    eval_ar1_taylor = here(output_base, "tables/taylor_eval_ar1_table.tex"),
    
    # Auto Arma Benchmark
    eval_auto_arma_okun = here(output_base, "tables/okun_eval_auto_arma_table.tex"),
    eval_auto_arma_philips = here(output_base, "tables/philips_eval_auto_arma_table.tex"),
    eval_auto_arma_taylor = here(output_base, "tables/taylor_eval_auto_arma_table.tex"),
    
    # Initial Params & Constraints Tables
    okun_param_table = here(output_base, "tables/okun_param_table.tex"),
    philips_param_table = here(output_base, "tables/philips_param_table.tex"),
    taylor_param_table = here(output_base, "tables/taylor_param_table.tex")
    
  ),
  
  aux_results = list(
    # Regressions
    aux_reg_okun = here(output_base, "aux_results/aux_okun_reg_table.tex"),
    aux_reg_phillips = here(output_base, "aux_results/aux_phillips_reg_table.tex"),
    aux_reg_taylor = here(output_base, "aux_results/aux_taylor_reg_table.tex"),
    
    # Plots
    aux_plot_okun = here(output_base, "aux_results/aux_okun_plot.png"),
    aux_plot_phillips = here(output_base, "aux_results/aux_phillips_plot.png"),
    aux_plot_taylor = here(output_base, "aux_results/aux_taylor_plot.png"),
    
    # HP Plot
    aux_hp_plot_okun = here(output_base, "aux_results/aux_hp_okun_plot.png"),
    aux_hp_plot_phillips = here(output_base, "aux_results/aux_hp_phillips_plot.png"),
    aux_hp_plot_taylor = here(output_base, "aux_results/aux_hp_taylor_plot.png")
  )
)


# ==============================================================================
# --- ESTIMATION CONFIGURATION ---
# ==============================================================================

# Not yet implemented
estimation_settings <- list(
  
  # Nelder-Mead -> doesn't use gradient or slopes but is slow
  # uses approximation -> converges fast but can get stuck with bad surfaces
  # BFGS very precise (quasi Newtow gradient method), but highly sensitive to initial params
  
  # Warm Start currently depreciated
  
  # Okun's Law Model Configuration
  okun = list(
    methods    = c("Nelder-Mead", "bobyqa"), # , "BFGS"
    iters      = 3,
    warm_start = FALSE
  ),
  
  # Phillips Curve Model Configuration
  phillips = list(
    methods    = c("Nelder-Mead", "bobyqa"), # , "BFGS"
    iters      = 3,
    warm_start = FALSE
  ),
  
  # Taylor / Shadow Rate Model Configuration
  # Note: BFGS removed to prevent crashes, running 3 iters instead
  taylor = list(
    methods    = c("Nelder-Mead", "bobyqa"), 
    iters      = 3,
    warm_start = FALSE
  ),
  
  general_settings = list(
    show_warnings = FALSE,
    max_runs = 2000   # amount of times it calculates the log likelihood per iteration (iters)
  )
)


forecast_starting_date <- as.yearqtr("1985 Q1")

# For Indexing
indexing_date <- "2020-12-01"

# Set to false if you want to load estimation from Disk or from parallel estimation
run_rolling_estimation <- FALSE

# Forecasting Horizon
h <- 8
