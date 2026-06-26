################################################################################
# CONFIGURATION AND INITIALIZATION MANIFEST
#
# All variables that are chosen, selected, and defined are here.
# 
# SUMMARY OF OPERATIONS IN THIS SCRIPT:
#   1. Load Internal Functions (Data, Kalman Engine, Forecasting, Plots)
#   2. Define Input & Output System Paths (Raw Data & Directory Creation)
#   3. Assign External API & Programmatic Metadata IDs (KOF, BFS, SECO, Eurostat)
#   4. Establish Global Time-Series & Query Parameters (SNB API, Download Switches)
#   5. Map Data Assets & CSV/XLSX Save Formats (Raw & Joined Processed Sets)
#   6. Define Core Dynamic Model Target & Time-Series Arrays
#   7. Configure Model-Specific Parameter Guesses & Bounds (Okun, Phillips, Taylor)
#   8. Initialize Output Storage Ingestion Targets (Plots, Params, TeX Tables)
################################################################################


# ==============================================================================
# 1. Load internal functions
# ==============================================================================


# Data Handling / Formatting Functions
# ------------------------------------------------------------------------------
source(here("R", "load_snb_data.R")) # Data loading SNB
source(here("R", "load_kof_data.R")) # Data loading KOF
source(here("R", "utils.R")) # Diverse Utility functions
source(here("R", "model_input_preparation.R")) # Functions to prepare model Data Inputs
source(here("R", "latex_formatting.R"))


# Kalman Implementation Functions
# ------------------------------------------------------------------------------

# General Functions implementation
source(here("R", "kalman_estimation_engine.R")) # Functions to
source(here("R", "build_model_matrices.R"))

# Specific Math Stuff
source(here("R", "kalman_implementation_base.R"))
source(here("R", "kalman_implementation_philips.R"))
source(here("R", "kalman_implementation_taylor.R"))

#Forecasting Functions
source(here("R", "ssm_forecasting.R"))

# Visualizations
source(here("R", "visualizations.R"))


# ==============================================================================
# 2. FILE SYSTEM PATHS & DIRECTORY ENVIRONMENT SETUP
# ==============================================================================
# This section handles all paths with here. All paths are relative to the repo
# root. Further we handle external id's or link to download data from external
# sources. Finally we handle the date range for downloads and if we want to
# force a re-download of all data



# --- Set Paths / create folders ---


# Base Data  Storage Path
# ------------------------------------------------------------------------------
data_base_path <- here("data")

raw_data_path  <- here("data", "raw")
if (!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)


# Data Keys for Downloads / API Calls
# ------------------------------------------------------------------------------

### BFS Data
#cpi_asset_id <- "36483229" # ID for CPI Download
#emp_asset_id <- "px-x-0602000000_101" # ID From Package
#unemp_asset_id <- "36453929" # Id from package Catalogue

# alternative id for other value:
# unemp_asset_id = "36453929", # Id from package Catalog


# --- External Metadata and IDs ---
# IDs for programmatic downloads via BFS/KOF wrappers
data_external_ids <- list(
  bfs_cpi        = "36483229",
  bfs_employment = "px-x-0602000000_101",
  bfs_unemployment = "36589267",
  url_seco_gdp   = "https://scheduler.swissdatas.ch/scheduled/ch-seco-gdp.csv",
  ppi_asset_id =  "36532319",
  kof_data_key = "kof_consensus_forecast_mean"
  
  )


# --- Define Parameters ---

# Define start and end dates for SNB Data
from_date <- "1972-01"
to_date <- format(Sys.Date(), "%Y-%m")

# Set True if you want to re-download Data
# If there is no data, API gets called automatically
do_api_call <- FALSE

if (do_api_call) {
  message("do_api_call set to TRUE. Redownloading all files")
  
}


# Data Save Paths
# ------------------------------------------------------------------------------
data_save_paths <- list(
  
  # --- Raw Data: Source files directly from APIs or URLs ---
  raw = list(
    
    # --- SNB Data ---
    money_market_csv    = here(raw_data_path, "money_market.csv"),
    money_market_json   = here(raw_data_path, "money_market_metadata.json"),
    
    gov_bonds_csv       = here(raw_data_path, "gov_bonds.csv"),
    gov_bonds_json      = here(raw_data_path, "gov_bonds_metadata.json"),
    
    reer_ppi_eu_csv     = here(raw_data_path, "reer_ppi_eu.csv"),
    reer_ppi_eu_json    = here(raw_data_path, "reer_ppi_eu_metadata.json"),
    
    ex_eur_av_csv       = here(raw_data_path, "ex_eur_av.csv"),
    ex_eur_av_json      = here(raw_data_path, "ex_eur_av_metadata.json"),
    
    ex_eur_eom_csv      = here(raw_data_path, "ex_eur_eom.csv"),
    ex_eur_eom_json     = here(raw_data_path, "ex_eur_eom_metadata.json"),
    
    snb_policy_csv     = here(raw_data_path, "snb_policy_rate.csv"),
    snb_policy_json    = here(raw_data_path, "snb_policy_rate_metadata.json"),
    
    # --- KOF Data ---
    kof_master_csv      = here(raw_data_path, "kof_consensus_master.csv"),
    
    # --- BFS Data ---
    cpi_series_xlsx     = here(raw_data_path, "cpi_series.xlsx"),
    employment_csv      = here(raw_data_path, "employment_data.csv"),
    unemployment_csv    = here(raw_data_path, "unemployment_canton.csv"),
    ppi_csv             = here(raw_data_path, "ppi_ch.xlsx"),
    
    # --- SECO Data ---
    gdp_seco_csv        = here(raw_data_path, "gdp.csv"),
    
    # --- EUROSTAT ---
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


# --- Names of Data to keep ---

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


#===============================================================================
#Forecasting Settings
#===============================================================================

# ---  General Settings ---
forecast_starting_date <- as.yearqtr("2024 Q2")

# FOr Indexing
indexing_date <- "2020-12-01"

# If FALSE loading from files
run_estimation <- TRUE

# Forecast Horizon
h <- 8

# If true use params from last estimation, if false reestimate over full sample
fit_plot_last_est <- TRUE

# Inflation Lag is wether we use quarterly or yearly inf differentia
# inf_diff_q <- 4

# hp_filter_burn_in <- "1990-01-01"
  
# ==============================================================================
# Parameter Guesses Settings
# ==============================================================================


# Define outside your functions as your master configuration object
okun_manifest_source <- list(
  beta1 = list(
    name = "beta1",
    label = "Contemporaneous GDP gap coefficient",
    category = "Structural Dynamics",
    val = -0.1,
    rule = 0
  ),
  beta2 = list(
    name = "beta2",
    label = "First lag GDP gap coefficient",
    category = "Structural Dynamics",
    val = -0.1,
    rule = 0
  ),
  beta3 = list(
    name = "beta3",
    label = "Second lag GDP gap coefficient",
    category = "Structural Dynamics",
    val = -0.1,
    rule = 0
  ),
  phi = list(
    name = "phi",
    label = "Autoregressive persistence parameter",
    category = "Structural Dynamics",
    val = 0.6,
    rule = 2
  ),
  sigma_unemp_rate = list(
    name = "sigma_unemp_rate",
    label = "Unemployment rate deviation shock SD",
    category = "Measurement Noise",
    val = 0.5,
    rule = 1
  ),
  sigma_spf_5y_unemp = list(
    name = "sigma_spf_5y_unemp",
    label = "Survey expectation shock SD",
    category = "Measurement Noise",
    val = 0.5,
    rule = 1
  ),
  xi_n = list(
    name = "xi_n",
    label = "Natural rate innovation variance",
    category = "State Innovation",
    val = 0.1,
    rule = 1
  ),
  
  # Filter Environment States
  state_init = list(
    name = "state_init",
    label = "Initial State rho_0",
    category = "State Initialization",
    val = 1.3,
    rule = -1
  ),
  sigma_init = list(
    name = "sigma_init",
    label = "Initial Variance Sigma_0",
    category = "State Initialization",
    val = 10.0,
    rule = -1
  )
)

# Define this outside your functions as your master configuration object
philips_manifest_source <- list(
  beta_y = list(
    name = "beta_y",
    label = "Output gap coefficient",
    category = "Structural Dynamics",
    val = 0.1,
    rule = 3,
    low = 0.001,
    high = 0.3
  ),
  psi_lop = list(
    name = "psi_lop",
    label = "Law of One Price coefficient",
    category = "Structural Dynamics",
    val = -0.1,
    rule = 0
  ),
  phi = list(
    name = "phi",
    label = "Autoregressive persistence",
    category = "Structural Dynamics",
    val = 0.8,
    rule = 3,
    low = 0.1,
    high = 0.99
  ),
  sigma_cpi = list(
    name = "sigma_cpi",
    label = "CPI deviation shock SD",
    category = "Measurement Noise",
    val = 0.2,
    rule = 1
  ),
  sigma_spf = list(
    name = "sigma_spf",
    label = "Survey expectation shock SD",
    category = "Measurement Noise",
    val = 0.2,
    rule = 1
  ),
  xi_n = list(
    name = "xi_n",
    label = "Latent factor shock variance",
    category = "State Innovation",
    val = 0.1,
    rule = 1
  )
)

taylor_manifest_source <- list(
  gamma_pi = list(
    name = "gamma_pi",
    label = "Inflation gap response coefficient",
    category = "Structural Dynamics",
    val = 0.9,
    rule = 1
  ),
  gamma_y = list(
    name = "gamma_y",
    label = "Output gap response coefficient",
    category = "Structural Dynamics",
    val = 0.5,
    rule = 1
  ),
  phi = list(
    name = "phi",
    label = "Policy interest rate smoothing parameter",
    category = "Structural Dynamics",
    val = 0.5,
    rule = 2
  ),
  rho_tp = list(
    name = "rho_tp",
    label = "Cyclical term premium persistence",
    category = "Structural Dynamics",
    val = 0.6,
    rule = 2
  ),
  sigma_policy = list(
    name = "sigma_policy",
    label = "Shadow policy equation shock SD",
    category = "Measurement Noise",
    val = 0.1,
    rule = 1
  ),
  xi_i = list(
    name = "xi_i",
    label = "Nominal natural rate shock SD",
    category = "State Innovation",
    val = 0.1,
    rule = 1
  ),
  xi_tp_bar = list(
    name = "xi_tp_bar",
    label = "Trend term premium shock SD",
    category = "State Innovation",
    val = 0.05,
    rule = 1
  ),
  xi_tp_cycl = list(
    name = "xi_tp_cycl",
    label = "Cyclical term premium shock SD",
    category = "State Innovation",
    val = 0.05,
    rule = 1
  ),
  
  # Filter Environment States
  state_init = list(
    name = "state_init",
    label = "Initial State rho_0 Vector",
    category = "State Initialization",
    val = c(6, 1, 0.1),
    rule = -1
  ),
  sigma_init = list(
    name = "sigma_init",
    label = "Initial Variance Sigma_0 Matrix Seed",
    category = "State Initialization",
    val = c(10),
    rule = -1
  )
)

# FOr model 2 the SNB data is released with a year delay -> more like 3q due to late release of all other data
SNB_REER_DELAY <- 3

# change this as to just take the firs non NA obs of the reer bns
model_philips_burn_in <- "2000 Q4"


# ==============================================================================
# Output Save Path handling
# ==============================================================================

# Either store in Temp for just checking results or in persisten for true runs / proper output
# This only concerns the output folders

# Boolean to adjust
store_temp <- FALSE

# Logic
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
    spaghetti_taylor      = here(output_base, "plots/spaghetti_taylor.png")
    
  ),
  forecasts = list(
    forecast_df_okun    = here(output_base, "forecasts/unemployment_forecasts.csv"),
    forecast_df_philips = here(output_base, "forecasts/inflation_forecasts.csv"),
    forecast_df_taylor = here(output_base, "forecasts/policy_rate_forecasts.csv"),
    forecast_df_gdp_arima = here(output_base, "forecasts/gdp_arima_forecasts.csv")
  ),
  
  tables = list(
    
    # --- Parameter Tables ---
    
    okun_param_table = here(output_base, "tables/okun_param_table.tex"),
    philips_param_table = here(output_base, "tables/philips_param_table.tex"),
    taylor_param_table = here(output_base, "tables/taylor_param_table.tex"),
    
    
    # --- Benchmark Evaluation Tables ---
    
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
    eval_auto_arma_taylor = here(output_base, "tables/taylor_eval_auto_arma_table.tex")
  )
)
