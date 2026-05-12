################################################################################
# 
# All variables that are choses, selected, defined are here
#
################################################################################

# -----------------------
# Load functions
# -------------------------
source(here("R", "load_snb_data.R"))
source(here("R", "load_kof_data.R"))
source(here("R", "utils.R"))
source(here("R", "kalman_estimation_engine.R"))
source(here("R", "kalman_implementation.R"))
source(here("R", "ssm_forecasting.R"))
source(here("R", "build_model_matrices.R"))
source(here("R", "model_input_preparation.R"))

# ----------------------------------------------------------
# Data Config Section
# ----------------------------------------------------------

# This section handles all paths with here. All paths are relative to the repo
# root. Further we handle external id's or link to download data from external
# sources. Finally we handle the date range for downloads and if we want to
# force a re-download of all data

# --- Set Paths / create folders ---

# Set Raw Data Path
raw_path <- here("data", "raw")

### SNB Data
mm_csv   <- file.path(raw_path, "money_market.csv")
mm_json  <- file.path(raw_path, "money_market_metadata.json")

gb_csv   <- file.path(raw_path, "gov_bonds.csv")
gb_json  <- file.path(raw_path, "gov_bonds_metadata.json")

reer_csv   <- file.path(raw_path, "reer_ppi_eu.csv")
reer_json  <- file.path(raw_path, "reer_ppi_eu_metadata.json")

ex_eur_av_csv  <- file.path(raw_path, "ex_eur_av.csv")
ex_eur_av_json  <- file.path(raw_path, "ex_eur_av_metadata.csv")

ex_eur_eom_csv  <- file.path(raw_path, "ex_eur_eom.csv")
ex_eur_eom_json  <- file.path(raw_path, "ex_eur_eom_metadata.csv")

### KOF Data
kof_master <- file.path(raw_path, "kof_consensus_master.csv")
kof_data_key <- "kof_consensus_forecast_mean"

### BFS Data
cpi_asset_id <- "36483229" # ID for CPI Download
cpi_xlsx <- file.path(raw_path, "cpi_series.xlsx") # is stored as excel file (Learned by downloading and saving wrong)

emp_asset_id <- "px-x-0602000000_101" # ID From Package
emp_csv <- file.path(raw_path, "employment_data.csv")

unemp_asset_id <- "36453929" # Id from package Catalogue
unemp_csv <- file.path(raw_path, "unemployment_canton.csv")

ppi_asset_id <- "36532319"
ppi_csv <- file.path(raw_path, "ppi_ch.xlsx")




# SECO Data
gdp_csv <- file.path(raw_path, "gdp.csv")
url_gdp_csv <-
  "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"  # SECO GDP CSV URL

# Master df Path
master_df_path <- here("data")

# Create folder for raw data if it doesn't exist
if (!dir.exists(raw_path)) dir.create(raw_path) 


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




# ----------------------------------------------------------
# Data Config Section
# ----------------------------------------------------------
######################################################################3

# between two is a work in progress
# Set Base Data Paths
data_base_path <- here("data")
raw_base_path  <- here("data", "raw")

# Create folder for raw data if it doesn't exist
if (!dir.exists(raw_base_path)) dir.create(raw_base_path, recursive = TRUE)


# Organize Data Paths into a structured list
# sub-lists correspond to the data processing stage (raw vs processed/master)
data_save_paths <- list(
  
  # --- Raw Data: Source files directly from APIs or URLs ---
  raw = list(
    ## SNB Data
    money_market_csv    = here(raw_base_path, "money_market.csv"),
    money_market_json   = here(raw_base_path, "money_market_metadata.json"),
    
    gov_bonds_csv       = here(raw_base_path, "gov_bonds.csv"),
    gov_bonds_json      = here(raw_base_path, "gov_bonds_metadata.json"),
    
    reer_ppi_eu_csv     = here(raw_base_path, "reer_ppi_eu.csv"),
    reer_ppi_eu_json    = here(raw_base_path, "reer_ppi_eu_metadata.json"),
    
    ex_eur_av_csv       = here(raw_base_path, "ex_eur_av.csv"),
    ex_eur_av_json      = here(raw_base_path, "ex_eur_av_metadata.json"),
    
    ex_eur_eom_csv      = here(raw_base_path, "ex_eur_eom.csv"),
    ex_eur_eom_json     = here(raw_base_path, "ex_eur_eom_metadata.json"),
    
    snb_policy_csv     = here(raw_base_path, "snb_policy_rate.csv"),
    snb_policy_json    = here(raw_base_path, "snb_policy_rate_metadata.json"),
    
    ## KOF Data
    kof_master_csv      = here(raw_base_path, "kof_consensus_master.csv"),
    
    ## BFS Data
    cpi_series_xlsx     = here(raw_base_path, "cpi_series.xlsx"),
    employment_csv      = here(raw_base_path, "employment_data.csv"),
    unemployment_csv    = here(raw_base_path, "unemployment_canton.csv"),
    ppi_csv             = here(raw_base_path, "ppi_ch.xlsx"),
    
    ## SECO Data
    gdp_seco_csv        = here(raw_base_path, "gdp.csv")
  ),
  
  # --- Processed Data: Formatted and joined datasets ---
  processed = list(
    master_df_csv       = here(data_base_path, "master.csv")
  )
)

# --- External Metadata and IDs ---
# IDs for programmatic downloads via BFS/KOF wrappers
data_external_ids <- list(
  bfs_cpi        = "36483229",
  bfs_employment = "px-x-0602000000_101",
  bfs_unemployment = "36453929",
  kof_forecast   = "kof_consensus_forecast_mean",
  url_seco_gdp   = "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"
)


####################################################



# --- Names of Data to keep ---

# KOF data series we need for this project
kof_cols_to_keep <- c("date",
                      "ch.kof.consensus.q_qn_unemp_5y.mean",
                      "ch.kof.consensus.q_qn_prices_5y.mean",
                      "ch.kof.consensus.q_qn_3minterest_3m.mean",
                      "ch.kof.consensus.q_qn_3minterest_12m.mean"
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
  "snb_policy_rate_ts"
)


################################################################################
#Forecasting Settings
################################################################################

# ---  General Settings ---
forecast_starting_date <- as.yearqtr("2024 Q3")

run_estimation <- FALSE

h <- 8

  
# --- Okun Model Configuration settings ---

# We run the HP filter to estimate the gap. this determines the first observation
# of the GDP Variable
hp_filter_burn_in <- "2010-01-01"

# --- Initial guesses for Okun ---
# Initial parameters guess for okun model
okun_parameter_guess <- list(
  beta1 = -0.1,
  beta2 = -0.1,
  beta3 = -0.1,
  sigma_unemp_rate = 0.01,
  sigma_spf_5y_unemp = 0.005,
  xi_n = 0.001
)

# xi_n = 0.001



# --- Initial Guesses for Philips Model ---

philips_parameter_guess <- list(
  
  # Parameters on exogenous variables
  beta1 = 0.4,
  beta2 = 0.2,
  beta3 = 0.2,
  
  # sd on measurement variables
  sigma_cpi = 0.001,
  sigma_spf = 0.001,
  
  # state innovation
  xi_n = 0.001
)

# --- Initial guesses for Taylor Rule / SNB Policy Rate ---
# Initial parameters guess for the Taylor Rule State-Space Model
snb_rate_parameter_guess <- list(
  # Taylor Rule Parameters (Unconstrained)
  gamma_pi            = 0.9,    # Inflation gap coefficient
  gamma_y             = 0.5,    # Output gap coefficient
  
  # Persistence Parameters (Rule 2: Logit 0 to 1)
  phi                 = 0.5,    # Interest rate smoothing (it = rho*i_t-1 + ...)
  rho_tp              = 0.6,    # Persistence of cyclical term premium component
  
  # Measurement Noise Standard Deviations (Rule 1: Exponential > 0)
  sigma_policy        = 0.1,   # Error in the shadow policy rate equation
  # sigma_fwd           = 0.2,   # Error in the 5y-5y forward rate identification

  # State Innovation Standard Deviations (Rule 1: Exponential > 0)
  xi_i                = 0.1,  # Shock to the nominal natural rate (random walk)
  xi_tp_bar         = 0.05,  # Shock to the trend term premium (random walk)
  xi_tp_cycl          = 0.05    # Shock to the cyclical term premium (AR1)
)

# FOr model 2 the SNB data is released with a year delay -> more like 3q due to late release of all other data
SNB_REER_DELAY <- 3
model_philips_burn_in <- "2010 Q1"



# --- Output Save paths ---

# Only concerns the output folder

# Define the base output directory
output_base <- here("output")

# Organize paths into a structured list 
# The Lists inside the List correspond to teh folders inside the output folder
# Naming: always first what then model last
output_save_paths <- list(
  params = list(
    rolling_param_est_okun    = here(output_base, "parameter_estimation/okun_params.csv"),
    rolling_param_est_philips = here(output_base, "parameter_estimation/philips_params.csv")
  ),
  plots = list(
    params_okun    = here(output_base, "plots/params_okun_model.png"),
    params_philips = here(output_base, "plots/params_philips_model.png"),
    spaghetti_okun      = here(output_base, "plots/spaghetti_okun.png"),
    spaghetti_philips      = here(output_base, "plots/spaghetti_philips.png")
    
  ),
  forecasts = list(
    forecast_df_okun    = here(output_base, "forecasts/unemployment_forecasts.csv"),
    forecast_df_philips = here(output_base, "forecasts/inflation_forecasts.csv")
  )
)


