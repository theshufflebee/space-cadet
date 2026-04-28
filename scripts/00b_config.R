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
source(here("R", "kalman_builder.R"))
source(here("R", "kalman_procedures.R"))
source(here("R", "ssm_forecasting.R"))
source(here("R", "okun_matrices.R"))


       
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
  "ppi_ch_ts"
)

# Okun Model Configuration settings

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
  sigma_spf_5y_unemp = 0.005
)

# xi_n = 0.001


# FOr model 2 the SNB data is released with a year delay -> more like 3q due to late release of all other data
SNB_REER_DELAY <- 3
model_philips_burn_in <- "2010 Q1"
