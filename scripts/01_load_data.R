# ---------------------
# Load Libraries
# ------------------------

# -----------------------
# Load functions
# -------------------------
source(here("R", "load_snb_data.R"))
source(here("R", "load_kof_data.R"))
source(here("R", "utils.R"))

# ----------------------------------------------------------
# Data Config Section
# ----------------------------------------------------------

# Paths to data files that are loaded in this section

# Set Raw Data Path
raw_path <- here("data", "raw")

### SNB Data
mm_csv   <- file.path(raw_path, "money_market.csv")
mm_json  <- file.path(raw_path, "money_market_metadata.json")

gb_csv   <- file.path(raw_path, "gov_bonds.csv")
gb_json  <- file.path(raw_path, "gov_bonds_metadata.json")

reer_csv   <- file.path(raw_path, "reer_ppi_eu.csv")
reer_json  <- file.path(raw_path, "reer_ppi_eu.json")


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



# Create folder for raw data if it doesn't exist
if (!dir.exists(raw_path)) dir.create(raw_path) 


# Define start and end dates for SNB Data
from_date <- "1972-01"
to_date <- format(Sys.Date(), "%Y-%m")

# Set True if you want to redownload Data
# If there is no data, API gets called automatically
do_api_call <- FALSE

if (do_api_call) {
  message("do_api_call set to TRUE. Redownloading all files")
  
}


# Naming of columns
names_snb_reer_df <- c("date", "overall_cpi", "eu_cpi", "overall_ppi", "eu_ppi")



#--- Starting Data Download ---

# ----------------------------------------------------------
# Load the SNB Data
# ----------------------------------------------------------


# --- MONEY MARKET DATA ---
get_snb_data_wrapper(mm_csv, do_api_call, "zimoma", "money_market",  
                 c('SARON', '3M0'))

# Load the Data as a df
mm_data <- read.table(mm_csv, skip=3, header = TRUE, sep=";")

# Load the Metadata as a df
mm_meta_data <- fromJSON(paste(readLines(mm_json, encoding = "UTF
8"), collapse=""))



# --- GOVERNMENT BOND DATA --
get_snb_data_wrapper(gb_csv, do_api_call, "rendoblim", "gov_bonds",  
                 c("5J", "10J"))

# Load the Data as a df
gb_data <- read.table(gb_csv, skip=3, header = TRUE, sep=";")

# Load the Metadata as a df
gb_meta_data <- fromJSON(paste(readLines(gb_json, encoding = "UTF
8"), collapse=""))



# --- REER ---

# Currently a manual download from the website. Not sure exactly how I can get
# it via API. Will look at it more later

reer_raw <- read_excel("data/raw/snb_reer_manual_download.xlsx", sheet = 1, skip = 15)

names(reer_raw) <- names_snb_reer_df

# ----------------------------------------------
# Download Data From Swiss Gov via BFS Package
# -----------------------------------------------

# --- CPI Data ---
bfs_wrapper(cpi_asset_id, cpi_xlsx, do_api_call = FALSE, type = "asset")
  
# Load Excel: selected sheet number and skip after inspecting file
cpi_raw <- read_excel(cpi_xlsx, sheet = 1, skip = 3) 


# --- Unemployment Data ---

# Note: BFS 'ts' files are often Excel (.xlsx), so check the extension
bfs_wrapper(unemp_asset_id, unemp_csv, do_api_call = FALSE, type = "asset")



unemployment_raw <- read.table(
  unemp_csv, 
  header = TRUE, 
  sep = ",",           
  strip.white = TRUE,   # Cleans up whitespace
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# --- Employment Data

bfs_wrapper(emp_asset_id, emp_csv, do_api_call = FALSE, type = "data")

emp_raw <- read.csv(emp_csv)


# --------------------------------------
# Download GDP data from SECO (via Viktor)
# ---------------------------------------


# Add Wrapper
download_url_csv_wrapper(url = url_gdp_csv,
                         filepath  = gdp_csv,
                         do_api_call = do_api_call)
  
# Next Script
data_gdp_raw <- read_csv(gdp_csv)                   



# ------------------------------------------
# KOF Data
# ------------------------------------------


download_kof_data_wrapper(file=kof_master,
                          kof_data_key = kof_data_key,
                          do_api_call = do_api_call)


master_kof_consensus_forecast <- read.table(kof_master, header = TRUE, sep = ",", check.names = FALSE)

