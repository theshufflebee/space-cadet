# ---------------------
# Load Libraries
# ------------------------
library(here)
library(jsonlite)
library(readxl)
library(readr)
library(kofdata)
library(zoo)
library(BFS)


# -----------------------
# Load functions
# -------------------------
source(here("R", "load_snb_data.R"))

# ----------------------------------------------------------
# Load the SNB Data
# ----------------------------------------------------------

# Paths to data files that are loaded in this section
raw_path <- here("data", "raw")
mm_csv   <- file.path(raw_path, "money_market.csv")
mm_json  <- file.path(raw_path, "money_market_metadata.json")
gb_csv   <- file.path(raw_path, "gov_bonds.csv")
gb_json  <- file.path(raw_path, "gov_bonds_metadata.json")
reer_csv   <- file.path(raw_path, "reer_ppi_eu.csv")
reer_json  <- file.path(raw_path, "reer_ppi_eu.json")
kof_master <- file.path(raw_path, "kof_consensus_master.csv")
cpi_xlsx <- file.path(raw_path, "cpi_series.xlsx") # is stored as excel file (Learned by downloading and saving wrong)
emp_csv <- file.path(raw_path, "employment_data.csv")
unemp_csv <- file.path(raw_path, "unemployment_canton.csv")



# Define start and end dates for SNB Data
fromDate <- "1972-01"
toDate <- format(Sys.Date(), "%Y-%m")

force_redownload <- FALSE


# --- MONEY MARKET DATA ---

if (force_redownload) {
  message("Forced Redownload. All data will be downloaded again.")
}

# First check if data exists, if so load it, except if there is a forced redownload
if (file.exists(mm_csv) && file.exists(mm_json) && !force_redownload) {
  message("Loading Money Market data from local disk...")
  
  # Data loaded with metadata in a list for simple inspection
  mm_data <- list(
    data = read.table(mm_csv, skip = 3, header = TRUE, sep = ";", check.names = FALSE),
    metadata = fromJSON(mm_json)
  )
} else {
  message("Money Market files missing. Calling SNB API...")
  
  mm_data <- load_snb_data(
    cube = "zimoma",
    folder = raw_path, # choose storage folder
    file_name = "money_market", # choose file name
    fromDate = fromDate, # choose start date
    toDate = toDate, # choose end date
    ids = c('SARON', '3M0'), # Id's can be a string or a vector of strings
  )
}

# --- GOVERNMENT BOND DATA ---

# First check if data exists, if so load it, except if there is a forced redownload
if (file.exists(gb_csv) && file.exists(gb_json) && !force_redownload) {
  message("Loading Government Bond data from local disk...")
  
  # Data loaded with metadata in a list for simple inspection
  gb_data <- list(
    data = read.table(gb_csv, skip = 3, header = TRUE, sep = ";", check.names = FALSE),
    metadata = fromJSON(gb_json)
  )
} else {
  message("Government Bond files missing. Calling SNB API...")
  
  gb_data <- load_snb_data(
    cube = "rendoblim",
    folder = raw_path,
    file_name = "gov_bonds",
    fromDate = fromDate, # choose start date
    toDate = toDate, # choose end date
    ids = c("5J", "10J")
  )
}


# --- REER ---

# Currently a manual download from the website. Not sure exactly how I can get
# it via API. Will look at it more later

reer_raw <- read_excel("data/raw/snb_reer_manual_download.xlsx", sheet = 1, skip = 15)

names(reer_raw) <- c("date", "overall_cpi", "eu_cpi", "overall_ppi", "eu_ppi")

# --- End of SNB Loading ---
message("SNB Data ready for analysis.")

# ----------------------------------------------
# Download Data From Swiss Gov via BFS Package
# -----------------------------------------------

# --- CPI Data ---

if (file.exists(cpi_xlsx) && !force_redownload) {
  message("Loading CPI Data from local disk...")
  
  cpi_raw <- read_excel(cpi_xlsx, sheet = 1, skip = 3) # selected sheet and skip after inspecting file
} else {
  message("CPI files missing. Calling BFS API...")

  # CPI ID from catalogue of BFS Package
  asset_id <- "36483229"

  # Download the file via BFS Package
  bfs_download_asset(
    number_asset = asset_id,
    destfile = cpi_xlsx
  )
  
  message("CPI Long Series downloaded to: ", cpi_xlsx)
  
  cpi_raw <- read_excel(cpi_xlsx, sheet = 1, skip = 3)
}



#================================================================================
# Work on this Part

# --- Unemployment Data ---

# 1. Define the path where you want to save the file
# Note: BFS 'ts' files are often Excel (.xlsx), so check the extension

if (!file.exists(unemp_csv) && !force_redownload) {
  message("Downloading BFS Unmployment Data...") 
  
  # Id from package Catalogue
  unemp_id <- "36453929"

  # load_asset with the numerical ID
  bfs_download_asset(
    number_asset = unemp_id, 
    destfile = unemp_csv
    )

  message("File downloaded to: ", unemp_csv)
}else{
  unemployment_raw <- read.table(
    unemp_csv, 
    header = TRUE, 
    sep = ",",           # Ct
    strip.white = TRUE,   # Cleans up whitespace
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# --- Employment Data

if (!file.exists(emp_csv) && !force_redownload) {
  message("Downloading BFS Employment Data...")
  employment_id <- "px-x-0602000000_101"
  
  # This returns a tidy data frame automatically
  emp_raw <- bfs_get_data(
    number_bfs = employment_id, 
    language = "fr"
  )
  
  write.csv(emp_raw, emp_csv, row.names = FALSE)
} else {
  emp_raw <- read.csv(emp_csv)
}




# --------------------------------------
# Download data from SECO (via Viktor)
# ---------------------------------------

url_csv <-
  "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"  # SECO GDP CSV URL
data_gdp <- read_csv(url_csv)                                                                   # load CSV from SECO website

data_gdp <- data_gdp %>% 
  filter(structure %in% c("gdp","inv_constr","inv_fixed", "cons_priv","cons_gov",          # choose GDP components
                          "exp_good_ex_vm","exp_serv","imp_serv","imp_good_ex_v"),
         type == "real", seas_adj == "cssa")  

message("GDP Data ready for analysis.")

# ------------------------------------------
# KOF Data
# ------------------------------------------


if (file.exists(kof_master) && !force_redownload) {
  message("Loading KOF Fata from local disk...")
  
  # Data loaded with metadata in a list for simple inspection
  master_kof_consensus_forecast <- read.table(kof_master, header = TRUE, sep = ",", check.names = FALSE)

} else {
  message("KOF Data missing. Calling KOF API...")
  
  # As far as I can see no API Key needed, but added to be sure
  kof_consensus_forecast <- get_collection("kof_consensus_forecast_mean", api_key = NULL, show_progress = FALSE)

  kof_merged <- do.call(merge, lapply(kof_consensus_forecast, as.zoo))
  master_kof_consensus_forecast <- data.frame(
    date = as.yearmon(index(kof_merged)), # Converts to month (could just be quarter if needed)
    coredata(kof_merged)                  
  )

  kof_save_path <- here("data", "raw", "kof_consensus_master.csv")
  if (!dir.exists(dirname(kof_save_path))) dir.create(dirname(kof_save_path), recursive = TRUE)

  write_csv(master_kof_consensus_forecast, kof_save_path)
}

master_kof_consensus_forecast <- master_kof_consensus_forecast %>%
  select("ch.kof.consensus.q_qn_unemp_5y.mean",
         "ch.kof.consensus.q_qn_prices_5y.mean",
         "ch.kof.consensus.q_qn_3minterest_3m.mean",
         "ch.kof.consensus.q_qn_3minterest_12m.mean")
