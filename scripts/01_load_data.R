# ---------------------
# Load Libraries
# ------------------------

package_loader("jsonlite") # To handle json files
package_loader("readxl")
package_loader("readr")
package_loader("kofdata") # To download KOF Data
package_loader("zoo") # Handle dates
package_loader("BFS") # Download BFS Data
package_loader("dplyr") 
package_loader("RCurl") # For API Calls


# -----------------------
# Load functions
# -------------------------
source(here("R", "load_snb_data.R"))

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

### BFS Data
cpi_asset_id <- "36483229" # ID for CPI Download
cpi_xlsx <- file.path(raw_path, "cpi_series.xlsx") # is stored as excel file (Learned by downloading and saving wrong)

emp_asset_id <- "px-x-0602000000_101" # ID From Package
emp_csv <- file.path(raw_path, "employment_data.csv")

unemp_asset_id <- "36453929" # Id from package Catalogue
unemp_csv <- file.path(raw_path, "unemployment_canton.csv")



# Define start and end dates for SNB Data
from_date <- "1972-01"
to_date <- format(Sys.Date(), "%Y-%m")

# Create folder for raw data if it doesn't exist
raw_path <- here("data", "raw")
if (!dir.exists(raw_path)) dir.create(raw_path) 


#force_redownload <- FALSE

# Set True if you want to redownload Data
# If there is no data, API gets called automatically
do_api_call <- FALSE

if (do_api_call) {
  message("do_api_call set to TRUE. Redownloading all files")
  
}

#--- Starting Data Download ---

# ----------------------------------------------------------
# Load the SNB Data
# ----------------------------------------------------------

# --- MONEY MARKET DATA ---

# if file doesn't exist or the api call is set to true download data
if (!file.exists(mm_csv) | do_api_call) {
  
  message("Downloading Money Market files via SNB API...")
  
  # Load Money Market Data
  load_snb_data(
    cube = "zimoma",
    folder = raw_path, # storage folder
    file_name = "money_market", # file saved as name
    from_date = from_date, # choose start date
    to_date = to_date, # choose end date
    ids = c('SARON', '3M0')
    ) # Id's can be a string or a vector of strings
}

# Load the Data as a df
mm_data <- read.table(mm_csv, skip=3, header = TRUE, sep=";")

# Load the Metadata as a df
mm_meta_data <- fromJSON(paste(readLines(mm_json, encoding = "UTF-8"), collapse=""))

message("Money Market files Loaded")



# --- GOVERNMENT BOND DATA ---

# if file doesn't exist or the api call is set to true download data
if (!file.exists(gb_csv) | do_api_call) {
  
  message("Downloading Money Market files via SNB API...")
  
  # Load Money Market Data
  gb_data <- load_snb_data(
    cube = "rendoblim",
    folder = raw_path,
    file_name = "gov_bonds",
    from_date = from_date, # choose start date
    to_date = to_date, # choose end date
    ids = c("5J", "10J")
  ) 
}

# Load the Data as a df
gb_data <- read.table(gb_csv, skip=3, header = TRUE, sep=";")

# Load the Metadata as a df
gb_meta_data <- fromJSON(paste(readLines(gb_json, encoding = "UTF-8"), collapse=""))

message("Gouvernment Bond Data files Loaded")


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

if (!file.exists(cpi_xlsx) | do_api_call) {
  
  message("Downloading CPI Data from BFS API...")
  
  # Download the file via BFS Package
  bfs_download_asset(
    number_asset = cpi_asset_id,
    destfile = cpi_xlsx
  )
  
  message("CPI Long Series downloaded to: ", cpi_xlsx)
}

message("Loading CPI Data from local disk...")

# Load Excel: selected sheet number and skip after inspecting file
cpi_raw <- read_excel(cpi_xlsx, sheet = 1, skip = 3) 


# --- Unemployment Data ---

# Note: BFS 'ts' files are often Excel (.xlsx), so check the extension

if (!file.exists(unemp_csv) | do_api_call) {
  
  message("Downloading BFS Unmployment Data...") 

  # load_asset with the numerical ID
  bfs_download_asset(
    number_asset = unemp_asset_id, 
    destfile = unemp_csv
    )

  message("File downloaded to: ", unemp_csv)
}


unemployment_raw <- read.table(
  unemp_csv, 
  header = TRUE, 
  sep = ",",           
  strip.white = TRUE,   # Cleans up whitespace
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# --- Employment Data

if (!file.exists(emp_csv) | do_api_call) {
  message("Downloading BFS Employment Data...")
  
  # This returns a tidy data frame automatically
  emp_raw <- bfs_get_data(
    number_bfs = emp_asset_id, 
    language = "fr"
  )
  
  write.csv(emp_raw, emp_csv, row.names = FALSE)
} else {
  emp_raw <- read.csv(emp_csv)
}


message("BFS Data ready for analysis.")


# --------------------------------------
# Download GDP data from SECO (via Viktor)
# ---------------------------------------


url_csv <-
  "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"  # SECO GDP CSV URL
data_gdp_raw <- read_csv(url_csv)                                                                   # load CSV from SECO website

data_gdp <- data_gdp_raw %>% 
  dplyr::filter(
    structure %in% c("gdp","inv_constr","inv_fixed", "cons_priv","cons_gov",          # choose GDP components
                          "exp_good_ex_vm","exp_serv","imp_serv","imp_good_ex_v"),
    type == "real",
    seas_adj == "cssa",
    structure == "gdp"
    
    )  

message("GDP Data ready for analysis.")

# ------------------------------------------
# KOF Data
# ------------------------------------------


if (!file.exists(kof_master) | do_api_call) {
  
  message("KOF Data missing. Calling KOF API...")
  
  # Get KOF Data from KOF Package
  # As far as I can see no API Key needed, but added for easier handling if needed later
  kof_consensus_forecast <- get_collection("kof_consensus_forecast_mean", api_key = NULL, show_progress = FALSE)
  
  # dataset is a list of time series objects. this puts them into a single time series object
  # Merge usually takes 2 arguments, here we have multiple. ts objects and go though them one by one
  kof_merged <- do.call(merge, lapply(kof_consensus_forecast, as.zoo))
  
  # Transform this zoo/ts object into a dataframe
  master_kof_consensus_forecast <- data.frame(
    date = as.yearmon(index(kof_merged)), # Converts ts data to month (could just be quarter if needed)
    coredata(kof_merged) # selects all data from zoo object                 
  )
  

  write_csv(master_kof_consensus_forecast, kof_master)

} else {
  message("Loading KOF Fata from local disk...")
  
  # Data loaded with metadata in a list for simple inspection
  master_kof_consensus_forecast <- read.table(kof_master, header = TRUE, sep = ",", check.names = FALSE)
}

master_kof_consensus_forecast <- master_kof_consensus_forecast %>%
  select("ch.kof.consensus.q_qn_unemp_5y.mean",
         "ch.kof.consensus.q_qn_prices_5y.mean",
         "ch.kof.consensus.q_qn_3minterest_3m.mean",
         "ch.kof.consensus.q_qn_3minterest_12m.mean")

message("KOF Data ready for analysis.")

