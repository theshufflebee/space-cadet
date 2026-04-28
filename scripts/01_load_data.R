################################################################################
# 
# Download Data
#
################################################################################


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
8", warn = FALSE), collapse=""))


# --- GOVERNMENT BOND DATA --
get_snb_data_wrapper(gb_csv, do_api_call, "rendoblim", "gov_bonds",  
                 c("5J", "10J"))

# Load the Data as a df
gb_data <- read.table(gb_csv, skip=3, header = TRUE, sep=";")

# Load the Metadata as a df
gb_meta_data <- fromJSON(paste(readLines(gb_json, encoding = "UTF
8", warn = FALSE), collapse=""))


# --- REER ---
# Currently a manual download from the website. Not sure exactly how I can get
# it via API. Will look at it more later


get_snb_data_wrapper(reer_csv,
                     do_api_call,
                     "devwkieffid",
                     "reer_ppi_eu",
                     D0 = "P",
                     D1 = "E",
                     D2 ="I")

# Load the Data as a df
reer_raw <- read.table(reer_csv, skip=3, header = TRUE, sep=";")

# Load the Metadata as a df
reer_meta_data <- fromJSON(paste(readLines(reer_json, encoding = "UTF8", warn = FALSE), collapse=""), )

# --- Exchange Rate ---
get_snb_data_wrapper(ex_eur_av_csv, do_api_call, "devkum", "ex_eur_av",  
                     D0 = "M0",
                     D1 = "EUR1")

# Load the Data as a df
ex_av_raw <- read.table(ex_eur_av_csv, skip=3, header = TRUE, sep=";")



get_snb_data_wrapper(ex_eur_eom_csv, do_api_call, "devkum", "ex_eur_eom",  
                     D0 = "M1",
                     D1 = "EUR1")

# Load the Data as a df
ex_eom_raw <- read.table(ex_eur_eom_csv, skip=3, header = TRUE, sep=";")


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

# --- PPI Data ---

ppi_asset_id <- "36532319"
ppi_csv <- file.path(raw_path, "ppi_ch.xlsx")



bfs_wrapper(ppi_asset_id, ppi_csv, do_api_call = FALSE, type = "asset")



# --------------------------------------
# Download GDP data from SECO (via Viktor)
# ---------------------------------------

download_url_csv_wrapper(url = url_gdp_csv,
                         filepath  = gdp_csv,
                         do_api_call = do_api_call)
  
data_gdp_raw <- read_csv(gdp_csv)                   



# ------------------------------------------
# KOF Data
# ------------------------------------------


download_kof_data_wrapper(file=kof_master,
                          kof_data_key = kof_data_key,
                          do_api_call = do_api_call)


master_kof_consensus_forecast <- read.table(kof_master, header = TRUE, sep = ",", check.names = FALSE)



# ---------------------------------------------
# EUROSTAT Data
# ------------------------------------------------

# Install if you haven't already
# install.packages("eurostat")
library(eurostat)

# 1. Download the PPI dataset
# 'sts_inppd_m' = Producer prices in industry, monthly
raw_ppi_eur <- get_eurostat("sts_inppd_m", time_format = "date")

# 2. Filter for the specific series you need
# - geo = "EA20" (Euro area - 20 countries) or "EA19"
# - nace_r2 = "B-D" (Total industry, except construction)
ppi_eur_clean <- raw_ppi_eur %>%
  filter(
    geo == "EA20", # EA20 -> those that adapted the euro
    nace_r2 == "B-D", # exclude the agriculture / mining sectors
    unit == "I21" # Index, 2021 = 100
  ) %>%
  select(date = TIME_PERIOD, ppi_eur = values)

write_csv(ppi_eur_clean, file.path(raw_path, "eu_ppi_raw.csv") )








