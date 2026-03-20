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
reer_meta_data <- fromJSON(paste(readLines(reer_json, encoding = "UTF8"), collapse=""))




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

