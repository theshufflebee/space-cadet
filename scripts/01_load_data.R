################################################################################
# 
# Download Data
#
################################################################################

# This script Downloads all the Data Needed for the Project. It uses a variety of
# Packages, API calls or direct downloads online. It automatically downloads data
# it finds is missing and if do_api_call is set to TRUE, it redownloads all the
# data.

# ==============================================================================
# Load the SNB Data
# ==============================================================================


# --- MONEY MARKET DATA ---
get_snb_data_wrapper(data_save_paths$raw$money_market_csv,
                     do_api_call,
                     "zimoma",
                     "money_market", 
                     c('SARON', '3M0'))


# --- GOVERNMENT BOND DATA --
get_snb_data_wrapper(data_save_paths$raw$gov_bonds_csv,
                     do_api_call,
                     "rendoblim",
                     "gov_bonds",
                     c("5J", "10J"))


# --- REER ---
get_snb_data_wrapper(data_save_paths$raw$reer_ppi_eu_csv,
                     do_api_call,
                     "devwkieffid",
                     "reer_ppi_eu",
                     D0 = "P",
                     D1 = "E",
                     D2 ="I")


get_snb_data_wrapper(data_save_paths$raw$reer_ppi_eu_csv,
                     do_api_call,
                     "devwkieffid",
                     "reer_ppi_eu",
                     D0 = "P",
                     D1 = "E",
                     D2 ="I")


# --- Exchange Rate ---

# Average Exchange rates
get_snb_data_wrapper(data_save_paths$raw$ex_eur_av_csv, do_api_call, "devkum", "ex_eur_av",  
                     D0 = "M0",
                     D1 = "EUR1")


# End of Months Exchange rates
get_snb_data_wrapper(data_save_paths$raw$ex_eur_eom_csv, do_api_call, "devkum", "ex_eur_eom",  
                     D0 = "M1",
                     D1 = "EUR1")


# --- Policy Rate ---
get_snb_data_wrapper(data_save_paths$raw$snb_policy_csv, do_api_call, "snboffzisa", "snb_policy_rate",  
                     D0 = "LZ,OG0,UG0")




# ==============================================================================
# Download Data From Swiss Gov via BFS Package
# ==============================================================================


# --- CPI Data ---
bfs_wrapper(data_external_ids$bfs_cpi,
            data_save_paths$raw$cpi_series_xlsx,
            do_api_call = do_api_call, type = "asset")


# --- Unemployment Data ---
# Note: BFS 'ts' files are often Excel (.xlsx), so check the extension
bfs_wrapper(data_external_ids$bfs_unemployment,
            data_save_paths$raw$unemployment_csv,
            do_api_call = TRUE,
            type = "asset")



# --- Employment Data
bfs_wrapper(data_external_ids$emp_asset_id,
            data_save_paths$raw$employment_csv,
            do_api_call = do_api_call,
            type = "data")


# --- PPI Data ---
bfs_wrapper(data_external_ids$ppi_asset_id,
            data_save_paths$raw$ppi_csv,
            do_api_call = do_api_call,
            type = "asset")


# ==============================================================================
# Download GDP data from SECO (via Viktor)
# ==============================================================================

download_url_csv_wrapper(url = data_external_ids$url_seco_gdp,
                         filepath  = data_save_paths$raw$gdp_seco_csv,
                         do_api_call = do_api_call)
  

# ==============================================================================
# KOF Data
# ==============================================================================

download_kof_data_wrapper(file = data_save_paths$raw$kof_master_csv,
                          kof_data_key = data_external_ids$kof_data_key,
                          do_api_call = do_api_call)





# ==============================================================================
# EUROSTAT Data
# ==============================================================================

# Install if you haven't already
# install.packages("eurostat")

# Download the PPI dataset
# 'sts_inppd_m' = Producer prices in industry, monthly


if (do_api_call || !file.exists(data_save_paths$raw$eu_ppi_csv)) {
  raw_ppi_eur <- get_eurostat("sts_inppd_m", time_format = "date")
  
  # Filter for the specific series you need
  # - geo = "EA20" (Euro area - 20 countries) or "EA19"
  # - nace_r2 = "B-D" (Total industry, except construction)
  ppi_eur_clean <- raw_ppi_eur %>%
    filter(
      str_starts(geo, "EA|EU"),
      geo == "EA20", # EA20 -> those that adapted the euro
      nace_r2 == "B-D", # exclude the agriculture / mining sectors
      unit == "I21" # Index, 2021 = 100
    ) %>%
    
  select(date = TIME_PERIOD, ppi_eur = values)
  
  write_csv(ppi_eur_clean, data_save_paths$raw$eu_ppi_csv)
  message("Eurostat PPI data successfully donloaded and saved...")
} else {
  message("Eurostat PPI data exists. Loading from disk...")
  ppi_eur_clean <- read_csv(data_save_paths$raw$eu_ppi_csv)
}

message("[SUCCESS] DATA LOADING DONE...")





