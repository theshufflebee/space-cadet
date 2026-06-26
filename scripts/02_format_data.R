################################################################################
#
# Format Data into a time series df
#
################################################################################

# This Script Loads the data used in this Project from their respective file paths.
# It does not download the Data. 



# ==============================================================================
# Format SNB Data
# ==============================================================================


# --- SNB POLICY RATE ---

# Load Data
snb_policy_rate <- read.table(data_save_paths$raw$snb_policy_csv, skip=3, header = TRUE, sep=";")

# Aggregate SNB Data by firs calculating the mean between upper and lower band of
# LIBOR which was target until 12.06.2019, after its the SARON (policy rate)
snb_policy_rate_ts <- snb_policy_rate %>%
  pivot_wider(names_from = D0, values_from = Value) %>%
  mutate(
    # Use the point rate (LZ) if it exists, otherwise calculate the midpoint
    policy_rate = if_else(is.na(LZ), (UG0 + OG0) / 2, LZ)
  ) %>%
  select(all_of(c("Date", "policy_rate"))) %>%
  arrange(Date) %>% 
  mutate(date = as.yearmon(Date))

# --- Money Market Data

# Load Data
mm_data <- read.table(data_save_paths$raw$money_market_csv, skip=3, header = TRUE, sep=";")

# Format Data
saron_ts <- snb_api_data_to_ts(mm_data, "SARON", "saron")
libor_ts <- snb_api_data_to_ts(mm_data, "3M0", "3m_libor")


# --- Government Bond Data ---

# Load Data
gb_data <- read.table(data_save_paths$raw$gov_bonds_csv, skip=3, header = TRUE, sep=";")

# Format Data
gb_5y_ts <- snb_api_data_to_ts(gb_data, "5J", "5y_bond")
gb_10y_ts <- snb_api_data_to_ts(gb_data, "10J", "10y_bond")


# --- Exchange Rate Data ---

# Load Average Rate Data
ex_av_raw <- read.table(data_save_paths$raw$ex_eur_av_csv, skip=3, header = TRUE, sep=";")

# Format Average Rate Data
ex_av_ts <- format_time_series_df(ex_av_raw, "Date", "Value",  "ex_av", "%Y-%m")


# Load End of Month Data
ex_eom_raw <- read.table(data_save_paths$raw$ex_eur_eom_csv, skip=3, header = TRUE, sep=";")

# Format End of Month Data
ex_eom_ts  <- format_time_series_df(ex_eom_raw, "Date", "Value",  "ex_eom", "%Y-%m")



# REER Data does not need that step since it is only one TS in the download
# Its year date select end of month obs

# Load SNB REER DATA

# reer_raw <- read.table(data_save_paths$raw$reer_ppi_eu_csv, skip=3, header = TRUE, sep=";")


# Use Excel download
reer_raw <- read_excel(here("data/raw/reer_snb.xlsx"), sheet = 1, skip = 15) 


# Select the Good REER Data, sort it and make it year month
reer_ym <- reer_raw %>%
  rename(
    Date  = "...1",
    Value = "Index Euroraum PPI-basiert"
  ) %>%
  select(all_of(c("Value", "Date")))%>%
  arrange("Date")%>%
  mutate(date_ym = as.yearmon(Date)) %>%
  
  group_by(date_ym) %>%
  
  summarise(
    last_value = last(Value, order_by = Date),
    .groups = "drop"
  )

# Format SNB REER Data
reer_eu_ppi_ts <- format_time_series_df(reer_ym, "date_ym", "last_value", "reer_eu_ppi", "%b %Y")


# --- Load Associated SNB API METADATA ---
# ------------------------------------------------------------------------------
mm_meta_data <- fromJSON(paste(readLines(data_save_paths$raw$money_market_json,
                                         encoding = "UTF 8",
                                         warn = FALSE),
                               collapse=""))

gb_meta_data <- fromJSON(paste(readLines(data_save_paths$raw$gov_bonds_json,
                                         encoding = "UTF8",
                                         warn = FALSE),
                               collapse=""))

reer_meta_data <- fromJSON(paste(readLines(data_save_paths$raw$reer_ppi_eu_json,
                                           encoding = "UTF8",
                                           warn = FALSE),
                                 collapse=""), )


# ==============================================================================
# --- Format BFS Data ---
# ==============================================================================

# --- CPI ---
cpi_raw <- read_excel(data_save_paths$raw$cpi_series_xlsx, sheet = 1, skip = 3) 

cpi_ts <- format_time_series_df(cpi_raw, "Datum / Date", "38687", "cpi", "%Y-%m-%d", seasonal_adj = TRUE)

# --- PPI ---

# Process PPI by indexing in 2020 and formating date correlcty
ppi_ch_raw <- read_excel(data_save_paths$raw$ppi_csv, skip = 6, sheet = 1) 

ppi_ch_clean <- ppi_ch_raw %>%
  select(
    raw_date = "Datum",
    values  = "Dez 2020 = 100"
  ) %>%
  mutate(
    date = as.Date(as.numeric(raw_date), origin = "1899-12-30")
  )


ppi_ch_ts <- format_time_series_df(ppi_ch_clean, "date", "values", "ppi_ch", "%Y-%m-%d" )

# --- Unemployment ---

unemployment_raw <- read_csv(
  file = data_save_paths$raw$unemployment_csv,
  trim_ws = TRUE,        # Cleans up whitespace automatically
  show_col_types = FALSE # Keepsconsole clean
)

unemployment <- unemployment_raw %>%
  filter(REGION %in% "Total")

unemployment_ts <- format_time_series_df(unemployment, "PERIOD", "VALUE", "unemployment", "%Y-%m", seasonal_adj = TRUE)

# --- Employment ---

emp_raw <- read.csv(data_save_paths$raw$employment_csv)

employment <- emp_raw %>%
  # Filter out unnecessary data
  filter(Division.économique == "5-96 Total",
         Taux.d.occupation == "Equivalents plein temps, désaisonnalisé",
         Sexe == "Sexe - total") %>%
  
  # Transform quarterly into monthly with last month of quarter
  mutate(date_clean = zoo::as.yearmon(zoo::as.yearqtr(Trimestre, format = "%YQ%q"), fraction = 1))

# Format as ts

employment_ts <- format_time_series_df(employment,
                                           "date_clean",
                                           "Emplois.selon.la.division.économique..le.taux.d.occupation.et.le.sexe",
                                           "employment",
                                           "%b %Y",
                                           seasonal_adj = FALSE)


# ==============================================================================
# GDP Data 
# ==============================================================================

# Read Data
data_gdp_raw <- read_csv(data_save_paths$raw$gdp_seco_csv,
                         name_repair = "minimal") %>% 
  # Remove the first index column
  select(-1)                  

# Select the data that has the correct type, adjustments and structure
gdp_data <- data_gdp_raw %>% 
  dplyr::filter(
    structure %in% c("gdp","inv_constr","inv_fixed", "cons_priv","cons_gov",          # these are all components in the structure
                     "exp_good_ex_vm","exp_serv","imp_serv","imp_good_ex_v"),
    type == "real",
    seas_adj == "cssa",
    structure == "gdp"
    
  )  

gdp_ts <- format_time_series_df(gdp_data, "date", "value", "gdp", "%Y-%m-%d", seasonal_adj = TRUE)


# ==============================================================================
# KOF DATA
# ==============================================================================

# Load Data
master_kof_consensus_forecast <- read.table(data_save_paths$raw$kof_master_csv,
                                            header = TRUE,
                                            sep = ",",
                                            check.names = FALSE)


# Select columns for mapping and application of format_time_series_df FUnction
cols_to_keep <- c("date", map_chr(kof_mapping, "old_col"))
kof_spf_df   <- master_kof_consensus_forecast %>% select(all_of(cols_to_keep))

# Mapping application: Processes and create the Dataframe with new names
processed_kof_series <- imap(kof_mapping, function(config, new_name) {
  format_time_series_df(
    data           = kof_spf_df, 
    date_col     = "date", 
    value_col      = config$old_col, 
    new_name = new_name, 
    date_format  = "%b %Y", 
    seasonal_adj = config$seasonal_adj
  )
})

# Extract them into separate dfs
kof_5y_unemp     <- processed_kof_series[["5y_unemp_forecast"]]
kof_5y_cpi       <- processed_kof_series[["5y_cpi_forecast"]]
kof_3m_interest  <- processed_kof_series[["3m_interest_forecast"]]
kof_12m_interest <- processed_kof_series[["12m_interest_forecast"]]


# ==============================================================================
# EUROSTAT Data
# ==============================================================================

# Load Data
eu_ppi_raw <-read_csv(data_save_paths$raw$eu_ppi_csv) 

# Format Time Series
eu_ppi_ts <- format_time_series_df(eu_ppi_raw, "date", "ppi_eur", "ppi_eur", "%Y-%m-%d", seasonal_adj = TRUE)



################################################################################
# Build Master df
################################################################################


master_df <- ts_names %>%
  
  # mget looks for objects in the environment matching the strings and loads as list of lists
  mget(envir = .GlobalEnv) %>%
  
  # transform into a dataframe
  reduce(full_join, by = "date") %>%
  
  arrange(date)

write_csv(master_df, data_save_paths$processed$master_df_csv)


message("Data formatting done")

