################################################################################
#
# Format Data into a time series df
#
################################################################################


# --- Format SNB Data ---

saron_ts <- snb_api_data_to_ts(mm_data, "SARON", "saron")

libor_ts <- snb_api_data_to_ts(mm_data, "3M0", "3m_libor")

gb_5y_ts <- snb_api_data_to_ts(gb_data, "5J", "5y_bond")

gb_10y_ts <- snb_api_data_to_ts(gb_data, "10J", "10y_bond")



# REER Data does not need that step since it is only one TS in the download
# Its year date select end of month obs

reer_ym <- reer_raw %>%
  select("Value", "Date")%>%
  arrange("Date")%>%
  mutate(date_ym = as.yearmon(Date)) %>%
  
  group_by(date_ym) %>%
  
  summarise(
    last_value = last(Value, order_by = Date),
    .groups = "drop"
  )

reer_eu_ppi_ts <- format_time_series_df(reer_ym, "date_ym", "last_value", "reer_eu_ppi", "%b %Y")


# --- Format BFS Data ---

# CPI
cpi_ts <- format_time_series_df(cpi_raw, "Datum / Date", "38687", "cpi", "%Y-%m-%d")

# Unemployment
unemployment <- unemployment_raw %>%
  filter(REGION %in% "Total")

unemployment_ts <- format_time_series_df(unemployment, "PERIOD", "VALUE", "unemployment", "%Y-%m")

# Employment
employment <- emp_raw %>%
  # Filter unnecessary data
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
                                       "%b %Y")


# --- GDP Data ---
gdp_data <- data_gdp_raw %>% 
  dplyr::filter(
    structure %in% c("gdp","inv_constr","inv_fixed", "cons_priv","cons_gov",          # choose GDP components
                     "exp_good_ex_vm","exp_serv","imp_serv","imp_good_ex_v"),
    type == "real",
    seas_adj == "cssa",
    structure == "gdp"
    
  )  

gdp_ts <- format_time_series_df(gdp_data, "date", "value", "gdp", "%Y-%m-%d")


# --- KOF DATA ---
# Selecting columns defined in config.R

kof_spf_df <- master_kof_consensus_forecast %>%
  select(all_of(kof_cols_to_keep))

kof_5y_unemp <- format_time_series_df(kof_spf_df, "date", kof_cols_to_keep[2], "5y_unemp_forecast", "%b %Y")

kof_5y_cpi <- format_time_series_df(kof_spf_df, "date", kof_cols_to_keep[3], "5y_cpi_forecast", "%b %Y")

kof_3m_interest <- format_time_series_df(kof_spf_df, "date", kof_cols_to_keep[4], "3m_interest_forecast", "%b %Y")

kof_12m_interest <- format_time_series_df(kof_spf_df, "date", kof_cols_to_keep[5], "12m_interest_forecast", "%b %Y")


#########################
# Build Master df
#########################


master_df <- ts_names %>%
  
  # mget looks for objects in the environment matching the strings and loads as list of lists
  mget(envir = .GlobalEnv) %>%
  
  # transform into a dataframe
  reduce(full_join, by = "date") %>%
  
  arrange(date)

write_csv(master_df, "data/master.csv")

message("Data formatting done")