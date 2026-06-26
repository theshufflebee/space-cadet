################################################################################
#
# This Script will transform the Data to the model input Dataframes
#
################################################################################

# This Script Formats the Data into Dataframes that are then used in the Models
# This can be things as unified scale (Decimals or else) but also Log Transformations
# and other transformations
# It also creates new variables if needed for estimation


# --- General Transformations for Full dataset ---

# Transformation here are things that happen to the whole dataset, but contain
# Creation of new variables

# FOr example the transformation to log gdp or the extraction of gdp trend and cycle


# Rename unemployment rate
master_df$unemp_rate <- master_df$unemployment 
                         
# Rename spf data (change back later)
master_df$spf_5y_unemp <- master_df$`5y_unemp_forecast`



# Variable indexing_date set in config
master_df <- master_df %>%
  arrange(date) %>%
  mutate(
    # Rebase Swiss PPI (PCH) and (EUR) ro same date
    ppi_ch_idx = (ppi_ch / ppi_ch[date == indexing_date]) * 100,
    ppi_eur_idx = (ppi_eur / ppi_eur[date == indexing_date]) * 100,
    
    # Calculate REER_CREA
    # Formula: St * (PCH / PEUR)
    # Using 'ex_eom' as St (ensure it is EUR per 1 CHF)
    REER_CREA = ex_eom * (ppi_ch_idx / ppi_eur_idx)
  ) %>%
  mutate(
    forward_rate = calculate_forward_rate(`5y_bond`, `10y_bond`))


# --- Transform df to quarterly

# Most of the models need quarterly data. Therefore we transofrm the df to quarterly
# This is done here to keep the data raw monthly data in the master df

# We use mean over a quarter
# when the data is quarterly we have the na.rm = TRUE, which in that case will take the
# mean of the only value that is present which is just the value itself

master_quarterly <- master_df %>%
  arrange(date) %>%
  mutate(quarter = yearqtr(date)) %>%
  group_by(quarter) %>%
  summarise(
    # Average all numeric columns except the end of month exchange rate
    across(where(is.numeric) & !c(ex_eom, REER_CREA, policy_rate), na.omit(~mean(.x, na.rm = TRUE))),
    
    # Grab the last actual observation for the exchange rate -> end of quarter
    REER_CREA = last(na.omit(REER_CREA)),
    ex_eoq = last(na.omit(ex_eom)),
    snb_policy_rate = last(na.omit(policy_rate)),
    
    .groups = "drop"
  )


# Otherwise there are NaN and NA values depending on column -> makes all NA
master_quarterly <- master_quarterly %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.x), NA, .x)))


# ---  GDP Transofrmations ---

# Add Log GDP as Variable
# This drops all rows where there are nan's in gdp
# This shortens the dataframe and it now starts in 1980s
# This is no issue as our estimations start way later
master_quarterly <- master_quarterly %>%
  mutate(log_gdp = log(gdp)) %>% # add log_gdp
  filter(!is.na(log_gdp)) # drop rows where gdp is NAN


# Apply HP Filter to extract the output gap
# freq = 1600 is the standard parameter for quarterly data
hp_res <- mFilter::hpfilter(master_quarterly$log_gdp, freq = 1600)

# We add both trend and gap
master_quarterly <- master_quarterly %>%
  mutate(
    gdp_trend = as.numeric(hp_res$trend),
    gdp_gap = as.numeric(hp_res$cycle) * 100
  )

# we add the one and 2 quarter lag for the gdp gap
master_quarterly <- master_quarterly %>%
  mutate(
    gdp_gap_lag_1 = dplyr::lag(gdp_gap, 1),
    gdp_gap_lag_2 = dplyr::lag(gdp_gap, 2)
  )


# CPI Transformations

master_quarterly <- master_quarterly %>%
  mutate(
    cpi = as.numeric(cpi),
    log_cpi = log(cpi),
    log_inflation_diff = (log_cpi - dplyr::lag(log(cpi), 1)) * 100 * 4,
    yoy_inf = (log_cpi - dplyr::lag(log(cpi), 4)) * 100,
    lag_log_inflation_diff = dplyr::lag(log_inflation_diff, 1),
    # if we use quarterly we multiply log inf by 4 to have yearly values for interpretation
    # SPF is already yearly
    )


################################################################################
#
# Prepare Data for the specific models
#
################################################################################

# Here we extract the specific data series that are needed for each model


# ==============================================================================
# MASTER OKUN + Auxiliary Data Frames
# ==============================================================================

# All necessary variables for the model

# Note: As the GDP Gap depends on the horizon it needs to be recalculated for
# each pseudo forecast. Therefore we only select log gdp


# DF 1: MASTER OKUN Contains all values needed from the Data
# ------------------------------------------------------------------------------
# Extract directly from master_quarterly, used for the rolling estimation
master_okun <- master_quarterly %>%
  select(
    quarter,
    unemp_rate,
    spf_5y_unemp,
    log_gdp,
    gdp_gap,
    gdp_gap_lag_1,
    gdp_gap_lag_2
    
  ) %>%
  # Drop NAs for second and third lag
  filter(!is.na(gdp_gap_lag_1) & !is.na(gdp_gap_lag_2)) %>%
  arrange(quarter)

write_csv(master_okun, data_save_paths$processed$okun_master_csv)

message("Okun Data Prep Succesful")



# ==============================================================================
# DF 1: MASTER PHILIPS
# ==============================================================================

# All necessary variables for the model

# Note: As the GDP Gap depends on the horizon it needs to be recalculated for
# each pseudo forecast. Therefore we only select log gdp



master_philips <- master_quarterly %>%
  select(
    quarter,
    `5y_cpi_forecast`,
    cpi,
    reer_eu_ppi,
    gdp,
    log_gdp,
    ex_av,
    ex_eoq,
    ppi_eur_idx,
    ppi_ch_idx,
    REER_CREA,
    log_inflation_diff,
    lag_log_inflation_diff
  )

write.csv(master_philips, data_save_paths$processed$philips_master_csv)




message("Philips Data Formatting Done")


master_taylor <- master_quarterly %>%
  select(
    quarter,
    saron,
    `3m_libor`,
    `5y_bond`,
    `10y_bond`,
    forward_rate,
    `3m_interest_forecast`,
    `12m_interest_forecast`,
    log_gdp,
    cpi,
    log_inflation_diff,
    gdp_gap
  )

master_quarterly <- master_quarterly %>%
  # Add SARON LIBOR SPLICE
  mutate(
    # Ensure quarter is recognized as a yearqtr object first
    quarter_idx = as.yearqtr(quarter),
    
    # Apply the splice point between saron and libor
    saron_libor_splice = if_else(
      quarter_idx < as.yearqtr("2021 Q1"), 
      `3m_libor`, 
      saron
    ),
    saron_libor_splice = saron_libor_splice / 100
  ) %>%
  select(-quarter_idx) %>%
  filter(!is.na(log_inflation_diff)) %>%
  mutate(
    
    # 3. Temporary Trend: HP filter 'trend' component of yoy_inflation
    # Note: Using $trend extracts the trend to subtract from the actual
    hp_inf_gap = mFilter::hpfilter(log_inflation_diff, freq = 1600)$cycle,
    
    # 4. Lagged Policy Rate
    lag_rate = lag(saron_libor_splice, 1),
    
  ) %>%
  mutate(
    true_snb_rate = saron_libor_splice,
    saron_libor_splice = case_when(
      # Use as.yearqtr to match the column type
      quarter >= zoo::as.yearqtr("2009 Q2") & quarter <= zoo::as.yearqtr("2022 Q2") ~ NA_real_,
      TRUE ~ saron_libor_splice
    )
  ) 


master_taylor <- master_quarterly %>%
  select(all_of(c("quarter", "saron_libor_splice", "true_snb_rate",
                  "forward_rate", "log_cpi", "log_inflation_diff", "yoy_inf",
                  "lag_rate", "log_gdp", "gdp_gap", "12m_interest_forecast", "hp_inf_gap"))) %>%
  mutate(
    across(c("saron_libor_splice", "true_snb_rate",
             "forward_rate", "lag_rate"), ~ .x * 100) 
  ) %>%
  mutate(
    inf_gap = yoy_inf -1,
    
  )

write.csv(master_taylor, data_save_paths$processed$taylor_master_csv)

message("BUILT ALL MASTER DATA FRAMES")


# --- Start buildung the GDP forecasts
# Later maybe turn into rolling window

if (run_estimation || !file.exists(output_save_paths$forecasts$forecast_df_gdp_arima)) {
  
  # Generate true log GDP levels using the updated arima loop wrapper
  gdp_forecasts_arima <- gdp_forecast_wrapper(
    fcast_start = forecast_starting_date,
    data        = master_quarterly,
    date_col    = "quarter",
    gdp_col     = "log_gdp",
    horizon     = h
  )
  
  # Save the pure log GDP level matrix layout
  write_csv(gdp_forecasts_arima, output_save_paths$forecasts$forecast_df_gdp_arima)
  message("GDP FORECATSTS SAVED SUCCESSFULLY")
  
} else {
  # Read from disk and dynamically format matrix structures back into expected shapes
  message("LOADING GDP FORECATSTS...")
  
  gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima) %>%
    mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
    rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)
}


message("DATA PREPARATION SUCCESSFUL, CONTINUING TO ESTIMATION...")

