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

# ==============================================================================
# TASK 1: GENERAL & QUARTERLY TRANSFORMATIONS
# ==============================================================================

# --- 1.1 Monthly Level Variable Renaming & Indexing ---
master_df <- master_df %>%
  arrange(date) %>%
  rename(
    unemp_rate    = unemployment,
    spf_5y_unemp  = `5y_unemp_forecast`
  ) %>%
  mutate(
    ppi_ch_idx   = (ppi_ch / ppi_ch[date == indexing_date]) * 100,
    ppi_eur_idx  = (ppi_eur / ppi_eur[date == indexing_date]) * 100,
    REER_CREA    = ex_eom * (ppi_ch_idx / ppi_eur_idx),
    forward_rate = calculate_forward_rate(`5y_bond`, `10y_bond`)
  )

# --- 1.2 Aggregation to Quarterly Frequency ---
master_quarterly <- master_df %>%
  arrange(date) %>%
  mutate(quarter = yearqtr(date)) %>%
  group_by(quarter) %>%
  summarise(
    across(where(is.numeric) & !c(ex_eom, REER_CREA, policy_rate), na.omit(~mean(.x, na.rm = TRUE))),
    REER_CREA       = last(na.omit(REER_CREA)),
    ex_eoq          = last(na.omit(ex_eom)),
    snb_policy_rate = last(na.omit(policy_rate)),
    .groups         = "drop"
  ) %>%
  # Turn all Missing data into NA (not NaN for example)
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

# --- 1.3 GDP & Output Gap Transformations ---
hp_gdp_res <- master_quarterly %>%
  mutate(log_gdp = log(gdp)) %>%
  filter(!is.na(log_gdp))

hp_gdp_filter <- mFilter::hpfilter(hp_gdp_res$log_gdp, freq = 1600)

master_quarterly <- hp_gdp_res %>%
  mutate(
    gdp_trend     = as.numeric(hp_gdp_filter$trend),
    gdp_gap       = as.numeric(hp_gdp_filter$cycle) * 100,
    gdp_gap_lag_1 = dplyr::lag(gdp_gap, 1),
    gdp_gap_lag_2 = dplyr::lag(gdp_gap, 2)
  )

# --- 1.4 Inflation & CPI Transformations ---
master_quarterly <- master_quarterly %>%
  mutate(
    cpi                     = as.numeric(cpi),
    log_cpi                 = log(cpi),
    log_inflation_diff      = (log_cpi - dplyr::lag(log_cpi, 1)) * 100 * 4,
    yoy_inf                 = (log_cpi - dplyr::lag(log_cpi, 4)) * 100,
    lag_log_inflation_diff  = dplyr::lag(log_inflation_diff, 1)
  )

# --- 1.5 Taylor Rule Policy Rate & Gap Transformations ---
master_quarterly <- master_quarterly %>%
  mutate(
    # Splice SARON and 3M LIBOR at 2021 Q1 (keep in percentage scale: 100% basis)
    saron_libor_splice = if_else(
      as.yearqtr(quarter) < as.yearqtr("2021 Q1"),
      `3m_libor`,
      saron
    ),
    
    # Lagged spliced policy rate
    lag_rate = dplyr::lag(saron_libor_splice, 1),
    
    # Mask ZLB / Negative Interest Rate Environment Period (2009 Q2 - 2022 Q2)
    true_snb_rate = saron_libor_splice,
    saron_libor_splice = case_when(
      quarter >= zoo::as.yearqtr("2009 Q2") & quarter <= zoo::as.yearqtr("2022 Q2") ~ NA_real_,
      TRUE ~ saron_libor_splice
    ),
    
    # Rescale select log and rate metrics where needed
    log_cpi_scaled    = log_cpi * 100,
    forward_rate      = forward_rate * 100,
    interest_fc_12m   = `12m_interest_forecast` * 100,
    
    # Inflation gaps
    hp_inf_gap        = if_else(!is.na(log_inflation_diff), mFilter::hpfilter(log_inflation_diff, freq = 1600)$cycle, NA_real_),
    inf_gap           = yoy_inf - 1
  )


# ==============================================================================
# TASK 2: SPLIT AND EXPORT MODEL DATAFRAMES
# ==============================================================================

# --- 2.1 Model 1: Master Okun Dataframe ---
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
  filter(!is.na(gdp_gap_lag_1) & !is.na(gdp_gap_lag_2)) %>%
  arrange(quarter)

write_csv(master_okun, data_save_paths$processed$okun_master_csv)
message("Okun Data Prep Successful")

# --- 2.2 Model 2: Master Phillips Dataframe ---
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

write_csv(master_philips, data_save_paths$processed$philips_master_csv)
message("Phillips Data Prep Successful")

# --- 2.3 Model 3: Master Taylor Rule Dataframe ---
master_taylor <- master_quarterly %>%
  filter(!is.na(log_inflation_diff)) %>%
  select(
    quarter,
    saron_libor_splice,
    true_snb_rate,
    forward_rate,
    log_cpi = log_cpi_scaled,
    log_inflation_diff,
    yoy_inf,
    lag_rate,
    log_gdp,
    gdp_gap,
    `12m_interest_forecast` = interest_fc_12m,
    inf_gap
    )

write_csv(master_taylor, data_save_paths$processed$taylor_master_csv)
message("Taylor Data Prep Successful")

message("BUILT ALL MASTER DATA FRAMES")
