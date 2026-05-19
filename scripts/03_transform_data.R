################################################################################
#
# This Script will transform the Data to the model input Dataframes
#
################################################################################



# --- General Transformations for Full dataset ---

# Transformation here are things that happen to the whole dataset, but contain
# Creation of new variables

# FOr example the transformation to log gdp or the extraction of gdp trend and cycle

# --- Unemployment Rate ---

# We divide the unemployment number by the employment numbers to get the unemployment rate
master_df$unemp_rate <- master_df$unemployment / (master_df$unemployment + master_df$employment)

# The data is in the format where 5% is 0.05. The spf data is in format 5.00 for 5%
# we therefore divide by 100
# This is done so there are no variable mismatches that although technically don't 
# break the model might break the optimizer
master_df$spf_5y_unemp <- master_df$`5y_unemp_forecast` / 100


# move out later
base_date <- "2020-12-01"
# Also maybe need to download other CPI data

master_df <- master_df %>%
  arrange(date) %>%
  mutate(
    # Rebase Swiss PPI (PCH) so Dec 2020 = 100
    # ppi_ch represents Swiss CPI column
    ppi_ch_idx = (ppi_ch / ppi_ch[date == base_date]) * 100,
    
    # Step B: Rebase Euro PPI (PEUR) so Dec 2020 = 100
    ppi_eur_idx = (ppi_eur / ppi_eur[date == base_date]) * 100,
    
    # Step C: Calculate REER_CREA
    # Formula: St * (PCH / PEUR)
    # Using 'ex_eom' as St (ensure it is EUR per 1 CHF)
    REER_CREA = ex_eom * (ppi_ch_idx / ppi_eur_idx)
  ) %>%
  mutate(
    forward_rate = calculate_forward_rate(`5y_bond`, `10y_bond`))


# --- Transform df to quarterly

# Most of the models need quarterly data. Therefore we transofrm the df to quarterly
# This is done here to keep the data raw in the master df

# We use mean over 3 months comprising a quarter
# when the data is quarterly we have the na.rm = TRUE, which in that case will take the
# mean of the only value that is present qhich is just the value itself

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
    gdp_gap = as.numeric(hp_res$cycle)
  )

# we add the one and 2 quarter lag for the gdp gap
master_quarterly <- master_quarterly %>%
  mutate(
    gdp_gap_lag_1 = dplyr::lag(gdp_gap, 1),
    gdp_gap_lag_2 = dplyr::lag(gdp_gap, 2)
  )



################################################################################
#
# Prepare Data for the specific models
#
################################################################################

# Here we extract the specific data series that are needed for each model


# =================================
# Data Extraction for Okun Model
# =================================

# All necessary variables for the model

# Note: As the GDP Gap depends on the horizon it needs to be recalculated for
# each pseudo forecast. Therefore we only select log gdp

master_okun_model_long <- master_quarterly %>%
  select(
    quarter,
    unemp_rate,
    employment,
    `spf_5y_unemp`,
    gdp,
    log_gdp
  ) %>%
  pivot_longer(
    cols = -quarter,
    names_to = "variable",
    values_to = "value"
  )


# Get the Measurement variables
Y_okun <- master_okun_model_long %>%
  filter(variable %in% c("unemp_rate", "spf_5y_unemp")) %>%
  pivot_wider(names_from = variable,
              values_from = value)%>%
  arrange(quarter) %>%
  mutate(`spf_5y_unemp` = `spf_5y_unemp`)

# Get the Exogenous variable
X_okun <- master_okun_model_long %>%
  filter(variable %in% c("log_gdp")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(quarter) %>%
  select(quarter, log_gdp)


message("Okun Data Formatting Done")



# Here we extract the specific data series that are needed for each model


# =================================
# Data Extraction for Philips Curve Model
# =================================

# All necessary variables for the model

# Note: As the GDP Gap depends on the horizon it needs to be recalculated for
# each pseudo forecast. Therefore we only select log gdp

master_quarterly <- master_quarterly %>%
  mutate(
    cpi = as.numeric(cpi),
    log_inflation_diff = log(cpi) - dplyr::lag(log(cpi), 4),
    lag_log_inflation_diff = dplyr::lag(log_inflation_diff, 1),
    `5y_cpi_forecast` = `5y_cpi_forecast`
    )

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
    cpi
  )

master_taylor <- master_taylor %>%
  mutate(
    # Ensure quarter is recognized as a yearqtr object first
    quarter_idx = as.yearqtr(quarter),
    
    # Apply the transition specified in 0_technical_note.pdf
    saron_libor_splice = if_else(
      quarter_idx < as.yearqtr("2021 Q1"), 
      `3m_libor`, 
      saron
    ),
    saron_libor_splice = saron_libor_splice / 100
  ) %>%
  select(-quarter_idx) #clean up helper column


