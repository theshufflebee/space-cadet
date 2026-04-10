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

# We divide the unemployment nu,ber by the employment numbers to get the unemployment rate
master_df$unemp_rate <- master_df$unemployment / (master_df$unemployment + master_df$employment)

# The data is in the format where 5% is 0.05. The spf data is in format 5.00 for 5%
# we therefore divide by 100
# This is done so there are no variable mismatches that although technically don't 
# break the model might break the optimizer
master_df$spf_5y_unemp <- master_df$`5y_unemp_forecast` / 100


# --- Transform df to quarterly

# Most of the models need quarterly data. Therefore we transofrm the df to quarterly
# This is done here to keep the data raw in the master df

# We use mean over 3 months comprising a quarter
# when the data is quarterly we have the na.rm = TRUE, which in that case will take the
# mean of the only value that is present qhich is just the value itself
master_quarterly <- master_df %>%
  arrange(date) %>%
  mutate(quarter = yearqtr(date)) %>% # add quarter column
  group_by(quarter) %>% # then group all data in same quarter together
  summarise(
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)), # mean logic
    .groups = "drop"
  )


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
  arrange(quarter)

# Get the Exogenous variable
X_okun <- master_okun_model_long %>%
  filter(variable %in% c("log_gdp")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(quarter) %>%
  select(quarter, log_gdp)


message("Okun Data Formatting Done")



