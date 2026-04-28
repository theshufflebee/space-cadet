################################################################################
# 
# RUN FORECASTS FOR MODEL 2 / INFLATIONS
#
################################################################################

source(here("R", "arma_forecasting.R"))
source(here("R", "philips_matrices.R"))




data_for_arma_forecast <- master_philips%>%
  select(all_of(c("quarter", "REER_CREA", "ppi_eur_idx", "ppi_ch_idx", "ex_eoq")))



# LOP FORECAST
# ------------------------------------------------------------------------------
results_eu <- get_arma_forecast(current_T = as.yearqtr("2021 Q4"),
                                h = 8, 
                                data = data_for_arma_forecast,
                                data_col = "ppi_eur_idx",
                                date_col = "quarter")


results_ch <- get_arma_forecast(current_T = as.yearqtr("2021 Q4"),
                                h = 8, 
                                data = data_for_arma_forecast,
                                data_col = "ppi_ch_idx",
                                date_col = "quarter")



reer_forecast <- forecast_reer_components(current_T = as.yearqtr("2021 Q4"),
                                          h = 8,
                                          data = data_for_arma_forecast)


# Data Splicing


###############


# First Exogenous Variable: LOP Gap
LOP_forecast <- splice_snb_series(vantage_quarter = "2023 Q2",
                                  snb_reer_delay = 3,
                                  data = master_philips,
                                  burn_in = "2010 Q1")

# Second Exogenous variable :

vantage_q <- as.yearqtr("2023 Q2")

h <- 8

# The "T-1" pipe
cpi_t_minus_1 <- master_philips %>%
  filter(quarter == vantage_q - 1/4) %>%
  select(quarter, cpi) # Replace 'cpi' with your actual column name


# Third Exogenous variable: Output Gap
# Use system in the previous forecast

log_gdp <- master_philips %>%
  arrange(quarter) %>%
  filter(quarter <= vantage_q) %>%
  select(quarter, log_gdp) #

log_gdp_forecast <- master_philips %>%
  arrange(quarter) %>% #
  filter(quarter > vantage_q) %>%
  slice_head(n = h) %>%
  select(quarter, log_gdp)



# fuse together and get cycle
gdp_extended <- bind_rows(log_gdp, log_gdp_forecast) %>%
  arrange(quarter)

# 2. Apply HP Filter and extract the cycle
# freq = 1600 is standard for quarterly data
hp_obj <- hpfilter(gdp_extended$log_gdp, freq = 1600)

# 3. Add the result back to your dataframe
# 'cycle' in hpfilter is the Gap (Actual - Trend)
gdp_extended <- gdp_extended %>%
  mutate(
    gdp_trend = as.numeric(hp_obj$trend),
    gdp_gap   = as.numeric(hp_obj$cycle)
  )

# 4. Extract the gap specifically for your Phillips Curve vantage point
# (Assuming you want the gap value for Time T-1 relative to the vantage point)
final_gap_value <- gdp_extended %>%
  filter(quarter == vantage_q) %>%
  pull(gdp_gap)


# Build X for the mu_t matrix by joining
# final_gap_value, cpi_t-1, just move up -> last quarters cpi is this quarters independent variable, and output gap
# selecting data from forecast start to vantage bpiz





library(dplyr)
library(zoo)
library(mFilter)

#' Extract HP Filter Gap at a specific Vantage Point
#' 
get_hp_gap <- function(data,
                       gdp_forecast_data,
                       vantage_q,
                       h = 8,
                       gdp_col = "log_gdp",
                       date_col = "quarter",
                       lag_val = 0) {
  
  vantage_q <- as.yearqtr(vantage_q)
  
  # 1. Past + Current observed data
  obs_data <- data %>%
    arrange(.data[[date_col]]) %>%
    filter(.data[[date_col]] <= vantage_q) %>%
    select(all_of(c(date_col, gdp_col)))
  
  # 2. Tail extension (Forecasts)
  forecast_data <- gdp_forecast_data %>%
    arrange(.data[[date_col]]) %>%
    filter(.data[[date_col]] > vantage_q) %>%
    slice_head(n = h) %>%
    select(all_of(c(date_col, gdp_col)))
  
  # 3. Fuse and check for enough data
  extended_df <- bind_rows(obs_data, forecast_data)
  
  # Safety check for minimum data points (HP filter needs sufficient length)
  if (nrow(extended_df) < 12) return(NULL)
  
  # 4. Apply HP Filter
  y <- extended_df[[gdp_col]]
  hp_obj <- hpfilter(y, freq = 1600)
  
  # 5. Build Result Dataframe
  # We only return up to vantage_q to keep the data 'truthful' to the vantage point
  result_df <- extended_df %>%
    mutate(
      trend = as.numeric(hp_obj$trend),
      gap = as.numeric(hp_obj$cycle)
    ) %>%
    filter(.data[[date_col]] <= vantage_q) %>%
    select(all_of(date_col), gap)
}



build_X_data_matrix_philips <- function(T_0 = "2015-01-01",
                                        vantage_quarter = "2023 Q1",
                                        data = master_philips,
                                        h = 8) {
  
  # Lag CPI before burn cutoff
  raw_data <- data %>%
    arrange(quarter) %>%
    mutate(
      cpi_lagged = lag(cpi, n = 1)
    )
  
  # Add burn in cutoff
  raw_data <- raw_data %>%
    filter(quarter >= as.yearqtr(model_philips_burn_in)) %>%
    filter(quarter <= as.yearqtr(vantage_quarter)) %>%
    arrange(quarter)
  
  LOP_forecast <- splice_snb_series(vantage_quarter = vantage_quarter,
                                    snb_reer_delay = SNB_REER_DELAY,
                                    data = raw_data,
                                    burn_in = model_philips_burn_in)

  HP_gap_data <- get_hp_gap(raw_data,
                            raw_data,
                            vantage_q = vantage_quarter)
  
  X_matrix <- raw_data %>%
    select(quarter, cpi_lagged) %>%
    left_join(LOP_forecast %>% select(quarter, lop_gap), by = "quarter") %>%
    left_join(HP_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
    # Filter to return the specific estimation sample
    filter(quarter >= as.yearqtr(T_0)) %>%
    arrange(quarter) %>%
    as.matrix()

  return(X_matrix)
}


X_philips <- build_X_data_matrix_philips()





