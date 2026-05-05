################################################################################
#
# THIS SCRIPT CONTAINS THE FUNCTIONS FOR ARMA FORECASTS
#
################################################################################

# library(forecast)
# library(dplyr)
# library(lubridate)

#' Generate a single h-step forecast for a rolling window
get_arma_forecast <- function(current_T, T_0 = "2000 Q1", h = 8, data, data_col, date_col = "quarter") {
  
  # Ensure current_T is a yearqtr object
  current_T <- as.yearqtr(current_T)
  
  # 1. Filter data from start (T_0) until current T
  train_data <- data %>%
    filter(.data[[date_col]] <= current_T) %>%
    filter(.data[[date_col]] >= as.yearqtr(T_0)) %>%
    arrange(.data[[date_col]]) %>%
    pull(.data[[data_col]])
  
  message("Train Data ARMA Forecast")
  print(train_data)
  
  # 2. Check for enough data (24 quarters = 6 years)
  n_obs <- length(na.omit(train_data))
  if (n_obs < 24) {
    warning(paste("Insufficient data for", data_col, ": only", n_obs, "obs found."))
    return(NULL)
  }
  
  # 3. Fit ARIMA on log levels
  # auto.arima detects if the series needs differencing (I(1))
  fit <- tryCatch({
    auto.arima(log(train_data), seasonal = FALSE)
  }, error = function(e) {
    warning(paste("auto.arima failed for", data_col, ":", e$message))
    return(NULL)
  })
  
  if (is.null(fit)) {
    message("ARIMA FIT IS NULLL")
    return(NULL)
  }
  
  # 4. Forecast h steps ahead (h is quarters here)
  fc <- forecast(fit, h = h)
  
  # 5. Convert LOG forecasts back to LEVELS
  fc_levels <- exp(fc$mean)
  
  # 6. Create QUARTERLY dates for the forecast
  # In zoo, 1 unit = 1 year, so 1/4 = 1 quarter
  fc_dates <- seq(current_T + 1/4, by = 1/4, length.out = h)
  
  # 7. Return tidy tibble
  return(tibble(
    origin_date = current_T,
    forecast_date = as.yearqtr(fc_dates),
    h_step = 1:h,
    predicted_value = as.numeric(fc_levels)
  ))
}



#' Forecast REER using Component ARMA and Random Walk
#' 
#' @param current_T The origin date (yearqtr)
#' @param h Horizon (quarters)
#' @param data Your master quarterly dataframe
#' @return A tibble with forecasted REER_CREA
forecast_reer_components <- function(current_T, h = 8, data) {
  
  raw_data <- data
  
  message("Data Date COl")
  print(head(raw_data[["quarter"]]))
  
  # 1. Forecast PPI Switzerland (using your existing function)
  fc_ppi_ch <- get_arma_forecast(current_T = current_T,
                                 h = h,
                                 data = raw_data,
                                 data_col = "ppi_ch_idx",
                                 date_col = "quarter")
  
  # 2. Forecast PPI Euro Area
  fc_ppi_eur <- get_arma_forecast(current_T = current_T,
                                  h = h,
                                  data = raw_data,
                                  data_col = "ppi_eur_idx",
                                  date_col = "quarter")
  
  # 3. Get the last known Nominal Exchange Rate (Random Walk Assumption)
  last_ex <- data %>%
    filter(quarter == as.yearqtr(current_T)) %>%
    pull(ex_eoq)
  
  message("PPI CH Pred")
  print(head(fc_ppi_ch))
  
  message("PPI EUR Pred")
  print(head(fc_ppi_eur))
  
  # 4. Combine and calculate predicted REER
  # Formula: St * (PCH / PEUR)
  reer_forecast <- fc_ppi_ch %>%
    rename(ppi_ch_pred = predicted_value) %>%
    left_join(fc_ppi_eur, by = c("origin_date", "forecast_date", "h_step")) %>%
    rename(ppi_eur_pred = predicted_value) %>%
    mutate(
      ex_rw = last_ex,
      predicted_reer = ex_rw * (ppi_ch_pred / ppi_eur_pred)
    ) %>%
    select(origin_date, forecast_date, h_step, predicted_reer)
  
  return(reer_forecast)
}


splice_snb_series <- function(vantage_quarter = "2023 Q2",
                              snb_reer_delay = 3,
                              data = master_philips,
                              burn_in = "2010 Q1") {
  
  # Define the vantage point and the 'Knowledge Cutoff'
  vantage_q <- as.yearqtr(vantage_quarter) 
  
  # The last quarter where SNB REER is 'actually' known at the pseudo date
  # as our forecasts have a delay due to data releases 3 quarters is enough
  # forecast for Q2 2023 realistically happen only after the release of Q1 data
  # of other variables so during Q2 when Q2 2022 REER is already available
  # can be changed
  cutoff_q  <- vantage_q - snb_reer_delay/4 
  
  # Data prep
  
  # We need the values at the moment the official SNB data ends at the vantage
  # point -> so at the cutoff
  anchor_data <- data %>%
    filter(quarter == cutoff_q) %>%
    select(reer_eu_ppi, REER_CREA)
  
  val_snb_T  <- anchor_data$reer_eu_ppi
  val_crea_T <- anchor_data$REER_CREA
  
  
  
  # Step-by-Step Construction of the Spliced Series
  # First select from burn in
  # This gives us the data for the filter until the vantage point
  series_for_filter <- data %>%
    filter(quarter >= as.yearqtr(burn_in)) %>% # Burn-in start
    mutate(
      # Use real SNB data up to the cutoff the if part
      # Beyond the cutoff, use the observed proxy -> else part
      reer_simulated = ifelse(quarter <= cutoff_q,
                              reer_eu_ppi,
                              val_snb_T * (REER_CREA / val_crea_T))
    ) %>%
    select(quarter, reer_simulated)
  
  
  # forcast for the h step ahead forecast
  # vantage point is the start
  reer_fc_at_vantage <- forecast_reer_components(current_T = vantage_q, h = 8, data = data)
  
  # Filter series is full dataframe (goes beyond vantage point) but has replaced snb reer
  # by proxy at cutoff
  # so cut off that df at the vantage point and then add the forecasted reer in top of it
  series_extended <- series_for_filter %>%
    filter(quarter <= vantage_q) %>% # Keep up to current knowledge in pseudo out of sample forecast
    bind_rows(
      reer_fc_at_vantage %>% 
        mutate(reer_simulated = val_snb_T * (predicted_reer / val_crea_T)) %>%
        select(quarter = forecast_date, reer_simulated)
    )
  
  # Extract LOP Gap
  # Apply HP filter on the full simulation + forecasted values
  series_extended <- series_extended %>%
    mutate(
      log_reer = log(reer_simulated),
      trend    = hpfilter(log_reer, freq = 1600)$trend,
      lop_gap  = log_reer - trend   # inverse so a positive beta is the correct one
    )
  
  return(series_extended)
} 


