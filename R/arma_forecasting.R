################################################################################
#
# THIS SCRIPT CONTAINS THE FUNCTIONS FOR ARMA FORECASTS
#
################################################################################


#' Generate a single h-step ARMA forecast for a rolling window
#'
#' Estimates an ARIMA model from a dataset with a data and a date column to a
#' specific time origin and returns forecast. The data doesn't need to be transformed
#' and prepared before running the function, i.e logs and differentiation gets handled in
#' the function (logs) and by the auto arima (diff). Only works on quarterly data.
#' 
#' @details
#' The function performs a log-transformation of the input data before estimation:
#' 
#' The ARIMA model is selected automatically and model accounts for non-stationarity
#' through automatic differencing by the auto.arima function
#' 
#' Forecasts are converted back to levels using the exponential function:
#'
#' @param current_T The forecast origin date (as a `yearqtr` or character string).
#' @param T_0 The start date for the training sample (default is "2000 Q1").
#' @param h Integer. The number of quarters to forecast ahead (default is 8).
#' @param data A dataframe or tibble containing the time series.
#' @param data_col Character. The name of the column to be forecasted.
#' @param date_col Character. The name of the date column (default is "quarter").
#'
#' @return A tibble with `origin_date`, `forecast_date`, `h_step`, and `predicted_value`. 
#' Returns `NULL` if estimation fails or data is insufficient.
#'
#' @import tidyverse
#' @importFrom zoo as.yearqtr
#' @importFrom forecast auto.arima forecast
#'
#' @export
get_arma_forecast <- function(current_T, T_0 = "2000 Q1", h = 8, data, data_col, date_col = "quarter") {
  
  # Ensure current_T is a yearqtr object
  current_T <- as.yearqtr(current_T)
  
  # Filter data from start (T_0) until current T / vantage point
  # The .data allows to access date_col
  raw_series <- data %>%
    filter(.data[[date_col]] <= current_T) %>%
    filter(.data[[date_col]] >= as.yearqtr(T_0)) %>%
    arrange(.data[[date_col]]) %>%
    pull(.data[[data_col]])
  
  log_raw_series <- log(raw_series)
  
  # Check for enough data (24 quarters = 6 years)
  n_obs <- length(na.omit(log_raw_series))
  if (n_obs < 24) {
    warning(paste("Insufficient data for", data_col, ": only", n_obs, "obs found."))
    return(NULL)
  }
  
  # Fit ARIMA
  # auto.arima detects if the series needs differencing (I(1))
  fit <- tryCatch({
    auto.arima(log_raw_series, seasonal = FALSE)
  }, error = function(e) {
    warning(paste("auto.arima failed for", data_col, ":", e$message))
    return(NULL)
  })
  
  # Second debug / warning message
  if (is.null(fit)) {
    message("ARIMA FIT IS NULLL")
    return(NULL)
  }
  
  # Forecast h steps ahead
  fc <- forecast(fit, h = h)
  
  # Convert log forecasts back to levels
  fc_levels <- exp(fc$mean)
  
  # Create quarterly dates for the forecast
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
#' @description
#' This function generates a forecast for the Real Effective Exchange Rate (REER) 
#' by decomposing it into its constituent parts: the domestic Producer Price Index (PPI), 
#' the foreign (Euro Area) PPI, and the nominal exchange rate. 
#' 
#' @details
#' The forecast assumes that the nominal exchange rate follows a Random Walk (RW), 
#' while the price indices follow independent ARMA processes. The REER forecast is 
#' constructed using the following identity:
#' 
#' \deqn{REER_{T+h} = S_{T} \times \left( \frac{PPI^{CH}_{T+h}}{PPI^{EUR}_{T+h}} \right)}
#' 
#' Where:
#' \itemize{
#'   \item \eqn{S_{T}} is the last observed nominal exchange rate (\code{ex_eoq}) at origin \eqn{T}.
#'   \item \eqn{PPI^{CH}_{T+h}} is the \eqn{h}-step ahead ARMA forecast of the Swiss PPI.
#'   \item \eqn{PPI^{EUR}_{T+h}} is the \eqn{h}-step ahead ARMA forecast of the Euro Area PPI.
#' }
#' 
#' Both price index forecasts are generated via \code{\link{get_arma_forecast}}, 
#' which applies a log-transformation and utilizes \code{forecast::auto.arima} for 
#' optimal model selection based on unit-root testing and information criteria.
#' 
#' @param current_T The origin date (yearqtr object or character convertible to yearqtr).
#' @param h Integer. The forecast horizon in quarters. Default is 8.
#' @param data A dataframe containing \code{quarter}, \code{ppi_ch_idx}, \code{ppi_eur_idx}, 
#' and \code{ex_eoq}.
#' 
#' @return A tibble containing:
#' \itemize{
#'   \item \code{origin_date}: The date from which the forecast was made.
#'   \item \code{forecast_date}: The target date of the forecast.
#'   \item \code{h_step}: The horizon step (1 to \eqn{h}).
#'   \item \code{predicted_reer}: The reconstructed REER forecast in levels.
#' }
#' 
#' @seealso 
#' \code{\link{get_arma_forecast}} for the underlying ARMA logic.
#' 
#' @import tidyverse
#' @importFrom zoo as.yearqtr
#' @importFrom forecast auto.arima
#' @export
forecast_reer_components <- function(current_T, h = 8, data) {
  
  raw_data <- data

  # Forecast PPI Switzerland (using your existing function)
  # Inside arma function happens: log, and then in the auto arima
  # there is a automatic detection of need of differentiation
  fc_ppi_ch <- get_arma_forecast(current_T = current_T,
                                 h = h,
                                 data = raw_data,
                                 data_col = "ppi_ch_idx",
                                 date_col = "quarter")
  
  # Forecast PPI Euro Area
  # Inside arma function happens: log, and then in the auto arima
  # there is a automatic detection of need of differentiation
  fc_ppi_eur <- get_arma_forecast(current_T = current_T,
                                  h = h,
                                  data = raw_data,
                                  data_col = "ppi_eur_idx",
                                  date_col = "quarter")
  
  # Get the last known Nominal Exchange Rate
  # Model assumption is a random walk for exchange rate
  last_ex <- data %>%
    filter(quarter == as.yearqtr(current_T)) %>%
    pull(ex_eoq)
  
  # Combine and calculate predicted REER
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





###############################################################################
#' Splice Official SNB REER with CREA Proxy and Component Forecasts
#' 
#' @param vantage_quarter The date of the forecast origin.
#' @param snb_reer_delay Number of quarters SNB data is assumed to be lagging.
#' @param data Master quarterly dataframe.
#' @param burn_in Start date for the series history.
#' @return A tibble with [quarter, reer_simulated] extended h-steps ahead.
splice_reer_series <- function(vantage_quarter = "2023 Q2",
                               snb_reer_delay = 3,
                               data = master_philips,
                               burn_in = "2010 Q1") {
  
  vantage_q <- as.yearqtr(vantage_quarter)
  cutoff_q  <- vantage_q - snb_reer_delay/4 
  
  # 1. Identify anchor values at the last known official data point
  anchor_data <- data %>%
    filter(quarter == cutoff_q) %>%
    select(reer_eu_ppi, REER_CREA)
  
  val_snb_T  <- anchor_data$reer_eu_ppi
  val_crea_T <- anchor_data$REER_CREA
  
  # 2. Build the historical segment (SNB data + Proxy for the lag period)
  series_historical <- data %>%
    filter(quarter >= as.yearqtr(burn_in), quarter <= vantage_q) %>%
    mutate(
      reer_simulated = ifelse(quarter <= cutoff_q,
                              reer_eu_ppi,
                              val_snb_T * (REER_CREA / val_crea_T))
    ) %>%
    select(quarter, reer_simulated)
  
  # 3. Build the forecast segment (h=8)
  reer_fc <- forecast_reer_components(current_T = vantage_q, h = 8, data = data)
  
  series_forecast <- reer_fc %>% 
    mutate(reer_simulated = val_snb_T * (predicted_reer / val_crea_T)) %>%
    select(quarter = forecast_date, reer_simulated)
  
  # 4. Combine
  series_full <- bind_rows(series_historical, series_forecast)
  
  return(series_full)
}




