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
get_arma_forecast <- function(current_T, T_0 = "2001 Q1", h = 8, data, data_col, date_col = "quarter") {
  
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
                              burn_in = "2001 Q1") {
  
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
      trend    = as.numeric(mFilter::hpfilter(log_reer, freq = 1600)$trend), 
      lop_gap  = as.numeric(log_reer - trend)                               
    )
  
  return(series_extended)
} 





###############################################################################
# These are not yet implemented / tested





#' Splice Official SNB REER with CREA Proxy and Component Forecasts
#' 
#' @details
#' To run the pseudo out of sample forecasts correctly we need to take into account
#' that the SNB REER releases with a delay of 1 year. Therefore we need to remove
#' that timeframe from the data and replace it with a proxy. Due to teh release delay
#' of data in reality the delay is usually only 3 quarters, as GDP for example releases
#' in the middle of the next quarter, at whch point the SNB data is already here.
#' To be conservative however we still use 4 quarters
#' 
#' @param vantage_quarter The date of the forecast origin.
#' @param snb_reer_delay Number of quarters SNB data is assumed to be lagging.
#' @param data Master quarterly dataframe.
#' @param burn_in Start date for the series history.
#' @return A tibble with [quarter, reer_simulated] extended h-steps ahead.
splice_reer_series <- function(vantage_quarter = "2023 Q2",
                               snb_reer_delay = 4,
                               data = master_philips,
                               burn_in = "2000 Q4") {
  
  vantage_q <- as.yearqtr(vantage_quarter)
  cutoff_q  <- vantage_q - snb_reer_delay/4 
  
  # Identify anchor values at the last known official data point
  anchor_data <- data %>%
    filter(quarter == cutoff_q) %>%
    select(reer_eu_ppi, REER_CREA)
  
  val_snb_T  <- anchor_data$reer_eu_ppi
  val_crea_T <- anchor_data$REER_CREA
  
  # Build the historical segment (SNB data until before the cutoff
  # then the Proxy for the lag period. as the proxy is based on 3
  # datapoints available at t, it is calculated in the data transofrmation and
  # just gets added here
  series_historical <- data %>%
    filter(quarter >= as.yearqtr(burn_in), quarter <= vantage_q) %>%
    mutate(
      reer_simulated = ifelse(quarter < cutoff_q,
                              reer_eu_ppi,
                              val_snb_T * (REER_CREA / val_crea_T))
    ) %>%
    select(quarter, reer_simulated)
  
  # Build the forecast segment (h=8)
  reer_fc <- forecast_reer_components(current_T = vantage_q, h = 8, data = data)
  
  series_forecast <- reer_fc %>% 
    mutate(reer_simulated = val_snb_T * (predicted_reer / val_crea_T)) %>%
    select(quarter = forecast_date, reer_simulated)
  
  # Combine
  series_full <- bind_rows(series_historical, series_forecast)
  
  return(series_full)
}




#' Extract Law of One Price (LOP) Gap from Spliced REER
#' 
#' @param spliced_df Output from splice_reer_series.
#' @param hp_freq Frequency parameter for HP Filter (default 1600 for quarterly).
#' @return A tibble with [quarter, reer_simulated, log_reer, trend, lop_gap].
extract_lop_gap <- function(spliced_df, hp_freq = 1600) {
  
  if (!requireNamespace("mFilter", quietly = TRUE)) stop("Package 'mFilter' required.")
  
  res <- spliced_df %>%
    mutate(
      log_reer = log(reer_simulated),
      # Extract trend component using mFilter
      trend    = as.numeric(mFilter::hpfilter(log_reer, freq = hp_freq)$trend),
      # Gap is Observed - Trend
      # A positive gap = Appreciation = Downward inflation pressure
      lop_gap  = log_reer - trend
    )
  
  return(res)
}



#' Extract HP Filter Gap at a specific Vantage Point
#' 
#' @description
#' This function calculates the Hodrick-Prescott (HP) cyclical component (gap) by 
#' splicing historical observed data untilt valtage point T with forecasted extensions.
#' This approach mitigates the "end-point bias" of the HP filter in pseudo out-of-sample 
#' forecasting contexts, and also allows to forecast the Output gap
#' 
#' @details
#' The function constructs an augmented time series \eqn{Y^*} by combining 
#' observed data up to the vantage point \eqn{T} and forecasted values up to 
#' \eqn{T+h}:
#' 
#' The HP filter is then applied to the entire augmented series to extract the 
#' cycle Component representing the Output gap. if \code{return_forecasts = FALSE}
#' it returns the oputput gap until the vantage point, where the forecasts serve
#' as a stabilizer to mitigate endpoint bias, while if its is set to \code{TRUE}
#' then it returns both historical and the full forecasts of the Output gap up
#' to horizon \code{T+h}.
#' 
#' @param data A dataframe containing historical observed data.
#' @param gdp_forecast_data A dataframe containing forecasted values for the series extension.
#' @param vantage_q The forecast origin date (as a \code{yearqtr} or character string). 
#' Represents "today" in the pseudo out-of-sample context.
#' @param h Integer. The number of forecast quarters to append as an extension. Default is 8.
#' @param gdp_col Character. The name of the column containing the series to be filtered (e.g., "log_gdp").
#' @param date_col Character. The name of the column containing dates. Default is "quarter".
#' @param return_forecasts Logical. If \code{FALSE} (default), returns the historical 
#' gaps up to \eqn{T}. If \code{TRUE}, returns the filtered gaps for the forecast 
#' horizon \eqn{T+1} to \eqn{T+h}.
#' 
#' @return A tibble containing the date column and the calculated \code{gap}. 
#' Returns \code{NULL} if the fused series contains fewer than 12 observations.
#' 
#' @import dplyr
#' @importFrom zoo as.yearqtr
#' @importFrom mFilter hpfilter
#' @importFrom tibble as_tibble
#' 
#' @seealso [build_data_matrix_philips()]
#' 
#' @export
get_hp_gap <- function(data,
                       gdp_forecast_data,
                       vantage_q,
                       h = 8,
                       gdp_col = "log_gdp",
                       date_col = "quarter",
                       return_forecasts = FALSE) {
  
  vantage_q <- as.yearqtr(vantage_q)
  vantage_name <- as.character(vantage_q)
  
  # from observed data select up until the vantage point
  # that you have observed at that time
  obs_data <- data %>%
    arrange(.data[[date_col]]) %>%
    filter(.data[[date_col]] <= vantage_q) %>%
    select(all_of(c(date_col, gdp_col)))
  
  # Select the forecast data as t+h extension
  forecast_data <- gdp_forecast_data %>%
    arrange(.data[[date_col]]) %>%
    filter(.data[[date_col]] > vantage_q) %>%
    slice_head(n = h) %>%
    select(all_of(c(date_col, gdp_col)))
  
  # Fuse and check for enough data
  extended_df <- bind_rows(obs_data, forecast_data)
  
  # Safety check for minimum data points
  if (nrow(extended_df) < 12) {
    message("Not enough Data for HP Filter")
    return(NULL)
  }
  
  # Apply HP Filter
  y <- extended_df[[gdp_col]]
  hp_obj <- hpfilter(y, freq = 1600)
  
  # Build Result Dataframe
  result_df <- extended_df %>%
    mutate(gap = as.numeric(hp_obj$cycle))
  
  # return logic either for estimation ->return until vantage point t
  if (!return_forecasts) {
    # Standard historical gap return
    return(result_df %>% 
             filter(.data[[date_col]] <= vantage_q) %>% 
             select(all_of(date_col), gap))
  } else {
    # return for forecasting just forecasted gdp gap
    return(result_df %>%
             filter(.data[[date_col]] > vantage_q) %>%
             slice_head(n = h) %>%
             select(all_of(date_col), gap)
    )
  }
}

