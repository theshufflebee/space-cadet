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
  
  current_T <- zoo::as.yearqtr(current_T)
  
 # Filter until the Vantage point
  vint_series <- data %>%
    filter(.data[[date_col]] <= current_T) %>%
    arrange(.data[[date_col]])
  
  # gets all non NA then selects first obs and from it the date
  first_valid_row <- which(!is.na(vint_series[[data_col]]))[1] 
  
  # Safeguard if all NA
  if (is.na(first_valid_row)) {
    stop(sprintf("Error: No valid history found for column '%s' up to %s", data_col, as.character(current_T)))
  }
  
  # Slice accordingly
  raw_series <- vint_series %>%
    slice(first_valid_row:n()) %>%
    pull(.data[[data_col]])
  
  log_raw_series <- log(raw_series)
  
  # Check for enough data (16 quarters = 4 years)
  n_obs <- length(na.omit(log_raw_series))
  if (n_obs < 16) {
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
                              burn_in = "1982 Q1") {
  
  # Define the vantage point and the 'Knowledge Cutoff'
  vantage_q <- zoo::as.yearqtr(vantage_quarter)
  
  # The last quarter where SNB REER is 'actually' known at the pseudo date
  # as our forecasts have a delay due to data releases 3 quarters is enough
  # forecast for Q2 2023 realistically happen only after the release of Q1 data
  # of other variables so during Q2 when Q2 2022 REER is already available
  # can be changed
  cutoff_q  <- vantage_q - snb_reer_delay/4 
  
  cutoff_q <- max(as.yearqtr("2000 Q4"), cutoff_q)
  
  # Data prep
  
  # We need the values at the moment the official SNB data ends at the vantage
  # point -> so at the cutoff
  # Is data from one date only
  anchor_data <- data %>%
    mutate(quarter = as.yearqtr(quarter)) %>%
    filter(quarter == cutoff_q) %>%
    select(reer_eu_ppi, REER_CREA)
  
  val_snb_T  <- anchor_data$reer_eu_ppi
  val_crea_T <- anchor_data$REER_CREA
  
  # If the cutoff is same as vantage quarter then we skipp this
  if (length(val_crea_T) > 0 && is.na(val_crea_T)) {
    message("WARNING: CREA REER IS NA. CALLED FROM splice_snb_series()")
  }
  
  
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
    mutate(quarter = as.yearqtr(quarter)) %>%
    select(quarter, reer_simulated)
  
  if(vantage_q < as.yearqtr("2004 Q1")){
    
    # 1. Historical series up to vintage date
    hist_part <- series_for_filter %>%
      mutate(quarter = zoo::as.yearqtr(quarter)) %>%
      filter(quarter <= vantage_q)
    
    # 2. Simple ARMA forecast on reer_simulated (h = 8)
    arma_fc <- get_arma_forecast(
      current_T = vantage_q,
      T_0       = burn_in,
      h         = 8,
      data      = hist_part,
      data_col  = "reer_simulated",
      date_col  = "quarter"
    )
    
    # Extract predicted values or fallback to flat line if ARMA returns NULL
    if (!is.null(arma_fc)) {
      fc_part <- arma_fc %>%
        select(quarter = forecast_date, reer_simulated = predicted_value)
    } else {
      last_val <- tail(hist_part$reer_simulated, 1)
      fc_part <- tibble(
        quarter = zoo::as.yearqtr(seq(vantage_q + 0.25, by = 0.25, length.out = 8)),
        reer_simulated = last_val
      )
    }
    
    # 3. Combine history + forecast and extract LOP gap via HP filter
    series_extended <- bind_rows(hist_part, fc_part) %>%
      arrange(quarter) %>%
      mutate(
        log_reer = log(reer_simulated),
        trend    = as.numeric(mFilter::hpfilter(log_reer, freq = 1600)$trend), 
        lop_gap  = as.numeric(log_reer - trend)
      )
    
  } else {
    # forcast for the h step ahead forecast
    # vantage point is the start
    reer_fc_at_vantage <- forecast_reer_components(current_T = vantage_q, h = 8, data = data)
    
    # Filter series is full dataframe (goes beyond vantage point) but has replaced snb reer
    # by proxy at cutoff
    # so cut off that df at the vantage point and then add the forecasted reer in top of it
    series_extended <- series_for_filter %>%
      mutate(quarter = as.yearqtr(quarter)) %>%
      filter(quarter <= vantage_q) %>% # Keep up to current knowledge in pseudo out of sample forecast
      bind_rows(
        reer_fc_at_vantage %>% 
          mutate(reer_simulated = val_snb_T * (predicted_reer / val_crea_T)
          ) %>%
          # Ensure forecast_date is also a yearqtr object
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
    
  }
  
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
                               burn_in = "1982 Q1") {
  
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



get_hp_gap <- function(data,
                       gdp_forecast_data,
                       vantage_q,
                       h = 8,
                       gdp_col = "log_gdp",
                       date_col = "quarter",
                       return_type = c("history", "forecast", "full")) {
  
  # Standardize string choice argument
  return_type <- match.arg(return_type)
  
  vantage_q   <- zoo::as.yearqtr(vantage_q)
  vantage_str <- format(vantage_q, format = "%Y Q%q")
  
  # 1. Gather historical observed vector context
  obs_data <- data %>%
    mutate(across(all_of(date_col), zoo::as.yearqtr)) %>%
    arrange(.data[[date_col]]) %>%
    filter(.data[[date_col]] <= vantage_q) %>%
    select(all_of(c(date_col, gdp_col)))
  
  # 2. Extract out-of-sample real-time forecast extension
  forecast_data_clean <- gdp_forecast_data[, c("date", vantage_str)] %>%
    stats::setNames(c("quarter", "log_gdp")) %>%
    mutate(quarter = zoo::as.yearqtr(quarter)) %>%
    drop_na()
  
  # 3. Anchor Discrepancy Gate
  obs_val   <- obs_data$log_gdp[obs_data$quarter == vantage_q]
  fcast_val <- forecast_data_clean$log_gdp[forecast_data_clean$quarter == vantage_q]
  
  if (length(obs_val) && length(fcast_val) && abs(obs_val - fcast_val) > 1e-5) {
    warning(sprintf("[DISCREPANCY] At %s: Obs = %.5f, Fcast = %.5f", vantage_str, obs_val, fcast_val))
  }
  
  # 4. Fuse seamlessly while dropping the overlapping duplicate row
  extended_df <- bind_rows(
    obs_data, 
    filter(forecast_data_clean, quarter > vantage_q)
  )
  
  if (nrow(extended_df) < 15) {
    stop("Insufficient rows to compute stable Hodrick-Prescott trend extraction matrix.")
  }
  
  # 5. Extract Trend via Hodrick-Prescott Filter
  # Lambda 1600 is standard for quarterly tracking frequencies
  hp_res <- mFilter::hpfilter(extended_df[[gdp_col]], freq = 1600, type = "lambda")
  extended_df$gap <- as.numeric(hp_res$cycle)
  
  # ----------------------------------------------------------------------------
  # 6. Structured Output Selection Routing
  # ----------------------------------------------------------------------------
  out_df <- switch(
    return_type,
    "history"  = filter(extended_df, quarter <= vantage_q),
    "forecast" = filter(extended_df, quarter > vantage_q) %>% head(h),
    "full"     = extended_df
  )
  
  return(out_df)
}



# ==============================================================================
# Helper Forecasts
# ==============================================================================


#' Generate a Real-Time Rolling Matrix of GDP Forecasts
#'
#' @description Loops through a sequence of historical quarters (vintages) to simulate
#' pseudo-real-time forecasting. At each origin date, it strips away future data, 
#' fits an ARIMA model on log GDP growth, and projects levels out-of-sample.
#'
#' @param fcast_start String or yearqtr. The starting vintage date for the rolling loop.
#' @param fcast_end String or yearqtr. The ending vintage date. Defaults to the maximum date in data.
#' @param data A data frame or tibble containing the full historical time series.
#' @param date_col Character. The name of the column containing the quarter index.
#' @param gdp_col Character. The name of the column containing the raw or log GDP values.
#' @param horizon Integer. The number of quarters ahead to forecast (default is 8).
#' @param hp_lambda Numeric. Smoothing parameter for the downstream HP filter (default is 1600).
#'
#' @return A data frame in a triangle forecast layout where the first column is \code{date}, 
#' and subsequent columns represent the forecast origins.
#' @seealso [forecast_gdp()]
#' @export
gdp_forecast_wrapper <- function(fcast_start,
                                 fcast_end = NULL,
                                 data,
                                 date_col,
                                 gdp_col,
                                 horizon = 8,
                                 hp_lambda = 1600) {
  
  # Ensure parsing into clean zoo yearqtr types
  start_date <- zoo::as.yearqtr(fcast_start)
  all_dates  <- zoo::as.yearqtr(data[[date_col]])
  
  if (is.null(fcast_end)) {
    end_date <- max(all_dates)
  } else {
    end_date <- zoo::as.yearqtr(fcast_end)
  }
  
  # Isolate the list of origin dates (vintages) to loop through
  all_fcast_dates <- all_dates[all_dates >= start_date & all_dates <= end_date]
  all_fcast_dates <- sort(unique(all_fcast_dates))
  
  # Determine total chronological span needed for rows (Target Dates)
  # Rows must span from your first origin date all the way to the final horizon of the last origin
  min_row_date <- min(all_fcast_dates)
  max_row_date <- max(all_fcast_dates) + (horizon / 4)
  all_row_dates <- seq(min_row_date, max_row_date, by = 1/4)
  
  # 3. Create a clean matrix structure filled with NAs
  # Matrix size: length(all_row_dates) rows by length(all_fcast_dates) columns
  mat <- matrix(NA_real_, 
                nrow = length(all_row_dates), 
                ncol = length(all_fcast_dates))
  
  # Name rows and columns temporarily with standard strings for easy tracking
  rownames(mat) <- format(all_row_dates, format = "%Y Q%q")
  colnames(mat) <- format(all_fcast_dates, format = "%Y Q%q")
  
  # 4. Run the Rolling Forecast Loop
  for (i in seq_along(all_fcast_dates)) {
    origin_date <- all_fcast_dates[i]
    origin_str  <- format(origin_date, format = "%Y Q%q")
    
    # Strip out any data ahead of the current origin date 
    historical_subset <- data %>% 
      filter(zoo::as.yearqtr(.data[[date_col]]) <= origin_date)
    
    # Run your pure-numeric ARIMA + HP filter function
    gdp_forecast <- forecast_gdp(data      = historical_subset,
                                 date_col  = date_col,
                                 gdp_col   = gdp_col,
                                 horizon   = horizon,
                                 hp_lambda = hp_lambda)
    
    # Extract the last history point (origin value) and the subsequent forecasts
    # This grabs exactly 1 + horizon rows
    fcasts_t <- gdp_forecast %>%
      filter(type == "Forecast" | quarter == origin_date) %>%
      arrange(quarter)
    
    # Map the generated predictions back into the main matrix matching row/col text keys
    for (j in 1:nrow(fcasts_t)) {
      target_str <- format(fcasts_t$quarter[j], format = "%Y Q%q")
      if (target_str %in% rownames(mat)) {
        # FIXED HERE: Changed gdp_gap to log_gdp
        mat[target_str, origin_str] <- fcasts_t$log_gdp[j]
      }
    }
  }
  
  # 5. Convert to final Tibble layout and construct your strict downstream tracking column
  fcast_df <- as.data.frame(mat)
  
  # Convert column names to required 'YYYYQX' format
  colnames(fcast_df) <- format(all_fcast_dates, format = "%Y Q%q")
  
  # Build and inject the tracking date column to the far left
  fcast_df <- fcast_df %>%
    mutate(
      date = format(zoo::as.yearqtr(rownames(mat), format = "%Y Q%q"), format = "%Y Q%q")
    ) %>%
    relocate(date, .before = everything())
  
  # Reset row names to clean tibble defaults
  rownames(fcast_df) <- NULL
  
  return(fcast_df)
}



# ----------------------------------------------------------------------------
#' Forecast GDP Growth using ARIMA with COVID Dummies
#'
#' @description Cleans raw input data, computes log GDP growth rates, and fits an 
#' \code{auto.arima} model. Automatically integrates independent pulse regressors 
#' for 2020 Q2 and 2020 Q3 if they fall within the historical window, preventing parameter 
#' distortion from the pandemic shock.
#'
#' @param data A data frame containing historical quarters and log GDP.
#' @param date_col Character. Name of the quarter column.
#' @param gdp_col Character. Name of the log GDP column.
#' @param horizon Integer. Quarters ahead to forecast (default is 8).
#' @param hp_lambda Numeric. HP smoothing parameter (default is 1600).
#'
#' @return A continuous tibble binding historical observations and out-of-sample 
#' level forecasts labeled by \code{type}.
#' @seealso [gdp_forecast_wrapper()]
#' @export
forecast_gdp <- function(data, date_col, gdp_col, horizon = 8, hp_lambda = 1600) {
  
  # Clean format and calculate growth rate using base/zoo
  prep_df <- data %>%
    mutate(
      date = zoo::as.yearqtr(.data[[date_col]]),
      log_gdp = as.numeric(.data[[gdp_col]])
    ) %>%
    arrange(date) %>%
    mutate(gdp_growth = log_gdp - dplyr::lag(log_gdp, 1))
  
  # Create and store the data frame first
  hist_growth_df <- prep_df %>% 
    filter(!is.na(gdp_growth))
  
  growth_vector <- hist_growth_df %>% pull(gdp_growth)
  n_obs <- length(growth_vector)
  
  # Add the pulse dummies
  hist_growth_df <- hist_growth_df %>%
    mutate(
      dummy_2020q2 = if_else(date == zoo::as.yearqtr("2020 Q2"), 1, 0),
      dummy_2020q3 = if_else(date == zoo::as.yearqtr("2020 Q3"), 1, 0)
    )
  
  # --------------------------------------------------------------------------
  # DYNAMIC XREG CLEANING & BOUNDARY CONTROL (CRITICAL FIX)
  # --------------------------------------------------------------------------
  # Extract only the dummy columns
  dummy_df <- hist_growth_df %>% select(dummy_2020q2, dummy_2020q3)
  
  # Keep ONLY columns that actually contain a 1 in this rolling window
  active_columns <- colSums(dummy_df) > 0
  dummy_df_cleaned <- dummy_df[, active_columns, drop = FALSE]
  
  # Safety Flag: We only use xreg if:
  # 1. There is at least one active dummy column
  # 2. The dummy isn't exclusively sitting on the very last row (causes singular fits)
  use_covid_xreg <- ncol(dummy_df_cleaned) > 0
  if (use_covid_xreg) {
    # If the only active dummy is pinned to the final row, turn xreg off for this step
    last_row_sums <- rowSums(tail(dummy_df_cleaned, 1))
    if (last_row_sums > 0 && sum(as.matrix(dummy_df_cleaned)) == 1) {
      use_covid_xreg <- FALSE
    }
  }
  
  # 2. Fit ARIMA on growth rates & generate out-of-sample path
  if (use_covid_xreg) {
    xreg_hist <- as.matrix(dummy_df_cleaned)
    
    # Fit ARIMA with the cleaned regressor columns
    arima_fit <- forecast::auto.arima(growth_vector, seasonal = FALSE, xreg = xreg_hist)
    
    # Set future dummies to 0 matching the exact active column count
    xreg_future <- matrix(0, nrow = horizon, ncol = ncol(xreg_hist))
    colnames(xreg_future) <- colnames(xreg_hist)
    
    growth_fc <- forecast::forecast(arima_fit, h = horizon, xreg = xreg_future)
  } else {
    # Clean fallback route for pre-COVID or boundary edge windows
    arima_fit <- forecast::auto.arima(growth_vector, seasonal = FALSE)
    growth_fc <- forecast::forecast(arima_fit, h = horizon)
  }
  future_growth_rates <- as.numeric(growth_fc$mean)
  
  # 3. Create the future timeline
  last_hist_date <- max(prep_df$date)
  last_hist_gdp  <- tail(prep_df$log_gdp, 1)
  future_dates   <- seq(last_hist_date + 1/4, length.out = horizon, by = 1/4)
  
  # Accumulate future log levels: y_{t+h} = y_{t+h-1} + \Delta y_{t+h}
  future_log_gdp <- cumsum(future_growth_rates) + last_hist_gdp 
  
  # 4. Bind history and future log levels into one continuous vector
  combined_df <- tibble(
    quarter = c(prep_df$date, future_dates),
    log_gdp = c(prep_df$log_gdp, future_log_gdp),
    type    = c(rep("History", nrow(prep_df)), rep("Forecast", horizon))
  )
  
  # PRINT ARIMA SUMMARY DETAILS
  # -----------------------------------------------------------------------------
  cat("\n", rep("=", 50), "\n", sep = "")
  cat(sprintf("ARIMA VINTAGE SPECIFICATION FOR ORIGIN: %s\n", as.character(last_hist_date)))
  if (use_covid_xreg) {
    cat(sprintf("STATUS: Active COVID XREG (Columns Used: %s)\n", paste(colnames(xreg_hist), collapse=", ")))
  } else {
    cat("STATUS: Baseline ARIMA Standard Setup\n")
  }
  cat(rep("-", 50), "\n", sep = "")
  
  print(arima_fit)
  
  cat(rep("=", 50), "\n\n", sep = "")
  
  return(combined_df)
}



################################################################################
#
# Forecast Helper for Inflation
#
################################################################################

#' Forecast YoY Inflation using ARIMA with COVID Dummies
#'
#' @description Cleans raw inputs, computes changes in quarterly inflation, 
#' fits an \code{auto.arima} model with COVID-shock overrides, and reconstructs 
#' rolling 4-quarter cumulative paths to return Year-on-Year headline inflation rates.
#'
#' @param data A data frame containing historical quarters and quarterly inflation.
#' @param date_col Character. Name of the quarter column.
#' @param inf_col Character. Name of the inflation column.
#' @param horizon Integer. Quarters ahead to forecast (default is 8).
#'
#' @return A continuous tibble binding historical quarters, forecasted YoY inflation rates,
#' and data type labels.
#' @export
forecast_inflation <- function(data, date_col, inf_col, horizon = 8) {
  
  # Clean format and calculate changes in quarterly inflation momentum
  prep_df <- data %>%
    mutate(
      date = zoo::as.yearqtr(.data[[date_col]]),
      inflation = as.numeric(.data[[inf_col]])
    ) %>%
    arrange(date) %>%
    mutate(inf_diff = inflation - dplyr::lag(inflation, 1))
  
  hist_diff_df <- prep_df %>% 
    filter(!is.na(inf_diff))
  
  diff_vector <- hist_diff_df %>% pull(inf_diff)
  
  # Add the pulse dummies matching the critical shocks
  hist_diff_df <- hist_diff_df %>%
    mutate(
      dummy_2020q2 = if_else(date == zoo::as.yearqtr("2020 Q2"), 1, 0),
      dummy_2020q3 = if_else(date == zoo::as.yearqtr("2020 Q3"), 1, 0)
    )
  
  # Extract and clean active dummy fields
  dummy_df <- hist_diff_df %>% select(dummy_2020q2, dummy_2020q3)
  active_columns <- colSums(dummy_df) > 0
  dummy_df_cleaned <- dummy_df[, active_columns, drop = FALSE]
  
  use_covid_xreg <- ncol(dummy_df_cleaned) > 0
  if (use_covid_xreg) {
    last_row_sums <- rowSums(tail(dummy_df_cleaned, 1))
    if (last_row_sums > 0 && sum(as.matrix(dummy_df_cleaned)) == 1) {
      use_covid_xreg <- FALSE
    }
  }
  
  # Fit ARIMA on changes
  if (use_covid_xreg) {
    xreg_hist <- as.matrix(dummy_df_cleaned)
    arima_fit <- forecast::auto.arima(diff_vector, seasonal = FALSE, xreg = xreg_hist)
    
    xreg_future <- matrix(0, nrow = horizon, ncol = ncol(xreg_hist))
    colnames(xreg_future) <- colnames(xreg_hist)
    
    diff_fc <- forecast::forecast(arima_fit, h = horizon, xreg = xreg_future)
  } else {
    arima_fit <- forecast::auto.arima(diff_vector, seasonal = FALSE)
    diff_fc   <- forecast::forecast(arima_fit, h = horizon)
  }
  future_diff_rates <- as.numeric(diff_fc$mean)
  
  # Create the future timeline
  last_hist_date <- max(prep_df$date)
  last_hist_inf  <- tail(prep_df$inflation, 1)
  future_dates   <- seq(last_hist_date + 1/4, length.out = horizon, by = 1/4)
  
  # Reconstruct the forward quarterly levels: inf_{t+h} = inf_{t+h-1} + \Delta inf_{t+h}
  future_inf_levels <- cumsum(future_diff_rates) + last_hist_inf
  
  # Compile continuous vector array
  full_inf_path <- c(prep_df$inflation, future_inf_levels)
  full_timeline <- c(prep_df$date, future_dates)
  
  # Reconstruct YoY Inflation via a rolling 4-quarter moving accumulation window
  # For quarter-on-quarter tracking metrics, YoY is computed as the trailing average
  yoy_inflation_vector <- vector("numeric", length = length(full_inf_path))
  for (k in 1:length(full_inf_path)) {
    if (k < 4) {
      # Fallback anchor for edge samples below trailing timeline boundaries
      yoy_inflation_vector[k] <- full_inf_path[k]
    } else {
      yoy_inflation_vector[k] <- mean(full_inf_path[(k-3):k])
    }
  }
  
  combined_df <- tibble(
    quarter       = full_timeline,
    yoy_inflation = yoy_inflation_vector,
    type          = c(rep("History", nrow(prep_df)), rep("Forecast", horizon))
  )
  
  # PRINT ARIMA SUMMARY DETAILS TO THE CONSOLE
  cat("\n", rep("=", 50), "\n", sep = "")
  cat(sprintf("INFLATION ARIMA VINTAGE SPECIFICATION FOR ORIGIN: %s\n", as.character(last_hist_date)))
  if (use_covid_xreg) {
    cat(sprintf("STATUS: Active COVID XREG (Columns Used: %s)\n", paste(colnames(xreg_hist), collapse=", ")))
  } else {
    cat("STATUS: Baseline ARIMA Standard Setup\n")
  }
  cat(rep("-", 50), "\n", sep = "")
  print(arima_fit)
  cat(rep("=", 50), "\n\n", sep = "")
  
  return(combined_df)
}



#' Generate a Real-Time Rolling Matrix of Inflation Forecasts
#'
#' @description Loops through a sequence of historical quarters (vintages) to simulate
#' pseudo-real-time forecasting. At each origin date, it strips away future data, 
#' fits an ARIMA model on changes in inflation, and projects YoY inflation out-of-sample.
#'
#' @param fcast_start String or yearqtr. The starting vintage date for the rolling loop.
#' @param fcast_end String or yearqtr. The ending vintage date. Defaults to the maximum date in data.
#' @param data A data frame or tibble containing the full historical time series.
#' @param date_col Character. The name of the column containing the quarter index.
#' @param inf_col Character. The name of the column containing the quarterly inflation values.
#' @param horizon Integer. The number of quarters ahead to forecast (default is 8).
#'
#' @return A data frame in a triangle forecast layout where the first column is \code{date}, 
#' and subsequent columns represent the forecast origins.
#' @export
inflation_forecast_wrapper <- function(fcast_start,
                                       fcast_end = NULL,
                                       data,
                                       date_col,
                                       inf_col,
                                       horizon = 8) {
  
  # Ensure parsing into clean zoo yearqtr types
  start_date <- zoo::as.yearqtr(fcast_start)
  all_dates  <- zoo::as.yearqtr(data[[date_col]])
  
  if (is.null(fcast_end)) {
    end_date <- max(all_dates)
  } else {
    end_date <- zoo::as.yearqtr(fcast_end)
  }
  
  # Isolate the list of origin dates (vintages) to loop through
  all_fcast_dates <- all_dates[all_dates >= start_date & all_dates <= end_date]
  all_fcast_dates <- sort(unique(all_fcast_dates))
  
  # Determine total chronological span needed for rows (Target Dates)
  min_row_date <- min(all_fcast_dates)
  max_row_date <- max(all_fcast_dates) + (horizon / 4)
  all_row_dates <- seq(min_row_date, max_row_date, by = 1/4)
  
  # Create a clean matrix structure filled with NAs
  mat <- matrix(NA_real_, 
                nrow = length(all_row_dates), 
                ncol = length(all_fcast_dates))
  
  rownames(mat) <- format(all_row_dates, format = "%YQ%q")
  colnames(mat) <- format(all_fcast_dates, format = "%YQ%q")
  
  # Run the Rolling Forecast Loop
  for (i in seq_along(all_fcast_dates)) {
    origin_date <- all_fcast_dates[i]
    origin_str  <- format(origin_date, format = "%YQ%q")
    
    # Strip out any data ahead of the current origin date 
    historical_subset <- data %>% 
      filter(zoo::as.yearqtr(.data[[date_col]]) <= origin_date)
    
    # Run the numeric ARIMA inflation function
    inf_forecast <- forecast_inflation(data     = historical_subset,
                                       date_col = date_col,
                                       inf_col  = inf_col,
                                       horizon  = horizon)
    
    # Extract the last history point (origin value) and subsequent forecasts
    fcasts_t <- inf_forecast %>%
      filter(type == "Forecast" | quarter == origin_date) %>%
      arrange(quarter)
    
    # Map predictions back into the matrix matching text keys
    for (j in 1:nrow(fcasts_t)) {
      target_str <- format(fcasts_t$quarter[j], format = "%YQ%q")
      if (target_str %in% rownames(mat)) {
        mat[target_str, origin_str] <- fcasts_t$yoy_inflation[j]
      }
    }
  }
  
  # Convert to final Tibble layout
  fcast_df <- as.data.frame(mat)
  colnames(fcast_df) <- format(all_fcast_dates, format = "%YQ%q")
  
  # Build and inject the tracking date column to the far left
  fcast_df <- fcast_df %>%
    mutate(
      date = format(zoo::as.yearqtr(rownames(mat), format = "%YQ%q"), format = "%YQ%q")
    ) %>%
    relocate(date, .before = everything())
  
  rownames(fcast_df) <- NULL
  return(fcast_df)
}



# ==============================================================================
# Function for merging two fcst DFs
# ==============================================================================


#' Merge State-Space Inflation Forecasts into ARIMA Forecast DataFrame
#'
#' @description Aligns and overlays an advanced state-space forecast matrix onto 
#' a baseline ARIMA forecast triangle. Splicing matches exact historical target dates 
#' (rows) and vintage origin points (columns).
#'
#' @param arima_df Data Frame. The triangle forecast output generated by \code{inflation_forecast_wrapper}.
#' @param ssm_df Data Frame. The State-Space forecast triangle frame (\code{fcst_df_inf}) starting in 2010.
#'
#' @return A consolidated data frame matching the original ARIMA dimensions, updated with 
#' State-Space forecasts where overlapping cells exist.
#' @export
merge_inflation_forecast_vintages <- function(arima_df, ssm_df) {
  
  message("=== STARTING CROSS-VINTAGE FORECAST CONSOLIDATION ===")
  
  # 1. Standardize tracking structures to prevent formatting drops
  arima_df$date <- format(zoo::as.yearqtr(arima_df$date), format = "%Y Q%q")
  ssm_df$date   <- format(zoo::as.yearqtr(ssm_df$date), format = "%Y Q%q")
  
  colnames(arima_df)[colnames(arima_df) != "date"] <- format(zoo::as.yearqtr(colnames(arima_df)[colnames(arima_df) != "date"]), format = "%Y Q%q")
  
  colnames(ssm_df)[colnames(ssm_df) != "date"] <- format(zoo::as.yearqtr(colnames(ssm_df)[colnames(ssm_df) != "date"]), format = "%Y Q%q")
                               
  
  # 2. Extract shared operational coordinates
  # We find the intersection of vintage points (columns) present in both datasets
  shared_vintages <- intersect(colnames(arima_df), colnames(ssm_df))
  shared_vintages <- shared_vintages[shared_vintages != "date"]
  
  if (length(shared_vintages) == 0) {
    stop("CRITICAL ERROR: No overlapping vintage columns found between ARIMA and SSM data frames.")
  }
  
  message(sprintf("Found %d overlapping real-time vintage columns (from %s to %s).", 
                  length(shared_vintages), min(shared_vintages), max(shared_vintages)))
  
  # 3. Create a deep mutable copy of the baseline ARIMA layout
  consolidated_df <- arima_df
  
  # Convert target dates to explicit row keys for matrix address operations
  rownames(consolidated_df) <- consolidated_df$date
  rownames(ssm_df)           <- ssm_df$date
  
  # Count modified entries for diagnostic verification logging
  update_counter <- 0
  
  # 4. Execute the Asymmetric Cell Overwrite Loop
  for (v_col in shared_vintages) {
    
    # Identify target dates populated within the State-Space vintage column
    # We filter out NAs to make sure we only copy real, generated model projections
    valid_ssm_rows <- ssm_df$date[!is.na(ssm_df[[v_col]])]
    
    for (t_row in valid_ssm_rows) {
      if (t_row %in% rownames(consolidated_df)) {
        
        # Extract the advanced state-space forecast point value
        ssm_value <- ssm_df[t_row, v_col]
        
        # Splice the point value directly into the corresponding coordinates
        consolidated_df[t_row, v_col] <- ssm_value
        update_counter <- update_counter + 1
      }
    }
  }
  
  # 5. Clean up row indices to return standard tibble layouts
  rownames(consolidated_df) <- NULL
  
  message(sprintf("SUCCESS: Consolidation complete. Spliced %d point values into the target matrix.", update_counter))
  cat(rep("=", 50), "\n\n")
  
  return(consolidated_df)
}

################################################################################
#
# BUILD MODEL DATA MATRICES
#
################################################################################


# ==============================================================================
# Phillips Model Data Matrix
# ==============================================================================


#' Build the X Data Matrix for the Phillips Curve State-Space Model
#'
#' @description
#' Prepares all observed variables (y) and exogenous variables (mu_t) and collects
#' them in a matrix for the Phillips curve estimation at a specific 
#' vantage point. The function first transforms the time-series data (logs and lags), 
#' integrates separately constructed components like the Law of One Price (LOP) gap and 
#' GDP gap, and ensures alignment between observed inflation and expectations.
#'
#' @export
build_data_matrix_philips <- function(T_0 = "1982-01-01",
                                      vantage_quarter,
                                      data = master_philips,
                                      gdp_forecasts = gdp_forecasts_arima,
                                      h = h,
                                      set_silent = TRUE,
                                      quarterly = TRUE) {
  
  T_0 <- as.yearqtr(T_0)
  
  if(quarterly) {inf_lag <- 1} else {inf_lag <- 4}
  
  vantage_quarter <- as.yearqtr(vantage_quarter)
  
  # Lag CPI before burn cutoff
  raw_data <- data %>%
    arrange(quarter) %>%
    mutate(
      cpi = as.numeric(cpi),
      log_inflation_diff = (log(cpi) - dplyr::lag(log(cpi), inf_lag)) * 100 * (4/inf_lag),
      lag_log_inflation_diff = dplyr::lag(log_inflation_diff, 1)
    )
  
  # Add burn in cutoff
  raw_data <- raw_data %>%
    filter(quarter <= as.yearqtr(vantage_quarter)) %>%
    arrange(quarter)
  
  if(!set_silent){
    message("RAW DATA DEBUG")
    print(tail(raw_data))
    print(head(raw_data))
    
  }
  
  
  LOP_forecast <- splice_snb_series(vantage_quarter = vantage_quarter,
                                    snb_reer_delay = SNB_REER_DELAY,
                                    data = raw_data
  )
  
  if(!set_silent){
    message("LOP FORECAST")
    print(tail(LOP_forecast))
  }
  
  
  HP_gap_data <- get_hp_gap(raw_data,
                            gdp_forecast_data = gdp_forecasts, # forecast data
                            vantage_q = vantage_quarter
  )
  
  if(!set_silent) {
    message("HP GAP DATA DONE")
    print(HP_gap_data)
  }
  
  
  X_matrix <- raw_data %>%
    select(quarter, log_inflation_diff, lag_log_inflation_diff, log_gdp, `5y_cpi_forecast`) %>%
    left_join(LOP_forecast %>% select(quarter, lop_gap), by = "quarter") %>%
    left_join(HP_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
    mutate(
      # Force columns to be standard numeric vectors to strip hidden ts matrix attributes
      lop_gap     = as.numeric(lop_gap) * 100,
      lag_lop_gap = dplyr::lag(lop_gap, 1),
      
      gdp_gap     = as.numeric(gdp_gap) * 100,
      lag_gdp_gap = dplyr::lag(gdp_gap, 1),
      
      # Clean expectations format
      `5y_cpi_forecast` = as.numeric(`5y_cpi_forecast`)
    ) %>%
    # Filter to return the specific estimation sample
    filter(quarter >= as.yearqtr(T_0)) %>%
    arrange(quarter)
  
  
  if(!set_silent) {
    message("X_Matrix sucessfully built")
    print(X_matrix)
  }
  
  return(X_matrix)
}



# ==============================================================================
# TAYLOR Model Data Matrix
# ==============================================================================

#' Builder for the Data Input matrices X and Y of the Taylor State space model
#' 
#' @description
#' This function basically builds a data matrix that is then split into X and Y 
#' By the corresponding ssm_estimation function. We can specify wether we want a forward or backward
#' looking taylor rule and also how we want the inf gap to be constructed.
#' 
build_data_matrix_taylor <- function(T_0 = "1981-01-01",
                                    vantage_quarter,
                                    data = master_taylor,
                                    gdp_forecasts = gdp_forecasts_arima,
                                    h = h,
                                    set_silent = TRUE,
                                    quarterly = TRUE,
                                    taylor_rule_spec = "backward",
                                    if_gap_spec = "minus_1") {
  
  # Make sure it's yearquarter object
  T_0 <- as.yearqtr(T_0)
  
  if(taylor_rule_spec == "backward") {
    
    if(inf_gap_spec == "minus_1") {
      
      # Slice Data available "Today"
      data_t <- data[data[[quarter]] <= vantage_quarter, ]
      
      # Process Exogenous Data
      # HP Filter is estimated inclduing the burn in period before the start of the information set
      
      # Error if there ar not enough valid gdp obs 
      valid_inf_indices <- which(!is.na(data_t$log_cpi))
      
      # select GDP series
      first_obs_idx <- valid_inf_indices[1]
      inf_series    <- data_t$log_cpi[first_obs_idx:nrow(data_t)]
      
      gdp_gap_data <- get_hp_gap(data = data_t,
                                 gdp_forecast_data = gdp_forecasts, # forecast data
                                 vantage_q = target_date
      )
      gdp_gap_data$gap <- gdp_gap_data$gap * 100
      
      
      inf_gap_data <- data_t %>%
        select(all_of(c("quarter", "inf_gap")))
      
      
      processed_data <- data_t %>%
        select(quarter, saron_libor_splice, forward_rate, yoy_inf) %>%
        left_join(gdp_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
        left_join(inf_gap_data %>% select(quarter, inf_gap), by = "quarter") %>%
        filter(quarter >= as.yearqtr(val_T1)) %>%
        arrange(quarter) %>%
        filter(complete.cases(gdp_gap, inf_gap))%>%
        # Filter to return the specific estimation sample
        filter(quarter >= as.yearqtr(T_0))
      
    } else if(if_gap_spec == "ssm_gap") {
      
      # Need to extract all states from each of the estimations then format them into a df and subtract from observed data
      # then take the state at t minus observed value at t
      # maybe use fitted obs instead
      # also use long format for this
      # quarter, vantage_quarter, variable, value should be all i need, then filter for specific set
      
    } else {
      message("NO VALID INF GAP SPECIFICATION GIVEN. ABORTING...")
      stop("STOPPING EXECUTION AT build_data_matrix_taylor()")
    }
    
    
    
  } else if(taylor_rule_spec == "forward") {
    
    if(inf_gap_spec == "minus_1") {
      
      # use SPF CY or NY means minus 1
      
      # USE SPF forecasts minus 1
      
    } else if(if_gap_spec == "ssm_gap") {
      
      # USE SPF FORECASTS MINUS 1
      
    } else {
      message("NO VALID INF GAP SPECIFICATION GIVEN. ABORTING...")
      stop("STOPPING EXECUTION AT build_data_matrix_taylor()")
    }
    
  } else {
    message("NO VALID TAYLOR RULE SPECIFICATION GIVEN. ABORTING...")
    stop("STOPPING EXECUTION AT build_data_matrix_taylor()")
  }
  
  return(processed_data)
  
} 


get_ssm_inf_gap <- function(ssm_folder) {
  
  
  # find target dir
  target_dir <- here::here("output", "para", ssm_folder)
  if (!dir.exists(target_dir)) stop(sprintf("Directory does not exist: %s", target_dir))
  
  saved_files <- list.files(path = target_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(saved_files) == 0) return(matrix(NA_real_, 0, 0))
  
  # get list of results
  vintages_data <- lapply(saved_files, function(file_path) {
    payload <- readRDS(file_path)
    
    # Extract vantage quarter string
    vantage_str <- format(zoo::as.yearqtr(payload$target_date), format = "%Y Q%q")
    
    #get historical time series
    start_date <- zoo::as.yearqtr("1990 Q1")
    total_obs  <- nrow(payload$states$fitted.obs)
    date_seq   <- format(seq(start_date, by = 0.25, length.out = total_obs), format = "%Y Q%q")
    
    # calculate inflation gap
    # fitted inflation (column 1) - trend state (column 1) = implied inflation gap
    fitted_inf <- payload$states$fitted.obs[, 1]
    trend_state <- payload$states$r[, 1]
    calculated_gap <- fitted_inf - trend_state
    
    return(list(
      vantage    = vantage_str,
      dates      = date_seq,
      inf_gaps   = calculated_gap
    ))
  })
  
  # select unique vantage quarter first to last rolling est
  all_vantages <- sort(unique(sapply(vintages_data, function(x) x$vantage)))
  
  # Select unique dates from T_0 to last vantage date
  all_dates    <- sort(unique(unlist(lapply(vintages_data, function(x) x$dates))))
  
  # Build an empty evaluation matrix populated with NA values
  inf_gap_matrix <- matrix(NA_real_, 
                           nrow = length(all_vantages), 
                           ncol = length(all_dates),
                           dimnames = list(all_vantages, all_dates))
  
  # 4. Map the calculated elements into the grid spaces
  for (v_data in vintages_data) {
    v_row <- v_data$vantage
    inf_gap_matrix[v_row, v_data$dates] <- v_data$inf_gaps
  }
  
  # Convert matrix structure to a friendly dataframe presentation layout
  inf_gap_df <- as.data.frame(inf_gap_matrix)
  
  return(inf_gap_df)
  
}

get_ssm_forecasted_inf_gap <- function(inf_forecasts_df) {
  
  # select
  
}
