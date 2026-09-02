################################################################################
#
# THIS SCRIPT CONTAINS THE FUNCTIONS FOR ARMA FORECASTS
#
################################################################################

# REMARK: The Roxygen documentation was made using AI

#' Generate a Single h-Step Auto-ARIMA Forecast for a Given Origin
#'
#' Estimates an automated ARIMA model on log-transformed quarterly data up to a specified
#' forecast origin (\code{current_T}) and returns point forecasts transformed back to original levels.
#'
#' @details
#' The input series is trimmed to valid observations up to \code{current_T} and log-transformed:
#' \deqn{y_t = \log(x_t)}
#' An optimal ARIMA(\eqn{p,d,q}) model is selected via \code{forecast::auto.arima(..., seasonal = FALSE)}.
#' Point forecasts \eqn{\hat{y}_{T+h|T}} are generated and mapped back to levels via exponentiation:
#' \deqn{\hat{x}_{T+h|T} = \exp(\hat{y}_{T+h|T})}
#'
#' @param current_T Character, Date, or \code{yearqtr}. The forecast origin/vantage point date.
#' @param T_0 Optional character or \code{yearqtr}. Start date of the training window (retained for signature compatibility). Defaults to \code{"2000 Q1"}.
#' @param h Integer. Forecast horizon in quarters. Defaults to \code{8}.
#' @param data A data frame or tibble containing the historical time series.
#' @param data_col Character. Name of the column containing the target series to forecast.
#' @param date_col Character. Name of the date column in \code{data}. Defaults to \code{"quarter"}.
#'
#' @return A tibble with \code{h} rows containing:
#' \item{origin_date}{The forecast origin date of class \code{yearqtr}.}
#' \item{forecast_date}{Target quarterly forecast dates of class \code{yearqtr}.}
#' \item{h_step}{Integer forecast steps \eqn{1, \dots, h}.}
#' \item{predicted_value}{Point forecasts on the original level scale.}
#' Returns \code{NULL} if estimation fails or fewer than 16 valid observations are available.
#'
#' @export
#' @importFrom dplyr filter arrange slice pull tibble %>%
#' @importFrom rlang .data
#' @importFrom zoo as.yearqtr
#' @importFrom forecast auto.arima forecast
#' @importFrom stats na.omit
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
  
  # Return tidy tibble
  return(tibble(
    origin_date = current_T,
    forecast_date = as.yearqtr(fc_dates),
    h_step = 1:h,
    predicted_value = as.numeric(fc_levels)

  ))
}



#' Forecast REER via Component ARMA and Random Walk
#'
#' Projects the Real Effective Exchange Rate (REER) forward by decomposing it into
#' its constituent series: domestic Swiss PPI, foreign Euro Area PPI, and the nominal
#' exchange rate.
#'
#' @details
#' Assumes the nominal CHF/EUR exchange rate follows a driftless random walk
#' (\eqn{S_{T+h|T} = S_T}), while Swiss and Euro Area PPI series are projected
#' via independent automated log-ARIMA models. The combined point forecast is:
#' \deqn{\widehat{REER}_{T+h|T} = S_T \cdot \left( \frac{\widehat{PPI}^{\text{CH}}_{T+h|T}}{\widehat{PPI}^{\text{EUR}}_{T+h|T}} \right)}
#'
#' @param current_T Character, Date, or \code{yearqtr}. The forecast origin date \eqn{T}.
#' @param h Integer. Forecast horizon in quarters. Defaults to \code{8}.
#' @param data A data frame containing quarterly series for \code{quarter},
#'   \code{ppi_ch_idx}, \code{ppi_eur_idx}, and \code{ex_eoq}.
#'
#' @return A tibble containing:
#' \item{origin_date}{The forecast origin date \eqn{T} of class \code{yearqtr}.}
#' \item{forecast_date}{Target quarterly forecast dates of class \code{yearqtr}.}
#' \item{h_step}{Integer forecast steps \eqn{1, \dots, h}.}
#' \item{predicted_reer}{Reconstructed point forecasts for the REER level.}
#'
#' @seealso \code{\link{get_arma_forecast}}
#'
#' @export
#' @importFrom dplyr filter pull rename left_join mutate select %>%
#' @importFrom zoo as.yearqtr
forecast_reer_components <- function(current_T, h = 8, data) {
  
  raw_data <- data

  # Forecast PPI CH Area
  # Inside arma function happens: log, and then in the auto arima
  # there is a automatic detection of need of differentiation
  fc_ppi_ch <- get_arma_forecast(current_T = current_T,
                                 h = h,
                                 data = raw_data,
                                 data_col = "ppi_ch_idx",
                                 date_col = "quarter")
  
  # Forecast PPI Euro Area
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


#' Construct Real-Time Spliced REER Series and Extract LOP Gap
#'
#' Reconstructs the historical Real Effective Exchange Rate (REER) series available at a
#' given forecast vantage point, corrects for official SNB publication lags using a chained
#' CREA proxy, and projects it 8 quarters ahead to extract the Law of One Price (LOP) gap via the HP filter.
#'
#' @details
#' The function accounts for publication lags by identifying the last published SNB REER
#' observation (\code{cutoff_q}) and chaining forward to the forecast origin (\code{vantage_quarter})
#' using the observed CREA proxy growth rates.
#'
#' For out-of-sample projections (8 quarters ahead):
#' \itemize{
#'   \item \bold{Pre-2004 Q1:} Forecasts the spliced index directly using an automated univariate ARIMA model.
#'   \item \bold{From 2004 Q1 onward:} Decomposes the REER into nominal CHF/EUR exchange rate (Random Walk)
#'   and Swiss/Euro Area PPIs (auto-ARIMA), projecting the series forward via chained growth rates.
#' }
#' The combined historical and projected series is then filtered via \code{mFilter::hpfilter} (\eqn{\lambda = 1600})
#' to extract the cyclical LOP gap and mitigate endpoint filter bias.
#'
#' @param vantage_quarter Character, Date, or \code{yearqtr}. The pseudo-real-time forecast origin.
#'   Defaults to \code{"2023 Q2"}.
#' @param snb_reer_delay Integer. Publication delay of the official SNB REER in quarters. Defaults to \code{3}.
#' @param data A data frame containing quarterly series for \code{quarter}, \code{reer_eu_ppi},
#'   \code{REER_CREA}, \code{ppi_ch_idx}, \code{ppi_eur_idx}, and \code{ex_eoq}.
#' @param burn_in Character or \code{yearqtr}. Start date of the historical sample to mitigate HP filter
#'   starting-point bias. Defaults to \code{"1982 Q1"}.
#'
#' @return A tibble spanning from \code{burn_in} through \code{vantage_quarter + 8 quarters} containing:
#' \item{quarter}{Quarterly dates of class \code{yearqtr}.}
#' \item{reer_simulated}{Spliced historical and projected REER level.}
#' \item{log_reer}{Natural logarithm of \code{reer_simulated}.}
#' \item{trend}{HP filter trend component (\eqn{\lambda = 1600}).}
#' \item{lop_gap}{Cyclical LOP gap (\eqn{\log(\text{REER}) - \text{trend}}).}
#'
#' @seealso \code{\link{get_arma_forecast}}, \code{\link{forecast_reer_components}}
#'
#' @export
#' @importFrom dplyr filter mutate select bind_rows arrange %>%
#' @importFrom zoo as.yearqtr
#' @importFrom tibble tibble
#' @importFrom mFilter hpfilter
#' @importFrom utils tail
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
    
    # Historical series up to vintage date
    hist_part <- series_for_filter %>%
      mutate(quarter = zoo::as.yearqtr(quarter)) %>%
      filter(quarter <= vantage_q)
    
    # Simple ARMA forecast on reer_simulated (h = 8)
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
    
    # Combine history + forecast and extract LOP gap via HP filter
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




#' Splice SNB REER with CREA Proxy and Component Projections
#'
#' Constructs a real-time REER level trajectory up to \eqn{T+8} quarters ahead by splicing 
#' official historical SNB data with the chained CREA proxy across publication lag periods 
#' and appending component-based forecasts.
#'
#' @details
#' Official SNB REER data is subject to publication lags. The function determines the last 
#' published observation at \code{cutoff_q = vantage_quarter - snb_reer_delay/4} and bridges 
#' the gap to the forecast origin (\code{vantage_quarter}) using chained growth rates of the 
#' observed CREA proxy. 
#' 
#' From the vantage point forward, 8-quarter-ahead projections are generated via 
#' \code{\link{forecast_reer_components}} (Random Walk exchange rates and auto-ARIMA price indices) 
#' and chained to the spliced level.
#'
#' @param vantage_quarter Character, Date, or \code{yearqtr}. The pseudo-real-time forecast origin. 
#'   Defaults to \code{"2023 Q2"}.
#' @param snb_reer_delay Integer. Publication lag of the official SNB REER in quarters. Defaults to \code{3}.
#' @param data A data frame containing quarterly series for \code{quarter}, \code{reer_eu_ppi}, 
#'   \code{REER_CREA}, \code{ppi_ch_idx}, \code{ppi_eur_idx}, and \code{ex_eoq}.
#' @param burn_in Character or \code{yearqtr}. Historical sample start date. Defaults to \code{"1982 Q1"}.
#'
#' @return A tibble with two columns:
#' \item{quarter}{Quarterly dates of class \code{yearqtr} spanning \code{burn_in} to \code{vantage_quarter + 2 years}.}
#' \item{reer_simulated}{Spliced historical and forecasted REER levels.}
#'
#' @seealso \code{\link{forecast_reer_components}}, \code{\link{splice_snb_series}}
#'
#' @export
#' @importFrom dplyr filter mutate select bind_rows %>%
#' @importFrom zoo as.yearqtr
splice_reer_series <- function(vantage_quarter = "2023 Q2",
                               snb_reer_delay = 3,
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




#' Extract Law of One Price (LOP) Gap from Spliced REER Series
#'
#' Applies the Hodrick-Prescott (HP) filter to the natural logarithm of a spliced
#' Real Effective Exchange Rate (REER) series to isolate the cyclical LOP gap from its trend.
#'
#' @param spliced_df A data frame or tibble output from \code{\link{splice_reer_series}}
#'   containing \code{quarter} and \code{reer_simulated}.
#' @param hp_freq Numeric. Smoothing parameter (\eqn{\lambda}) for the HP filter.
#'   Defaults to \code{1600} for quarterly time series.
#'
#' @return A tibble augmented with HP filter decomposition columns:
#' \item{quarter}{Quarterly dates of class \code{yearqtr}.}
#' \item{reer_simulated}{Spliced REER series in levels.}
#' \item{log_reer}{Natural logarithm of \code{reer_simulated}.}
#' \item{trend}{Extracted HP filter trend component.}
#' \item{lop_gap}{Cyclical LOP gap (\eqn{\log(\text{REER}) - \text{trend}}).}
#'
#' @seealso \code{\link{splice_reer_series}}
#'
#' @export
#' @importFrom dplyr mutate %>%
#' @importFrom mFilter hpfilter
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


#' Generate Rolling Real-Time REER Forecast Matrix for the SNB REER
#'
#' @param data DataFrame containing master historical series.
#' @param date_col Character. Column name for target dates in output (default "date").
#' @param start_date Character/yearqtr. First forecast origin date (default "1990 Q1").
#' @param end_date Character/yearqtr. Last forecast origin date (default NULL -> max date).
#' @param forecast_h Integer. Forecast horizon in quarters (default 8).
#' @param snb_reer_delay Integer. Lag in quarters before official SNB REER is published (default 3).
#' @param burn_in Character/yearqtr. Burn-in start date (default "1982 Q1").
#'
#' @return A data.frame with target dates formatted as YYYYQX in the first column (`date_col`) and vintage origin columns.
#' @export
generate_snb_reer_forecasts <- function(data = master_philips,
                                        date_col = "date",
                                        start_date = "1990 Q1",
                                        end_date = NULL,
                                        forecast_h = 8,
                                        snb_reer_delay = 4,
                                        burn_in = "1982 Q1") {
  
  message("Generating REER Level Forecasts")
  
  # Floating-point safe quarter-to-string conversion
  to_qtr_str <- function(dates) {
    # Round to 4 decimal places to prevent float drift (e.g. 2004.250000000002)
    clean_dates <- zoo::as.yearqtr(round(as.numeric(zoo::as.yearqtr(dates)) * 4) / 4)
    format(clean_dates, "%YQ%q")
  }
  
  input_date_var <- if ("quarter" %in% colnames(data)) "quarter" else date_col
  
  clean_data <- data %>%
    mutate(quarter = zoo::as.yearqtr(.data[[input_date_var]])) %>%
    arrange(quarter)
  
  all_dates <- clean_data %>% pull(quarter)
  
  loop_start <- if (!is.null(start_date)) zoo::as.yearqtr(start_date) else min(all_dates)
  loop_end   <- if (!is.null(end_date))   zoo::as.yearqtr(end_date)   else max(all_dates)
  
  origin_dates <- all_dates[all_dates >= loop_start & all_dates <= loop_end]
  
  # Construct grid using integer steps instead of float addition
  min_date <- min(origin_dates)
  max_date <- max(origin_dates) + (forecast_h / 4)
  n_steps  <- round((as.numeric(max_date) - as.numeric(min_date)) * 4)
  extended_rows <- zoo::as.yearqtr(as.numeric(min_date) + (0:n_steps) * 0.25)
  
  eval_mat <- matrix(NA_real_, nrow = length(extended_rows), ncol = length(origin_dates))
  rownames(eval_mat) <- to_qtr_str(extended_rows)
  colnames(eval_mat) <- to_qtr_str(origin_dates)
  
  for (i in seq_along(origin_dates)) {
    vantage_q   <- origin_dates[i]
    vantage_str <- to_qtr_str(vantage_q)
    
    # Safe cutoff calculation
    cutoff_val <- max(as.numeric(zoo::as.yearqtr("2000 Q4")), 
                      as.numeric(vantage_q) - (snb_reer_delay * 0.25))
    cutoff_q   <- zoo::as.yearqtr(round(cutoff_val * 4) / 4)
    
    anchor_data <- clean_data %>%
      filter(to_qtr_str(quarter) == to_qtr_str(cutoff_q)) %>%
      select(reer_eu_ppi, REER_CREA)
    
    val_snb_T  <- anchor_data$reer_eu_ppi
    val_crea_T <- anchor_data$REER_CREA
    
    series_for_filter <- clean_data %>%
      filter(quarter >= zoo::as.yearqtr(burn_in) & quarter <= vantage_q) %>%
      mutate(
        reer_simulated = ifelse(
          quarter <= cutoff_q,
          reer_eu_ppi,
          val_snb_T * (REER_CREA / val_crea_T)
        )
      ) %>%
      select(quarter, reer_simulated)
    
    # Forecast branching
    if (vantage_q < zoo::as.yearqtr("2004 Q1")) {
      arma_fc <- get_arma_forecast(
        current_T = vantage_q,
        T_0       = burn_in,
        h         = forecast_h,
        data      = series_for_filter,
        data_col  = "reer_simulated",
        date_col  = "quarter"
      )
      
      if (!is.null(arma_fc) && nrow(arma_fc) > 0) {
        fc_part <- arma_fc %>%
          select(quarter = forecast_date, reer_simulated = predicted_value)
      } else {
        stop("REER Forecast Failed")
      }
      
      series_extended <- bind_rows(series_for_filter, fc_part)
      
    } else {
      reer_fc_at_vantage <- forecast_reer_components(current_T = vantage_q, h = forecast_h, data = clean_data)
      
      fc_part <- reer_fc_at_vantage %>%
        mutate(reer_simulated = val_snb_T * (predicted_reer / val_crea_T)) %>%
        select(quarter = forecast_date, reer_simulated)
      
      series_extended <- bind_rows(series_for_filter, fc_part)
    }
    
    fc_lookup <- setNames(fc_part$reer_simulated, to_qtr_str(fc_part$quarter))
    matched_fc_rows <- intersect(names(fc_lookup), rownames(eval_mat))
    
    if (length(matched_fc_rows) > 0) {
      eval_mat[matched_fc_rows, i] <- fc_lookup[matched_fc_rows]
    }
    
    true_hist_val <- clean_data %>% 
      filter(to_qtr_str(quarter) == vantage_str) %>% 
      pull(reer_eu_ppi)
    
    if (length(true_hist_val) > 0 && !is.na(true_hist_val[1])) {
      eval_mat[vantage_str, i] <- true_hist_val[1]
    }
  }
  
  eval_df <- as.data.frame(eval_mat) %>%
    tibble::rownames_to_column(var = date_col)
  
  message("ENDED REER Level Forecasts")
  return(eval_df)
}



#' Construct Real-Time HP Filter Gap Forecast Matrix
#'
#' Generates a lower-triangular real-time HP gap forecast matrix across expanding
#' vintage origins. For each vintage quarter \eqn{T}, historical data up to \eqn{T}
#' is combined with out-of-sample level forecasts (\eqn{T+1, \dots, T+h}) and filtered
#' via \code{mFilter::hpfilter} to extract pseudo-real-time cyclical gaps and mitigate
#' filter endpoint bias.
#'
#' @param Y_df A data frame containing historical realized observations.
#' @param fcst_df A matrix or data frame containing out-of-sample level forecasts,
#'   where columns represent vintage origins and rows (or first column) represent target dates.
#' @param val_col Character. Name of the column containing values in \code{Y_df}.
#' @param date_col Character. Column name identifying quarterly dates across data frames. Defaults to \code{"quarter"}.
#' @param freq Numeric. Smoothing parameter (\eqn{\lambda}) for the HP filter. Defaults to \code{1600}.
#' @param is_already_logged Logical. If \code{FALSE} (default), computes natural logarithms before HP filtering.
#' @param burn_in Character or \code{yearqtr}. Historical sample start date to mitigate starting-point filter bias.
#'   Defaults to \code{"1982 Q1"}.
#'
#' @return A named list containing:
#' \item{forecast_matrix}{A data frame formatted as a lower-triangular matrix with target dates as rows and vintage origins as columns, where values represent real-time HP gap forecasts (in percentage terms, \eqn{\times 100}).}
#' \item{full_sample_gap}{A data frame containing the full-sample ex-post HP gap benchmark series.}
#'
#' @export
#' @importFrom dplyr mutate filter select arrange distinct bind_rows rename all_of %>%
#' @importFrom rlang sym .data :=
#' @importFrom tibble tibble rownames_to_column
#' @importFrom zoo as.yearqtr
#' @importFrom mFilter hpfilter
#' @importFrom stats setNames
get_hp_gap_forecast_matrix <- function(Y_df,
                                       fcst_df,
                                       val_col,
                                       date_col = "quarter",
                                       freq = 1600,
                                       is_already_logged = FALSE,
                                       burn_in = "1982 Q1") {
  
  # Float-safe helper: rounds to nearest quarter to eliminate float drift
  # Sometimes the rounding leads to the dates being off
  clean_qtr <- function(dates) {
    zoo::as.yearqtr(round(as.numeric(zoo::as.yearqtr(dates)) * 4) / 4)
  }
  
  # Formats date into standard YYYYQX (e.g. "1990Q1")
  to_qtr_str <- function(dates) {
    format(clean_qtr(dates), "%YQ%q")
  }
  
  # Format everyting as needed
  burn_in_q <- clean_qtr(burn_in)
  
  Y_clean <- Y_df %>%
    mutate(quarter = clean_qtr(.data[[date_col]]),
           val     = as.numeric(.data[[val_col]])) %>%
    filter(!is.na(val)) %>%
    filter(quarter >= burn_in_q) %>%
    select(quarter, val) %>%
    arrange(quarter)
  
  # Check logging status
  y_vals_full <- if (is_already_logged) Y_clean$val else log(Y_clean$val)
  
  hp_full_trend <- as.numeric(mFilter::hpfilter(y_vals_full, freq = freq)$trend)
  
  Y_clean <- Y_clean %>%
    mutate(full_sample_hp_gap = (y_vals_full - hp_full_trend) * 100)
  
  full_gap_lookup <- setNames(Y_clean$full_sample_hp_gap, to_qtr_str(Y_clean$quarter))
  
  # formaz forecast matrix input
  # shouöd be standardized later
  if (is.data.frame(fcst_df) && date_col %in% colnames(fcst_df)) {
    target_dates_str <- fcst_df[[date_col]]
    mat_vals         <- as.matrix(fcst_df %>% select(-all_of(date_col)))
  } else if (is.matrix(fcst_df)) {
    target_dates_str <- rownames(fcst_df)
    mat_vals         <- fcst_df
  } else {
    target_dates_str <- fcst_df[[1]]
    mat_vals         <- as.matrix(fcst_df[, -1])
  }
  
  # Clean all dates through float-safe rounder
  target_dates  <- clean_qtr(target_dates_str)
  vintage_dates <- clean_qtr(colnames(mat_vals))
  
  gap_mat <- matrix(NA_real_, nrow = length(target_dates), ncol = length(vintage_dates))
  rownames(gap_mat) <- to_qtr_str(target_dates)
  colnames(gap_mat) <- to_qtr_str(vintage_dates)
  
  # Rolling Out-of-Sample HP Filter Loop
  for (j in seq_along(vintage_dates)) {
    vantage_q   <- vintage_dates[j]
    vantage_str <- to_qtr_str(vantage_q)
    
    # Safe numerical cutoff check (strictly <= T)
    hist_sub <- Y_clean %>%
      filter(as.numeric(quarter) <= as.numeric(vantage_q) + 1e-5)
    
    vantage_col_vals <- mat_vals[, j]
    names(vantage_col_vals) <- to_qtr_str(target_dates)
    
    # Safe numerical cutoff check (strictly > T, at least 1 quarter ahead)
    fcst_dates_future <- target_dates[as.numeric(target_dates) >= as.numeric(vantage_q) + 0.24]
    fcst_dates_str    <- to_qtr_str(fcst_dates_future)
    
    fcst_vals <- vantage_col_vals[fcst_dates_str]
    valid_fcst_mask <- !is.na(fcst_vals)
    
    if (nrow(hist_sub) == 0 && sum(valid_fcst_mask) == 0) next
    
    fcst_sub <- tibble(
      quarter = fcst_dates_future[valid_fcst_mask],
      val     = as.numeric(fcst_vals[valid_fcst_mask])
    )
    
    combined_series <- bind_rows(
      hist_sub %>% select(quarter, val),
      fcst_sub
    ) %>%
      arrange(quarter) %>%
      distinct(quarter, .keep_all = TRUE)
    
    if (nrow(combined_series) < 12) next
    
    # Apply log transformation only if series is not pre-logged
    comb_vals <- if (is_already_logged) combined_series$val else log(combined_series$val)
    
    hp_trend <- as.numeric(mFilter::hpfilter(comb_vals, freq = freq)$trend)
    combined_series$hp_gap <- (comb_vals - hp_trend) * 100
    
    gap_lookup <- setNames(combined_series$hp_gap, to_qtr_str(combined_series$quarter))
    
    # Populate forecast Matrix
    # Diagonal: Full-sample HP gap
    if (vantage_str %in% rownames(gap_mat) && vantage_str %in% names(full_gap_lookup)) {
      gap_mat[vantage_str, j] <- full_gap_lookup[[vantage_str]]
    }
    
    # Populate Forecast Horizon (T+1 .. T+h) by named key matching
    eval_target_strs <- to_qtr_str(fcst_dates_future)
    valid_eval_strs  <- eval_target_strs[eval_target_strs %in% names(gap_lookup) & 
                                           eval_target_strs %in% rownames(gap_mat)]
    
    if (length(valid_eval_strs) > 0) {
      gap_mat[valid_eval_strs, j] <- gap_lookup[valid_eval_strs]
    }
  }
  
  # Build Return Outputs
  forecast_matrix_df <- as.data.frame(gap_mat) %>%
    tibble::rownames_to_column(var = date_col)
  
  full_sample_gap_df <- Y_clean %>%
    mutate(quarter = to_qtr_str(quarter)) %>%
    select(quarter, full_sample_hp_gap) %>%
    rename(!!sym(date_col) := quarter) %>%
    as.data.frame()
  
  return(list(
    forecast_matrix = forecast_matrix_df,
    full_sample_gap = full_sample_gap_df
  ))
}




#' Extract Real-Time and Forecasted HP Output Gaps
#'
#' Extends historical log GDP data at a given forecast origin (\code{vantage_q}) with
#' out-of-sample GDP forecasts, applies the Hodrick-Prescott (HP) filter (\eqn{\lambda = 1600})
#' across the fused series to mitigate endpoint bias, and extracts the cyclical output gap.
#'
#' @param data A data frame containing historical observed GDP data.
#' @param gdp_forecast_data A data frame containing real-time GDP forecast vintages,
#'   where columns include \code{"date"} and vintage labels in \code{"YYYY Qq"} format.
#' @param vantage_q Character, Date, or \code{yearqtr}. The forecast origin/vintage quarter.
#' @param h Integer. Forecast horizon in quarters when returning forecast rows. Defaults to \code{8}.
#' @param gdp_col Character. Name of the log GDP column in \code{data}. Defaults to \code{"log_gdp"}.
#' @param date_col Character. Name of the date column in \code{data}. Defaults to \code{"quarter"}.
#' @param return_type Character string specifying the subset of the series to return:
#'   \code{"history"} (observations \eqn{\le} vantage point), \code{"forecast"} (the \eqn{h}-step
#'   ahead projection), or \code{"full"} (the complete spliced trajectory). Defaults to \code{"history"}.
#'
#' @return A tibble containing the date column, the log GDP series, and \code{gap} (the extracted HP cyclical component).
#'
#' @export
#' @importFrom dplyr mutate across all_of arrange filter select bind_rows %>%
#' @importFrom rlang .data
#' @importFrom tidyr drop_na
#' @importFrom zoo as.yearqtr
#' @importFrom mFilter hpfilter
#' @importFrom stats setNames
#' @importFrom utils head
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
  
  # Gather historical observed vector context
  obs_data <- data %>%
    mutate(across(all_of(date_col), zoo::as.yearqtr)) %>%
    arrange(.data[[date_col]]) %>%
    filter(.data[[date_col]] <= vantage_q) %>%
    select(all_of(c(date_col, gdp_col)))
  
  # Extract out-of-sample real-time forecast extension
  forecast_data_clean <- gdp_forecast_data[, c("date", vantage_str)] %>%
    stats::setNames(c("quarter", "log_gdp")) %>%
    mutate(quarter = zoo::as.yearqtr(quarter)) %>%
    drop_na()
  
  # Check if Discrepancy
  obs_val   <- obs_data$log_gdp[obs_data$quarter == vantage_q]
  fcast_val <- forecast_data_clean$log_gdp[forecast_data_clean$quarter == vantage_q]
  
  if (length(obs_val) && length(fcast_val) && abs(obs_val - fcast_val) > 1e-5) {
    warning(sprintf("[DISCREPANCY] At %s: Obs = %.5f, Fcast = %.5f", vantage_str, obs_val, fcast_val))
  }
  
  # Fuse on the overlapping row (Vantage point)
  extended_df <- bind_rows(
    obs_data, 
    filter(forecast_data_clean, quarter > vantage_q)
  )
  
  if (nrow(extended_df) < 15) {
    stop("Insufficient rows to compute stable Hodrick-Prescott trend extraction matrix.")
  }
  
  # Extract Trend via Hodrick-Prescott Filter
  # Lambda 1600 is standard for quarterly tracking frequencies
  hp_res <- mFilter::hpfilter(extended_df[[gdp_col]], freq = 1600, type = "lambda")
  extended_df$gap <- as.numeric(hp_res$cycle)
  
  # Output construction according to specs
  out_df <- switch(
    return_type,
    "history"  = filter(extended_df, quarter <= vantage_q),
    "forecast" = filter(extended_df, quarter > vantage_q) %>% head(h),
    "full"     = extended_df
  )
  
  return(out_df)
}


#' Generate Real-Time Rolling Matrix of GDP Forecasts
#'
#' Simulates recursive real-time GDP forecasting across a sequence of historical
#' quarterly origins (vintages). At each origin \eqn{T}, historical data is restricted
#' to \eqn{t \le T}, an ARIMA model is fitted, and multi-step level projections
#' are mapped into a forecast triangle matrix.
#'
#' @param fcast_start Character, Date, or \code{yearqtr}. Initial forecast origin date.
#' @param fcast_end Optional character, Date, or \code{yearqtr}. Terminal forecast origin date.
#'   If \code{NULL}, defaults to the maximum date available in \code{data}.
#' @param data A data frame containing the historical time series.
#' @param date_col Character. Column name identifying quarterly dates in \code{data}.
#' @param gdp_col Character. Column name containing GDP or log GDP values.
#' @param horizon Integer. Forecast horizon in quarters. Defaults to \code{8}.
#' @param hp_lambda Numeric. Smoothing parameter (\eqn{\lambda}) passed to downstream HP filter routines.
#'   Defaults to \code{1600}.
#'
#' @return A data frame formatted as a lower-triangular forecast matrix where the first column
#'   is \code{date} (target quarters) and subsequent columns contain projected values from each vintage origin.
#'
#' @seealso \code{\link{forecast_gdp}}
#'
#' @export
#' @importFrom dplyr filter arrange mutate relocate everything %>%
#' @importFrom rlang .data
#' @importFrom zoo as.yearqtr
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
  
  # Create a clean matrix structure filled with NAs
  # Matrix size: length(all_row_dates) rows by length(all_fcast_dates) columns
  mat <- matrix(NA_real_, 
                nrow = length(all_row_dates), 
                ncol = length(all_fcast_dates))
  
  # Name rows and columns temporarily with standard strings for easy tracking
  rownames(mat) <- format(all_row_dates, format = "%Y Q%q")
  colnames(mat) <- format(all_fcast_dates, format = "%Y Q%q")
  
  # Run the Rolling Forecast Loop
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
  
  # Convert to final Tibble layout and construct your strict downstream tracking column
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



#' Forecast GDP Growth using ARIMA with Pandemic Intervention Dummies
#'
#' Fits an automated ARIMA model on quarter-on-quarter log GDP growth rates and projects
#' multi-step log GDP levels forward. Automatically detects and includes pulse intervention
#' regressors for 2020 Q2 and 2020 Q3 when active in the historical sample window.
#'
#' @details
#' The model computes first differences of log GDP (\eqn{\Delta y_t = y_t - y_{t-1}}) and estimates
#' an optimal ARIMA(\eqn{p,d,q}) specification via \code{forecast::auto.arima}. Out-of-sample growth
#' forecasts (\eqn{\Delta \hat{y}_{T+h|T}}) are cumulated onto the terminal historical level:
#' \deqn{\hat{y}_{T+h|T} = y_T + \sum_{j=1}^h \Delta \hat{y}_{T+j|T}}
#'
#' Exogenous pulse dummies (\code{dummy_2020q2}, \code{dummy_2020q3}) are integrated dynamically
#' via \code{xreg} and set to zero across the forecast horizon, with edge-case checks to prevent
#' collinearity on rolling sample boundaries.
#'
#' @param data A data frame containing historical quarterly dates and log GDP series.
#' @param date_col Character. Column name identifying quarterly dates in \code{data}.
#' @param gdp_col Character. Column name containing log GDP values.
#' @param horizon Integer. Out-of-sample forecast horizon in quarters. Defaults to \code{8}.
#' @param hp_lambda Numeric. Smoothing parameter for downstream HP filter routines (retained for interface compatibility). Defaults to \code{1600}.
#'
#' @return A tibble containing:
#' \item{quarter}{Quarterly dates of class \code{yearqtr} spanning historical and forecast periods.}
#' \item{log_gdp}{Combined historical and projected log GDP levels.}
#' \item{type}{Character indicator distinguishing \code{"History"} from \code{"Forecast"}.}
#'
#' @seealso \code{\link{gdp_forecast_wrapper}}
#'
#' @export
#' @importFrom dplyr mutate arrange filter pull select if_else lag %>%
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom zoo as.yearqtr
#' @importFrom forecast auto.arima forecast
#' @importFrom utils tail
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
  
  # Fit ARIMA on growth rates & generate out-of-sample path
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
  
  # Create the future timeline
  last_hist_date <- max(prep_df$date)
  last_hist_gdp  <- tail(prep_df$log_gdp, 1)
  future_dates   <- seq(last_hist_date + 1/4, length.out = horizon, by = 1/4)
  
  # Accumulate future log levels: y_{t+h} = y_{t+h-1} + \Delta y_{t+h}
  future_log_gdp <- cumsum(future_growth_rates) + last_hist_gdp 
  
  # Bind history and future log levels into one continuous vector
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



#' Forecast Year-on-Year Inflation using ARIMA with Pandemic Intervention Dummies
#'
#' Estimates an automated ARIMA model on first-differenced quarterly inflation rates,
#' projects future trajectory paths, and reconstructs annualized Year-on-Year (YoY)
#' headline inflation rates via a 4-quarter rolling trailing average. Automatically integrates
#' pulse intervention regressors for 2020 Q2 and 2020 Q3 when active in the historical sample.
#'
#' @details
#' The model computes quarter-on-quarter inflation differences (\eqn{\Delta \pi_t = \pi_t - \pi_{t-1}})
#' and selects an optimal ARIMA specification via \code{forecast::auto.arima}. Out-of-sample
#' differenced predictions are cumulated onto the terminal historical value:
#' \deqn{\hat{\pi}_{T+h|T} = \pi_T + \sum_{j=1}^h \Delta \hat{\pi}_{T+j|T}}
#'
#' The complete historical and projected quarterly path is then mapped to YoY inflation rates
#' using a trailing 4-quarter window:
#' \deqn{\pi^{\text{YoY}}_t = \frac{1}{4} \sum_{k=0}^3 \pi_{t-k}}
#'
#' @param data A data frame containing quarterly dates and inflation series.
#' @param date_col Character. Column name identifying quarterly dates in \code{data}.
#' @param inf_col Character. Column name containing quarterly inflation values.
#' @param horizon Integer. Out-of-sample forecast horizon in quarters. Defaults to \code{8}.
#'
#' @return A tibble containing:
#' \item{quarter}{Quarterly dates of class \code{yearqtr} spanning historical and forecast horizons.}
#' \item{yoy_inflation}{Historical and projected 4-quarter trailing average inflation rates.}
#' \item{type}{Character indicator distinguishing \code{"History"} from \code{"Forecast"}.}
#'
#' @export
#' @importFrom dplyr mutate arrange filter pull select if_else lag %>%
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom zoo as.yearqtr
#' @importFrom forecast auto.arima forecast
#' @importFrom utils tail
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



#' Generate Real-Time Rolling Matrix of Inflation Forecasts
#'
#' Simulates recursive real-time inflation forecasting across a sequence of historical
#' quarterly origins (vintages). At each origin \eqn{T}, historical data is restricted
#' to \eqn{t \le T}, an ARIMA model is fitted to inflation momentum, and reconstructed
#' Year-on-Year (YoY) paths are mapped into a forecast triangle matrix.
#'
#' @param fcast_start Character, Date, or \code{yearqtr}. Initial forecast origin date.
#' @param fcast_end Optional character, Date, or \code{yearqtr}. Terminal forecast origin date.
#'   If \code{NULL}, defaults to the maximum date available in \code{data}.
#' @param data A data frame or tibble containing the full historical time series.
#' @param date_col Character. Column name identifying quarterly dates in \code{data}.
#' @param inf_col Character. Column name containing quarterly inflation values.
#' @param horizon Integer. Forecast horizon in quarters. Defaults to \code{8}.
#'
#' @return A data frame formatted as a lower-triangular forecast matrix where the first column
#'   is \code{date} (target quarters in \code{"YYYYQq"} format) and subsequent columns contain
#'   projected YoY inflation rates from each vintage origin.
#'
#' @seealso \code{\link{forecast_inflation}}
#'
#' @export
#' @importFrom dplyr filter arrange mutate relocate everything %>%
#' @importFrom rlang .data
#' @importFrom zoo as.yearqtr
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



prepare_inflation_forecast_taylor <- function(df, phillips_params_df = phillips_parmas_df) {
  # Format quarter to "YYYYQq" (e.g., "1999Q1") using zoo::yearqtr
  phillips_params_df$quarter <- format(zoo::as.yearqtr(phillips_params_df$quarter), "%YQ%q")
  
  # Subtract each vintage's natural_rate directly from its matching column
  for (i in seq_len(nrow(phillips_params_df))) {
    col_name <- phillips_params_df$quarter[i]
    nat_rate <- phillips_params_df$natural_rate[i]
    
    if (col_name %in% names(df)) {
      df[[col_name]] <- df[[col_name]] - nat_rate
    }
  }
  
  return(df)
}

#' Build Data Matrices for the Phillips Curve State-Space Model
#'
#' Prepares observation and exogenous regressor series for the Open-Economy
#' Phillips curve state-space model at a given forecast origin. Computes annualized
#' log inflation differences, extracts real-time Law of One Price (LOP) gaps and
#' HP output gaps, and aligns the estimation sample to the target start date.
#'
#' @param T_0 Character, Date, or \code{yearqtr}. Start date of the estimation window.
#'   Defaults to \code{"1982-01-01"}.
#' @param vantage_quarter Character, Date, or \code{yearqtr}. Real-time forecast origin/vintage date.
#' @param data A data frame containing raw quarterly time series (\code{cpi}, \code{log_gdp},
#'   \code{5y_cpi_forecast}, \code{reer_eu_ppi}). Defaults to \code{master_philips}.
#' @param gdp_forecasts A data frame containing real-time GDP forecast vintages. Defaults to \code{gdp_forecasts_arima}.
#' @param h Integer. Forecast horizon in quarters. Defaults to \code{h}.
#' @param set_silent Logical. If \code{TRUE}, suppresses diagnostic logging and printouts. Defaults to \code{TRUE}.
#' @param quarterly Logical. If \code{TRUE}, computes annualized quarter-on-quarter inflation differences (\code{inf_lag = 1});
#'   if \code{FALSE}, computes Year-on-Year differences (\code{inf_lag = 4}). Defaults to \code{TRUE}.
#'
#' @return A data frame spanning \code{T_0} to \code{vantage_quarter} containing:
#' \item{quarter}{Quarterly dates of class \code{yearqtr}.}
#' \item{log_inflation_diff, lag_log_inflation_diff}{Annualized inflation rate and its first lag.}
#' \item{5y_cpi_forecast}{5-year CPI survey forecast expectations.}
#' \item{lop_gap, lag_lop_gap}{Current and lagged cyclical Law of One Price gaps (\eqn{\times 100}).}
#' \item{gdp_gap, lag_gdp_gap}{Current and lagged cyclical HP output gaps (\eqn{\times 100}).}
#'
#' @seealso \code{\link{splice_snb_series}}, \code{\link{get_hp_gap}}, \code{\link{rolling_est_philips_ssm}}
#'
#' @export
#' @importFrom dplyr arrange mutate filter select left_join lag %>%
#' @importFrom zoo as.yearqtr
#' @importFrom utils head tail
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
    select(quarter, log_inflation_diff, lag_log_inflation_diff, log_gdp, `5y_cpi_forecast`, reer_eu_ppi) %>%
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




get_phillips_inflation_gap <- function(vantage_date = "2020Q2",
                                       phillips_folder = TARGET_FOLDER_PHILLIPS,
                                       inflation_df = master_phillips,
                                       inf_col = "log_inflation_diff",
                                       date_col = "quarter") {
  
  # Parse input (e.g. "2020Q2") safely
  vantage_qtr <- zoo::as.yearqtr(vantage_date)
  
  # Format target file suffix (e.g. "2020_Q2") and output dates (e.g. "2020Q2")
  file_suffix <- format(vantage_qtr, "%Y_Q%q")
  to_qtr_str  <- function(dates) format(zoo::as.yearqtr(dates), "%YQ%q")
  
  # ----------------------------------------------------------------------------
  # 1. Locate and Load the Target Vantage RDS File
  # ----------------------------------------------------------------------------
  all_files    <- list.files(here("output", "para", phillips_folder), pattern = "\\.rds$", full.names = TRUE)
  pattern      <- paste0(file_suffix, "\\.rds$")
  matched_file <- grep(pattern, all_files, value = TRUE)
  
  if (length(matched_file) == 0) {
    stop(sprintf("No RDS file ending in '%s.rds' found in folder: %s", file_suffix, phillips_folder))
  }
  
  vantage_obj <- readRDS(matched_file[1])
  
  # ----------------------------------------------------------------------------
  # 2. Extract Natural Rate Vector (First Column of states$r)
  # ----------------------------------------------------------------------------
  r_matrix <- vantage_obj[["states"]][["r"]]
  natural_rate_vals <- as.numeric(if (is.matrix(r_matrix) || is.data.frame(r_matrix)) r_matrix[, 1] else r_matrix)
  
  # ----------------------------------------------------------------------------
  # 3. Clean Historical Inflation Data up to Vantage Date
  # ----------------------------------------------------------------------------
  input_date_var <- if (date_col %in% colnames(inflation_df)) date_col else "date"
  
  actual_inf <- inflation_df %>%
    mutate(quarter_obj = zoo::as.yearqtr(.data[[input_date_var]]),
           date        = to_qtr_str(quarter_obj),
           inf_actual  = as.numeric(.data[[inf_col]])) %>%
    filter(!is.na(inf_actual)) %>%
    filter(quarter_obj <= vantage_qtr) %>%
    arrange(quarter_obj)
  
  # ----------------------------------------------------------------------------
  # 4. Tail-Align: Match Newest Values Backwards
  # ----------------------------------------------------------------------------
  n_match <- min(nrow(actual_inf), length(natural_rate_vals))
  
  actual_inf_sub <- tail(actual_inf, n_match)
  natural_rate_sub <- tail(natural_rate_vals, n_match)
  
  # ----------------------------------------------------------------------------
  # 5. Compute Inflation Gap
  # ----------------------------------------------------------------------------
  gap_df <- data.frame(
    quarter    = as.yearqtr(actual_inf_sub$date),
    inf_gap = actual_inf_sub$inf_actual - natural_rate_sub
  )
  
  return(gap_df)
}







#' Build Data Matrices for the Taylor Rule State-Space Model
#'
#' Constructs and aligns the observation and exogenous regressor time series
#' required for estimating the Taylor rule state-space model at a given forecast origin.
#' Supports configurable Taylor rule specifications (backward-looking vs. forward-looking)
#' and flexible inflation gap definitions.
#'
#' @param T_0 Character, Date, or \code{yearqtr}. Start date of the estimation window.
#'   Defaults to \code{"1981-01-01"}.
#' @param vantage_quarter Character, Date, or \code{yearqtr}. The real-time forecast origin/vintage date.
#' @param data A data frame containing quarterly series (\code{quarter}, \code{log_cpi},
#'   \code{saron_libor_splice}, \code{forward_rate}, \code{yoy_inf}, \code{inf_gap}). Defaults to \code{master_taylor}.
#' @param gdp_forecasts A data frame containing real-time GDP forecast vintages. Defaults to \code{gdp_forecasts_arima}.
#' @param h Integer. Forecast horizon in quarters. Defaults to \code{h}.
#' @param set_silent Logical. If \code{TRUE}, suppresses informational console messages. Defaults to \code{TRUE}.
#' @param quarterly Logical. Frequency flag for quarterly tracking. Defaults to \code{TRUE}.
#' @param taylor_rule_spec Character string specifying the policy rule formulation:
#'   \code{"backward"} (realized/lagged macro variables) or \code{"forward"} (forecast/survey expectations). Defaults to \code{"backward"}.
#' @param inf_gap_spec Character string defining the inflation gap construction:
#'   \code{"minus_1"} (deviations from a 1\% target benchmark) or \code{"ssm_gap"} (deviations from latent trend estimates). Defaults to \code{"minus_1"}.
#'
#' @return A data frame aligned over \code{[T_0, vantage_quarter]} containing:
#' \item{quarter}{Quarterly dates of class \code{yearqtr}.}
#' \item{saron_libor_splice}{Policy interest rate splice series.}
#' \item{forward_rate}{Long-term forward rate expectations proxy.}
#' \item{yoy_inf}{Year-on-Year realized inflation rate.}
#' \item{gdp_gap}{Real-time cyclical HP output gap (\eqn{\times 100}).}
#' \item{inf_gap}{Configured inflation gap series.}
#'
#' @seealso \code{\link{get_hp_gap}}, \code{\link{rolling_est_taylor_ssm}}, \code{\link{initialize_taylor_ssm}}
#'
#' @export
#' @importFrom dplyr select left_join filter arrange mutate all_of %>%
#' @importFrom zoo as.yearqtr
#' @importFrom stats complete.cases
build_data_matrix_taylor <- function(T_0 = "1981-01-01",
                                    vantage_quarter,
                                    data = master_taylor,
                                    gdp_forecasts = gdp_forecasts_arima,
                                    h = h,
                                    set_silent = TRUE,
                                    quarterly = TRUE,
                                    taylor_rule_spec = "backward",
                                    inf_gap_spec = "minus_1") {
  
  # Make sure it's yearquarter object
  T_0 <- as.yearqtr(T_0)
  
  if(taylor_rule_spec == "backward") {
    
    if(inf_gap_spec == "minus_1") {
      
      # Slice Data available "Today"
      data_t <- data[data[["quarter"]] <= vantage_quarter, ]
      
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
      
    } else if(inf_gap_spec == "ssm_gap") {
      
      # Slice Data available "Today"
      data_t <- data[data[["quarter"]] <= vantage_quarter, ]
      
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
      
      inf_gap_data <- get_phillips_inflation_gap(vantage_date = vantage_quarter)
      
      
      processed_data <- data_t %>%
        select(quarter, saron_libor_splice, forward_rate, yoy_inf) %>%
        left_join(gdp_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
        left_join(inf_gap_data %>% select(quarter, inf_gap), by = "quarter") %>%
        filter(quarter >= as.yearqtr(val_T1)) %>%
        arrange(quarter) %>%
        filter(complete.cases(gdp_gap, inf_gap))%>%
        # Filter to return the specific estimation sample
        filter(quarter >= as.yearqtr(T_0))
      
    } else {
      message("NO VALID INF GAP SPECIFICATION GIVEN. ABORTING...")
      stop("STOPPING EXECUTION AT build_data_matrix_taylor()")
    }
    
    
    
  } else if(taylor_rule_spec == "forward") {
    
    if(inf_gap_spec == "minus_1") {
      
      # use SPF CY or NY means minus 1
      
      # USE SPF forecasts minus 1
      
    } else if(inf_gap_spec == "ssm_gap") {
      
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

#' Extract Historical Inflation Gaps Across State-Space Estimation Vintages
#'
#' Reads serialized State-Space Model (\code{.rds}) vintage outputs from a target directory,
#' calculates the implied cyclical inflation gap (\eqn{\hat{\pi}_t - \hat{\tau}_t}) for each
#' estimation window, and aggregates them into a vintage-by-date matrix.
#'
#' @details
#' For each saved vintage result, the function computes the implied real-time inflation gap
#' as the difference between the fitted measurement series (\code{fitted.obs[, 1]}) and the
#' filtered latent trend state (\code{r[, 1]}):
#' \deqn{\text{gap}_t = \hat{y}_{t|t} - \hat{\rho}_{t|t}}
#' The resulting vectors are mapped into a matrix where rows correspond to forecast vintage
#' origins (\code{vantage_quarter}) and columns represent historical quarterly calendar dates.
#'
#' @param ssm_folder Character. Subdirectory name inside \code{output/para/} containing
#'   the vintage \code{.rds} result files.
#'
#' @return A data frame formatted as a matrix where:
#' \item{rownames}{Estimation vintage origins (\code{"YYYY Qq"}).}
#' \item{colnames}{Historical calendar quarters (\code{"YYYY Qq"}) starting from \code{1990 Q1}.}
#' \item{values}{Estimated cyclical inflation gap values (\code{NA} for out-of-sample periods).}
#' Returns an empty \eqn{0 \times 0} matrix if no \code{.rds} files are found.
#'
#' @seealso \code{\link{rolling_est_philips_ssm}}, \code{\link{build_data_matrix_taylor}}
#'
#' @export
#' @importFrom here here
#' @importFrom zoo as.yearqtr
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

#' Build the INf gap using the structural rate of inflation
#' 
#' NOT YET IMPLEMENTED
#' @export
get_ssm_forecasted_inf_gap <- function(inf_forecasts_df) {
  
  # select
  
}
