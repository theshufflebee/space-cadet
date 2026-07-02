################################################################################
#
# These functions are used to create forecast
#
################################################################################


#' Function to predict the random walk ssm with exogenous betas
#' 
#' @param who_start the last state observation from the estimation
#' @param betas the estimated beta coefficients as dataframe or matrix
#' @param exog_features exogenous features corresponding to betas as matrix or dataframe
#' 
#' This function takes the last state variable and adds the exogenous component
#' built from using a matrix multiplication of exogenous features and betas and
#' adding the state.
#' 
#' gdp features is a data frame where each row is a time in h and each column is a lag
#' 
predict_ssm_path_rw <- function(rho_start, betas, exog_features) {
  cyclical_impact <- as.matrix(exog_features) %*% matrix(betas)
  return(as.numeric(rho_start + cyclical_impact))
}


#' Pseudo Out-of-Sample Forecasting for Okun State-Space Model
#'
#' @description
#' This function performs a rolling pseudo out-of-sample forecast for the unemployment rate.
#' It iterates through a series of "vantage points" (dates), recalculates the cyclical 
#' GDP gap using available history and external GDP forecasts, and applies estimated 
#' SSM parameters to generate a forecast path.
#'
#' @param params_df A dataframe containing estimated parameters for each vintage. 
#' Must include \code{natural_rate}, \code{beta1}, \code{beta2}, and \code{beta3}.
#' @param date_col Character. The name of the date column (default "quarter").
#' @param exog_var_col Character. The column name for the exogenous GDP data (default "log_gdp").
#' @param forecast_h Integer. The forecast horizon in quarters (default 8).
#' @param Y_data_df Dataframe containing observed dependent variable data (\code{unemp_rate}).
#' @param target_variable Character. The variable to be forecast (default "unemp_rate").
#' @param X_data Dataframe containing historical exogenous data (log GDP).
#' @param gdp_gap_forecasts_input Dataframe containing external future GDP forecasts 
#' used to extend the HP filter and drive the Okun cyclical component.
#'
#' @details
#' \bold{The Pseudo-Real-Time Approach:}
#' To mimic actual forecasting conditions, the function:
#' \enumerate{
#'   \item \bold{Data Fusing:} For each vintage, it combines historical GDP with 
#'   external forecasts to create a "complete" series known at that vantage point.
#'   \item \bold{Recalibrated Filtering:} It runs the HP filter (\eqn{\lambda = 1600}) 
#'   on this fused series to derive the GDP gap. This captures how real-time estimates 
#'   of the cycle change as new data arrives.
#'   \item \bold{Okun Projection:} The cyclical impact is calculated as 
#'   \eqn{X_t \beta} and added to the latent structural trend (\eqn{\rho_T}) 
#'   following a random walk assumption for the forecast horizon.
#' }
#'
#' @return A dataframe (Evaluation Matrix) where:
#' \itemize{
#'   \item \bold{Columns:} Represent the "Vantage Point" (the date the forecast was made).
#'   \item \bold{Rows:} Represent the "Target Date" (the date being forecast).
#'   \item \bold{Diagonal:} Contains the actual observed values known at that time.
#'   \item \bold{Below Diagonal:} Contains the \eqn{h}-step ahead forecasts.
#' }
#'
#' @seealso [predict_ssm_path_rw()]
#' @export
forecast_okun_ssm <- function(params_df,                  
                              date_col = "quarter",
                              exog_var_col = "log_gdp",
                              forecast_h = 8,
                              Y_data_df = Y_okun,
                              target_variable = "unemp_rate",
                              X_data = X_okun,
                              gdp_gap_forecasts_input = gdp_gap_forecasts,
                              use_true_gdp = FALSE) {
  
  # Clean prepare params_df
  params <- params_df %>%
    mutate(quarter = as.yearqtr(.data[[date_col]]))
  
  dates <- params %>% pull(.data[[date_col]])
  
  # Setup Eval Matrix
  extended_rows <- seq(min(dates), by = 0.25, length.out = length(dates) + forecast_h)
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(extended_rows)
  colnames(eval_mat) <- as.character(dates)
  
  # Align Actuals -> true obs in diag
  actual_unemp_df <- Y_data_df %>%
    filter(quarter %in% dates) %>%
    arrange(quarter)
  
  for(i in seq_along(dates)) {
    date_str <- as.character(dates[i])
    eval_mat[date_str, i] <- actual_unemp_df[[target_variable]][i]
  }
  
  # ============================================================================
  # MAIN FORECAST LOOP (Restored)
  # ============================================================================
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    
    # Select params and components for this specific vintage (Restored)
    current_params <- params[i, ]
    rho_T  <- current_params$natural_rate 
    betas  <- c(current_params$beta1, current_params$beta2, current_params$beta3)
    phi    <- current_params$phi
    
    # Isolate history baseline up to this point (Restored)
    history_gdp <- X_data %>%
      mutate(quarter = zoo::as.yearqtr(.data[[date_col]])) %>%
      filter(quarter <= forecast_origin) %>% 
      select(quarter, gdp_val = all_of(exog_var_col))
    

    # Convert actual date index to string
    vintage_col_str <- format(forecast_origin, format = "%Y Q%q")
    
    # Extract raw triangle matrix path for this origin column
    raw_forecasts <- gdp_gap_forecasts_input %>%
      select(date = date, gdp_val = all_of(vintage_col_str)) %>%
      mutate(quarter = zoo::as.yearqtr(date, format = "%Y Q%q")) %>%
      filter(!is.na(gdp_val)) %>%
      arrange(quarter)
    
    # Track baseline from history dataframe via correct column mapping
    hist_baseline_val <- history_gdp %>%
      filter(quarter == forecast_origin) %>%
      pull(gdp_val)
    
    matrix_origin_val <- raw_forecasts %>%
      filter(quarter == forecast_origin) %>%
      pull(gdp_val)
    
    # Verification Step
    if (length(hist_baseline_val) > 0 && length(matrix_origin_val) > 0) {
      if (abs(matrix_origin_val - hist_baseline_val) > 1e-6) {
        warning(sprintf("[VINTAGE MISMATCH] Baseline drift at %s. History: %f, Matrix Diagonal: %f", 
                        format(forecast_origin, "%Y Q%q"), hist_baseline_val, matrix_origin_val))
      }
    }
    
    # Pulled from correct parent table and removed diagonal baseline entry
    future_gdp <- raw_forecasts %>%
      filter(quarter > forecast_origin) %>%
      slice(1:forecast_h) %>%
      select(quarter, gdp_val)
    
    # Combine history and chosen future forecast data track
    combined_gdp <- bind_rows(history_gdp, future_gdp) %>%
      arrange(quarter)
    
    # Re-calculate HP Cycle (The Pseudo-Real-Time approach) including forecasts
    hp_res <- mFilter::hpfilter(combined_gdp$gdp_val, freq = 1600)
    combined_gdp$y_gap <- as.numeric(hp_res$cycle) * 100
    
    # Create Lags and extract only the forecast horizon
    gdp_features_wide <- combined_gdp %>%
      mutate(
        gdp_gap  = y_gap,
        gap_lag1 = dplyr::lag(y_gap, 1),
        gap_lag2 = dplyr::lag(y_gap, 2)
      ) %>%
      filter(quarter > forecast_origin) %>%
      arrange(quarter) %>%
      slice(1:forecast_h) %>%
      select(gdp_gap, gap_lag1, gap_lag2)
    
    # Generate paths / forecasts and store
    if (nrow(gdp_features_wide) > 0) {
      h_available <- nrow(gdp_features_wide)
      path <- numeric(h_available)
      
      actual_unemp_T <- actual_unemp_df[[target_variable]][i]
      current_u_tilde <- as.numeric(actual_unemp_T - rho_T)
      
      for (h in 1:h_available) {
        
        # Extract contemporaneous and lagged output gaps for step h
        y_t   <- gdp_features_wide$gdp_gap[h]
        y_lm1 <- gdp_features_wide$gap_lag1[h]
        y_lm2 <- gdp_features_wide$gap_lag2[h]
        
        # Evaluate modified Okun cycle step: u_tilde_t = phi*u_tilde_{t-1} + beta(L)y_t
        okun_fundamental <- (betas[1] * y_t) + (betas[2] * y_lm1) + (betas[3] * y_lm2)
        new_u_tilde <- (phi * current_u_tilde) + okun_fundamental
        
        # Combine back with the random-walk natural rate (E_T[u_bar_{T+h}] = u_bar_T)
        path[h] <- rho_T + new_u_tilde
        
        # Update the rolling persistence lag for the next step (h + 1)
        current_u_tilde <- new_u_tilde
      }
      
      # Store the path in the evaluation matrix
      look_ahead_dates <- seq(forecast_origin + 0.25, by = 0.25, length.out = h_available)
      target_rows <- as.character(look_ahead_dates)
      eval_mat[target_rows, i] <- path
    }
  }
  
  return(as.data.frame(eval_mat))
}




################################################################################



forecast_philips_ssm <- function(params_df,
                                 date_col = "quarter",
                                 master_df,
                                 forecast_h = 8,
                                 exogenous_gdp_forecast_data,
                                 use_true_data = FALSE) {
  
  message("STARTING PHILLIPS FORECASTS")
  
  # Ensure dates are uniform yearqtr objects
  parameters <- params_df %>%
    mutate(quarter = zoo::as.yearqtr(.data[[date_col]]))
  
  dates <- parameters %>% pull(quarter)
  
  # Setup sequence of extended rows spanning history + forecast horizon
  extended_rows <- seq(min(dates), by = 0.25, length.out = length(dates) + forecast_h)
  
  # Initialize the triangular evaluation matrix
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(zoo::as.yearqtr(extended_rows))
  colnames(eval_mat) <- as.character(zoo::as.yearqtr(dates))
  
  # Populate diagonal matrix with observed inflation data from history
  
  for(i in seq_along(dates)) {
    vantage_str <- as.character(dates[i])
    actual_val <- master_df %>% 
      filter(quarter == dates[i]) %>% 
      pull(log_inflation_diff) 
    
    if(length(actual_val) > 0) eval_mat[vantage_str, i] <- actual_val
  }
  
  # Main Forecast Loop across all rolling vintages
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    current_params <- parameters[i, ]
    
    actual_gdp_val <- master_df %>% 
      filter(quarter == dates[i]) %>% 
      pull(log_gdp) 
    
    # Map econometric parameters
    pi_bar  <- current_params$natural_rate 
    beta_y  <- current_params$beta_y
    psi_lop <- current_params$psi_lop
    phi     <- current_params$phi
    fitted_inf <- current_params$fitted_obs
    
    # Starting historical anchor for the AR loop
    y_t_t <- master_df %>% 
      filter(quarter == forecast_origin) %>% 
      pull(log_inflation_diff)
    
    if(use_true_data) {
      message("TRUE DATA USE DEPRECIATED")
    }else{
      
      vintage_col_str <- format(forecast_origin, format = "%Y Q%q")
      
      future_gdp <- exogenous_gdp_forecast_data %>%
        select(quarter = date, log_gdp = all_of(vintage_col_str)) %>%
        mutate(quarter = zoo::as.yearqtr(quarter, format = "%Y Q%q")) %>%
        # Filter out the empty triangle blocks (NAs)
        filter(!is.na(log_gdp)) %>%
        arrange(quarter)
      
      matrix_baseline_val <- future_gdp$log_gdp[1]
      # check for possible mismatch coding error
      if (length(actual_gdp_val) > 0 && length(matrix_baseline_val) > 0) {
        if (abs(matrix_baseline_val - actual_gdp_val) > 1e-6) {
          warning(sprintf("[BASELINE OBS GDP MISMATCH] at origin %s. History: %f, Matrix Diagonal: %f", 
                          vintage_col_str, actual_gdp_val, matrix_baseline_val))
        }
      }
      
      future_gdp <- future_gdp %>%
        filter(quarter > forecast_origin) %>%
        slice(1:forecast_h)
    }
    
    # Extract GDP gap forecasts (returns raw decimal 'gap')
    gdp_forecasts <- get_hp_gap(vantage_q = forecast_origin,
                                h = forecast_h,
                                data = master_df,
                                gdp_forecast_data = exogenous_gdp_forecast_data,
                                return_type = "forecast")
    
    
    # Extract sSpliced LOP series (returns raw decimal 'lop_gap')
    lop_forecast <- splice_snb_series(vantage_quarter = forecast_origin,
                                      snb_reer_delay = SNB_REER_DELAY,
                                      data = master_df,
                                      burn_in = model_philips_burn_in)
    
    # Join the dataframes
    X_future <- gdp_forecasts %>%
      rename(gdp_gap = gap) %>%
      left_join(lop_forecast %>% select(quarter, lop_gap), by = "quarter") %>%
      filter(quarter > forecast_origin) %>%
      # Multiply gaps by 100 to perfectly mirror the estimation matrix scaling!
      mutate(
        gdp_gap = as.numeric(gdp_gap) * 100,
        lop_gap = as.numeric(lop_gap) * 100
      )
    
    h_available <- nrow(X_future)
    
    if (h_available > 0 && length(y_t_t) > 0) {
      current_inflation <- y_t_t 
      
      for (h in 1:h_available) {
        fdate <- forecast_origin + h/4
        fdate_str <- as.character(fdate)
        
        exog_now <- X_future %>% filter(quarter == fdate)
        
        lop_gap_h <- exog_now$lop_gap 
        gdp_gap_h <- exog_now$gdp_gap
        
        # New Keynesian Hybrid Phillips Curve Projection Math
        cyclical_inflation <- (psi_lop * lop_gap_h) + (beta_y * gdp_gap_h)
        
        inf_deviation <- current_inflation - pi_bar # uses the fitted inf to get the true cyclical deviation
        
        pred_inflation     <- pi_bar + phi * inf_deviation + cyclical_inflation
        

        # Store forecast
        eval_mat[fdate_str, i] <- pred_inflation
        
        # 3. FIXED: Crucial recursive step update for the autoregressive layer
        current_inflation <- pred_inflation 
      }
    }
  }
  message("ENDED PHILIPS FORECASTS")
  return(eval_mat)
}


#############################################################################

forecast_taylor_ssm <- function(params_df,
                                date_col = "quarter",
                                master_df,
                                forecast_h = 8,
                                exogenous_gdp_forecast_data,
                                exogenous_inf_forecast_data = fcst_df_inf,
                                hp_inf_gap = FALSE,
                                inf_target = 1,
                                zlb_0_start = "2009 Q2",
                                zlb_075_start = "2015 Q1",
                                zlb_end = "2022 Q2",
                                use_true_data = FALSE) {
  
  zlb_0_start <- zoo::as.yearqtr(zlb_0_start)
  zlb_075_start <- zoo::as.yearqtr(zlb_075_start)
  zlb_end   <- zoo::as.yearqtr(zlb_end)
  
  parameters <- params_df %>%
    mutate(quarter = zoo::as.yearqtr(!!sym(date_col)))
  
  param_dates <- parameters %>% pull(quarter)
  
  inf_matrix_dates <- zoo::as.yearqtr(exogenous_inf_forecast_data[[1]])
  
  fcast_start <- max(min(param_dates), min(inf_matrix_dates), na.rm = TRUE)
  
  fcast_end   <- min(max(param_dates), max(inf_matrix_dates), na.rm = TRUE)
  
  message(sprintf("Data alignment successful. Filtering loop window: %s to %s", 
                  as.character(fcast_start), as.character(fcast_end)))
  
  dates <- param_dates[param_dates >= fcast_start & param_dates <= fcast_end]
  
  # build full pseudo out of sample fcst dates vector
  min_global_date <- min(dates)
  max_global_date <- max(dates) + (forecast_h / 4)
  extended_rows   <- seq(min_global_date, max_global_date, by = 1/4)
  
  # Setup size -> + h rows so we can forecast into the future
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  
  # Setup Names
  rownames(eval_mat) <- as.character(zoo::as.yearqtr(extended_rows))
  colnames(eval_mat) <- as.character(zoo::as.yearqtr(dates))
  
  
  for(i in seq_along(dates)) {
    vantage_str <- as.character(dates[i])
    # Using the observed Policy Rate (SARON/LIBOR) as the starting point

    actual_val <- master_df %>% 
      filter(quarter == dates[i]) %>% 
      pull(true_snb_rate) 
    
    if(length(actual_val) > 0) eval_mat[vantage_str, i] <- actual_val
  }
  
  # Main Forecast Loop
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    
    # Parameters for this specific vintage (Economic Space)
    current_params <- parameters[i, ]
    

    i_bar <- current_params$natural_rate 
    g_pi   <- current_params$gamma_pi
    g_y    <- current_params$gamma_y
    phi    <- current_params$phi
    

    # Checks if the vintage origin falls inside ANY of the ZLB regimes combined
    if (forecast_origin >= zlb_0_start && forecast_origin <= zlb_end) {
      # Inside ZLB era: Use the latent filter state estimate to anchor smoothing
      y_t_t <- current_params$fitted_obs
    } else {
      # Outside ZLB era: Use the observed policy rate (SARON / LIBOR)
      y_t_t <- master_df %>% 
        filter(quarter == forecast_origin) %>% 
        pull(true_snb_rate)
    }
    vintage_col_str <- format(forecast_origin, format = "%Y Q%q")
    

    }
    
    gdp_forecasts <- get_hp_gap(vantage_q = forecast_origin,
                                h = forecast_h,
                                data = master_df,
                                gdp_forecast_data = exogenous_gdp_forecast_data,
                                return_type = "forecast")
    
    gdp_forecasts <- gdp_forecasts %>%
      mutate(quarter = as.yearqtr(quarter))
    
    

    
    vintage_col_str <- format(forecast_origin, format = "%Y Q%q")
    
    future_inf <- exogenous_inf_forecast_data %>%
      select(quarter = date, inf = all_of(vintage_col_str)) %>%
      mutate(quarter = zoo::as.yearqtr(quarter, format = "%Y Q%q"),
             inf = inf - 1) %>% #inflation gap so -1
      # Filter out the empty triangle blocks (NAs)
      filter(!is.na(inf)) %>%
      arrange(quarter)
    
    actual_inf_val <- master_df %>% 
      filter(quarter == forecast_origin) %>% 
      pull(inf_gap)
    
    # for debugguing
    # print(future_inf)
    
    matrix_baseline_val <- future_inf$inf_gap[1]
    # check for possible mismatch coding error
    if (length(actual_inf_val) > 0 && length(matrix_baseline_val) > 0) {
      if (abs(matrix_baseline_val - actual_inf_val) > 1e-6) {
        warning(sprintf("[BASELINE OBS INF GAP MISMATCH] at origin %s. History: %f, Matrix Diagonal: %f", 
                        vintage_col_str, actual_inf_val, matrix_baseline_val))
      }
    }
    
    future_inf <- future_inf %>%
      filter(quarter > forecast_origin) %>%
      slice(1:forecast_h)
  
    X_future <- gdp_forecasts %>%
      rename(gdp_gap = gap) %>%
      left_join(future_inf, by = "quarter") %>%
      filter(quarter > forecast_origin)
    
    X_future$gdp_gap <- X_future$gdp_gap*100
    
    h_available <- nrow(X_future)
    
    if (h_available > 0) {
      # Initialize the 'running' shadow rate with the last filtered shadow rate
      # This ensures the smoothing (phi) starts from the 'true' latent state
      current_shadow_rate <- y_t_t 
      
      
      for (h in 1:h_available) {
        
        # print(forecast_origin)
        fdate <- forecast_origin + h/4
        
        fdate_str <- as.character(fdate)
        

        # Get Exogenous components
        exog_now <- X_future %>% filter(quarter == fdate)
        

        # Calculate the forecasts
        inf_gap_h <- exog_now$inf 
        gdp_gap_h <- exog_now$gdp_gap
        
        # Taylor rule part 
        target_rate <- i_bar + (g_pi * inf_gap_h) + (g_y * gdp_gap_h)
        new_shadow_rate <- (phi * current_shadow_rate) + (1 - phi) * target_rate
        

        if (fdate >= zlb_075_start && fdate <= zlb_end) {
          active_floor <- -0.75
        } else if (fdate >= zlb_0_start && fdate < zlb_075_start) {
          active_floor <- 0.00
        } else {
          active_floor <- -Inf # Outside structural bounds, never take the floor
        }
        
        observed_forecast <- max(active_floor, new_shadow_rate)
        current_shadow_rate <- new_shadow_rate
        
        # 5. Store the observed (capped) rate in the evaluation matrix
        eval_mat[fdate_str, i] <- observed_forecast
    }
  }
  
  return(eval_mat)
}




#' Strict Pseudo-Out-of-Sample Taylor Rule State-Space Model Forecasting Engine
#'
#' @description Takes estimated latent structural parameter timelines and rolls forward
#' across matched data vintages. Restricts operations to strict information sets available 
#' at each origin to calculate pure out-of-sample policy rate forecast trajectories.
#' Throws a critical error if any requested vintage columns are missing to prevent data leakage.
#'
#' @param params_df Dataframe containing the historical optimized parameter paths.
#' @param date_col Character. Column name for the time index in params_df (default: "quarter").
#' @param master_df Dataframe containing observed historical macro values (e.g., true_snb_rate).
#' @param forecast_h Integer. Forward-looking planning horizon in quarters (default: 8).
#' @param exogenous_gdp_forecast_data Matrix/DF containing real-time rolling level GDP projections.
#' @param exogenous_inf_forecast_data Matrix/DF containing real-time rolling inflation projections.
#' @param hp_inf_gap Logical. If TRUE, runs a rolling HP filter extraction on CPI logs (default: FALSE).
#' @param inf_target Numeric. Steady-state central bank inflation target (default: 1).
#' @param zlb_0_start String/yearqtr. Lower bound era threshold marking a 0% floor.
#' @param zlb_075_start String/yearqtr. Lower bound era threshold marking a -0.75% floor.
#' @param zlb_end String/yearqtr. The final termination quarter of the negative interest rate regime.
#' @param use_true_data Logical. If TRUE, sets forecasts using perfect ex-post realizations (cheating benchmark).
#'
#' @return A matrix containing the tracking trajectories across target dates (rows) and forecast origins (columns).
#' @export
forecast_taylor_ssm <- function(params_df,
                                date_col = "quarter",
                                master_df,
                                forecast_h = 8,
                                exogenous_gdp_forecast_data,
                                exogenous_inf_forecast_data = fcst_df_inf,
                                hp_inf_gap = FALSE,
                                inf_target = 1,
                                zlb_0_start = "2009 Q2",
                                zlb_075_start = "2015 Q1",
                                zlb_end = "2026 Q2",
                                use_true_data = FALSE) {
  
  # 1. Parse structural boundary time coordinates safely
  zlb_0_start   <- zoo::as.yearqtr(zlb_0_start)
  zlb_075_start <- zoo::as.yearqtr(zlb_075_start)
  zlb_end       <- zoo::as.yearqtr(zlb_end)
  
  parameters <- params_df %>%
    mutate(quarter = zoo::as.yearqtr(!!sym(date_col)))
  
  param_dates      <- parameters %>% pull(quarter)
  inf_matrix_dates <- zoo::as.yearqtr(exogenous_inf_forecast_data[[1]])
  
  fcast_start <- max(min(param_dates), min(inf_matrix_dates), na.rm = TRUE)
  fcast_end   <- min(max(param_dates), max(inf_matrix_dates), na.rm = TRUE)
  
  message(sprintf("Data alignment successful. Filtering loop window: %s to %s", 
                  as.character(fcast_start), as.character(fcast_end)))
  
  dates <- sort(unique(param_dates))
  dates <- dates[dates >= fcast_start & dates <= fcast_end]
  
  # Determine matrix dimension mapping requirements
  min_global_date <- min(dates)
  max_global_date <- max(dates) + (forecast_h / 4)
  extended_rows   <- seq(min_global_date, max_global_date, by = 1/4)
  
  # Setup empty tracking matrix
  eval_mat <- matrix(NA_real_, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(zoo::as.yearqtr(extended_rows))
  colnames(eval_mat) <- as.character(zoo::as.yearqtr(dates))
  
  # Seed evaluation matrix diagonal endpoints with true historical initial reference constraints
  for(i in seq_along(dates)) {
    vantage_str <- as.character(dates[i])
    actual_val <- master_df %>% 
      filter(quarter == dates[i]) %>% 
      pull(true_snb_rate) 
    
    if(length(actual_val) > 0) eval_mat[vantage_str, i] <- actual_val
  }
  
  # ============================================================================
  # MAIN FORECAST ROLLING LOOP
  # ============================================================================
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    
    current_params <- parameters %>% filter(quarter == forecast_origin) %>% slice(1)
    
    i_bar <- current_params$natural_rate 
    g_pi  <- current_params$gamma_pi
    g_y   <- current_params$gamma_y
    phi   <- current_params$phi
    
    if (forecast_origin >= zlb_0_start && forecast_origin <= zlb_end) {
      y_t_t <- current_params$fitted_obs
    } else {
      y_t_t <- master_df %>% 
        filter(quarter == forecast_origin) %>% 
        pull(true_snb_rate)
    }
    
    # ------------------------------------------------------------------------
    # PART 1: EXOGENOUS PROCESSING & ALIGNMENT
    # ------------------------------------------------------------------------
    v_str_space   <- format(forecast_origin, format = "%Y Q%q")
    v_str_compact <- format(forecast_origin, format = "%YQ%q")
    
    if (v_str_space %in% colnames(exogenous_inf_forecast_data)) {
      target_inf_col <- v_str_space
    } else {
      target_inf_col <- v_str_compact
    }
    
    # Safe Base R Column Name Selection for clean renaming pipeline
    future_inf <- exogenous_inf_forecast_data[, c(colnames(exogenous_inf_forecast_data)[1], target_inf_col)]
    colnames(future_inf) <- c("quarter", "inf")
    
    future_inf <- future_inf %>%
      mutate(quarter = zoo::as.yearqtr(quarter),
             inf = inf - 1) %>% 
      filter(!is.na(inf))
    
    # Pull out structural output gaps directly via your helper function
    gdp_forecasts <- get_hp_gap(vantage_q = forecast_origin,
                                h = forecast_h,
                                data = master_df,
                                gdp_forecast_data = exogenous_gdp_forecast_data,
                                return_type = "forecast") %>%
      mutate(quarter = zoo::as.yearqtr(quarter))
    
    # ------------------------------------------------------------------------
    # PART 2: VECTOR FUSION 
    # ------------------------------------------------------------------------
    X_future <- gdp_forecasts %>%
      rename(gdp_gap = gap) %>%
      left_join(future_inf, by = "quarter") %>%
      # Ensure we only evaluate horizons STRICTLY ahead of our rolling viewpoint
      filter(quarter > forecast_origin) %>%
      arrange(quarter)
    
    X_future$gdp_gap <- X_future$gdp_gap * 100
    h_available      <- nrow(X_future)
    
    # ------------------------------------------------------------------------
    # PART 3: RECURSIVE FORECAST MATRIX POPULATION
    # ------------------------------------------------------------------------
    if (h_available > 0) {
      current_shadow_rate <- y_t_t 
      
      for (h in 1:min(h_available, forecast_h)) {
        fdate     <- forecast_origin + (h / 4)
        fdate_str <- as.character(fdate) # Matches matrix rownames layout format
        
        inf_gap_h <- X_future$inf[h]
        gdp_gap_h <- X_future$gdp_gap[h]
        
        # Calculate unconstrained Taylor Rule target vector value
        target_rate     <- i_bar + (g_pi * inf_gap_h) + (g_y * gdp_gap_h)
        new_shadow_rate <- (phi * current_shadow_rate) + (1 - phi) * target_rate
        
        # Bind floor rules by chronological policy regimes
        if (fdate >= zlb_075_start && fdate <= zlb_end) {
          active_floor <- -0.75
        } else if (fdate >= zlb_0_start && fdate < zlb_075_start) {
          active_floor <- 0.00
        } else {
          active_floor <- -Inf
        }
        
        observed_forecast   <- max(active_floor, new_shadow_rate)
        current_shadow_rate <- new_shadow_rate
        
        # Commit values to the evaluation matrix layout
        eval_mat[fdate_str, i] <- observed_forecast
      }
    }
  }
  
  return(eval_mat)
}



