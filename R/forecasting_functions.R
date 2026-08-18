################################################################################
#
# These functions are used to create forecast
#
################################################################################

# REMARK: All Documentation done with AI


#' Out-of-Sample Forecasting for the Okun State-Space Model
#'
#' Generates recursive out-of-sample unemployment rate forecasts across rolling vintage origins
#' using filtered Okun state-space model estimates and real-time GDP gap projections.
#'
#' @details
#' At each quarterly vintage origin \eqn{T}, the cyclical unemployment rate \eqn{\tilde{u}_{T+h|T}}
#' is projected recursively by combining autoregressive dynamics with distributed lags of the GDP gap:
#' \deqn{\tilde{u}_{T+h|T} = \phi \, \tilde{u}_{T+h-1|T} + \beta_1 y_{T+h} + \beta_2 y_{T+h-1} + \beta_3 y_{T+h-2}}
#' Level forecasts are reconstructed by adding the random-walk natural rate trend state:
#' \deqn{\hat{u}_{T+h|T} = \bar{u}_{T|T} + \tilde{u}_{T+h|T}}
#'
#' @param params_df A data frame containing filtered parameters and state estimates across quarters
#'   (\code{natural_rate}, \code{state_2_trend}, \code{beta1}, \code{beta2}, \code{beta3}, \code{phi}).
#' @param date_col Character. Column name identifying quarterly dates. Defaults to \code{"quarter"}.
#' @param exog_var_col Character. Column name for the exogenous log GDP series in \code{X_data}. Defaults to \code{"log_gdp"}.
#' @param forecast_h Integer. Out-of-sample forecast horizon in quarters. Defaults to \code{8}.
#' @param Y_data_df A data frame containing actual historical observations. Defaults to \code{Y_okun}.
#' @param target_variable Character. Column name of the observed series to forecast. Defaults to \code{"unemp_rate"}.
#' @param X_data A data frame containing historical exogenous variables. Defaults to \code{X_okun}.
#' @param gdp_gap_forecasts_input A data frame formatted as a lower-triangular forecast matrix containing real-time GDP projections. Defaults to \code{gdp_gap_forecasts}.
#' @param use_true_gdp Logical. Retained for interface consistency. Defaults to \code{FALSE}.
#'
#' @return A data frame in a triangle forecast matrix layout where the first column is \code{date_col}
#'   (target quarters) and subsequent columns contain projected values from each vintage origin.
#'
#' @export
#' @importFrom dplyr mutate filter arrange select slice pull bind_rows %>%
#' @importFrom rlang .data all_of
#' @importFrom tibble rownames_to_column
#' @importFrom zoo as.yearqtr
#' @importFrom mFilter hpfilter
forecast_okun_ssm <- function(params_df,                  
                              date_col = "quarter",
                              exog_var_col = "log_gdp",
                              forecast_h = 8,
                              Y_data_df = Y_okun,
                              target_variable = "unemp_rate",
                              X_data = X_okun,
                              gdp_gap_forecasts_input = gdp_gap_forecasts,
                              use_true_gdp = FALSE) {
  
  # Clean and prepare params_df
  params <- params_df %>%
    mutate(quarter = as.yearqtr(.data[[date_col]]))
  
  dates <- params %>% pull(.data[[date_col]])
  
  # Setup Evaluation Matrix
  extended_rows <- zoo::as.yearqtr(seq(min(dates), by = 0.25, length.out = length(dates) + forecast_h))
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(extended_rows)
  colnames(eval_mat) <- as.character(zoo::as.yearqtr(dates))
  
  # Align Actuals -> true observations in matrix diagonal
  actual_unemp_df <- Y_data_df %>%
    filter(quarter %in% dates) %>%
    arrange(quarter)
  
  for(i in seq_along(dates)) {
    date_str <- as.character(dates[i])
    eval_mat[date_str, i] <- actual_unemp_df[[target_variable]][i]
  }
  
  # ============================================================================
  # MAIN FORECAST LOOP
  # ============================================================================
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    
    # Extract estimated parameters and latent states from this vintage row
    current_params <- params[i, ]
    
    # get GDP
    rho_T           <- current_params$natural_rate   # u_bar_{T|T}
    current_u_tilde <- current_params$state_2_trend   # u_tilde_{T|T}
    
    betas  <- c(current_params$beta1, current_params$beta2, current_params$beta3)
    phi    <- current_params$phi
    
    # Isolate history baseline up to this point
    history_gdp <- X_data %>%
      mutate(quarter = zoo::as.yearqtr(.data[[date_col]])) %>%
      filter(quarter <= forecast_origin) %>% 
      select(quarter, gdp_val = all_of(exog_var_col))
    
    # Convert actual date index to string matching vintage layout
    vintage_col_str <- format(forecast_origin, format = "%Y Q%q")
    
    # Extract raw triangle matrix path for this origin column
    raw_forecasts <- gdp_gap_forecasts_input %>%
      select(date = date, gdp_val = all_of(vintage_col_str)) %>%
      mutate(quarter = zoo::as.yearqtr(date, format = "%Y Q%q")) %>%
      filter(!is.na(gdp_val)) %>%
      arrange(quarter)
    
    hist_baseline_val <- history_gdp %>% filter(quarter == forecast_origin) %>% pull(gdp_val)
    matrix_origin_val <- raw_forecasts %>% filter(quarter == forecast_origin) %>% pull(gdp_val)
    
    if (length(hist_baseline_val) > 0 && length(matrix_origin_val) > 0) {
      if (abs(matrix_origin_val - hist_baseline_val) > 1e-6) {
        warning(sprintf("[VINTAGE MISMATCH] Baseline drift at %s.", format(forecast_origin, "%Y Q%q")))
      }
    }
    
    future_gdp <- raw_forecasts %>%
      filter(quarter > forecast_origin) %>%
      slice(1:forecast_h) %>%
      select(quarter, gdp_val)
    
    combined_gdp <- bind_rows(history_gdp, future_gdp) %>% arrange(quarter)
    
    # Re-calculate Real-Time HP Cycle
    hp_res <- mFilter::hpfilter(combined_gdp$gdp_val, freq = 1600)
    combined_gdp$y_gap <- as.numeric(hp_res$cycle) * 100
    
    # Create Lags and extract only the forecast horizon rows
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
    
    # Generate paths and project forward
    if (nrow(gdp_features_wide) > 0) {
      h_available <- nrow(gdp_features_wide)
      path <- numeric(h_available)
      
      for (h in 1:h_available) {
        y_t   <- gdp_features_wide$gdp_gap[h]
        y_lm1 <- gdp_features_wide$gap_lag1[h]
        y_lm2 <- gdp_features_wide$gap_lag2[h]
        
        # Evaluate cycle step: u_tilde_t = phi * u_tilde_{t-1} + beta(L)y_t
        # Note: Since xi_cycle = 0.0001, expected future shocks E[xi_{t+h}] are 0
        okun_fundamental <- (betas[1] * y_t) + (betas[2] * y_lm1) + (betas[3] * y_lm2)
        new_u_tilde      <- (phi * current_u_tilde) + okun_fundamental
        
        # Combine back with random-walk trend expectation (E_T[u_bar_{T+h}] = u_bar_{T|T})
        path[h] <- rho_T + new_u_tilde
        
        # Update the iteration cycle lag variable for step h + 1
        current_u_tilde <- new_u_tilde
      }
      
      # Store path back into evaluation frame
      look_ahead_dates <- zoo::as.yearqtr(seq(forecast_origin + 0.25, by = 0.25, length.out = h_available))
      target_rows <- as.character(look_ahead_dates)
      
      # Safety Guard: Ensures all target rows exist in matrix before assignment
      valid_rows <- target_rows[target_rows %in% rownames(eval_mat)]
      eval_mat[valid_rows, i] <- path[seq_along(valid_rows)]
    }
  }
  
  eval_df <- as.data.frame(eval_mat) %>%
    tibble::rownames_to_column(var = date_col)
  
  return(eval_df)
}


#' Out-of-Sample Forecasting for the Phillips Curve State-Space Model
#'
#' Generates recursive out-of-sample inflation forecasts across rolling vintage origins
#' using structural Phillips curve state-space estimates, projected HP output gaps,
#' and spliced Law of One Price (LOP) gaps.
#'
#' @details
#' At each quarterly vintage origin \eqn{T}, headline inflation is projected recursively
#' using a New Keynesian Hybrid Open-Economy Phillips curve specification:
#' \deqn{\hat{\pi}_{T+h|T} = \bar{\pi}_{T|T} + \phi (\hat{\pi}_{T+h-1|T} - \bar{\pi}_{T|T}) + \psi_{\text{LOP}} \, \text{gap}^{\text{LOP}}_{T+h} + \beta_y \, y^{\text{gap}}_{T+h}}
#' where \eqn{\bar{\pi}_{T|T}} is the filtered latent trend inflation rate and \eqn{\hat{\pi}_{T|T}}
#' is initialized from observed inflation at the vintage boundary.
#'
#' @param params_df A data frame containing filtered parameters across quarters
#'   (\code{natural_rate}, \code{beta_y}, \code{psi_lop}, \code{phi}).
#' @param date_col Character. Column name identifying quarterly dates. Defaults to \code{"quarter"}.
#' @param master_df A data frame containing historical quarterly series, including \code{log_inflation_diff}.
#' @param forecast_h Integer. Out-of-sample forecast horizon in quarters. Defaults to \code{8}.
#' @param exogenous_gdp_forecast_data A data frame containing real-time GDP forecast vintages.
#'
#' @return A data frame formatted as a lower-triangular forecast matrix where the first column
#'   is \code{date_col} (target quarters) and subsequent columns represent vintage origins.
#'
#' @seealso \code{\link{get_hp_gap}}, \code{\link{splice_snb_series}}
#'
#' @export
#' @importFrom dplyr mutate arrange filter pull select rename left_join slice %>%
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
#' @importFrom zoo as.yearqtr
forecast_philips_ssm <- function(params_df,
                                 date_col = "quarter",
                                 master_df,
                                 forecast_h = 8,
                                 exogenous_gdp_forecast_data) {
  
  message("STARTING PHILLIPS FORECASTS")
  
  # 1. Standardize dates upfront as zoo::yearqtr
  parameters <- params_df %>%
    mutate(quarter = zoo::as.yearqtr(.data[[date_col]])) %>%
    arrange(quarter)
  
  master_clean <- master_df %>%
    mutate(quarter = zoo::as.yearqtr(.data[[date_col]])) %>%
    filter(quarter >= zoo::as.yearqtr("1984 Q1"))
  
  dates <- parameters %>% pull(quarter)
  
  # 2. Setup matrix rows spanning full historical range + forecast horizon
  max_date <- max(dates)
  extended_seq <- seq(min(dates), max_date + (forecast_h / 4), by = 0.25)
  extended_rows <- zoo::as.yearqtr(extended_seq)
  
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(extended_rows)
  colnames(eval_mat) <- as.character(dates)
  
  # 3. Populate diagonal with observed inflation history
  actuals_df <- master_clean %>% filter(quarter %in% dates)
  for (i in seq_along(dates)) {
    vantage_str <- as.character(dates[i])
    val <- actuals_df %>% filter(quarter == dates[i]) %>% pull(log_inflation_diff)
    if (length(val) > 0 && !is.na(val[1])) {
      eval_mat[vantage_str, i] <- val[1]
    }
  }
  
  # ============================================================================
  # MAIN FORECAST LOOP
  # ============================================================================
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    current_params  <- parameters[i, ]
    
    # Structural parameters
    pi_bar  <- current_params$natural_rate 
    beta_y  <- current_params$beta_y
    psi_lop <- current_params$psi_lop
    phi     <- current_params$phi
    
    # Starting historical anchor (inflation at T)
    hist_inf <- master_clean %>% 
      filter(quarter == forecast_origin) %>% 
      pull(log_inflation_diff)
    
    if (length(hist_inf) == 0 || is.na(hist_inf[1])) next
    current_inflation <- hist_inf[1]
    
    # Pull exogenous drivers (HP Gap & Spliced REER/LOP)
    gdp_forecasts <- get_hp_gap(
      vantage_q = forecast_origin,
      h = forecast_h,
      data = master_clean,
      gdp_forecast_data = exogenous_gdp_forecast_data,
      return_type = "forecast"
    )
    
    
    lop_forecast <- splice_snb_series(
      vantage_quarter = forecast_origin,
      snb_reer_delay   = SNB_REER_DELAY,
      data             = master_clean
    )
    
    # Join and scale exogenous variables
    X_future <- gdp_forecasts %>%
      rename(gdp_gap = gap) %>%
      mutate(quarter = zoo::as.yearqtr(quarter)) %>%
      left_join(
        lop_forecast %>% 
          mutate(quarter = zoo::as.yearqtr(quarter)) %>% 
          select(quarter, lop_gap), 
        by = "quarter"
      ) %>%
      filter(quarter > forecast_origin) %>%
      slice(1:forecast_h) %>%
      mutate(
        gdp_gap = as.numeric(gdp_gap) * 100,
        lop_gap = as.numeric(lop_gap) * 100
      )

    h_available <- nrow(X_future)
    
    # Project path forward recursively
    if (h_available > 0) {
      path <- numeric(h_available)
      
      for (h in 1:h_available) {
        lop_gap_h <- X_future$lop_gap[h]
        gdp_gap_h <- X_future$gdp_gap[h]
        
        # New Keynesian Hybrid Phillips Curve Projection
        cyclical_inflation <- (psi_lop * lop_gap_h) + (beta_y * gdp_gap_h)
        inf_deviation      <- current_inflation - pi_bar 
        pred_inflation     <- pi_bar + (phi * inf_deviation) + cyclical_inflation
        
        path[h] <- pred_inflation
        current_inflation <- pred_inflation  # Feed back for h + 1
      }

            # Assign full forecast trajectory into evaluation matrix in one block
      target_dates <- X_future$quarter
      target_rows  <- as.character(target_dates)
      valid_rows   <- target_rows[target_rows %in% rownames(eval_mat)]
      
      eval_mat[valid_rows, i] <- path[seq_along(valid_rows)]
    }
  }
  
  eval_df <- as.data.frame(eval_mat) %>%
    tibble::rownames_to_column(var = date_col)
  
  message("ENDED PHILLIPS FORECASTS")
  return(eval_df)
}




#' Strict Pseudo-Out-of-Sample Taylor Rule State-Space Model Forecasting Engine
#'
#' Generates recursive out-of-sample policy interest rate projections across rolling vintage
#' origins using estimated Taylor rule parameters, real-time output and inflation gaps, and
#' explicit regime-dependent zero/effective lower bound (ZLB/ELB) truncation.
#'
#' @details
#' At each quarterly vintage origin \eqn{T}, unconstrained policy rate targets are constructed via:
#' \deqn{i^*_{T+h|T} = \bar{i}_{T|T} + \gamma_\pi (\hat{\pi}_{T+h|T} - \pi^*) + \gamma_y \, y^{\text{gap}}_{T+h|T}}
#' Latent shadow rates are propagated with interest rate smoothing:
#' \deqn{s_{T+h|T} = \phi \, s_{T+h-1|T} + (1 - \phi) i^*_{T+h|T}}
#' Observed rate forecasts are obtained by applying regime-specific effective lower bounds:
#' \deqn{\hat{\imath}_{T+h|T} = \max(\text{Floor}_t, \, s_{T+h|T})}
#'
#' @param params_df A data frame containing optimized parameters and states across quarters
#'   (\code{natural_rate}, \code{gamma_pi}, \code{gamma_y}, \code{phi}, \code{fitted_obs}).
#' @param date_col Character. Column name identifying quarterly dates in \code{params_df}. Defaults to \code{"quarter"}.
#' @param master_df A data frame containing historical observed macro series, including \code{true_snb_rate}.
#' @param forecast_h Integer. Forecast horizon in quarters. Defaults to \code{8}.
#' @param exogenous_gdp_forecast_data Matrix or data frame containing real-time GDP projections.
#' @param exogenous_inf_forecast_data Matrix or data frame containing real-time inflation projections. Defaults to \code{fcst_df_inf}.
#' @param hp_inf_gap Logical. Retained for specification compatibility. Defaults to \code{FALSE}.
#' @param inf_target Numeric. Central bank inflation target (\eqn{\pi^*}). Defaults to \code{1}.
#' @param zlb_0_start Character, Date, or \code{yearqtr}. Start date of the 0.00\% lower bound regime. Defaults to \code{"2009 Q2"}.
#' @param zlb_075_start Character, Date, or \code{yearqtr}. Start date of the -0.75\% negative interest rate regime. Defaults to \code{"2015 Q1"}.
#' @param zlb_end Character, Date, or \code{yearqtr}. Termination quarter of the lower bound regime. Defaults to \code{"2099 Q4"}.
#' @param use_true_data Logical. Retained for interface compatibility. Defaults to \code{FALSE}.
#'
#' @return A data frame formatted as a lower-triangular forecast matrix with target dates as rows
#'   and vintage origins as columns.
#'
#' @seealso \code{\link{get_hp_gap}}, \code{\link{rolling_est_taylor_ssm}}
#'
#' @export
#' @importFrom dplyr mutate filter pull slice rename left_join arrange %>%
#' @importFrom rlang sym
#' @importFrom tibble rownames_to_column
#' @importFrom zoo as.yearqtr
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
                                zlb_end = "2099 Q4",
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
  eval_df <- as.data.frame(eval_mat) %>%
    tibble::rownames_to_column(var = date_col)
  
  return(eval_df)
}



