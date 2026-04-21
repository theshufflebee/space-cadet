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
                              gdp_gap_forecasts_input = gdp_gap_forecasts) {
  
  # Clean prepare params_df
  parameters <- params_df %>%
    mutate(quarter = as.yearqtr(!!sym(date_col)))
  
  dates <- parameters %>% pull(!!sym(date_col))
  
  # Setup Eval Matrix
  extended_rows <- seq(min(dates), by = 0.25, length.out = length(dates) + forecast_h)
  eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
  rownames(eval_mat) <- as.character(extended_rows)
  colnames(eval_mat) <- as.character(dates)
  
  # Align Actuals -> true ons in diag
  actual_unemp_df <- Y_data_df %>%
    filter(quarter %in% dates) %>%
    arrange(quarter)
  
  for(i in seq_along(dates)) {
    date_str <- as.character(dates[i])
    eval_mat[date_str, i] <- actual_unemp_df[[target_variable]][i]
  }
  
  #Main Forecast Loop -> loop over dates and each time
  for (i in seq_along(dates)) {
    forecast_origin <- dates[i] 
    
    # Select params for this vintage
    current_params <- parameters[i, ]
    # Check if column is named natural_rate or natural_rate_final from your previous step
    rho_T <- current_params$natural_rate 
    betas <- c(current_params$beta1, current_params$beta2, current_params$beta3)
    
    # 5. Build the GDP path (History + External Forecasts)
    history_gdp <- X_data %>%
      filter(quarter <= forecast_origin) %>% 
      select(quarter, all_of(exog_var_col))
    
    future_gdp <- gdp_gap_forecasts_input %>%
      filter(quarter > forecast_origin) %>% 
      slice(1:forecast_h) %>%
      select(quarter, all_of(exog_var_col))
    
    combined_gdp <- bind_rows(history_gdp, future_gdp) %>%
      arrange(quarter) %>%
      filter(!is.na(!!sym(exog_var_col)))
    
    # Re-calculate HP Cycle (The Pseudo-Real-Time approach)iincluding forecasts
    hp_res <- mFilter::hpfilter(combined_gdp[[exog_var_col]], freq = 1600)
    combined_gdp$y_gap <- as.numeric(hp_res$cycle)
    
    # Create Lags and extract only the forecast horizon
    gdp_features_wide <- combined_gdp %>%
      mutate(
        gdp_gap  = y_gap,
        gap_lag1 = dplyr::lag(y_gap, 1),
        gap_lag2 = dplyr::lag(y_gap, 2)
      ) %>%
      filter(quarter > forecast_origin) %>%
      arrange(quarter) %>%
      select(gdp_gap, gap_lag1, gap_lag2)
    
    # Generate paths / forecasts and store
    if (nrow(gdp_features_wide) > 0) {
      h_available <- nrow(gdp_features_wide)
      
      # Predict ssm is simple matrix opertaion
      path <- predict_ssm_path_rw(rho_T, betas, gdp_features_wide)
      
      look_ahead_dates <- seq(forecast_origin + 0.25, by = 0.25, length.out = h_available)
      target_rows <- as.character(look_ahead_dates)
      eval_mat[target_rows, i] <- path
    }
  }
  
  return(as.data.frame(eval_mat))
}
