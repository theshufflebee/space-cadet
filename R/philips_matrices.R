################################################################################
# 
# Build the Matrices for the Phillips Model
#
################################################################################


#' Extract HP Filter Gap at a specific Vantage Point
#' 
#' This function calculates the HP Gap for these models.
#' There are two inputs, one is the true data and the other is gdp forecast data.
#' The function then splices the series together at the vantage point, which represents
#' today in the pseudo out of sample forecasts, then adds h amounts of forecasts to the data
#' then runs the filter from T=0 to T=t+h
get_hp_gap <- function(data,
                       gdp_forecast_data,
                       vantage_q,
                       h = 8,
                       gdp_col = "log_gdp",
                       date_col = "quarter",
                       lag_val = 0,
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

######################333

build_X_data_matrix_philips <- function(T_0 = "2015-01-01",
                                        vantage_quarter = "2023 Q1",
                                        data = master_philips,
                                        h = 8) {
  
  # Lag CPI before burn cutoff
  raw_data <- data %>%
    arrange(quarter) %>%
    mutate(
      cpi = as.numeric(cpi),
      log_inflation_diff = log(cpi) - dplyr::lag(log(cpi), 1),
      lag_log_inflation_diff = dplyr::lag(log_inflation_diff, 1) # Fixed syntax
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
  
  message("LOP FORECAST")
  print(tail(LOP_forecast))
  
  HP_gap_data <- get_hp_gap(raw_data,
                            raw_data, # forecast data
                            vantage_q = vantage_quarter
                            )
  
  HP_gap_forecasts_data <- get_hp_gap(raw_data,
                            raw_data, # forecast data
                            vantage_q = vantage_quarter,
                            return_forecasts = TRUE)
  
  print(HP_gap_data)
  
  X_matrix <- raw_data %>%
    select(quarter, log_inflation_diff, lag_log_inflation_diff, log_gdp, `5y_cpi_forecast`) %>%
    left_join(LOP_forecast %>% select(quarter, lop_gap), by = "quarter") %>%
    mutate(lop_gap = lag(lop_gap, 1))%>% # currently lagged
    left_join(HP_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
    # Filter to return the specific estimation sample
    filter(quarter >= as.yearqtr(T_0)) %>%
    arrange(quarter) %>%
    mutate(`5y_cpi_forecast` = `5y_cpi_forecast` /100)
  
  
  X_forecast_matrix <- raw_data %>%
    select(quarter, log_inflation_diff, lag_log_inflation_diff, log_gdp, `5y_cpi_forecast`) %>%
    left_join(LOP_forecast %>% select(quarter, lop_gap), by = "quarter") %>%
    # mutate(lop_gap = lag(lop_gap, 1))%>% # currently lagged
    left_join(HP_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
    # Filter to return the specific estimation sample
    filter(quarter >= as.yearqtr(T_0)) %>%
    arrange(quarter) %>%
    mutate(`5y_cpi_forecast` = `5y_cpi_forecast` /100)

  return(X_matrix)
}

################################################################################

# Matrices

# The M MATRIX is the same as the OKUN ONE

# The H MATRIX is the same as the okun one

# THE N MATRIX IS THE SAME AS THE OKUN ONE

# G Matrix
# has different factor loadings


# LATER ADAPT G MATRIX SO IT TAKES A BOOLEAN FOR FACTOR LOADINGS


#' Build the G Matrix (Factor Loadings)
#'
#' @description
#' Constructs the loading matrix \eqn{G} that maps the latent structural state 
#' \eqn{\rho_t} (the Natural Rate) to the observed measurements \eqn{y_t}:
#' \deqn{y_t = \mu_t + G\rho_t + M\varepsilon_t}
#'
#' @details
#' For the Okun model, \eqn{G = \begin{bmatrix} 1 \\ 1 \end{bmatrix}}, implying 
#' that both observed unemployment and the SPF 5-year forecast load one-to-one 
#' on the latent structural trend.
#'
#' @return A \eqn{2 \times 1} matrix of factor loadings.
build_G_philips <- function() {
  
  matrix(c(1, 1), nrow = 2, ncol = 1) # 4 becasue spf is yearly ->  our forecasts are quarterly
}


build_N <- function(model_params, default_sig_xi = 0.0005) {
  
  # Check if a specific state sigma exists that is being estimated.
  # otherwise default to sd 0.01 and therefore variance 0.0001
  xi_n <- if(!is.null(model_params$xi_n)) model_params$xi_n else default_sig_xi
  matrix(xi_n, 1, 1)
}




initialize_my_philips_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # Define the Manifest
  # This acts as the "Single Source of Truth" for your parameters
  # Rules: 0 = Linear, 1 = Exponential (>0), 2 = Logit (0 to 1)
  required_params <- c("beta1", "beta2", "beta3", "sigma_cpi", "sigma_spf")
  if (!all(required_params %in% names(parameter_guesses))) {
    stop("Missing required parameters in parameter_guesses list!")
  }
  
  if(!all(c("xi_n") %in% names(parameter_guesses))) {
    message("Missing xi_n in parameters. Using Fixed variance")
  }
  
  # First bild manifest with the required parameters
  manifest <- list(
    # Okun's Law Betas (Unconstrained)
    beta1              = list(val = parameter_guesses$beta1, rule = 0),
    beta2              = list(val = parameter_guesses$beta2, rule = 0),
    beta3              = list(val = parameter_guesses$beta3, rule = 0),
    
    
    # Measurement Noise Standard Deviations (Must be positive)
    # Names match: "sigma_" + colnames(Y_data)
    sigma_cpi   = list(val = parameter_guesses$sigma_cpi,  rule = 1),
    sigma_spf = list(val = parameter_guesses$sigma_spf, rule = 1)
  )
  
  # Then add those that are optional
  
  # State Persistence (Bounded for stability)
  if ("phi" %in% names(parameter_guesses) && !is.null(parameter_guesses$phi)) {
    manifest$phi <- list(val = parameter_guesses$phi, rule = 2)
  } else {
    message("--- phi not provided. Fixing phi = 1 (Random Walk) ---")
  }
  
  if ("xi_n" %in% names(parameter_guesses) && !is.null(parameter_guesses$xi_n)) {
    manifest$xi_n <- list(val = parameter_guesses$xi_n, rule = 1)
  } else {
    message("--- xi_n not provided. Using Fixed variance defined in build_N ---")
  }
  
  # 2. Bundle the builders and data into the blueprint
  # These are the standalone functions you defined earlier
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), 
      X = as.matrix(X_data)
    ),
    manifest = manifest,
    builders = list(
      mu_t = build_mu_t,
      H    = build_H,
      G    = build_G_philips,
      M    = build_M,
      N    = build_N
    )
  )
  
  return(ssm)
}



