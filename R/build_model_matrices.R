################################################################################
#
# Generic Builder Matrices
#
################################################################################

# These fucnctions build the State space model model matrices for the Optimizer.
# The Optimizer will choose a value for these matrices, then these values will
# be put into the functions to build the matrix so the kalman filter can estimate
# the log likelihood. Then from that the optimizer will choose new values to then
# do the same thing again until it has found its optimas


#' Build the H Matrix (State Transition)
#'
#' @description
#' Defines the transition matrix \eqn{H} in the transition equation:
#' \deqn{\rho_t = \nu_t + H\rho_{t-1} + N\xi_t}
#'
#' @param model_params A named list. If "phi" is present, it is used as the 
#' transition coefficient; otherwise, defaults to 1.0 (Random Walk).
#'
#' @return A \eqn{1 \times 1} matrix representing the persistence of the 
#' latent state \eqn{\rho_t}.
build_H <- function(model_params) {
  # Explicitly look for "phi", default to 1 (Random Walk) if not found
  phi_val <- if(!is.null(model_params$phi)) model_params$phi else 1.0
  return(matrix(phi_val, 1, 1))
}


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
build_G <- function() {
  matrix(c(1, 1), nrow = 2, ncol = 1)
}


#' Build the M Matrix (Measurement Noise Covariance)
#'
#' @description
#' Constructs the diagonal standard deviat8ion matrix \eqn{M} for the 
#' measurement noise \eqn{\varepsilon_t}:
#' \deqn{y_t = \dots + M\varepsilon_t, \quad \varepsilon_t \sim N(0, I)}
#'
#' @param model_params A named list containing parameters starting with "sigma_". 
#' These represent the standard deviations (\eqn{\sigma}) of the observation errors.
#'
#' @details
#' The function squares the standard deviations to produce variances on the 
#' diagonal. This ensures the covariance structure is positive semi-definite.
#'
#' @return A \eqn{2 \times 2} diagonal matrix \eqn{M} where 
#' \eqn{M_{ii} = \sigma_i}.
build_M <- function(model_params) {
  
  # Dynamically identify all measurement noise parameters
  # This looks for any parameter starting with "sigma_" 
  # but excludes the state innovation "sigma_state_innov" (or similar)
  p_names <- grep("^sigma_", names(model_params), value = TRUE)
  
  if (length(p_names) == 0) {
    stop("build_M: No parameters starting with 'sigma_' found in model_params.")
  }
  
  # Extract values
  # We use the names to ensure we maintain the correct order
  sigmas <- unlist(model_params[p_names])
  ny <- length(sigmas)
  
  # Create diagonal matrix of variances (SD squared)
  # diag() handles the dimensions based on the length of sigmas
  M <- diag(sigmas, nrow = ny, ncol = ny)
  
  # Optional: Set row/colnames for easier debugging
  colnames(M) <- rownames(M) <- p_names
  
  return(M)
}


#' Build the N Matrix (Process Noise Covariance)
#'
#' @description
#' Constructs the standard deviaton matrix for the state innovation \eqn{\xi_t} 
#' (the variance of the Natural Rate shocks):
#' \deqn{\rho_t = \dots + N\xi_t, \quad \xi_t \sim N(0, I)}
#'
#' @param model_params A named list. If "xi_n" is present, it is used as the 
#' standard deviation; otherwise, it uses the provided \code{default_sig_xi}.
#' @param default_sig_xi Numeric. The default standard deviation if no 
#' parameter is estimated. Default is 0.01 (variance of 0.0001).
#'
#' @return A \eqn{1 \times 1} matrix representing the process noise sd 
#' \eqn{\xi_n}.
build_N <- function(model_params, default_sig_xi = 0.001) {
  
  # Check if a specific state sigma exists that is being estimated.
  # otherwise default to sd 0.01 and therefore variance 0.0001
  xi_n <- if(!is.null(model_params$xi_n)) model_params$xi_n else default_sig_xi
  matrix(xi_n, 1, 1)
}


#' Build the mu_t Matrix (Exogenous Impact)
#'
#' @description
#' Constructs the \eqn{\mu_t} vector which represents the contribution of 
#' exogenous variables to the measurement equation. In this Okun's Law context:
#' \deqn{\mu_t = \begin{bmatrix} X_t \beta \\ 0 \end{bmatrix}}
#' where \eqn{X_t \beta} is the cyclical impact of GDP gaps on unemployment.
#'
#' @param model_params A named list containing all parameters of the state space model.
#' @param X A \eqn{T \times K} matrix of exogenous regressors (e.g., lagged GDP gaps).
#'
#' @details 
#' The function identifies all parameters starting with "beta", extracts them in 
#' alphabetical order, and performs matrix multiplication with \eqn{X}. The 
#' second column is padded with zeros as the SPF forecast is assumed to be 
#' unbiased relative to the structural trend \eqn{\rho_t}.
#' In contrast to the other matrix builders it returns al mu_t for periods 1:T.
#' The kalman filter then selects the correct row t during estimation
#'
#' @return A \eqn{T \times 2} matrix representing the exogenous component of the 
#' measurement equation \eqn{y_t = \mu_t + G\rho_t + M\varepsilon_t}.
build_mu_t <- function(model_params, X) {
  # model_params is now a named list/vector
  # Extract all parameters starting with "beta"
  beta_names <- grep("^beta", names(model_params), value = TRUE)
  betas <- unlist(model_params[beta_names])
  
  # Multiply the betas with the X matrix
  # Impact is a 1 colum matrix with T rows
  impact <- X %*% matrix(betas, ncol = 1)
  
  # Return T x 2 matrix
  return(cbind(impact, rep(0, nrow(X))))
}


################################################################################
#
# OKUN SSM OBJECT
#
################################################################################



#' Initialize the Okun State-Space Blueprint
#'
#' @description
#' This function constructs the "Blueprint" or "SSM Object" required for Kalman filtering 
#' and parameter optimization. It defines the parameter manifest, which includes initial 
#' guesses and transformation rules, and bundles the data with the builder functions.
#'
#' @param Y_data A matrix or data frame of observed variables. Typically 
#' includes columns for the unemployment rate and the SPF 5-year forecast.
#' @param X_data A matrix of exogenous regressors, such as contemporaneous and 
#' lagged GDP gaps, used to calculate the cyclical component of the measurement equation.
#' @param parameter_guesses A named list containing initial values for the model 
#' parameters. Required keys: \code{"beta1"}, \code{"beta2"}, \code{"beta3"}, 
#' \code{"sigma_unemp_rate"}, and \code{"sigma_spf_5y_unemp"}. Optional: \code{"xi_n"}
#' and \code{"phi"}.
#'
#' @details
#' The function sets up a \bold{Manifest} that dictates how parameters are treated during optimization:
#' \itemize{
#'   \item \bold{Rule 0 (Linear):} No transformation. Used for coefficients like \eqn{\beta} that can be positive or negative.
#'   \item \bold{Rule 1 (Exponential):} Parameter is exponentiated (\eqn{e^\theta}). Used for standard deviations (\eqn{\sigma}, \eqn{\xi}) to ensure they remain strictly positive.
#'   \item \bold{Rule 2 (Logit):} Parameter is constrained between 0 and 1. Used for persistence parameters like \eqn{\phi}.
#' }
#' 
#' If \code{xi_n} is missing from \code{parameter_guesses}, the function will proceed 
#' with a fixed variance in the state equation, and \code{xi_n} will not be 
#' added to the manifest for estimation.
#' 
#' If \code{phi} is missing from \code{parameter_guesses}, the function will proceed 
#' with a \code{phi = 1}, treating the state equation as a random walk
#'
#' @return A structured list containing:
#' \item{data}{A list with matrices \eqn{Y} and \eqn{X}.}
#' \item{manifest}{A named list of parameter metadata for optimization.}
#' \item{builders}{A list of functions (\code{build_mu_t}, \code{build_H}, etc.) used to construct SSM matrices.}
#'
#' @export
initialize_my_okun_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # Define the Manifest
  # This acts as the "Single Source of Truth" for your parameters
  # Rules: 0 = Linear, 1 = Exponential (>0), 2 = Logit (0 to 1)
  required_params <- c("beta1", "beta2", "beta3", "sigma_unemp_rate", "sigma_spf_5y_unemp")
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
    sigma_unemp_rate   = list(val = parameter_guesses$sigma_unemp_rate,  rule = 1),
    sigma_spf_5y_unemp = list(val = parameter_guesses$sigma_spf_5y_unemp, rule = 1)
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
  
  # Bundle the builders and data into the blueprint
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
      G    = build_G,
      M    = build_M,
      N    = build_N
    ),
    name = "OKUN",
    rho_guess = c(0.1),
    sigma_guess = c(10)
    
  )
  
  return(ssm)
}


################################################################################
# 
# Phillips Model SSM and Data Matrix
#
################################################################################


#' Build the X Data Matrix for the Phillips Curve State-Space Model
#'
#' @description
#' Prepares all observed variables (y) and exogenous variables (mu_t) and collects
#' them in a matrix for the Phillips curve estimation at a specific 
#' vantage point. The function first transforms the time-series data (logs and lags), 
#' integrates separately constructed components like the Law of One Price (LOP) gap and 
#' GDP gap, and ensures alignment between observed inflation and expectations.
#'
#' @details
#' The function constructs the following variables for the model:
#' \itemize{
#'   \item \code{log_inflation_diff}: The first difference of log CPI: \eqn{\Delta \log(CPI_t)}.
#'   \item \code{lag_log_inflation_diff}: The lagged dependent variable: \eqn{\Delta \log(CPI_{t-1})}.
#'   \item \code{lop_gap}: The Law of One Price gap, derived from \code{\link{splice_reer_series}} 
#'         and \code{\link{extract_lop_gap}}, then lagged by one period.
#'   \item \code{gdp_gap}: The output gap extracted via \code{\link{get_hp_gap}}.
#'   \item \code{5y_cpi_forecast}: Five-year inflation expectations, scaled by 100 
#'         (\eqn{x / 100}) to align with decimal growth rates.
#' }
#'
#' The estimation sample is constrained between the burn-in period and the 
#' \code{vantage_quarter}, which serves as the "pseudo-today" Knowledge Cutoff.
#'
#' @param T_0 Character or \code{yearqtr}. The start date of the estimation sample. Default is "2015-01-01".
#' @param vantage_quarter Character or \code{yearqtr}. The forecast origin date. 
#' @param data A dataframe containing \code{quarter}, \code{cpi}, \code{log_gdp}, and \code{5y_cpi_forecast}.
#' @param h Integer. The forecast horizon (default is 8).
#'
#' @return A tibble containing the aligned time series for model estimation, 
#' filtered to start from \code{T_0}.
#'
#' @note 
#' This function relies on external global variables \code{model_philips_burn_in} 
#' and \code{SNB_REER_DELAY}. Ensure these are defined in the global environment 
#' or the calling script.
#' 
#' @seealso [get_hp_gap()]
#'
#' @import dplyr
#' @importFrom zoo as.yearqtr
#' @export
build_data_matrix_philips <- function(T_0 = "2015-01-01",
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

#' Initialize the Phillips Curve State-Space Model Blueprint
#'
#' @description
#' This function initializes a structured State-Space Model (SSM) object
#' specifically for the Phillips curve estimation. It defines the parameter 
#' manifest, containing what parameters are to be estimated and what the initial guesses are,
#' the functions that build the matrices for the optimizer and the used data.
#'
#' @details
#' The manifest also sets the specific constraints on parameter estimation:
#' \itemize{
#'   \item \bold{Rule 0 (Linear):} No transformation. Applied to \code{beta1} (lagged inflation), 
#'   \code{beta2} (output gap), and \code{beta3} (exchange rate gap).
#'   \item \bold{Rule 1 (Exponential):} Constraints parameters to be strictly positive (\eqn{>0}). 
#'   Applied to measurement noise (\code{sigma_cpi}, \code{sigma_spf}) and state variance (\code{xi_n}).
#'   \item \bold{Rule 2 (Logit):} Constraints parameters between 0 and 1. Applied to 
#'   state persistence (\code{phi}).
#' }
#' 
#' If \code{phi} is not provided, the model defaults to a Random Walk (\eqn{\phi = 1}) for the State Transition.
#' If \code{xi_n} is not provided, the model utilizes a fixed variance defined within 
#' the \code{build_N} function for the state variance
#'
#' @param Y_data A matrix or dataframe containing the observation variables (typically 
#' actual inflation and inflation expectations).
#' @param X_data A matrix or dataframe containing the exogenous drivers (typically 
#' lagged inflation, output gap(s), and LOP gap(s)).
#' @param parameter_guesses A named list containing initial starting values for 
#' \code{beta1}, \code{beta2}, \code{beta3}, \code{sigma_cpi}, and \code{sigma_spf}.
#' Optional parameters when left out will used predetermined fixed values.
#'
#' @return A structured list (\code{ssm}) containing:
#' \itemize{
#'   \item \code{data}: A list with matrices \code{Y} and \code{X}.
#'   \item \code{manifest}: A list defining the initial values and transformation rules for each parameter.
#'   \item \code{builders}: A list of function pointers (\code{mu_t}, \code{H}, \code{G}, \code{M}, \code{N}) 
#'   used to construct the SSM matrices during filtering.
#' }
#' 
#' @seealso \code{\link{build_mu_t}} for exogenous regressor matrix construction.
#' @seealso \code{\link{build_H}} for state factor loading matrices.
#' @seealso \code{\link{build_G}} for state-to-measurement mapping.
#' @seealso \code{\link{build_M}} for measurement error covariance structures.
#' @seealso \code{\link{build_N}} for state innovation covariance matrices.
#' 
#' @export
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
  
  # Bundle the builders and data into the blueprint
  # These are the standalone functions defined earlier
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), 
      X = as.matrix(X_data)
    ),
    manifest = manifest,
    builders = list(
      mu_t = build_mu_t,
      H    = build_H,
      G    = build_G,
      M    = build_M,
      N    = build_N
    ),
    name = "PHILIPS",
    rho_guess = c(0.1),
    sigma_guess = c(10)
    
  )
  
  return(ssm)
}


################################################################################
#
# Taylor Rule / Policy Rate Model Forecast
#
################################################################################


# --- Matrices ---


#' Build the H Matrix (State Transition)
#' @param model_params A named list containing "rho_tp" for the cyclical term premium.
#' @return A 3 x 3 matrix defining the transition dynamics.
build_H_taylor <- function(model_params) {
  # rho_tp determines the persistence of the cyclical term premium component
  rho_tp <- if(!is.null(as.numeric(model_params$rho_tp))) model_params$rho_tp else 0.9
  
  H <- diag(3)  # Natural rate random walk // Trend term premium random walk
  H[3,3] <- rho_tp  # Cyclical term premium AR(1)
  
  return(H)
}



#' Build the G Matrix (Factor Loadings)
#' @param model_params A named list containing the smoothing parameter "rho".
#' @return A 2 x 3 matrix of factor loadings.
build_G_taylor <- function(model_params) {
  # rho is the interest rate smoothing parameter
  rho_val <- if(!is.null(model_params$rho)) model_params$rho else 0.8
  
  G <- matrix(0, 3, 3)
  
  # Row 1: Policy Rate loads on natural rate i_t
  G[1,1] <- (1 - rho_val)
  
  # Row 2: Forward Rate loads on spf
  G[2,1] <- 1
  G[2,2] <- 0
  G[2,3] <- 0
  
  # Row 3: Forward Rate loads on i_t + trend TP + cyclical TP
  G[3,1] <- 1
  G[3,2] <- 1
  G[3,3] <- 1
  
  return(G)
}


#' Build the M Matrix (Measurement Noise Covariance)
#' @description 
#' Maps measurement noise for (1) Policy Rate and (2) Forward Rate.
#' If sigma_fwd is missing, it defaults to 0.
build_M_taylor <- function(model_params) {
  
  # 1. Extract Policy Rate noise (Required)
  s_policy <- as.numeric(model_params$sigma_policy)
  s_spf <- as.numeric(model_params$sigma_spf)
  
  # 2. Extract Forward Rate noise with a safety default
  # Checks if the name exists and is not NULL
  if ("sigma_fwd" %in% names(model_params) && !is.null(model_params$sigma_fwd)) {
    s_fwd <- as.numeric(model_params$sigma_fwd)
  } else {
    s_fwd <- 1e-9  # small bit of variance instead of 0 so the filter can update
  }
  
  if(is.null(model_params$sigma_spf)) {
    print("ERROR IN SPF VARIANCE")
  }
  
  # 3. Construct the 2x2 matrix
  # Dimensions must match the Y data: [Policy Rate, Forward Rate]
  M <- matrix(0, 3, 3)
  
  M[1, 1] <- s_policy  # Variance of Taylor Rule residual
  M[2, 2] <- s_spf   # Variance of Interest rate SPF measurement
  M[3, 3] <- s_fwd   # Variance of Forward Rate measurement
  
  return(M)
}

#' Build the N Matrix (Process Noise Covariance)
build_N_taylor <- function(model_params, default_sig = 0.01) {
  # Extract state innovation standard deviations
  # xi_i: natural rate, xi_tp_trend: trend TP, xi_tp_cycl: cyclical TP
  xi_i        <- if(!is.null(model_params$xi_i)) model_params$xi_i else default_sig
  xi_tp_trend <- if(!is.null(model_params$xi_tp_trend)) model_params$xi_tp_trend else default_sig
  xi_tp_cycl  <- if(!is.null(model_params$xi_tp_cycl)) model_params$xi_tp_cycl else default_sig
  
  N <- diag(c(xi_i, xi_tp_trend, xi_tp_cycl), 3, 3)
  
  return(N)
}


# --- The External Data Matrix ---
build_mu_t_taylor <- function(model_params, X_data) {
  
  # Ensure parameters are numeric scalars
  rho      <- as.numeric(model_params$rho)
  gamma_pi <- as.numeric(model_params$gamma_pi)
  gamma_y  <- as.numeric(model_params$gamma_y)
  
  # Ensure X_data columns are numeric vectors
  # Using drop = FALSE or as.numeric ensures we don't have a list-column issue
  lag_r   <- as.numeric(X_data[, "lag_rate"])
  gdp_g   <- as.numeric(X_data[, "gdp_gap"])
  inf_g   <- as.numeric(X_data[, "inf_gap"])
  
  # The Taylor Rule Exogenous Component
  # Formula: rho*i(t-1) + (1-rho)*(gamma_y*y_gap + gamma_pi*pi_gap)
  ex_comp <- (rho * lag_r) + 
    ((1 - rho) * (gamma_y * gdp_g + gamma_pi * inf_g))
  
  # Measurement 1: Policy Rate (i_t)
  # Measurement 2: Forward Rate (5y5y)
  return(cbind(ex_comp, rep(0, nrow(X_data)), rep(0, nrow(X_data))))
}


#' Initialize Taylor Rule State-Space Model
#'
#' @description
#' Bundles data, parameters, and builders for Model 3 as defined in 0_technical_note.pdf.
#' Variables include the natural rate (i_bar), term premium trend (TP_bar), 
#' and term premium cycle (TP_tilde).
initialize_taylor_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # 1. Define Required Parameters for Taylor Rule and Term Premium
  # gamma_pi: Response to inflation gap
  # gamma_y: Response to output gap
  # rho: Interest rate smoothing
  # rho_tp: Persistence of cyclical term premium
  required_params <- c("gamma_pi", "gamma_y", "rho", "rho_tp", 
                       "sigma_policy", "sigma_fwd", "sigma_spf", "xi_i", "xi_tp_bar", "xi_tp_cycl")
  
  if (!all(required_params %in% names(parameter_guesses))) {
    stop("Missing required parameters for Taylor SSM (Model 3)!")
  }
  
  # 2. Build the Manifest
  # Rules: 0 = Linear, 1 = Exponential (>0), 2 = Logit (0 to 1)
  manifest <- list(
    # Taylor Rule Coefficients (Unconstrained or slightly positive)
    gamma_pi    = list(val = parameter_guesses$gamma_pi, rule = 1),
    gamma_y     = list(val = parameter_guesses$gamma_y,  rule = 1),
    
    # Smoothing and Persistence (Bounded 0-1 for stability)
    rho         = list(val = parameter_guesses$rho,     rule = 2),
    rho_tp      = list(val = parameter_guesses$rho_tp,  rule = 2),
    
    # Measurement Noise (Must be positive) 
    # sigma_i: Policy rate noise | sigma_fwd: Forward rate noise
    sigma_policy     = list(val = parameter_guesses$sigma_policy,   rule = 1),
    sigma_fwd   = list(val = parameter_guesses$sigma_fwd, rule = 1),
    sigma_spf    = list(val = parameter_guesses$sigma_spf, rule = 1),
    
    # State Innovation Noise (Must be positive)
    # ibar: Natural rate | tp_bar: TP trend | tp_tilde: TP cycle
    xi_i     = list(val = parameter_guesses$xi_i,     rule = 1),
    xi_tp_bar   = list(val = parameter_guesses$xi_tp_bar,   rule = 1),
    xi_tp_cycl = list(val = parameter_guesses$xi_tp_cycl, rule = 1)
  )
  
  # 3. Handle Optional SPF Anchors (Optional Specifications) 
  if ("sigma_spf3m" %in% names(parameter_guesses)) {
    manifest$sigma_spf3m <- list(val = parameter_guesses$sigma_spf3m, rule = 1)
  }
  
  if ("sigma_spf12m" %in% names(parameter_guesses)) {
    manifest$sigma_spf12m <- list(val = parameter_guesses$sigma_spf12m, rule = 1)
  }
  
  # 4. Bundle Blueprint
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), # Should contain policy rate, forward rate
      X = as.matrix(X_data)  # Should contain inf_gap, gdp_gap, and lag_rate
    ),
    manifest = manifest,
    builders = list(
      mu_t = build_mu_t_taylor, 
      H    = build_H_taylor,   
      G    = build_G_taylor,   
      M    = build_M_taylor,  
      N    = build_N_taylor   
    ),
    name = "TAYLOR",
    rho_guess = c(0.1, 0.1, 0.1),
    sigma_guess = c(10)
  )
  
  return(ssm)
}
