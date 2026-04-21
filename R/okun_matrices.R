################################################################################
#
# OKUN MATRICES AND SSM OBJECT
#
################################################################################

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
  
  # 1. Dynamically identify all measurement noise parameters
  # This looks for any parameter starting with "sigma_" 
  # but excludes the state innovation "sigma_state_innov" (or similar)
  p_names <- grep("^sigma_", names(model_params), value = TRUE)

  if (length(p_names) == 0) {
    stop("build_M: No parameters starting with 'sigma_' found in model_params.")
  }
  
  # 2. Extract values
  # We use the names to ensure we maintain the correct order
  sigmas <- unlist(model_params[p_names])
  ny <- length(sigmas)
  
  # 3. Create diagonal matrix of variances (SD squared)
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
      G    = build_G,
      M    = build_M,
      N    = build_N
    )
  )
  
  return(ssm)
}
