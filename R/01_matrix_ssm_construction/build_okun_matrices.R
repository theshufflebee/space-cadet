################################################################################
#
# Matrices Builders for the Okun Model and SSM Object
#
################################################################################

# REMARK: All Documentation done with AI


#' Build State Transition Matrix (H) for Okun SSM
#'
#' Constructs the \eqn{2 \times 2} transition matrix \eqn{H} mapping the latent state vector
#' \eqn{\rho_t = [\bar{u}_t, \tilde{u}_t]^\top} in the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t}
#'
#' @details
#' Matrix representation:
#' \deqn{H = \begin{bmatrix} 1 & 0 \\ 0 & \phi \end{bmatrix}}
#' where \eqn{\bar{u}_t} follows a pure random walk and \eqn{\tilde{u}_t} follows an AR(1) process with persistence \eqn{\phi}.
#'
#' @param model_params A named list containing structural model parameters, including \code{phi}.
#'
#' @return A \eqn{2 \times 2} numeric matrix \eqn{H}.
#'
#' @export
build_H <- function(model_params) {
  phi_val <- if(!is.null(model_params$phi)) as.numeric(model_params$phi) else 0.0
  
  # Row 1: Trend(t) = 1*Trend(t-1) + 0*Cycle(t-1)
  # Row 2: Cycle(t) = 0*Trend(t-1) + phi*Cycle(t-1)
  H <- matrix(c(1,       0,
                0, phi_val), nrow = 2, ncol = 2, byrow = TRUE)
  return(H)
}


#' Build Observation Factor Loadings Matrix (G) for Okun SSM
#'
#' Constructs the \eqn{2 \times 2} measurement mapping matrix \eqn{G} linking the latent state vector
#' \eqn{\rho_t = [\bar{u}_t, \tilde{u}_t]^\top} to observed series \eqn{Y_t = [u_t, u^{\text{SPF, 5y}}_t]^\top} in the observation equation:
#' \deqn{Y_t = \mu_t + G \rho_t + \varepsilon_t}
#'
#' @details
#' Matrix representation:
#' \deqn{G = \begin{bmatrix} 1 & 1 \\ 1 & 0 \end{bmatrix}}
#' where observed headline unemployment (\eqn{u_t}) loads on both the natural rate trend (\eqn{\bar{u}_t})
#' and the unemployment cycle (\eqn{\tilde{u}_t}), while long-term 5-year SPF expectations (\eqn{u^{\text{SPF, 5y}}_t})
#' load exclusively on the natural rate trend (\eqn{\bar{u}_t}).
#'
#' @return A \eqn{2 \times 2} numeric matrix \eqn{G}.
#'
#' @export
build_G <- function() {
  # Row 1 (Observed Unemployment): Loads 1-to-1 on Trend AND Cycle
  # Row 2 (SPF 5Y Forecast): Loads 1-to-1 ONLY on Trend
  G <- matrix(c(1, 1,
                1, 0), nrow = 2, ncol = 2, byrow = TRUE)
  return(G)
}


#' Build Measurement Noise Factor Matrix (M) for Okun SSM
#'
#' Constructs the \eqn{2 \times 2} diagonal scaling matrix \eqn{M} defining the measurement error
#' covariance \eqn{R = M M^\top} in the observation equation:
#' \deqn{Y_t = \mu_t + G \rho_t + \varepsilon_t, \quad \varepsilon_t \sim \mathcal{N}(0, R)}
#'
#' @details
#' Matrix representation:
#' \deqn{M = \begin{bmatrix} \sigma_u & 0 \\ 0 & \sigma_{\text{SPF}} \end{bmatrix}}
#' resulting in:
#' \deqn{R = M M^\top = \begin{bmatrix} \sigma_u^2 & 0 \\ 0 & \sigma_{\text{SPF}}^2 \end{bmatrix}}
#'
#' @param model_params A named list containing \code{sigma_unemp} and \code{sigma_spf_5y_unemp}.
#' @param Y_data A matrix or data frame of observed series \eqn{Y_t}.
#'
#' @return A \eqn{2 \times 2} numeric diagonal matrix \eqn{M}.
#'
#' @export
build_M <- function(model_params, Y_data) {
  if (is.null(model_params$sigma_spf_5y_unemp) ||is.null(model_params$sigma_unemp)) {
    warning("Missing measurement parameters in model_params!")
  }
  
  M <- diag(c(as.numeric(model_params$sigma_unemp), as.numeric(model_params$sigma_spf_5y_unemp)), 
            nrow = ncol(Y_data), ncol = ncol(Y_data))
  return(M)
}


#' Build Process Noise Factor Matrix (N) for Okun SSM
#'
#' Constructs the \eqn{2 \times 2} diagonal standard deviation factor matrix \eqn{N} defining
#' the state innovation covariance matrix \eqn{Q = N N^\top} in the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t, \quad \eta_t \sim \mathcal{N}(0, Q)}
#'
#' @details
#' Matrix representation:
#' \deqn{N = \begin{bmatrix} \xi_{\text{trend}} & 0 \\ 0 & \xi_{\text{cycle}} \end{bmatrix}}
#' resulting in:
#' \deqn{Q = N N^\top = \begin{bmatrix} \xi_{\text{trend}}^2 & 0 \\ 0 & \xi_{\text{cycle}}^2 \end{bmatrix}}
#'
#' @param model_params A named list containing state shock standard deviations \code{xi_trend} and \code{xi_cycle}.
#'
#' @return A \eqn{2 \times 2} numeric diagonal matrix \eqn{N}.
#'
#' @export
build_N <- function(model_params) {
  if (is.null(model_params$xi_trend) || is.null(model_params$xi_cycle)) {
    warning("Missing process noise parameters in build_N!")
  }
  
  N <- diag(c(as.numeric(model_params$xi_trend), as.numeric(model_params$xi_cycle)), 
            nrow = 2, ncol = 2)
  return(N)
}


#' Build Exogenous Transition Intercept Matrix (nu_t) for Okun SSM
#'
#' Constructs the \eqn{T \times 2} time-varying exogenous intercept matrix \eqn{\nu_t} driving cyclical
#' dynamics via Okun's Law in the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t}
#'
#' @details
#' Matrix representation:
#' \deqn{\nu_t = \begin{bmatrix} 0 & X_t \beta \end{bmatrix} = \begin{bmatrix} 0 & \beta_1 y^{\text{gap}}_t + \beta_2 y^{\text{gap}}_{t-1} + \beta_3 y^{\text{gap}}_{t-2} \end{bmatrix}}
#' across each row \eqn{t = 1, \dots, T}.
#'
#' @param model_params A named list containing Okun's law elasticity coefficients (\code{beta1}, \code{beta2}, \code{beta3}).
#' @param X A \eqn{T \times 3} matrix of contemporaneous and lagged output gap regressors.
#'
#' @return A \eqn{T \times 2} numeric matrix \eqn{\nu_t}.
#'
#' @export
build_nu_t <- function(model_params, X) {
  beta_names <- grep("^beta", names(model_params), value = TRUE)
  betas      <- unlist(model_params[beta_names])
  
  # Structural cyclical impact of GDP gaps (T x 1 vector)
  cyc_drift <- X %*% matrix(betas, ncol = 1)
  
  # Row t returns: [0 for Trend, cyc_drift for Cycle]
  return(cbind(rep(0, nrow(X)), cyc_drift))
}


#' Build Exogenous Observation Intercept Matrix (mu_t) for Okun SSM
#'
#' Constructs the \eqn{T \times 2} exogenous intercept matrix \eqn{\mu_t} for the observation equation:
#' \deqn{Y_t = \mu_t + G \rho_t + \varepsilon_t}
#'
#' @details
#' Matrix representation:
#' \deqn{\mu_t = \begin{bmatrix} 0 & 0 \end{bmatrix}}
#' for all \eqn{t = 1, \dots, T}.
#'
#' @param model_params A named list of structural model parameters.
#' @param X A \eqn{T \times k} matrix of exogenous regressors used to determine the row dimension \eqn{T}.
#'
#' @return A \eqn{T \times 2} numeric matrix of zeros.
#'
#' @export
build_mu_t <- function(model_params, X) {
  # Returns a T x 2 matrix filled entirely with zeros
  return(matrix(0, nrow = nrow(X), ncol = 2))
}


################################################################################
#
# OKUN SSM OBJECT
#
################################################################################

#' Initialize Okun Law State-Space Model Blueprint
#'
#' Constructs the structural blueprint and parameter manifest for the bivariate Okun's Law
#' state-space model used in Kalman filter evaluation and parameter estimation.
#'
#' @details
#' The underlying state-space model is formulated by the following system equations:
#'
#' \bold{State (Transition) Equation:}
#' \deqn{
#' \begin{bmatrix} \bar{u}_t \\ \tilde{u}_t \end{bmatrix} = 
#' \begin{bmatrix} 0 \\ \beta_1 y_t^{\text{gap}} + \beta_2 y_{t-1}^{\text{gap}} + \beta_3 y_{t-2}^{\text{gap}} \end{bmatrix} + 
#' \begin{bmatrix} 1 & 0 \\ 0 & \phi \end{bmatrix} 
#' \begin{bmatrix} \bar{u}_{t-1} \\ \tilde{u}_{t-1} \end{bmatrix} + 
#' \begin{bmatrix} \eta_t^{\text{trend}} \\ \eta_t^{\text{cycle}} \end{bmatrix}
#' }
#' where:
#' \deqn{
#' \eta_t \sim \mathcal{N}\left(\mathbf{0}, \, Q = N N^\top = \begin{bmatrix} \xi_{\text{trend}}^2 & 0 \\ 0 & \xi_{\text{cycle}}^2 \end{bmatrix}\right)
#' }
#'
#' \bold{Observation (Measurement) Equation:}
#' \deqn{
#' \begin{bmatrix} u_t \\ u_t^{\text{SPF, 5y}} \end{bmatrix} = 
#' \begin{bmatrix} 0 \\ 0 \end{bmatrix} + 
#' \begin{bmatrix} 1 & 1 \\ 1 & 0 \end{bmatrix} 
#' \begin{bmatrix} \bar{u}_t \\ \tilde{u}_t \end{bmatrix} + 
#' \begin{bmatrix} \varepsilon_t^u \\ \varepsilon_t^{\text{SPF}} \end{bmatrix}
#' }
#' where:
#' \deqn{
#' \varepsilon_t \sim \mathcal{N}\left(\mathbf{0}, \, R = M M^\top = \begin{bmatrix} \sigma_u^2 & 0 \\ 0 & \sigma_{\text{SPF}}^2 \end{bmatrix}\right)
#' }
#'
#' @param Y_data A \eqn{T \times 2} matrix containing headline unemployment (\eqn{u_t}) and 5-year SPF expectations (\eqn{u_t^{\text{SPF, 5y}}}).
#' @param X_data A \eqn{T \times 3} matrix containing contemporaneous and distributed lags of the HP output gap (\eqn{y_t^{\text{gap}}, y_{t-1}^{\text{gap}}, y_{t-2}^{\text{gap}}}).
#' @param parameter_guesses A named list of initial structural parameters and priors:
#'   \itemize{
#'     \item \code{beta1}, \code{beta2}, \code{beta3}: Okun's law elasticity coefficients (unconstrained linear).
#'     \item \code{phi}: Unemployment cycle persistence (bounded in \eqn{[0.3, 0.75]}).
#'     \item \code{xi_trend}: Standard deviation of the natural rate shock (bounded in \eqn{[0.005, 0.025]}).
#'     \item \code{xi_cycle}: Standard deviation of the cyclical innovation (logit bounded in \eqn{[0, 1]}).
#'     \item \code{sigma_spf_5y_unemp}: Measurement standard deviation for SPF expectations (exponentially constrained \eqn{>0}).
#'     \item \code{sigma_unemp}: Measurement standard deviation for headline unemployment (bounded in \eqn{[0.2, 4.0]}).
#'     \item \code{state_init}: Prior state mean vector \eqn{\rho_0 = [\bar{u}_0, \tilde{u}_0]^\top}.
#'     \item \code{sigma_init}: Prior state covariance matrix \eqn{\Sigma_0}.
#'   }
#'
#' @return A structural state-space blueprint list containing:
#' \item{data}{Named list containing observation matrix \code{Y} and exogenous matrix \code{X}.}
#' \item{manifest}{Named list defining optimization transformations and parameter bounds.}
#' \item{builders}{List of builder functions for system matrices (\code{mu_t}, \code{nu_t}, \code{H}, \code{G}, \code{M}, \code{N}).}
#' \item{name}{Model identifier character string (\code{"okun"}).}
#' \item{rho_guess}{Initial state mean prior vector.}
#' \item{sigma_guess}{Initial state covariance prior matrix.}
#'
#' @seealso \code{\link{build_H}}, \code{\link{build_G}}, \code{\link{build_M}}, \code{\link{build_N}}, \code{\link{build_nu_t}}, \code{\link{build_mu_t}}
#'
#' @export
initialize_my_okun_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # All 8 required structural parameters must be present
  required_params <- c("beta1", "beta2", "beta3", "phi", 
                       "xi_trend", "xi_cycle", 
                       "sigma_spf_5y_unemp")
  
  missing_idx <- !(required_params %in% names(parameter_guesses))
  if (any(missing_idx)) {
    stop(paste("SSM OKUN INIZIALIZATION ERROR: Missing parameters:", 
               paste(required_params[missing_idx], collapse = ", ")))
  }
  
  # Build the parameter manifest mapping rules cleanly
  # Rule 0: Unconstrained Linear
  # Rule 1: exponential strictly positive
  # rule 2: logit between 0 and 1
  # rule 3: custom constraint
  manifest <- list(
    # Okun's Law Betas (Rule 0: Unconstrained Linear)
    beta1              = list(val = parameter_guesses$beta1, rule = 0),
    beta2              = list(val = parameter_guesses$beta2, rule = 0),
    beta3              = list(val = parameter_guesses$beta3, rule = 0),
    
    # Cyclical Persistence
    phi                = list(val = parameter_guesses$phi,
                              rule = 3,       
                              low  = 0.3,  
                              high = 0.75 
    ),
    
    # Process Noise Standard Deviations (Rule 1: Exponentiated to stay > 0)
    xi_trend           = list(val = parameter_guesses$xi_trend, 
                              rule = 3,       
                              low  = 0.005,   
                              high = 0.025    
    ),
    xi_cycle           = list(val = parameter_guesses$xi_cycle, rule = 2),
    
    # Measurement Noise Standard Deviations (Rule 1: Exponentiated to stay > 0)
    sigma_spf_5y_unemp = list(val = parameter_guesses$sigma_spf_5y_unemp,
                              rule = 1   
    ),
    sigma_unemp       = list(val = parameter_guesses$sigma_unemp,
                             rule = 3,       
                             low  = 0.2,   
                             high = 4 
  ))
  
  # Assemble structural blueprint bundle for the core filter
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), 
      X = as.matrix(X_data)
    ),
    manifest = manifest,
    builders = list(
      mu_t = build_mu_t,
      nu_t = build_nu_t,
      H    = build_H,
      G    = build_G,
      M    = build_M,
      N    = build_N
    ),
    name = "okun",
    # Initial state vector guesses: [Trend, Cycle]
    rho_guess   = parameter_guesses$state_init,
    # 2x2 Identity matrix flattened out to scale initial state variances safely
    sigma_guess = parameter_guesses$sigma_init 
  )
  
  return(ssm)
}
