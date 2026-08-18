################################################################################
#
# Matrices Builders for the Phillips Model and SSM Object
#
################################################################################

# REMARK: All Documentation done with AI


#' Build State Transition Matrix (H) for Phillips Curve SSM
#'
#' Constructs the \eqn{2 \times 2} transition matrix \eqn{H} mapping the latent state vector
#' \eqn{\rho_t = [\bar{\pi}_t, \tilde{\pi}_t]^\top} in the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t}
#'
#' @details
#' Matrix representation:
#' \deqn{H = \begin{bmatrix} 1 & 0 \\ 0 & \phi \end{bmatrix}}
#' where trend inflation \eqn{\bar{\pi}_t} follows a random walk and cyclical inflation \eqn{\tilde{\pi}_t}
#' follows a stationary AR(1) process governed by persistence parameter \eqn{\phi}.
#'
#' @param model_params A named list containing structural model parameters, including \code{phi}.
#'
#' @return A \eqn{2 \times 2} numeric matrix \eqn{H}.
#'
#' @export
build_H_philips <- function(model_params) {
  phi_val <- if(!is.null(model_params$phi)) as.numeric(model_params$phi) else 0.0
  
  # Row 1: Trend(t) = 1*Trend(t-1) + 0*Cycle(t-1)
  # Row 2: Cycle(t) = 0*Trend(t-1) + phi*Cycle(t-1)
  H <- matrix(c(1,       0,
                0, phi_val), nrow = 2, ncol = 2, byrow = TRUE)
  return(H)
}



#' Build Observation Factor Loadings Matrix (G) for Phillips Curve SSM
#'
#' Constructs the \eqn{2 \times 2} measurement mapping matrix \eqn{G} linking the latent state vector
#' \eqn{\rho_t = [\bar{\pi}_t, \tilde{\pi}_t]^\top} to observed series \eqn{Y_t = [\pi_t, \pi^{\text{SPF, 5y}}_t]^\top} in the observation equation:
#' \deqn{Y_t = \mu_t + G \rho_t + \varepsilon_t}
#'
#' @details
#' Matrix representation:
#' \deqn{G = \begin{bmatrix} 1 & 1 \\ 1 & 0 \end{bmatrix}}
#' where headline CPI inflation (\eqn{\pi_t}) loads on both trend inflation (\eqn{\bar{\pi}_t})
#' and the inflation cycle (\eqn{\tilde{\pi}_t}), while long-term 5-year SPF expectations (\eqn{\pi^{\text{SPF, 5y}}_t})
#' anchor exclusively on the unobserved trend (\eqn{\bar{\pi}_t}).
#'
#' @return A \eqn{2 \times 2} numeric matrix \eqn{G}.
#'
#' @export
build_G_philips <- function() {
  # Row 1 (Observed Inflation): Trend + Cycle
  # Row 2 (SPF Long-term Forecast): Trend ONLY
  G <- matrix(c(1, 1,
                1, 0), nrow = 2, ncol = 2, byrow = TRUE)
  return(G)
}


#' Build Measurement Noise Factor Matrix (M) for Phillips Curve SSM
#'
#' Constructs the \eqn{2 \times 2} diagonal scaling matrix \eqn{M} defining the measurement error
#' covariance \eqn{R = M M^\top} in the observation equation:
#' \deqn{Y_t = \mu_t + G \rho_t + \varepsilon_t, \quad \varepsilon_t \sim \mathcal{N}(0, R)}
#'
#' @details
#' Matrix representation:
#' \deqn{M = \begin{bmatrix} \sigma_{\text{cpi}} & 0 \\ 0 & \sigma_{\text{spf}} \end{bmatrix}}
#' resulting in:
#' \deqn{R = M M^\top = \begin{bmatrix} \sigma_{\text{cpi}}^2 & 0 \\ 0 & \sigma_{\text{spf}}^2 \end{bmatrix}}
#'
#' @param model_params A named list containing \code{sigma_cpi} and \code{sigma_spf}.
#' @param Y_data A matrix or data frame of observed series \eqn{Y_t}.
#'
#' @return A \eqn{2 \times 2} numeric diagonal matrix \eqn{M}.
#'
#' @export
build_M_philips <- function(model_params, Y_data) {
  ny <- ncol(Y_data)
  
  # Extract estimated SPF observation error scale
  sig_spf <- if(!is.null(model_params$sigma_spf)) as.numeric(model_params$sigma_spf) else 0.5
  sig_cpi <- if(!is.null(model_params$sigma_cpi)) as.numeric(model_params$sigma_cpi) else 0.5
  
  
  # Position 1: Observed inflation (Hardcoded baseline floor to isolate cycle process shocks)
  # Position 2: SPF Expectations (Estimated dynamically by the optimizer)
  sigmas <- c(sig_cpi, sig_spf)
  
  M <- diag(sigmas, nrow = ny, ncol = ny)
  colnames(M) <- rownames(M) <- c("sigma_cpi_fixed", "sigma_spf")
  return(M)
}



#' Build Process Noise Factor Matrix (N) for Phillips Curve SSM
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
#' @param default_sig_xi Numeric. Fallback value if variance parameters are absent. Defaults to \code{0.01}.
#'
#' @return A \eqn{2 \times 2} numeric diagonal matrix \eqn{N}.
#'
#' @export
build_N_philips <- function(model_params, default_sig_xi = 0.01) {
  sig_trend <- if(!is.null(model_params$xi_trend)) as.numeric(model_params$xi_trend) else default_sig_xi
  sig_cycle <- if(!is.null(model_params$xi_cycle)) as.numeric(model_params$xi_cycle) else default_sig_xi
  
  N <- diag(c(sig_trend, sig_cycle), nrow = 2, ncol = 2)
  return(N)
}

#' Build Exogenous Transition Intercept Matrix (nu_t) for Phillips Curve SSM
#'
#' Constructs the \eqn{T \times 2} time-varying exogenous intercept matrix \eqn{\nu_t} driving the
#' inflation cycle via demand and imported cost-push pressures in the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t}
#'
#' @details
#' Matrix representation:
#' \deqn{\nu_t = \begin{bmatrix} 0 & \beta_y \, y^{\text{gap}}_t + \psi_{\text{lop}} \, \text{gap}^{\text{lop}}_t \end{bmatrix}}
#' across each row \eqn{t = 1, \dots, T}.
#'
#' @param model_params A named list containing structural elasticities (\code{beta_y}, \code{psi_lop}).
#' @param X_data A \eqn{T \times 2} matrix containing \code{gdp_gap} and \code{lop_gap} series.
#'
#' @return A \eqn{T \times 2} numeric matrix \eqn{\nu_t}.
#'
#' @export
build_nu_t_philips <- function(model_params, X_data) {
  beta_y   <- as.numeric(model_params$beta_y)
  psi_lop  <- as.numeric(model_params$psi_lop)
  
  gdp_gap  <- as.numeric(X_data[, "gdp_gap"])
  lop_gap  <- as.numeric(X_data[, "lop_gap"])
  
  # Combined economic pressure shifting cyclical inflation
  cyc_drift <- (beta_y * gdp_gap) + (psi_lop * lop_gap)
  
  # Row t structural returns: [0 impact for Trend, cyc_drift impact for Cycle]
  return(cbind(rep(0, nrow(X_data)), cyc_drift))
}



#' Build Exogenous Observation Intercept Matrix (mu_t) for Phillips Curve SSM
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
#' @param X_data A matrix or data frame of exogenous regressors used to determine sample length \eqn{T}.
#'
#' @return A \eqn{T \times 2} numeric matrix of zeros.
#'
#' @export
build_mu_t_philips <- function(model_params, X_data) {
  return(matrix(0, nrow = nrow(X_data), ncol = 2))
}




#' Initialize Phillips Curve State-Space Model Blueprint
#'
#' Constructs the structural blueprint and parameter manifest for the bivariate Open-Economy
#' Phillips Curve state-space model used in Kalman filtering and maximum likelihood estimation.
#'
#' @details
#' The underlying state-space model is formulated by the following system equations:
#'
#' \bold{State (Transition) Equation:}
#' \deqn{
#' \begin{bmatrix} \bar{\pi}_t \\ \tilde{\pi}_t \end{bmatrix} = 
#' \begin{bmatrix} 0 \\ \beta_y \, y^{\text{gap}}_t + \psi_{\text{lop}} \, \text{gap}^{\text{lop}}_t \end{bmatrix} + 
#' \begin{bmatrix} 1 & 0 \\ 0 & \phi \end{bmatrix} 
#' \begin{bmatrix} \bar{\pi}_{t-1} \\ \tilde{\pi}_{t-1} \end{bmatrix} + 
#' \begin{bmatrix} \eta_t^{\text{trend}} \\ \eta_t^{\text{cycle}} \end{bmatrix}
#' }
#' where:
#' \deqn{
#' \eta_t \sim \mathcal{N}\left(\mathbf{0}, \, Q = N N^\top = \begin{bmatrix} \xi_{\text{trend}}^2 & 0 \\ 0 & \xi_{\text{cycle}}^2 \end{bmatrix}\right)
#' }
#'
#' \bold{Observation (Measurement) Equation:}
#' \deqn{
#' \begin{bmatrix} \pi_t \\ \pi_t^{\text{SPF, 5y}} \end{bmatrix} = 
#' \begin{bmatrix} 0 \\ 0 \end{bmatrix} + 
#' \begin{bmatrix} 1 & 1 \\ 1 & 0 \end{bmatrix} 
#' \begin{bmatrix} \bar{\pi}_t \\ \tilde{\pi}_t \end{bmatrix} + 
#' \begin{bmatrix} \varepsilon_t^{\text{cpi}} \\ \varepsilon_t^{\text{spf}} \end{bmatrix}
#' }
#' where:
#' \deqn{
#' \varepsilon_t \sim \mathcal{N}\left(\mathbf{0}, \, R = M M^\top = \begin{bmatrix} \sigma_{\text{cpi}}^2 & 0 \\ 0 & \sigma_{\text{spf}}^2 \end{bmatrix}\right)
#' }
#'
#' @param Y_data A \eqn{T \times 2} matrix containing headline CPI inflation (\eqn{\pi_t}) and 5-year SPF expectations (\eqn{\pi_t^{\text{SPF, 5y}}}).
#' @param X_data A \eqn{T \times 2} matrix containing cyclical output gaps (\code{gdp_gap}) and Law of One Price gaps (\code{lop_gap}).
#' @param parameter_guesses A named list of initial structural parameters and priors:
#'   \itemize{
#'     \item \code{beta_y}: Output gap elasticity coefficient (exponentially constrained \eqn{>0}).
#'     \item \code{psi_lop}: Law of One Price pass-through elasticity (unconstrained linear).
#'     \item \code{phi}: Cyclical inflation persistence (bounded in \eqn{[0.01, 0.70]}).
#'     \item \code{xi_trend}: Standard deviation of the trend inflation shock (bounded in \eqn{[0.01, 0.10]}).
#'     \item \code{xi_cycle}: Standard deviation of cyclical innovation (exponentially constrained \eqn{>0}).
#'     \item \code{sigma_spf}: Measurement standard deviation for SPF expectations (bounded in \eqn{[0.001, 4.0]}).
#'     \item \code{sigma_cpi}: Measurement standard deviation for headline inflation (bounded in \eqn{[0.1, 4.0]}).
#'   }
#'
#' @return A structural state-space blueprint list containing:
#' \item{data}{Named list containing observation matrix \code{Y} and exogenous matrix \code{X}.}
#' \item{manifest}{Named list defining optimization transformations and parameter bounds.}
#' \item{builders}{List of builder functions for system matrices (\code{mu_t}, \code{nu_t}, \code{H}, \code{G}, \code{M}, \code{N}).}
#' \item{name}{Model identifier character string (\code{"philips"}).}
#' \item{rho_guess}{Initial state mean prior vector (\code{c(2.0, 0.0)}).}
#' \item{sigma_guess}{Initial state covariance prior vector.}
#'
#' @seealso \code{\link{build_H_philips}}, \code{\link{build_G_philips}}, \code{\link{build_M_philips}}, \code{\link{build_N_philips}}, \code{\link{build_nu_t_philips}}, \code{\link{build_mu_t_philips}}
#'
#' @export
initialize_my_philips_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # 1. ENFORCE STRICT SELECTION: Validate all active structural coefficients
  required_params <- c("beta_y", "psi_lop", "phi", 
                       "xi_trend", "xi_cycle", "sigma_spf")
  
  missing_idx <- !(required_params %in% names(parameter_guesses))
  if (any(missing_idx)) {
    stop(paste("SSM PHILLIPS INIZIALIZATION ERROR: Missing parameters:", 
               paste(required_params[missing_idx], collapse = ", ")))
  }
  
  # 2. Build the structural optimization transformation constraints
  manifest <- list(
    # Exogenous Demand and Cost Shifters (Rule 0: Unconstrained Linear space)
    beta_y    = list(val = parameter_guesses$beta_y,  rule = 1),
    psi_lop   = list(val = parameter_guesses$psi_lop, rule = 0),
    
    # State Persistence Over Time (Rule 2: Logit transformation bounded safely between 0 and 1)
    phi       = list(val = parameter_guesses$phi,
                     rule = 3,       
                     low  = 0.01,   
                     high = 0.7),
    
    xi_trend           = list(val = parameter_guesses$xi_trend, 
                              rule = 3,       
                              low  = 0.01,   
                              high = 0.1
    ),
    xi_cycle           = list(val = parameter_guesses$xi_cycle, rule = 1),
    
    # Measurement Scale Variation (Rule 1: Exponentiated to map strictly > 0)
    sigma_spf = list(val = parameter_guesses$sigma_spf, rule = 3,
                     low = 0.001, # practically 0 -> avoid the =0 trap
                     high = 4),
    sigma_cpi = list(val = parameter_guesses$sigma_cpi, rule = 3,
                     low  = 0.1,   
                     high = 4)
  )
  
  # 3. Assemble complete structural framework capsule
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), 
      X = as.matrix(X_data)
    ),
    manifest = manifest,
    builders = list(
      mu_t   = build_mu_t_philips,
      nu_t   = build_nu_t_philips,
      H      = build_H_philips,
      G      = build_G_philips,
      M      = build_M_philips,
      N      = build_N_philips
    ),
    name = "philips",
    # Balanced initial guesses: Trend inflation assumes a ~2.0% structural target, cycle starts dead at 0%
    rho_guess   = c(2.0, 0.0), 
    # Clean 2x2 Identity mapping bounds for initial state spatial propagation
    sigma_guess = c(1, 0, 0, 1) 
  )
  
  return(ssm)
}
