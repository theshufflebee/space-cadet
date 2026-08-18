
################################################################################
#
# Taylor Rule / Policy Rate Model Forecast
#
################################################################################


# ==============================================================================
# --- Matrices ---
# ==============================================================================

#' Build State Transition Matrix (H) for Taylor Rule SSM
#'
#' Constructs the \eqn{3 \times 3} transition matrix \eqn{H} mapping the latent state vector
#' \eqn{\rho_t = [\bar{\imath}_t, \bar{\text{TP}}_t, \tilde{\text{TP}}_t]^\top} in the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t}
#'
#' @details
#' Matrix representation:
#' \deqn{H = \begin{bmatrix} 1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & \rho_{\text{tp}} \end{bmatrix}}
#' where the natural interest rate (\eqn{\bar{\imath}_t}) and the trend term premium (\eqn{\bar{\text{TP}}_t})
#' follow independent random walks, and the cyclical term premium (\eqn{\tilde{\text{TP}}_t}) follows
#' a stationary AR(1) process with persistence \eqn{\rho_{\text{tp}}}.
#'
#' @param model_params A named list containing structural model parameters, including \code{rho_tp}.
#'
#' @return A \eqn{3 \times 3} numeric matrix \eqn{H}.
#'
#' @export
build_H_taylor <- function(model_params) {
  # rho_tp determines the persistence of the cyclical term premium component
  rho_tp <- if(!is.null(model_params$rho_tp)) as.numeric(model_params$rho_tp) else 0.9
  
  H <- diag(3)      # Natural rate random walk // Trend term premium random walk [cite: 146, 159]
  H[3,3] <- rho_tp  # Cyclical term premium AR(1) [cite: 159]
  
  return(H)
}

#' Build Observation Factor Loadings Matrix (G) for Taylor Rule SSM
#'
#' Constructs the \eqn{2 \times 3} measurement mapping matrix \eqn{G} linking the latent state vector
#' \eqn{\rho_t = [\bar{\imath}_t, \bar{\text{TP}}_t, \tilde{\text{TP}}_t]^\top} to observed series \eqn{Y_t = [i_t, f_t]^\top} in the observation equation:
#' \deqn{Y_t = \mu_t + \Phi Y_{t-1} + G \rho_t + \varepsilon_t}
#'
#' @details
#' Matrix representation:
#' \deqn{G = \begin{bmatrix} 1 - \phi & 0 & 0 \\ 1 & 1 & 1 \end{bmatrix}}
#' where the policy rate (\eqn{i_t}) loads on the natural interest rate (\eqn{\bar{\imath}_t}) scaled by \eqn{(1 - \phi)},
#' and the long-term forward rate (\eqn{f_t}) loads unit-for-unit on the natural rate, trend term premium, and cyclical term premium.
#'
#' @param model_params A named list containing policy smoothing parameter \code{phi}.
#'
#' @return A \eqn{2 \times 3} numeric matrix \eqn{G}.
#'
#' @export
build_G_taylor <- function(model_params) {
  # phi is the interest rate smoothing parameter
  phi_val <- if(!is.null(model_params$phi)) as.numeric(model_params$phi) else 0.8
  
  G <- matrix(0, 2, 3)
  
  # Row 1: Policy Rate loads on natural rate i_t [cite: 134, 138]
  G[1,1] <- (1 - phi_val)
  
  # Row 2: Forward Rate loads on i_t + trend TP + cyclical TP [cite: 149, 158]
  G[2,1] <- 1
  G[2,2] <- 1
  G[2,3] <- 1
  
  return(G)
}

#' Build Measurement Noise Factor Matrix (M) for Taylor Rule SSM
#'
#' Constructs the \eqn{2 \times 2} diagonal scaling matrix \eqn{M} defining the measurement error
#' covariance \eqn{R = M M^\top} in the observation equation:
#' \deqn{Y_t = \mu_t + \Phi Y_{t-1} + G \rho_t + \varepsilon_t, \quad \varepsilon_t \sim \mathcal{N}(0, R)}
#'
#' @details
#' Matrix representation:
#' \deqn{M = \begin{bmatrix} \sigma_{\text{policy}} & 0 \\ 0 & \sigma_{\text{fwd}} \end{bmatrix}}
#' resulting in:
#' \deqn{R = M M^\top = \begin{bmatrix} \sigma_{\text{policy}}^2 & 0 \\ 0 & \sigma_{\text{fwd}}^2 \end{bmatrix}}
#'
#' @param model_params A named list containing \code{sigma_policy} and optionally \code{sigma_fwd}.
#' @param Y_data A matrix or data frame of observed series \eqn{Y_t}.
#'
#' @return A \eqn{2 \times 2} numeric diagonal matrix \eqn{M}.
#'
#' @export
build_M_taylor <- function(model_params, Y_data) {
  
  ny <- ncol(Y_data)
  
  s_policy <- as.numeric(model_params$sigma_policy)
  
  if ("sigma_fwd" %in% names(model_params) && !is.null(model_params$sigma_fwd)) {
    s_fwd <- as.numeric(model_params$sigma_fwd)
  } else {
    s_fwd <- 1e-4  # Small floor to preserve matrix inversion stability
  }
  
  M <- matrix(0, ny, ny)
  M[1, 1] <- s_policy  # Standard deviation of Taylor Rule residual [cite: 134]
  M[2, 2] <- s_fwd     # Standard deviation of Forward Rate measurement
  
  return(M)
}

#' Build Process Noise Factor Matrix (N) for Taylor Rule SSM
#'
#' Constructs the \eqn{3 \times 3} diagonal standard deviation factor matrix \eqn{N} defining
#' the state innovation covariance matrix \eqn{Q = N N^\top} in the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t, \quad \eta_t \sim \mathcal{N}(0, Q)}
#'
#' @details
#' Matrix representation:
#' \deqn{N = \begin{bmatrix} \xi_i & 0 & 0 \\ 0 & \xi_{\bar{\text{tp}}} & 0 \\ 0 & 0 & \xi_{\tilde{\text{tp}}} \end{bmatrix}}
#' resulting in:
#' \deqn{Q = N N^\top = \begin{bmatrix} \xi_i^2 & 0 & 0 \\ 0 & \xi_{\bar{\text{tp}}}^2 & 0 \\ 0 & 0 & \xi_{\tilde{\text{tp}}}^2 \end{bmatrix}}
#'
#' @param model_params A named list containing state shock standard deviations \code{xi_i}, \code{xi_tp_bar}, and \code{xi_tp_cycl}.
#' @param default_sig_trend Numeric. Fallback value for trend state standard deviations. Defaults to \code{0.1}.
#' @param default_sig_cycle Numeric. Fallback value for cyclical state standard deviations. Defaults to \code{0.5}.
#'
#' @return A \eqn{3 \times 3} numeric diagonal matrix \eqn{N}.
#'
#' @export
build_N_taylor <- function(model_params, default_sig_trend = 0.1, default_sig_cycle = 0.5) {
  xi_i        <- if(!is.null(model_params$xi_i)) as.numeric(model_params$xi_i) else default_sig_trend
  xi_tp_trend <- if(!is.null(model_params$xi_tp_bar)) as.numeric(model_params$xi_tp_bar) else default_sig_trend
  xi_tp_cycl  <- if(!is.null(model_params$xi_tp_cycl)) as.numeric(model_params$xi_tp_cycl) else default_sig_cycle
  
  N <- diag(c(xi_i, xi_tp_trend, xi_tp_cycl), 3, 3)
  return(N)
}

#' Build Exogenous Observation Intercept Matrix (mu_t) for Taylor Rule SSM
#'
#' Constructs the \eqn{T \times 2} time-varying exogenous intercept matrix \eqn{\mu_t} capturing the
#' unconstrained macro feedback response in the observation equation:
#' \deqn{Y_t = \mu_t + \Phi Y_{t-1} + G \rho_t + \varepsilon_t}
#'
#' @details
#' Matrix representation:
#' \deqn{\mu_t = \begin{bmatrix} (1 - \phi)(\gamma_y \, y^{\text{gap}}_t + \gamma_\pi \, \pi^{\text{gap}}_t) & 0 \end{bmatrix}}
#' across each row \eqn{t = 1, \dots, T}.
#'
#' @param model_params A named list containing structural coefficients (\code{gamma_pi}, \code{gamma_y}, \code{phi}).
#' @param X_data A \eqn{T \times 2} matrix containing \code{gdp_gap} and \code{inf_gap} series.
#'
#' @return A \eqn{T \times 2} numeric matrix \eqn{\mu_t}.
#'
#' @export
build_mu_t_taylor <- function(model_params, X_data) {
  gamma_pi <- as.numeric(model_params$gamma_pi)
  gamma_y  <- as.numeric(model_params$gamma_y)
  phi      <- as.numeric(model_params$phi)
  
  gdp_g <- as.numeric(X_data[, "gdp_gap"])
  inf_g <- as.numeric(X_data[, "inf_gap"])
  
  # Pure Taylor Rule Exogenous Component [cite: 134, 138]
  ex_comp <- ((1 - phi) * (gamma_y * gdp_g + gamma_pi * inf_g))
  
  # Row 1: Policy Rate Intercept, Row 2: Forward Rate Intercept (0)
  return(cbind(ex_comp, rep(0, nrow(X_data))))
}



#' Build Autoregressive Measurement Matrix (Phi) for Taylor Rule SSM
#'
#' Constructs the \eqn{2 \times 2} autoregressive coefficient matrix \eqn{\Phi = \texttt{ar\_mat}}
#' capturing interest rate inertia on lagged observables in the observation equation:
#' \deqn{Y_t = \mu_t + \Phi Y_{t-1} + G \rho_t + \varepsilon_t}
#'
#' @details
#' Matrix representation:
#' \deqn{\Phi = \begin{bmatrix} \phi & 0 \\ 0 & 0 \end{bmatrix}}
#' where policy inertia \eqn{\phi} operates exclusively on the lagged policy rate (\eqn{i_{t-1}}).
#'
#' @param model_params A named list containing the policy inertia coefficient \code{phi}.
#' @param Y_data A matrix or data frame of observed series \eqn{Y_t}.
#'
#' @return A \eqn{2 \times 2} numeric matrix \eqn{\Phi}.
#'
#' @export
build_AR_matrix <- function(model_params, Y_data) {
  phi <- as.numeric(model_params$phi)
  
  # Must be 2x2 to match the dimension of Y_t (Policy Rate, Forward Rate) 
  ar_mat <- matrix(0, nrow = 2, ncol = 2)
  ar_mat[1, 1] <- phi  # Map smoothing parameter exclusively to the policy rate [cite: 138]
  
  return(ar_mat)
}

#' Build Exogenous Transition Intercept Matrix (nu_t) for Taylor Rule SSM
#'
#' Constructs the \eqn{T \times 3} zero-drift matrix \eqn{\nu_t} for the state transition equation:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t}
#'
#' @details
#' Matrix representation:
#' \deqn{\nu_t = \begin{bmatrix} 0 & 0 & 0 \end{bmatrix}}
#' for all \eqn{t = 1, \dots, T}.
#'
#' @param model_params A named list of structural model parameters (unused).
#' @param X_data A \eqn{T \times k} matrix of exogenous regressors used to determine sample length \eqn{T}.
#'
#' @return A \eqn{T \times 3} numeric matrix of zeros.
#'
#' @export
build_nu_t_taylor <- function(model_params, X_data) {
  
  T_len <- nrow(X_data)
  
  # Structural state dimension is exactly 3 (i_bar, TP_bar, TP_tilde)
  nu_mat <- matrix(0, nrow = T_len, ncol = 3)
  
  return(nu_mat)
}



#' Initialize Taylor Rule State-Space Model Blueprint
#'
#' Constructs the structural blueprint and parameter manifest for the trivariate state,
#' bivariate observation Taylor Rule and Term Premium state-space model used in Kalman filtering
#' and parameter optimization.
#'
#' @details
#' The underlying state-space model is formulated by the following system equations:
#'
#' \bold{State (Transition) Equation:}
#' \deqn{
#' \begin{bmatrix} \bar{\imath}_t \\ \bar{\text{TP}}_t \\ \tilde{\text{TP}}_t \end{bmatrix} = 
#' \begin{bmatrix} 0 \\ 0 \\ 0 \end{bmatrix} + 
#' \begin{bmatrix} 1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & \rho_{\text{tp}} \end{bmatrix} 
#' \begin{bmatrix} \bar{\imath}_{t-1} \\ \bar{\text{TP}}_{t-1} \\ \tilde{\text{TP}}_{t-1} \end{bmatrix} + 
#' \begin{bmatrix} \eta_t^i \\ \eta_t^{\bar{\text{tp}}} \\ \eta_t^{\tilde{\text{tp}}} \end{bmatrix}
#' }
#' where:
#' \deqn{
#' \eta_t \sim \mathcal{N}\left(\mathbf{0}, \, Q = N N^\top = \begin{bmatrix} \xi_i^2 & 0 & 0 \\ 0 & \xi_{\bar{\text{tp}}}^2 & 0 \\ 0 & 0 & \xi_{\tilde{\text{tp}}}^2 \end{bmatrix}\right)
#' }
#'
#' \bold{Observation (Measurement) Equation:}
#' \deqn{
#' \begin{bmatrix} i_t \\ f_t \end{bmatrix} = 
#' \begin{bmatrix} (1 - \phi)(\gamma_y \, y_t^{\text{gap}} + \gamma_\pi \, \pi_t^{\text{gap}}) \\ 0 \end{bmatrix} + 
#' \begin{bmatrix} \phi & 0 \\ 0 & 0 \end{bmatrix} \begin{bmatrix} i_{t-1} \\ f_{t-1} \end{bmatrix} + 
#' \begin{bmatrix} 1 - \phi & 0 & 0 \\ 1 & 1 & 1 \end{bmatrix} 
#' \begin{bmatrix} \bar{\imath}_t \\ \bar{\text{TP}}_t \\ \tilde{\text{TP}}_t \end{bmatrix} + 
#' \begin{bmatrix} \varepsilon_t^{\text{policy}} \\ \varepsilon_t^{\text{fwd}} \end{bmatrix}
#' }
#' where:
#' \deqn{
#' \varepsilon_t \sim \mathcal{N}\left(\mathbf{0}, \, R = M M^\top = \begin{bmatrix} \sigma_{\text{policy}}^2 & 0 \\ 0 & \sigma_{\text{fwd}}^2 \end{bmatrix}\right)
#' }
#'
#' @param Y_data A \eqn{T \times 2} matrix containing the policy interest rate (\eqn{i_t}) and forward rate expectations (\eqn{f_t}).
#' @param X_data A \eqn{T \times 2} matrix containing the output gap (\code{gdp_gap}) and inflation gap (\code{inf_gap}).
#' @param parameter_guesses A named list of initial structural parameters and priors:
#'   \itemize{
#'     \item \code{gamma_pi}: Taylor rule inflation gap response coefficient (unconstrained linear).
#'     \item \code{gamma_y}: Taylor rule output gap response coefficient (unconstrained linear).
#'     \item \code{phi}: Policy rate smoothing persistence (bounded in \eqn{[0.60, 0.90]}).
#'     \item \code{rho_tp}: Cyclical term premium AR(1) persistence (bounded in \eqn{[0.01, 0.99]}).
#'     \item \code{sigma_policy}: Standard deviation of policy rate residual (exponentially constrained \eqn{>0}).
#'     \item \code{xi_i}: Standard deviation of natural interest rate shocks (bounded in \eqn{[0.01, 0.20]}).
#'     \item \code{xi_tp_bar}: Standard deviation of trend term premium innovations (bounded in \eqn{[0.01, 0.50]}).
#'     \item \code{xi_tp_cycl}: Standard deviation of cyclical term premium innovations (bounded in \eqn{[0.001, 0.50]}).
#'     \item \code{state_init}: Prior state mean vector \eqn{\rho_0 = [\bar{\imath}_0, \bar{\text{TP}}_0, \tilde{\text{TP}}_0]^\top}.
#'     \item \code{sigma_init}: Prior state covariance matrix \eqn{\Sigma_0}.
#'   }
#'
#' @return A structural state-space blueprint list containing:
#' \item{data}{Named list containing observation matrix \code{Y} and exogenous matrix \code{X}.}
#' \item{manifest}{Named list defining optimization transformations and parameter bounds.}
#' \item{builders}{List of builder functions for system matrices (\code{mu_t}, \code{nu_t}, \code{H}, \code{G}, \code{M}, \code{N}, \code{ar_mat}).}
#' \item{name}{Model identifier character string (\code{"taylor"}).}
#' \item{rho_guess}{Initial state mean prior vector.}
#' \item{sigma_guess}{Initial state covariance prior matrix.}
#'
#' @seealso \code{\link{build_H_taylor}}, \code{\link{build_G_taylor}}, \code{\link{build_M_taylor}}, \code{\link{build_N_taylor}}, \code{\link{build_mu_t_taylor}}, \code{\link{build_AR_matrix}}, \code{\link{build_nu_t_taylor}}
#'
#' @export
initialize_taylor_ssm <- function(Y_data, X_data, parameter_guesses) {
  
  # 1. Define Required Parameters for Taylor Rule and Term Premium [cite: 134, 138, 159]
  required_params <- c("gamma_pi", "gamma_y", "phi", "rho_tp", 
                       "sigma_policy", "xi_i", "xi_tp_bar", "xi_tp_cycl")
  
  if (!all(required_params %in% names(parameter_guesses))) {
    stop("SSM TAYLOR INIZIALIZATION ERROR: Missing parameters:")
  }
  
  # 2. Build the Manifest
  # Rule 1 = Exponential (>0), Rule 3 = Bounded Logistic (low to high)
  manifest <- list(
    gamma_pi     = list(val = parameter_guesses$gamma_pi, rule = 0),
    gamma_y      = list(val = parameter_guesses$gamma_y,  rule = 0),
    
    # Smoothing bounded strictly below 1 for dynamic stability
    phi          = list(val  = parameter_guesses$phi, 
                        rule = 3,
                        low = 0.6,
                        high = 0.90
    ),
    rho_tp       = list(val = parameter_guesses$rho_tp,  rule = 3, low = 0.01, high = 0.99),
    
    # Measurement Noise
    sigma_policy = list(val = parameter_guesses$sigma_policy, rule = 1),
    
    # State Innovation Noise 
    xi_i         = list(val = parameter_guesses$xi_i,         rule = 3,
                        low = 0.01,
                        high = 0.2),
    xi_tp_bar    = list(val = parameter_guesses$xi_tp_bar,    rule = 3,
                        low = 0.01,
                        high = 0.5),
    xi_tp_cycl   = list(val = parameter_guesses$xi_tp_cycl,   rule = 3,
                        low = 0.001,
                        high = 0.5
                        )
  )
  
  # Handle Optional SPF Anchors [cite: 165]
  if ("sigma_spf3m" %in% names(parameter_guesses)) {
    manifest$sigma_spf3m <- list(val = parameter_guesses$sigma_spf3m, rule = 1)
  }
  if ("sigma_spf12m" %in% names(parameter_guesses)) {
    manifest$sigma_spf12m <- list(val = parameter_guesses$sigma_spf12m, rule = 1)
  }
  
  # 3. Bundle Blueprint
  ssm <- list(
    data = list(
      Y = as.matrix(Y_data), # Column 1: Policy Rate, Column 2: Forward Rate 
      X = as.matrix(X_data)  # Column 1: inf_gap, Column 2: gdp_gap 
    ),
    manifest = manifest,
    builders = list(
      mu_t   = build_mu_t_taylor, 
      nu_t   = build_nu_t_taylor,  # Added to prevent NULL errors in log-likelihood loops
      H      = build_H_taylor,   
      G      = build_G_taylor,   
      M      = build_M_taylor,  
      N      = build_N_taylor,
      ar_mat = build_AR_matrix
    ),
    name = "taylor",
    rho_guess = parameter_guesses$state_init,
    sigma_guess = parameter_guesses$sigma_init
  )
  
  return(ssm)
}