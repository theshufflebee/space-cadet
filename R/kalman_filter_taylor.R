################################################################################
#
# Taylor Rule Kalman FIlter
#
################################################################################

# REMARK: All Documentation done with AI


#' Kalman Filter for the Taylor Rule State-Space Model
#'
#' Evaluates the log-likelihood and calculates filtered state trajectories for the
#' Taylor rule state-space model with lagged observable autoregression.
#'
#' @details
#' The state-space model is formulated by the following core system equations:
#'
#' \bold{State (Transition) Equation:}
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t, \quad \eta_t \sim \mathcal{N}(0, Q_t)}
#' where \eqn{Q_t = N N^\top}.
#'
#' \bold{Observation (Measurement) Equation:}
#' \deqn{Y_t = \mu_t + \Phi Y_{t-1} + G \rho_t + \varepsilon_t, \quad \varepsilon_t \sim \mathcal{N}(0, R_t)}
#' where \eqn{\Phi = \texttt{ar\_mat}} is the autoregressive coefficient matrix on lagged observables,
#' and \eqn{R_t = M M^\top}.
#'
#' @param Y_t Matrix (\eqn{T \times n_y}) of observed series.
#' @param nu_t Matrix (\eqn{T \times n_\rho}) of state intercept/deterministic terms.
#' @param H Matrix (\eqn{n_\rho \times n_\rho}) state transition matrix.
#' @param N Matrix (\eqn{n_\rho \times \dots}) defining state shock covariance \eqn{Q = N N^\top}.
#' @param mu_t Matrix (\eqn{T \times n_y}) of exogenous measurement intercepts.
#' @param G Matrix (\eqn{n_y \times n_\rho}) observation mapping matrix.
#' @param M Matrix (\eqn{n_y \times \dots}) defining measurement error covariance \eqn{R = M M^\top}.
#' @param ar_mat Matrix (\eqn{n_y \times n_y}) autoregressive coefficient matrix \eqn{\Phi} on lagged \eqn{Y_{t-1}}.
#' @param Sigma_0 Initial state covariance matrix (\eqn{n_\rho \times n_\rho}).
#' @param rho_0 Initial state vector (\eqn{n_\rho \times 1}).
#' @param indic_pos Vector indicating non-negativity constraints on state variables. Defaults to \code{0}.
#' @param Rfunction Function returning measurement noise covariance \eqn{R_t}. Defaults to \code{Rf}.
#' @param Qfunction Function returning state noise covariance \eqn{Q_t}. Defaults to \code{Qf}.
#' @param reconciliationf Optional reconciliation function applied after filtering update. Defaults to identity.
#'
#' @return A list containing filtered states (\code{r}), posterior covariances (\code{Sigma_tt}),
#'   total log-likelihood (\code{loglik}), one-step-ahead forecasts (\code{y_tp1_t}), and fitted values (\code{fitted.obs}).
#'
#' @export
kalman_filter_taylor <- function(Y_t, nu_t, H, N, mu_t, G, M, ar_mat, Sigma_0, rho_0,
                               indic_pos = 0,
                               Rfunction = Rf, Qfunction = Qf,
                               reconciliationf = function(x, opt) { x }) {
  ny <- NCOL(Y_t) 
  nr <- NCOL(G)   
  T  <- NROW(Y_t)
  
  loglik.vector <- NULL
  
  rho_tt      <- matrix(0, T, nr)
  rho_tp1_t   <- matrix(0, T, nr)
  y_tp1_t     <- matrix(0, T, ny)

  fitted.obs  <- matrix(0, T, ny)   # MOD TAYLOR: Initialize fitted.obs matrix here
  
  Sigma_tt    <- matrix(0, T, nr*nr)
  Sigma_tp1_t <- matrix(0, T, nr*nr)
  Omega_tt    <- matrix(0, T, ny*ny)
  Omega_tp1_t <- matrix(0, T, ny*ny)
  
  # Initial logl constant 
  logl <- -ny * T / 2 * log(2 * pi)
  
  for (t in 1:T) {
    
    # ==========================================================================
    # Forecasting step (between t-1 and t):
    # ==========================================================================
    if (t == 1) {
      rho_tp1_t[1, ] <- nu_t[1, ] + t(H %*% rho_0)
      R              <- Rfunction(M, rho_0) 
      Q              <- Qfunction(N, rho_0) 
      aux_Sigma_tp1_t <- Q + H %*% Sigma_0 %*% t(H)
      
      # MOD TAYLOR: Initialize the conditional lag vector for t=1
      # We assume the lag is 0 -> maybe change in future
      current_Y_lag <- matrix(0, nrow = ny, ncol = 1)
      
    } else {
      rho_tp1_t[t, ] <- nu_t[t, ] + t(H %*% rho_tt[t-1, ])
      R              <- Rfunction(M, rho_tt[t-1, ], t)
      Q              <- Qfunction(N, rho_tt[t-1, ], t)
      aux_Sigma_tp1_t <- Q + H %*% matrix(Sigma_tt[t-1, ], nr, nr) %*% t(H)
      
      # MOD TAYLOR: Extract last periods observed vector
      current_Y_lag <- matrix(Y_t[t-1, ], nrow = ny, ncol = 1)
      
      na_indices_Y_lag <- which(is.na(current_Y_lag))
      
      if (length(na_indices_Y_lag) > 0) {
        # Dynamically patch only the elements that are NA 
        # using the corresponding columns from last period's fitted tracking vector
        current_Y_lag[na_indices_Y_lag, 1] <- fitted.obs[t-1, na_indices_Y_lag]
      }
    }
    
    if (sum(indic_pos == 1) > 0) {
      rho_tp1_t[t, indic_pos == 1] <- pmax(rho_tp1_t[t, indic_pos == 1], 0)
    }

    
    # Project forward
    # MOD TAYLOR -> Added AR Matrix times Y_t_1  as forecasting step
    y_tp1_t[t, ]     <- mu_t[t, ] + t(G %*% matrix(rho_tp1_t[t, ], ncol = 1)) + t(ar_mat %*% current_Y_lag)
    Sigma_tp1_t[t, ] <- matrix(aux_Sigma_tp1_t, 1, nr*nr)
    
    omega           <- R + G %*% aux_Sigma_tp1_t %*% t(G)
    Omega_tp1_t[t, ] <- matrix(omega, 1, ny*ny)
    
    # ==========================================================================
    # Updating step
    # ==========================================================================
    vec.obs.indices <- which(!is.na(Y_t[t, ]))
    ny.aux          <- length(c(vec.obs.indices))
    
    if (ny.aux > 0) {
      G.aux <- matrix(G[vec.obs.indices, ], nrow = ny.aux)
      R.aux <- R[vec.obs.indices, vec.obs.indices, drop = FALSE]
      omega.aux <- omega[vec.obs.indices, vec.obs.indices, drop = FALSE]
      
      K <- aux_Sigma_tp1_t %*% t(G.aux) %*% MASS::ginv(R.aux + G.aux %*% aux_Sigma_tp1_t %*% t(G.aux))
      
      lambda_t       <- Y_t[t, ] - y_tp1_t[t, ]
      lambda_t       <- matrix(lambda_t[vec.obs.indices], ncol = 1)
      rho_tt[t, ]    <- t(rho_tp1_t[t, ] + K %*% lambda_t)
      
      if (sum(indic_pos == 1) > 0) {
        rho_tt[t, indic_pos == 1] <- pmax(rho_tt[t, indic_pos == 1], 0)
      }
      
      Id             <- diag(1, nrow = nr, ncol = nr)
      Sigma_tt[t, ]  <- matrix((Id - K %*% G.aux) %*% aux_Sigma_tp1_t, 1, nr*nr)
      
      if (length(c(omega.aux)) == 1) {
        det.omega <- omega.aux
      } else {
        det.omega <- det(omega.aux)
      }
      
      y2Bfitted  <- matrix(Y_t[t, ], ncol = 1)
      constant   <- matrix(mu_t[t, ], ncol = 1) # Restored mu_t constant linkage
      Rho_tt_1   <- rho_tp1_t[t, ]
      opt        <- list(y2Bfitted, constant, G, M, Rho_tt_1, Q)
      rho_tt[t, ] <- reconciliationf(rho_tt[t, ], opt)
      
      loglik.step <- -ny.aux / 2 * log(2 * pi) - 1 / 2 * (log(det.omega) + 
                                                            t(lambda_t) %*% MASS::ginv(omega.aux) %*% lambda_t)
      
      loglik.vector <- rbind(loglik.vector, loglik.step)
      logl          <- logl + loglik.step
      
    } else {
      rho_tt[t, ]     <- t(rho_tp1_t[t, ])
      Sigma_tt[t, ]   <- matrix(aux_Sigma_tp1_t, 1, nr*nr)
      loglik.vector  <- rbind(loglik.vector, 0)
    }
    # MOD TAYLOR added AR mat here
    # Also moved it into the loop as we cant calculate it statically at the end anymore
    fitted.obs[t, ] <- mu_t[t, ] + t(G %*% matrix(rho_tt[t, ], ncol = 1)) + t(ar_mat %*% current_Y_lag)
  }
  
  # fitted.obs <- mu_t + rho_tt %*% t(G) + ar_mat * Y_t_1
  
  output <- list(r = rho_tt, Sigma_tt = Sigma_tt, loglik = logl, y_tp1_t = y_tp1_t,
                 S_tp1_t = Sigma_tp1_t, r_tp1_t = rho_tp1_t,
                 loglik.vector = loglik.vector, Omega_tp1_t = Omega_tp1_t, M = M,
                 fitted.obs = fitted.obs)
  
  return(output)
}

