################################################################################
#
# Base Kalman Filter
#
################################################################################

# REMARK: All Documentation done with AI


#' Core Kalman Filter for Linear State-Space Models
#'
#' Evaluates the log-likelihood objective function and extracts filtered state trajectories
#' via the Kalman filter prediction error decomposition for linear Gaussian state-space models.
#'
#' @details
#' The state-space model is formulated by the following core system equations:
#'
#' \bold{State (Transition) Equation:}
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + \eta_t, \quad \eta_t \sim \mathcal{N}(0, Q_t)}
#' where \eqn{Q_t = N N^\top}.
#'
#' \bold{Observation (Measurement) Equation:}
#' \deqn{Y_t = \mu_t + G \rho_t + \varepsilon_t, \quad \varepsilon_t \sim \mathcal{N}(0, R_t)}
#' where \eqn{R_t = M M^\top}.
#'
#' Missing observations in \eqn{Y_t} are handled dynamically via sub-matrix selection 
#' across the measurement update step.
#'
#' @param Y_t Matrix (\eqn{T \times n_y}) of observed series.
#' @param nu_t Matrix (\eqn{T \times n_\rho}) of state transition intercepts/deterministic shifts.
#' @param H Matrix (\eqn{n_\rho \times n_\rho}) state transition matrix.
#' @param N Matrix (\eqn{n_\rho \times \dots}) defining the state innovation covariance \eqn{Q = N N^\top}.
#' @param mu_t Matrix (\eqn{T \times n_y}) of exogenous observation intercepts.
#' @param G Matrix (\eqn{n_y \times n_\rho}) measurement mapping matrix.
#' @param M Matrix (\eqn{n_y \times \dots}) defining the measurement error covariance \eqn{R = M M^\top}.
#' @param Sigma_0 Matrix (\eqn{n_\rho \times n_\rho}) initial prior state covariance.
#' @param rho_0 Matrix (\eqn{n_\rho \times 1}) initial prior state vector.
#' @param indic_pos Vector or integer indicating indices of state variables subject to non-negativity constraints. Defaults to \code{0}.
#' @param Rfunction Function returning the measurement error covariance matrix \eqn{R_t}. Defaults to \code{Rf}.
#' @param Qfunction Function returning the state innovation covariance matrix \eqn{Q_t}. Defaults to \code{Qf}.
#' @param reconciliationf Optional reconciliation function applied after the measurement update step. Defaults to identity.
#'
#' @return A list containing:
#' \item{r}{Filtered posterior state trajectories (\eqn{\rho_{t|t}}).}
#' \item{Sigma_tt}{Flattened posterior state covariance matrices.}
#' \item{loglik}{Scalar cumulative log-likelihood value.}
#' \item{y_tp1_t}{Prior one-step-ahead measurement forecasts (\eqn{Y_{t|t-1}}).}
#' \item{S_tp1_t}{Flattened prior state covariance matrices (\eqn{\Sigma_{t|t-1}}).}
#' \item{r_tp1_t}{Prior one-step-ahead state forecasts (\eqn{\rho_{t|t-1}}).}
#' \item{loglik.vector}{Vector of step-by-step log-likelihood contributions.}
#' \item{Omega_tp1_t}{Flattened forecast error covariance matrices (\eqn{\Omega_{t|t-1}}).}
#' \item{M}{The measurement error factor matrix.}
#' \item{fitted.obs}{Fitted in-sample observation estimates (\eqn{\hat{Y}_{t|t}}).}
#'
#' @seealso \code{\link{Rf}}, \code{\link{Qf}}, \code{\link{kalman_filter_taylor}}
#'
#' @export
kalman_filter_core <- function(Y_t, nu_t, H, N, mu_t, G, M, Sigma_0, rho_0,
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
    } else {
      rho_tp1_t[t, ] <- nu_t[t, ] + t(H %*% rho_tt[t-1, ])
      R              <- Rfunction(M, rho_tt[t-1, ], t)
      Q              <- Qfunction(N, rho_tt[t-1, ], t)
      aux_Sigma_tp1_t <- Q + H %*% matrix(Sigma_tt[t-1, ], nr, nr) %*% t(H)
    }
    
    if (sum(indic_pos == 1) > 0) {
      rho_tp1_t[t, indic_pos == 1] <- pmax(rho_tp1_t[t, indic_pos == 1], 0)
    }
    
    # Restored mu_t adding step here (adding 0 retains structural logic)
    y_tp1_t[t, ]     <- mu_t[t, ] + t(G %*% matrix(rho_tp1_t[t, ], ncol = 1))
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
  }
  
  # Final fitted observables calculation incorporates mu_t
  fitted.obs <- mu_t + rho_tt %*% t(G)
  
  output <- list(r = rho_tt, Sigma_tt = Sigma_tt, loglik = logl, y_tp1_t = y_tp1_t,
                 S_tp1_t = Sigma_tp1_t, r_tp1_t = rho_tp1_t,
                 loglik.vector = loglik.vector, Omega_tp1_t = Omega_tp1_t, M = M,
                 fitted.obs = fitted.obs)
  return(output)
}


#' Compute Measurement Noise Covariance Matrix
#'
#' Evaluates the measurement error covariance matrix \eqn{R = M M^\top}.
Rf <- function(M,RHO,t=0){
  return(M %*% t(M))
}

#' Compute State Innovation Covariance Matrix
#'
#' Evaluates the state transition shock covariance matrix \eqn{Q = N N^\top}.
Qf <- function(N,RHO,t=0){
  return(N %*% t(N))
}









################################################################################################




#' Unified Negative Log-Likelihood Evaluator for State-Space Models
#'
#' Evaluates the objective function (negative log-likelihood) for numerical optimization
#' across supported structural model types (\code{"okun"}, \code{"philips"}, or \code{"taylor"}).
#' Maps unconstrained optimizer parameters into structural economic coefficients, constructs
#' state-space system matrices, and routes computation to the appropriate Kalman filter engine.
#'
#' @details
#' Unconstrained parameter vector \code{theta} is mapped to structural parameter bounds via
#' \code{\link{param2model_gen}}. Dynamic builders assemble system matrices (\eqn{\mu_t, H, M, N, \nu_t, G}),
#' routing to \code{\link{kalman_filter_taylor}} for Taylor rule models or \code{\link{kalman_filter_core}}
#' for Okun and Phillips curve models. If numerical instability or non-finite values occur, a large penalty
#' value (\eqn{10^{10}}) is returned.
#'
#' @param theta Numeric vector of unconstrained parameters passed by the numerical optimizer.
#' @param ssm A list defining the standardized structural model specification, containing
#'   \code{name}, initial states (\code{rho_guess}, \code{sigma_guess}), input matrices (\code{data$Y}, \code{data$X}),
#'   and matrix generator functions (\code{builders}).
#' @param return_full_res Logical. If \code{FALSE} (default), returns the scalar negative log-likelihood
#'   for optimization. If \code{TRUE}, returns the complete list of filtered states, covariance matrices, and mapped parameters.
#' @param set_silent Logical. If \code{TRUE} (default), suppresses diagnostic parameter logs.
#'
#' @return If \code{return_full_res = FALSE}, returns a numeric scalar representing the negative log-likelihood
#'   (or \code{1e10} if estimation fails). If \code{return_full_res = TRUE}, returns the complete Kalman filter output list
#'   augmented with \code{param_debugs}.
#'
#' @seealso \code{\link{kalman_filter_core}}, \code{\link{kalman_filter_taylor}}, \code{\link{param2model_gen}}
#'
#' @export
loglik_ssm_core <- function(theta,
                            ssm,
                            return_full_res = FALSE,
                            set_silent = TRUE) {
  
  model_type <- match.arg(ssm$name, choices = c("okun", "philips", "taylor"))
  
  # 1. Coordinate initial state spatial boundaries
  rho_init <- matrix(ssm$rho_guess, nrow = length(ssm$rho_guess), ncol = 1)
  nr       <- nrow(rho_init)  
  sig_init <- matrix(as.numeric(ssm$sigma_guess), nrow = nr, ncol = nr)
  
  # 2. Map Parameters (Optimizer Space -> Economic Space)
  model_params <- param2model_gen(theta, ssm)
  
  if(!set_silent) {
    debug_msg <- paste0(names(model_params), " = ", round(unlist(model_params), 6), collapse = ", ")
    message("DEBUG [Economic Space]: ", debug_msg)  
  }
  
  # 3. Construct System Matrices via Component Builders
  mu_t   <- ssm$builders$mu_t(model_params, ssm$data$X)
  H      <- ssm$builders$H(model_params)
  M      <- ssm$builders$M(model_params, ssm$data$Y)
  N      <- ssm$builders$N(model_params) 
  nu_t   <- ssm$builders$nu_t(model_params, ssm$data$X)

  # Clean, standardized dynamic evaluation of G matrix properties
  if ("model_params" %in% names(formals(ssm$builders$G))) {
    G <- ssm$builders$G(model_params)
  } else {
    G <- ssm$builders$G()
  }
  
  # ============================================================================
  # 5. DYNAMIC FILTER ENGINE ROUTING
  # ============================================================================
  
  if (ssm$name == "taylor") {
    
    # Run the specialized policy filter variant that accounts for ELB missing data segments
    res <- kalman_filter_taylor(
      Y_t       = ssm$data$Y, 
      nu_t      = nu_t, 
      H         = H, 
      N         = N, 
      mu_t      = mu_t, 
      G         = G, 
      M         = M,
      ar_mat = ssm$builders$ar_mat(model_params, ssm$data$Y),
      Sigma_0   = sig_init, 
      rho_0     = rho_init
      )
    
  } else {
    
    # Standard linear multi-observable filter for Okun and Phillips blocks
    res <- kalman_filter_core(
      Y_t       = ssm$data$Y, 
      nu_t      = nu_t, 
      H         = H, 
      N         = N, 
      mu_t      = mu_t, 
      G         = G, 
      M         = M,
      Sigma_0   = sig_init, 
      rho_0     = rho_init
    )
    
  }
  
  # ============================================================================
  # 6. STANDARD SAFEGUARD MATRIX VALIDATION
  # ============================================================================
  if (is.null(res) || is.null(res$loglik.vector) || any(is.na(res$loglik.vector)) || any(is.infinite(res$loglik.vector))) {
    message(sprintf("!!! [%s] FILTER CRASHED: RETURNING HIGH PENALTY REGIME !!!", toupper(model_type)))
    return(1e10)
  }
  
  total_loglik <- -sum(res$loglik.vector)
  
  if (total_loglik > 50000) {
    message("WARNING: FILTER RETURNING ABNORMALLY HIGH LOG LIKELIHOOD DISTORTIONS")
  } 
  
  cat(sprintf("\r[%s] Log Likelihood: %.6f", toupper(model_type), total_loglik))  
  flush.console()
  
  if (return_full_res) {
    res$param_debugs <- model_params
    return(res) 
  } else {
    return(total_loglik) 
  }
}






#' Multi-Method Optimization Wrapper for Macroeconomic State-Space Models
#'
#' Executes sequential numerical optimization across specified algorithms (e.g., Nelder-Mead, BFGS)
#' via \code{optimx::optimx} to estimate structural parameters for macroeconomic state-space models.
#' Supports warm starts, chaining optimization steps across multiple iterative macro loops,
#' and mapping between unconstrained optimizer space and economic parameter bounds.
#'
#' @param ssm A list defining the standardized structural model specification, containing
#'   \code{name}, parameter metadata (\code{manifest}), observation/exogenous data (\code{data}),
#'   and system matrix builders (\code{builders}).
#' @param methods Character vector of optimization algorithms supported by \code{optimx} (e.g., \code{c("Nelder-Mead", "BFGS")}).
#' @param iters Integer. Number of sequential optimization loops to execute.
#' @param start_par Optional numeric vector of initial unconstrained parameters (\eqn{\theta}) for warm starts.
#'   If \code{NULL}, values are initialized from \code{ssm$manifest}.
#' @param set_silent Logical. If \code{TRUE} (default), suppresses verbose parameter transformation logs.
#'
#' @return A named list containing:
#' \item{params}{Named list of final estimated parameters transformed into structural economic space.}
#' \item{theta}{Named numeric vector of final estimated parameters in unconstrained optimizer space.}
#' \item{fit_summary}{The \code{optimx} convergence and estimation summary table from the final pass.}
#' \item{ssm}{The structural state-space model blueprint list.}
#'
#' @seealso \code{\link{loglik_ssm_core}}, \code{\link{param2model_gen}}, \code{\link{model2param_gen}}
#'
#' @export
#' @importFrom optimx optimx
ssm_optimizer_wrapper_core <- function(ssm, 
                                       methods = NULL, 
                                       iters = NULL, 
                                       start_par = NULL,
                                       set_silent = TRUE) {
  
  # 1. DYNAMIC CONFIGURATION RESOLUTION VIA FRAMEWORK METADATA
  # ============================================================================
  # Resolves internal naming mismatches (e.g., mapping framework designation "shadow" to "taylor")
  model_id <- match.arg(ssm$name, choices = c("okun", "philips", "taylor"))

  if (is.null(methods)) {
    message("Methods parameter not provided")
  }
  if (is.null(iters)) {
    message("Iters parameter not provided")  }
  
  # 2. STATE RECONSTRUCTION & METRIC PARSING
  # ============================================================================
  init_theta_econ <- sapply(ssm$manifest, function(x) x$val) 
  
  if (is.null(start_par)) {
    current_par_opt <- model2param_gen(init_theta_econ, ssm)
    
    if(!set_silent) {
      message("Debug: Economic Params (Initial): ", paste(names(init_theta_econ), "=", round(init_theta_econ, 4), collapse = ", "))
      message("Debug: Optimizer Params (Theta): ", paste(names(current_par_opt), "=", round(current_par_opt, 4), collapse = ", "))
    }
  } else {
    # Aligned with default blueprints: uses manifest vectors for transformation structures
    current_par_opt <- model2param_gen(init_theta_econ, ssm)
    
    if(!set_silent) {
      message("\n--- Transformed Optimizer Space (Theta) ---")
      message(paste0(names(current_par_opt), ": ", round(current_par_opt, 4), collapse = "\n"))
    }
  }
  
  n_par <- length(current_par_opt)
  message("BEGIN OPTIMIZATION (ENGINE SWITCH: ", toupper(model_id), ")")
  print(current_par_opt)
  
  # 3. INTERACTIVE REPETITIVE OPTIMIZATION PIPELINE
  # ============================================================================
  for (i in 1:iters) {
    fit <- optimx::optimx(
      par     = current_par_opt,
      fn      = loglik_ssm_core,
      ssm     = ssm,
      method  = methods,
      control = list(
        all.methods = FALSE, # We select our methods so FALSE
        follow.on   = TRUE, # Sequential parameter chaining
        dowarn      = estimation_settings$general_settings$show_warnings, 
        maximize    = FALSE,
        itnmax      = estimation_settings$general_settings$max_runs,  
        maxit       = estimation_settings$general_settings$max_runs,
        reltol      = 1e-5,  
        abstol      = 1e-5  
      )
    )
    
    final_row_idx <- nrow(fit)
    
    # Extract calculations, maintaining named identities
    proposed_df     <- fit[final_row_idx, 1:n_par, drop = FALSE]
    current_par_opt <- as.numeric(proposed_df)
    names(current_par_opt) <- colnames(proposed_df)
    
    if (!set_silent) {
      message("\nDEBUG FOR CURRENT PAR OPT AFTER [ITERATION ", i, "]")
      print(current_par_opt)
    }
    
    # Standardized NA Fallback Engine
    if (any(is.na(current_par_opt))) {
      message("!!! CRITICAL: OPTIMIZER RETURNED NA MATRIX ELEMENTS. REINITIALIZING DEFAULTS !!!")
      browser()
      current_par_opt <- model2param_gen(init_theta_econ, ssm)
    }
    
    proposed_par        <- current_par_opt
    names(proposed_par) <- names(current_par_opt)
    
    # Prevent text collisions on single lines (\r)
    cat("\n")
    
    formatted_theta_opt <- paste0(
      sprintf("  %-20s : %.4f", names(proposed_par), proposed_par), 
      collapse = "\n"
    )
    
    cat(rep("=", 45), "\n", sep = "")
    cat("ESTIMATED OPTIMIZER PARAMETERS (Optimizer Space)\n")
    cat(rep("-", 45), "\n", sep = "")
    cat(formatted_theta_opt, "\n")
    cat(rep("-", 45), "\n")
    
    # Numerical validation checking boundaries
    if (any(is.na(proposed_par)) || any(is.infinite(proposed_par))) {
      cat("\n!!! WARNING: Optimizer failed to converge securely (NA/Inf detected) !!!\n")
    } else {
      current_par_opt <- proposed_par
    }
  }
  
  # 4. POST-ESTIMATION SYSTEM TRANSFORMATION
  # ============================================================================
  final_params_econ <- param2model_gen(current_par_opt, ssm)
  econ_vector       <- unlist(final_params_econ)
  clean_names       <- gsub("\\..*$", "", names(econ_vector))
  
  formatted_params  <- paste0(
    sprintf("  %-21s : %.6f", clean_names, econ_vector), 
    collapse = "\n"
  )
  
  cat("\n\n", rep("=", 45), "\n", sep = "")
  cat("FINAL ESTIMATED ECONOMETRIC PARAMETERS (Economic Space)\n")
  cat(rep("-", 45), "\n", sep = "")
  cat(formatted_params, "\n")
  cat(rep("=", 45), "\n\n")
  
  message("Optimization loop finished. Results compiled.")
  
  return(list(
    params      = final_params_econ, 
    theta       = current_par_opt,   
    fit_summary = fit,          
    ssm         = ssm
  ))
}