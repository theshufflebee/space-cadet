#' Unified Log-Likelihood Evaluator for State-Space Models
#'
#' @param theta Vector of unconstrained optimizer parameters.
#' @param ssm The standardized structural model list.
#' @param model_type Character switch: "okun", "philips", or "shadow".
#' @param return_full_res Logical. If TRUE, returns the full filter history list.
#' @param set_silent Logical. If FALSE, prints economic-space parameter tables.
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
  sig_init <- diag(as.numeric(ssm$sigma_guess), nr)
  
  # 2. Map Parameters (Optimizer Space -> Economic Space)
  model_params <- param2model_gen(theta, ssm)
  
  if(!set_silent) {
    debug_msg <- paste0(names(model_params), " = ", round(unlist(model_params), 6), collapse = ", ")
    message("DEBUG [Economic Space]: ", debug_msg)  
  }
  
  # 3. Construct System Matrices via Component Builders
  mu_t   <- ssm$builders$mu_t(model_params, ssm$data$X)
  H      <- ssm$builders$H(model_params)
  M      <- ssm$builders$M(model_params)
  N      <- ssm$builders$N(model_params) 
  ar_mat <- ssm$builders$ar_mat(model_params, ssm$data$Y)
  
  # Clean, standardized dynamic evaluation of G matrix properties
  if ("model_params" %in% names(formals(ssm$builders$G))) {
    G <- ssm$builders$G(model_params)
  } else {
    G <- ssm$builders$G()
  }
  
  # 4. Initialize Intercept Matrix Context
  nu_t <- matrix(0, nrow(ssm$data$Y), nr)
  
  # ============================================================================
  # 5. DYNAMIC FILTER ENGINE ROUTING
  # ============================================================================
  filter_engine <- switch(
    model_type,
    "okun"    = kalman_filter_okun,
    "philips" = kalman_filter_philips,
    "taylor"  = kalman_filter_taylor
  )
  
  res <- filter_engine(
    Y_t       = ssm$data$Y, 
    nu_t      = nu_t, 
    H         = H, 
    N         = N, 
    mu_t      = mu_t, 
    G         = G, 
    M         = M,
    ar_matrix = ar_mat,
    Sigma_0   = sig_init, 
    rho_0     = rho_init
  )
  
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






#' Unified Optimizer Wrapper for Macroeconomic State-Space Models
#'
#' @param ssm The standardized structural model blueprint list containing data, manifest entries, and type context.
#' @param methods Vector of optimization engines. If NULL, dynamically matches against global configuration blueprints.
#' @param iters Number of sequential optimization macro loops to execute. If NULL, auto-assigned from configuration definitions.
#' @param start_par Optional vector of unconstrained numeric parameters for initiating warm starts.
#' @param set_silent Logical. If FALSE, prints descriptive parameters transformations during setup phases.
#'
#' @export
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
  
  # 3. INTERACTIVE REPETITIVE OPTIMIZATION PIPELINE
  # ============================================================================
  for (i in 1:iters) {
    fit <- optimx::optimx(
      par     = current_par_opt,
      fn      = loglik_ssm_core,
      ssm     = ssm,
      method  = methods,
      control = list(
        all.methods = FALSE, 
        follow.on   = TRUE, # Sequential parameter chaining
        dowarn      = FALSE, 
        maximize    = FALSE,
        itnmax      = 3000,  
        maxit       = 3000,
        reltol      = 1e-6,  
        abstol      = 1e-6  
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