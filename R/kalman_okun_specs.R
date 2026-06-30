################################################################################
#
# THIS FILE CONTAINS THE MATHEMATICAL IMPLEMENTATION OF THE KALMAN FILTER / SMOOTHER
#
################################################################################

# The implementation of the filter and the smoother are made through 2 different
# main functions each are supported by a couple of helper functions that replace 
# parts in the main function to make the code more readable.

# As of this version only the filter is implemented for the project


# ==============================================================================
#
# KALMAN FILTER
#
# ==============================================================================

#' Constrained Kalman Filter for Linear State-Space Models
#' 
#' Implements a Kalman filter for the estimation of latent states in a linear system.
#' This implementation supports exogenous components, missing values (NA), 
#' non-negativity constraints on latent factors, and potential quadratic reconciliation.
#' 
#' @section System Equations:
#' Measurement equations:
#' \deqn{y_t = \mu_t + G \rho_t + M \epsilon_t}
#' Transition equations:
#' \deqn{\rho_t = \nu_t + H \rho_{t-1} + N \xi_t}
#' 
#' @param Y_t Matrix. Observed data matrix of dimension \eqn{T \times ny}.
#' @param nu_t Matrix. Transition equation intercept/exogenous component (\eqn{T \times nr}). 
#'   Depends on past information only.
#' @param H Matrix. Transition matrix describing the evolution of the state (e.g., AR(1) coefficients).
#' @param N Matrix. State noise scaling matrix (process noise).
#' @param mu_t Matrix. Measurement intercept matrix (\eqn{T \times ny}). Would be 0 if data is demeaned.
#' @param G Matrix. Observation matrix containing the relations between observed and unobserved variables.
#' @param M Matrix. Measurement noise scaling matrix.
#' @param Sigma_0 Matrix. Initial covariance matrix guess for the state vector.
#' @param rho_0 Vector. Starting guess for the initial state vector.
#' @param indic_pos Vector. Binary indicators (0/1) specifying if specific latent factors 
#'   must be constrained to be \eqn{\ge 0}.
#' @param Rfunction Function. Defines the measurement noise covariance. Defaults to \code{calc_covariance}.
#' @param Qfunction Function. Defines the transition noise covariance. Defaults to \code{calc_covariance}.
#' @param reconciliationf Function. Define a modification for \code{rho_tt} (e.g., for consistency 
#'   in Augmented State Vectors in Quadratic Kalman Filters).
#' @param ar_matrix Matrix to add an AR(1) component to the estimation that either takes 
#'   the observed Y or the estimated Y in case of missing values
#'
#' @return A list containing:
#' \item{r}{Filtered states \eqn{\rho_{t|t}} (size \eqn{T \times nr}).}
#' \item{Sigma_tt}{Filtered state covariance \eqn{\Sigma_{t|t}} stored as flattened vectors.}
#' \item{loglik}{Scalar. Total log-likelihood of the model.}
#' \item{y_tp1_t}{Forecasted observables \eqn{y_{t+1|t}}.}
#' \item{S_tp1_t}{Predicted state covariance \eqn{\Sigma_{t+1|t}}.}
#' \item{r_tp1_t}{Predicted state vector \eqn{\rho_{t+1|t}}.}
#' \item{loglik.vector}{Vector of date-specific log-likelihoods.}
#' \item{Omega_tp1_t}{Innovation covariance \eqn{\Omega_{t+1|t}}.}
#' \item{M}{The measurement scaling matrix used.}
#' \item{fitted_obs_t_t}{Estimated observables given filtered states (\eqn{\hat{y}_t|t}).}
#' 
#' @importFrom MASS ginv
#' 
#' @seealso
#' \code{\link{project_state_forward}} for the transition step, 
#' \code{\link{forecast_measurement_eq}} for the observation step, and 
#' \code{\link{ssm_optimizer_wrapper_shadow}} for the higher-level estimation loop.
#' 
#' @note 
#' If \code{reconciliationf} is provided, \code{rho_tt} is modified to ensure consistency 
#' defined by the function (used in QKF for augmented state vectors).
#' 
#' @export
kalman_filter_okun <- function(Y_t,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0,
                               indic_pos=0,
                               Rfunction=calc_covariance, Qfunction=calc_covariance,
                               reconciliationf = function(x,opt){x},
                               ar_matrix = 0
){
  # Y_t is the observed data matrix
  # rho_0 is the starting guess for the state vector
  # Sigma_0 is the initial covariance matrix guess
  # mu_t is the measurement intercept (would be 0 if data is demeaned)
  # G is the matrix that contains the relations between observed and unobserved
  # variables
  # M is the measurement noise (?) (probably the scaling going on here)
  # nu_t is the transition equation intercept
  # H is the transition matrix that describes evolution of state (for example
  # AR(1) coefs)
  # N is the state noise (?) (probably the scaling going on here)
  #
  # Measurement equations:
  #    y_t   = mu_t + G * rho_t + M * eps_t
  # Transition equations:
  #    rho_t = nu_t + H * rho_t-1 + N * xi_t
  # Y_t is a T*ny-dimensional vector
  # nu_t and mu_t depend on past only (predetermined at date t)
  # indic post -> special feature where a variable can be imposed to not go below 0
  # This is constrained filter
  # R / Qfunction are there for time variance?
  # reconciliationf is for quadratic filter?
  #
  # The function returns:
  # .$r   filtered variables (rho_t|t) size T*nr
  # .$S  SIGMA t|t T*(nr*nr)
  
  # Notes:
  # 'indic_pos' is a vector of binary variables indicating if
  #     latent factors have to be >=0
  # If 'reconciliationf' is a function, then 'rho_tt' may be modified,
  #     in a way defined by this function. That is used in the QKF, to ensure
  #     that there is consistency in the augmented state vector (x,vec(xx')).
  
  # Number of observed variables:
  
  
  ny = NCOL(Y_t)
  # Number of unobs. variables:
  nr = NCOL(G)
  # Number of time periods:
  T = NROW(Y_t)
  
  # loglik.vector will contain the vector of date-specific log-likelihood
  loglik.vector <- NULL
  
  #Initilize output matrices:
  rho_tt    = matrix(0,T,nr) # After new obs
  rho_tp1_t = matrix(0,T,nr) # Before new obs -> forecast
  y_tp1_t   = matrix(0,T,ny) # forecast before new obs
  
  Sigma_tt    = matrix(0,T,nr*nr) # After new obs -> update
  Sigma_tp1_t = matrix(0,T,nr*nr) # Before new obs -> forecast
  Omega_tt    = matrix(0,T,ny*ny) # After new Obs -> update
  Omega_tp1_t = matrix(0,T,ny*ny) # Before new obs -> forecast
  
  fitted_obs_t_t <- matrix(0, T, ny)  # Fit obs at each point in time to be able to estimate recurively
  
  #Initilize log-Likelihood:
  logl = ny*T/2*log(2*pi)
  
  #Kalman algorithm:
  
  for (t in 1:T){
    
    # print(t)
    
    # ==========================================================================
    # Forecasting step (between t-1 and t):
    # ==========================================================================
    # Recursion only works for t>1 so for 0 need to start with the initial guess
    nu_step <- matrix(nu_t[t, ], ncol = 1)
    
    
    if(t==1){
      
      rho <- rho_0
      Sigma <- Sigma_0
      
      lag_val <- rho_0[1, 1]
      
    } else {
      rho <- rho_tt[t-1, ]
      Sigma <- Sigma_tt[t-1, ]
      
      if (is.na(Y_t[t-1, 1])) { # if there is no past y obs for value one
        # USE THE MODEL'S PREVIOUS PREDICTION
        lag_val <- as.numeric(fitted_obs_t_t[t-1, ])
      } else {
        # USE THE ACTUAL DATA
        lag_val <- as.numeric(Y_t[t-1, ])
      }
    }
    
    # First we project forward the initial guess of rho_0. here both H and nu_t[1,] will be estimated
    
    # I replaced the one below with the function visible next
    #rho_tp1_t[1,] = nu_t[1,] + t(H %*% rho_0) # rho_0 is initial guess, calculates first obs rho, nu_t 
    rho_tp1_t[t,] <- project_state_forward(rho = rho,
                                           H = H,
                                           nu_t = nu_step,
                                           nr = nr)
    
    
    
    # Calculate the Noise, these square M and N are the shocks hitting the system
    # Functions are defined below
    R = Rfunction(M, rho, t)
    Q = Qfunction(N, rho, t)
    
    
    
    # Here if any latent variable is imposed to be >=0 this selects the max of 0 and the variable
    if(sum(indic_pos==1)>0){ # Some latent variables imposed to be >= 0
      rho_tp1_t[t, indic_pos==1] = pmax(rho_tp1_t[t, indic_pos==1], 0)
    }
    
    # to put H into form
    
    H_mat     <- matrix(as.numeric(H), nrow = nr, ncol = nr)
    
    # Old way
    # aux_Sigma_tp1_t = Q + H %*% matrix(Sigma, nr, nr) %*% t(H) # predict sigma from t-1 var of state vector
    
    #new way
    aux_Sigma_tp1_t = Q + H_mat %*% matrix(Sigma, nr, nr) %*% t(H_mat)
    
    # Forecast the measurement using the previously forecasted state from t-1
    # the equation below is in function
    #y_tp1_t[t,] = mu_t[t,] + t(G %*% rho_tp1_t[t,]) # forecast the measurement from t-1
    
    
    y_tp1_t[t,] <- forecast_measurement_eq(mu_t = mu_t[t,],
                                           G = G, rho = rho,
                                           ar_mat = ar_matrix,
                                           lag_val = lag_val)
    
    
    
    
    
    # Predict Sigma: If first step, use initial guess
    # Here Q is the uncertainty of the shocks, while Sigma is the state estimation error cov
    # We predict the uncertainty around the state here
    
    # The equation below is repaced with the function
    #aux_Sigma_tp1_t <- Q + H %*% Sigma %*% t(H)
    aux_Sigma_tp1_t <- project_covariance_H(Q, Sigma, H, nr = nr)
    
    
    # Save sigma here in the storage again by turning it into a single vector
    Sigma_tp1_t[t,] = matrix(aux_Sigma_tp1_t, 1, nr*nr)
    
    # Omega takes the predicted uncertainty around the state and turns it into the predicted measurement error
    # Omega is innovation covariance
    
    # The euqation below is replaced with the following function
    # omega = R + G %*% aux_Sigma_tp1_t %*% t(G) # sums up noise from measurements (R) and predicted state uncertainty
    omega <- project_covariance_G(R, aux_Sigma_tp1_t, G, nr = nr, ny = ny)
    Omega_tp1_t[t,] = matrix(omega, 1, ny*ny) # flattens the whole thing again, for storage
    
    
    # ==========================================================================
    # Updating step
    # ==========================================================================
    
    # Detect observed variables:
    # Adjust what variables you can truly update (arent NA)
    vec_obs_indices <- which(!is.na(Y_t[t, ])) # indices of observed variables
    ny_aux <- length(c(vec_obs_indices)) # number of observed variables that arent na
    
    # Resize matrices accordingly and select the observed variables:
    G_aux <- matrix(G[vec_obs_indices,],nrow=ny_aux) # select observed variables from G matrix
    R_aux <- R[vec_obs_indices,] # select observed variables from R matrix
    omega <- omega[vec_obs_indices,] # select observed variables same from omega
    
    # safety step so class of objects dont throw errors
    if(class(R_aux)[1]=="numeric"){ 
      R_aux <- R_aux[vec_obs_indices] # second part of selection, first rows then coumns
      omega <- omega[vec_obs_indices]
    }else{
      R_aux <- R_aux[, vec_obs_indices]
      omega <- omega[, vec_obs_indices]
    }
    
    # resize to matrix
    R_aux <- matrix(R_aux, ny_aux,ny_aux)
    
    # Here essentially we just computed a new auxiliary matrix that is smaller and only contains
    # rows and columns from variables that are actually observed here
    
    #Compute gain K:
    if(dim(G_aux)[1]>0){ # only update if there is at least one new observation
      
      # Calculate Kalman gain
      # aux_Sigma_tp_1 is the uncertainty around the state. we take ratio of it to the shocks in R and the var of sigma and G
      # Equation below replaced by function
      # K = aux_Sigma_tp1_t %*% t(G_aux) %*% MASS::ginv(R_aux + G_aux %*% aux_Sigma_tp1_t %*% t(G_aux))
      
      K = calculate_kalman_gain(aux_Sigma_tp1_t, G_aux, R_aux)
      
      
      # Calculate Forecast error when you actually observe new variables
      lambda_t   = Y_t[t, ] - y_tp1_t[t, ] # forecast error
      
      # reorganize forecast error into a vector
      lambda_t <- matrix(lambda_t[vec_obs_indices], ncol=1) 
      
      # Update the state vector using the Kalmann gain. the higher it is the more you trust the updated innovation lambda_t (error)
      # The higher K the more of the forecast error or surprise is trusted
      
      # MAYBE REPLACE HERE WITH THE FORECAST FUNCTION AS WELL
      rho_tt[t,] = t(rho_tp1_t[t,] + K %*% lambda_t ) # new updated state vector as t is available with obs t available
      
      # Once again check if there are constraints on the states
      if(sum(indic_pos==1)>0){
        rho_tt[t,indic_pos==1] = pmax(rho_tt[t,indic_pos==1],0)
      }
      
      #update the uncertainty around the state based on kalmann gain
      Id         = diag(1,nrow=nr,ncol=nr) # identity matrix
      
      # updated sigma / var matrix
      Sigma_tt[t, ] = matrix((Id - K %*% G_aux) %*% aux_Sigma_tp1_t, 1, nr*nr) 
      
      # Computation of log-likelihood (determinant):
      if(length(c(omega))==1){
        det_omega <- omega
      }else{
        det_omega <- det(omega)
      }
      
      # Potential reconciliation between components of rho_tt (QKF):
      # QUadratic stuff not importat currently
      y2Bfitted  <- matrix(Y_t[t,],ncol=1)
      constant   <- matrix(mu_t[t,],ncol=1)
      Rho_tt_1   <- rho_tp1_t[t,]
      opt        <- list(y2Bfitted,constant,G,M,Rho_tt_1,Q)
      rho_tt[t,] <- reconciliationf(rho_tt[t,],opt)
      
      # Calculate log likelihood
      loglik.vector <- rbind(loglik.vector,
                             ny_aux/2*log(2*pi) - 1/2*(log(det_omega) +
                                                         t(lambda_t) %*% MASS::ginv(omega) %*% lambda_t)
      )
      
      
      # Sum pu for what needs to be finalized
      logl <- logl + loglik.vector[t] # what needs to be optimmized
      
    }else{ # if no update just use predictions (increases error)
      rho_tt[t, ] = t(rho_tp1_t[t, ])
      Sigma_tt[t, ] = matrix(aux_Sigma_tp1_t, 1, nr*nr)
    }
    
    # Calculate partial stuff here so next iter I can use it for shadow rate estim
    fitted_obs_t_t[t,] <- mu_t[t,] + rho_tt[t,] %*% t(G) + t(ar_matrix * as.numeric(lag_val - rho)) # fitted observables -> estimate of y_hat_t given t
    
    ##########---------------------
    # LAG VAL - RHO INTO A SINGLE NUMBER later
  }
  
  # Create output list
  output = list(r=rho_tt,Sigma_tt=Sigma_tt,loglik=logl,y_tp1_t=y_tp1_t,
                S_tp1_t=Sigma_tp1_t,r_tp1_t=rho_tp1_t,
                loglik.vector = loglik.vector,Omega_tp1_t=Omega_tp1_t,M=M,
                fitted_obs_t_t = fitted_obs_t_t)
  return(output)
}



#===============================================================================
# Helper Function for the Kalman Filter
#===============================================================================


#' Project Latent State Forward (Transition Equation)
#'
#' @description
#' Computes the a priori state estimate for the next period: 
#' \eqn{\rho_{t|t-1} = H \rho_{t-1} + \nu_t}.
#'
#' @param rho Numeric vector. The filtered state from the previous period (\eqn{\rho_{t-1|t-1}}).
#' @param H Matrix. The transition matrix defining the dynamics (AR components) of the states.
#' @param nu_t Vector. The state intercept or exogenous drift term for the current step.
#' @param nr Integer. The number of latent states (dimensions of the state vector).
#'
#' @return A column matrix representing the predicted latent state vector.
#' 
#' @seealso 
#' \code{\link{kalman_filter_taylor}} for where this prediction is utilized and
#' \code{\link{project_covariance_H}} for the uncertainty propagation associated with this step.
#' 
#' @export
project_state_forward <- function(rho, H, nu_t, nr) {
  # nu_t_step should be the specific row for time t: nu_t[t, ]
  # Ensure rho is a column vector
  
  rho <- as.numeric(rho)  # ADDED due to Bug in third model worked fine first two
  
  H_mat <- matrix(H, nrow = nr, ncol = nr) # resize H from vector to matrix
  
  rho_mat <- matrix(rho, nrow = nr, ncol = 1) # risize rho into correct form -> maight be able to zse t() check later
  
  rho_pred <- (H_mat %*% rho_mat) + matrix(nu_t)
  
  return(rho_pred)
}


forecast_measurement_eq <- function(mu_t, G, rho, ar_mat, lag_val) {
  
  
  obs_row <- nrow(G) # Number of active observable rows this quarter (1 or 2)
  
  # --------------------------------------------------------------------------
  # 1. ISOLATE CYCLICAL PERSISTENCE
  # --------------------------------------------------------------------------
  # Unemployment (Row 1) uses the phi parameter. 
  # SPF (Row 2) has NO cyclical persistence layer.
  phi_param    <- as.numeric(ar_mat[1])
  
  # Calculate the deviation of the last historical value from its latent state
  # This represents the unobserved cyclical component tracking forward
  cyc_unemp <- as.numeric(lag_val[1] - rho[1])
  
  # Reconstruct a clean, safe system vector:
  # Row 1 gets the AR persistence modification; Row 2 gets exactly 0
  full_ar_comp <- matrix(c(phi_param * cyc_unemp, 0), ncol = 1)
  
  # --------------------------------------------------------------------------
  # 2. DYNAMIC DIMENSION SLICING
  # --------------------------------------------------------------------------
  # Slice down the AR component to match what is observed this quarter
  if (obs_row == 1) {
    ar_vec_final <- full_ar_comp[1, , drop = FALSE]
  } else {
    ar_vec_final <- full_ar_comp
  }
  
  # --------------------------------------------------------------------------
  # 3. MATRIX PROJECTION MATH
  # --------------------------------------------------------------------------
  mu_vec     <- matrix(as.numeric(mu_t), nrow = obs_row, ncol = 1)
  ar_vec     <- matrix(as.numeric(ar_vec_final), nrow = obs_row, ncol = 1)
  state_comp <- G %*% matrix(as.numeric(rho))
  
  # Assemble total measurement forecast: y_{t|t-1} = G*rho + AR + Intercept
  y_tp1_t    <- state_comp + ar_vec + mu_vec
  
  return(as.vector(y_tp1_t))
}



#' Calculate Covariance Matrix from Standard Deviations
#'
#' Computes a variance-covariance matrix from a matrix of standard deviations.
#' Currently implements the identity correlation case: \eqn{\Sigma = M M'}.
#'
#' @param SD_matrix A matrix containing standard deviation coefficients (e.g., matrix \eqn{M} or \eqn{N}).
#' @param RHO Reserved for future implementation of correlation structures.
#' @param t Reserved for future time-varying covariance implementation.
#'
#' @return A symmetric, positive semi-definite covariance matrix.
#' 
#' @seealso 
#' \code{\link{project_covariance_H}} and \code{\link{project_covariance_G}} 
#' for functions that utilize these covariance matrices in state propagation.
#' 
#' @export
calc_covariance <- function(SD_matrix, RHO = NULL, t = 0) {
  # Standard formula: Var = SD * SD'
  return(SD_matrix %*% t(SD_matrix))
}


#' Project Covariance Matrix (Uncertainty Propagation)
#'
#' @description
#' Updates the state uncertainty matrix for the prediction step:
#' \eqn{\Sigma_{t|t-1} = H \Sigma_{t-1|t-1} H' + Q}.
#' It propagates the estimation error forward in time and adds process noise.
#'
#' @param noise Matrix. The process noise covariance matrix (\eqn{Q = N N'}).
#' @param sigma_base Matrix. The filtered covariance from the previous step (\eqn{\Sigma_{t-1|t-1}}).
#' @param transform_matrix Matrix. The transition matrix (\eqn{H}).
#' @param nr Integer. Number of latent states.
#'
#' @return A symmetric matrix representing the predicted state uncertainty.
#' 
#' @seealso 
#' \code{\link{kalman_filter_taylor}} for the filtering context and
#' \code{\link{calc_covariance}} for the initial noise matrix construction.
#' 
#' @export
project_covariance_H <- function(noise, sigma_base, transform_matrix, nr = nr) {
  
  H_mat <- matrix(transform_matrix, nrow = nr, ncol = nr)
  
  sig_mat <- matrix(sigma_base, nrow = nr, ncol = nr)
  
  uncertainty <- noise + H_mat %*% sig_mat %*% t(H_mat)
  
  return(uncertainty)
}


#' Calculate Innovation Covariance
#'
#' @description
#' Maps state uncertainty into observation space:
#' \eqn{\Omega_t = G \Sigma_{t|t-1} G' + R}.
#' This represents the total expected variance of the forecast error.
#'
#' @param noise Matrix. Measurement noise covariance (\eqn{R = M M'}).
#' @param sigma_base Matrix. Predicted state covariance (\eqn{\Sigma_{t|t-1}}).
#' @param transform_matrix Matrix. The observation matrix (\eqn{G}).
#' @param nr Integer. Number of latent states.
#' @param ny Integer. Number of observed variables.
#'
#' @return A matrix representing the innovation covariance.
#' 
#' @seealso 
#' \code{\link{kalman_filter_taylor}} for the filtering context and
#' \code{\link{calc_covariance}} for the initial noise matrix construction.
#' 
#' @export
project_covariance_G <- function(noise, sigma_base, transform_matrix, nr, ny) {
  
  G_mat <- matrix(transform_matrix, nrow = ny, ncol = nr)
  
  sig_mat <- matrix(sigma_base, nrow = nr, ncol = nr)
  
  uncertainty <- noise + G_mat %*% sig_mat %*% t(G_mat)
  
  return(uncertainty)
}


#' Calculate Kalman Gain
#'
#' @description
#' Computes the optimal weighting matrix (K) that determines how much the 
#' "surprise" (innovation) in new data should update the latent state estimates.
#' \eqn{K_t = \Sigma_{t|t-1} G' \Omega_t^{-1}}.
#'
#' @param sigma Matrix. Predicted state covariance (\eqn{\Sigma_{t|t-1}}).
#' @param G Matrix. Observation matrix (\eqn{G}).
#' @param R Matrix. Observation noise covariance (\eqn{R}).
#'
#' @return A matrix representing the Kalman gain.
#' @importFrom MASS ginv
#' 
#' @importFrom MASS ginv
#' @seealso 
#' \code{\link{kalman_filter_taylor}} for the full update step logic and
#' \code{\link{project_covariance_G}} for the innovation covariance calculation (\Omega).
#' 
#' @export
calculate_kalman_gain <- function(sigma, G, R){
  
  K = sigma %*% t(G) %*% MASS::ginv(R + G %*% sigma %*% t(G))
  
  return(K)
}



# ==============================================================================

#' Log-Likelihood Function for the Taylor-Rule State-Space Model
#'
#' @description
#' Calculates the negative log-likelihood of a state-space model given a set of 
#' unconstrained parameters. This function acts as the primary interface between 
#' the numerical optimizer and the Kalman Filter, handling parameter mapping, 
#' matrix construction, and numerical safeguards.
#'
#' @param theta A numeric vector of unconstrained parameters provided by the optimizer.
#' @param ssm A list/object of class 'ssm' containing data matrices (Y, X), 
#'   initial guesses, and builder functions for system matrices.
#' @param return_full_res Logical; if \code{TRUE}, returns the full Kalman Filter 
#'   output list (useful for debugging and final estimation). If \code{FALSE} (default), returns 
#'   the scalar negative log-likelihood for the optimizer.
#' @param rho_guess Initial guess for the state vector. Defaults to 1 (legacy support).
#' @param set_silent Logical; if \code{TRUE} (default), suppresses parameter 
#'   debug messages. Set to \code{FALSE} only for low-iteration debugging to avoid 
#'   console flooding.
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item \bold{Initialization:} Sets up the initial state vector (\code{rho_0}) 
#'         and covariance (\code{Sigma_0}) based on the model dimensions (e.g., 
#'         3 states for Model 3).
#'   \item \bold{Mapping:} Transforms parameters from unconstrained optimizer 
#'         space to constrained economic space using \code{param2model_gen}.
#'   \item \bold{Matrix Building:} Invokes dynamic builder functions to 
#'         construct \code{mu_t}, \code{H}, \code{M}, \code{N}, \code{G}, and 
#'         the autoregressive components.
#'   \item \bold{Filtering:} Executes the Kalman Filter to calculate the 
#'         likelihood vector.
#'   \item \bold{Safeguards:} Returns a high penalty value (\code{1e10}) if 
#'         numerical instability (NAs or Infs) is detected, ensuring the 
#'         optimizer stays within valid regions of the parameter space.
#' }
#'
#' @return If \code{return_full_res = FALSE}, a single numeric value representing 
#'   the negative log-likelihood. If \code{TRUE}, a list containing the Kalman 
#'   Filter results and the mapped economic parameters.
#'   
#' @importFrom stats matrix diag
#' @import optimx
#'
#' @seealso 
#' \code{\link{kalman_filter_taylor}} for the underlying filtering algorithm, 
#' \code{\link{param2model_gen}} for the parameter transformation logic, and
#' \code{\link{initialize_taylor_ssm}} for setting up the SSM object.
#'
#' @export
loglik_ssm_okun <- function(theta,
                            ssm,
                            return_full_res = FALSE,
                            rho_guess = c(6, 2, 1),
                            set_silent = TRUE) {
  
  # Initial State Vector (rho_0)
  # For model 3 its a 3x1 vector, for all others a 1x1 vector
  rho_init <- matrix(ssm$rho_guess, nrow = length(ssm$rho_guess), ncol = 1)
  
  # Determine Number of States (nr = 3 for Model 3, others 1) from rho_init (rho_guess)
  nr <- nrow(rho_init)  
  
  # Initial Covariance Matrix (Sigma_0)
  # Uses a diagonal structure to represent initial uncertainty, use nr to make sure dimensions fit
  # this allows to use just one guess for all variances or 3 different one for the third model
  sig_init <- diag(as.numeric(ssm$sigma_guess), nr)
  
  # Map Parameters (Optimizer Space -> Economic Space)
  # turns parameters from optimizer (unconstrained) into constrained space such as 
  model_params <- param2model_gen(theta, ssm)
  
  # This is an artifact from initial debugging issues, for later use if needed,
  # do not run the full estimation with set silent to false
  if(!set_silent){
    param_names <- names(model_params)
    param_values <- unlist(model_params)
    debug_msg <- paste0(param_names, " = ", round(param_values, 6), collapse = ", ")
    
    message("DEBUG [Economic Space]: ", debug_msg)  
  }
  
  # Build the matrices with the parameters to create the loglikelihood
  # The builders themselves are saved in the ssm object. They are functions as
  # they take in the parameters from the optimizer and calculate it
  mu_t <- ssm$builders$mu_t(model_params, ssm$data$X)
  # G    <- ssm$builders$G() # G is not static anymore need to remove before running other models
  H    <- ssm$builders$H(model_params)
  M    <- ssm$builders$M(model_params)
  N    <- ssm$builders$N(model_params) 
  ar_mat <- ssm$builders$ar_mat(model_params, ssm$data$Y) # the new matrix fo rthe AR process, Y passed in here for dimensions
  
  
  # CAN BE CUT NOW
  # formals checks for inputs, if an input is demanded it gives it to the builder, else it doesnt and just initializes it
  args_expected <- names(formals(ssm$builders$G))
  
  if ("model_params" %in% args_expected) {
    G <- ssm$builders$G(model_params)
  } else {
    G <- ssm$builders$G()
  }
  
  # Setup static components, 
  T_len <- nrow(ssm$data$Y)
  nu_t  <- matrix(0, T_len, nr) # we have no intercept so its just 0's ->  nr because needed number of states for proper math
  
  # Run Kalman Filter with the matrices
  # The Y data is from the ssm object the X data is already in mu_t
  # we also add initial guesses for the state and give a prior
  res <- kalman_filter_okun(
    Y_t     = ssm$data$Y, 
    nu_t    = nu_t, 
    H       = H, 
    N       = N, 
    mu_t    = mu_t, 
    G       = G, 
    M       = M,
    ar_matrix = ar_mat,
    Sigma_0 = sig_init, # Prior on variance (certainty of guess)
    rho_0   = rho_init # default is the second value of the Y column or the anchor
  )
  
  # Output handling
  # The optimizer only needs the loglikelihood, therefore we need that as a return
  # If we want to run a normal estimation with parameters we'd like to see the full
  # return, we specify this and we get state and all other variables
  
  # Numerical Safeguard check
  if (is.null(res) || is.null(res$loglik.vector) || any(is.na(res$loglik.vector)) || any(is.infinite(res$loglik.vector))) {
    
    message("FILTER CRASHED AND RETURNED NON NUMERIC RES: RETURNING HIGH PENALTY")
    return(1e10)
    browser()
    stop("STOPPING DUE TO CRASH")
    
  }
  
  total_loglik <- -sum(res$loglik.vector)
  
  # If the likelihood is extremely high such as due to failed filter or bad jump, return very high value as "punishment"
  if (total_loglik > 50000) {
    
    message("FILTER RETURNING EXTREMLY HIGH LOG LIKELIHOOD")
  } 
  
  cat(sprintf("\rLog Likelihood: %.6f", total_loglik))  
  flush.console()
  
  # For Debuggung you can have it return the full res
  if (return_full_res) {
    res$param_debugs <- model_params
    return(res) 
  } else {
    # Optimizer needs a single scalar to minimize
    return(-sum(res$loglik.vector)) 
  }
}




# ==============================================================================

#' Multi-Step State-Space Model Optimizer Wrapper
#'
#' @description
#' An optimization wrapper that cycles through multiple numerical 
#' optimization methods to find the global maximum of the state-space model 
#' likelihood. It supports "warm starts" for rolling estimations and implements 
#' early stopping if the likelihood improvement plateaus.
#'
#' @param ssm The fully initialized SSM blueprint object containing data, 
#'   manifest definitions, and builder functions.
#' @param methods A character vector of optimization algorithms available in 
#'   \code{optimx} (e.g., \code{"Nelder-Mead"}, \code{"bobyqa"}, \code{"BFGS"}).
#' @param iters Number of times to cycle through the entire list of \code{methods}. 
#'   Defaults to 2.
#' @param start_par Optional; a numeric vector of unconstrained parameters 
#'   (\code{theta}) from a previous estimation to be used as a warm start.
#' @param set_silent Logical; if \code{TRUE} (default), suppresses detailed 
#'   initialization debug messages in the console.
#'
#' @details
#' The wrapper performs the following sequence:
#' \enumerate{
#'   \item \bold{Parameter Preparation:} If no \code{start_par} is provided, it 
#'         extracts defaults from the \code{ssm$manifest} and transforms them 
#'         to unconstrained space.
#'   \item \bold{Sequential Optimization:} Iteratively calls \code{optimx} for 
#'         each method. It uses \code{follow.on = TRUE} so each algorithm 
#'         begins where the previous one finished.
#'   \item \bold{Convergence Monitoring:} Calculates the absolute difference 
#'         between the current and previous best log-likelihood. If the 
#'         improvement is less than \code{0.0001}, it triggers an early stop.
#'   \item \bold{Validation:} Checks for \code{NA} or \code{Inf} values in 
#'         the results to identify numerical divergence.
#'   \item \bold{Transformation:} Maps the final unconstrained parameters back 
#'         to economic space for reporting and forecasting.
#' }
#'
#' @return A list containing:
#' \itemize{
#'   \item \bold{params}: Named list of optimized parameters in economic space.
#'   \item \bold{theta}: Numeric vector of optimized parameters in unconstrained space.
#'   \item \bold{fit_summary}: The raw summary object from the last \code{optimx} call.
#'   \item \bold{ssm}: The original SSM object for traceability.
#' }
#'
#' @import optimx
#' 
#' @seealso 
#' \code{\link{loglik_ssm_shadow}} for the objective function being minimized, 
#' \code{\link{model2param_gen}} for the unconstraining transformation logic.
#'
#' @export
ssm_optimizer_wrapper_okun <- function(ssm, 
                                       methods = c("Nelder-Mead", "bobyqa", "BFGS"), 
                                       iters = 3, 
                                       start_par = NULL,
                                       set_silent = TRUE
) {
  
  # Prepare Initial Parameters
  if (is.null(start_par)) {
    # If no warm start, use manifest defaults
    # sapply here simplifies the nested list -> selects val from each element in the list
    init_theta_econ <- sapply(ssm$manifest, function(x) x$val) 
    
    current_par_opt <- model2param_gen(init_theta_econ, ssm)
    
    if(!set_silent){
      
      param_string <- paste(names(init_theta_econ), "=", round(init_theta_econ, 4), collapse = ", ")
      message("Debug: Economic Params (Initial): ", param_string)
      
      opt_string <- paste(names(current_par_opt), "=", round(current_par_opt, 4), collapse = ", ")
      message("Debug: Optimizer Params (Theta): ", opt_string)
    }
    
  } else {
    init_theta_econ <- sapply(ssm$manifest, function(x) x$val) 
    #  CURRENTLY RUNNING WITH ALWAYS INITIAL GUESS
    current_par_opt <- model2param_gen(init_theta_econ, ssm)
    # current_par_opt <- start_par
    
    if(!set_silent){
      message("\n--- Transformed Optimizer Space (Theta) ---")
      formatted_list <- paste0(names(current_par_opt), ": ", round(current_par_opt, 4), collapse = "\n")
      message(formatted_list)
    }
  }
  
  n_par <- length(current_par_opt)
  
  # Run Optimization Loop
  # ----------------------------------------------------------------------------
  # Cycles through each method for the specified number of iterations
  
  # Sets up a break of the method partloglik loop if repeated estimation doesn't lead to improvement 
  last_best_lik <- Inf
  
  message("BEGIN OPTIMIZATION")
  
  for (i in 1:iters) {
    for (m in methods) {
      fit <- optimx::optimx(
        par     = current_par_opt,
        fn      = loglik_ssm_okun,
        ssm     = ssm,
        method  = m,
        control = list(
          all.methods = FALSE, # Run them in order
          follow.on = TRUE,    # Method 2 starts where Method 1 ends
          dowarn = FALSE, 
          maximize = FALSE,
          itnmax = 5000,  # For bobyqa/optimx
          maxit = 5000,
          reltol = 1e-9,  # Stop if relative improvement is less than this
          abstol = 1e-9  # Stop if absolute improvement is less than this
        )
      )
      
      # Extract the new likelihood and parameters
      current_lik <- fit$value[1]
      current_par_opt <- as.numeric(fit[1, 1:n_par])
      
    }
    
    # Update current_par_opt for the next step in the loop
    proposed_par <- as.numeric(fit[1, 1:n_par])
    
    
    # COnsole output message result
    # formatted_theta_opt <- paste0(
    #   sprintf("  %-20s : %.4f", names(proposed_par), proposed_par), 
    #   collapse = "\n")
    
    #Message for Debugging of parameters
    # cat("\n", rep("=", 45), "\n", sep = "")
    # cat("Delete(?) ESTIMATED OPTIMIZER PARAMETERS (Economic Space)\n")
    # cat(rep("-", 45), "\n", sep = "")
    # cat(formatted_theta_opt, "\n")
    # cat(rep("-", 45), "\n")
    
    #VValidating results if there are no inf or NaN
    if (any(is.na(proposed_par)) || any(is.infinite(proposed_par))) {
      
      cat("\n!!! WARNING: Optimizer failed to converge (NA/Inf detected) !!!\n")
      formatted_theta <- paste0(
        sprintf("  %-20s : %.4f", names(proposed_par), proposed_par), 
        collapse = "\n"
      )
      cat(formatted_theta, "\n")
      cat(rep("-", 45), "\n")
      
    } else {
      
      # if the check doesnt throw alarms, update current_par_opt for the next vintage
      current_par_opt <- proposed_par
      
      formatted_theta <- paste0(
        sprintf("  %-20s : %.4f", names(current_par_opt), current_par_opt), 
        collapse = "\n"
      )
      cat(formatted_theta, "\n")
      cat(rep("-", 45), "\n")
    }
  }
  
  
  
  # Extract and transform Results
  # Map back to Economic Space
  final_params_econ <- param2model_gen(current_par_opt, ssm)
  
  # Format the parameter names and values into a clean list
  # This is so the console message looks good
  formatted_params <- paste0(
    sprintf("  %-20s : %.6f", names(final_params_econ), final_params_econ), 
    collapse = "\n"
  )
  
  # Construct the full console output message
  cat("\n", rep("=", 45), "\n", sep = "")
  cat("ESTIMATED ECONOMETRIC PARAMETERS (Economic Space)\n")
  cat(rep("-", 45), "\n", sep = "")
  cat(formatted_params, "\n")
  cat(rep("=", 45), "\n", sep = "")
  message("Optimization loop finished. Results stored.")
  
  # format return
  return(list(
    params = final_params_econ, # Economic scale (Named List)
    theta  = current_par_opt,   # Unconstrained scale (Vector for next warm start)
    fit_summary = fit,          # The last optimx result object
    ssm = ssm
  ))
}




#===============================================================================
# Rolling Estimation Functions
#===============================================================================

# Okun I can still implement the HP filter function

#' Rolling Okun SSM Estimation
#' 
#' Generate Rolling SSM Parameters for Forecasting
#' 
#' Estimates model parameters over an expanding window up to a specified end date.
#' Returns a list of parameters to be used in a separate forecasting step.
#' 
#' We have T = 1 as the start of the data, T=T is the end of the data,
#' T=t as the current date up until we have the data availabe for the pseudo forecast
#' and t+h would then be the forecast horizon.
#' What we do here is just get the parameter estimation at time t.
#' 
#' At each step we have to reestimate the HP filter to get the gaps. This is done via a burn in
#' where we start the estimation at the first observation in the dataframe. the dataframe is cut to the burn in start in the preparation
#' this assures there is no starting point bias.
#' 
#' @param data Raw merged dataframe containing log_gdp, unemp_rate, and spf_5y_unemp
#' @param forecast_start The first date to generate a forecast/parameter set for
#' @param forecast_end End date of rolling window (optional)
#' @param val_T1 The start date for the Kalman Filter (e.g., 2015-01-01)
rolling_est_okun_ssm <- function(data,
                                 forecast_start,
                                 forecast_end = NULL,
                                 date_col = "quarter",
                                 val_T1 = "1991-01-01") {
  
  # Assure correct format
  data[[date_col]] <- as.yearqtr(data[[date_col]])
  
  # Select the starting quarter
  start_q <- as.yearqtr(as.Date(forecast_start))
  
  # Sleect T = 0
  val_T1 <- as.yearqtr(as.Date(val_T1))
  
  # Either estimate until last obs or estimate until a given date
  if(is.null(forecast_end)) {
    end_q <- max(data[[date_col]], na.rm = TRUE)
  } else {
    end_q <- as.yearqtr(as.Date(forecast_end))
  }
  
  # Define the sequence of "Vantage Points" (The end of the period on which we estimate)
  # Is the today (end of information set) for the forecast
  forecast_dates <- data[[date_col]][data[[date_col]] >= start_q & data[[date_col]] <= end_q] 
  
  comp <- list()
  
  # Initial setup for the first warm start
  # We initialize with NULL so the wrapper uses manifest defaults for the first run
  current_theta <- NULL 
  
  message(sprintf("Rolling Estimation: %s to %s", start_q, end_q))
  
  # This always estimates from val_T1 to the forecast vantage point -> today
  # run parameter estimation on the full information set
  for (i in seq_along(forecast_dates)) {
    target_date <- forecast_dates[i]
    message("\n--- Vantage Point: ", target_date, " ---")
    
    # Slice Data available "Today"
    data_t <- data[data[[date_col]] <= target_date, ]
    
    # Process Exogenous Data
    # HP Filter is estimated inclduing the burn in period before the start of the information set
    
    # Error if there ar not enough valid gdp obs 
    valid_gdp_indices <- which(!is.na(data_t$log_gdp))
    if (length(valid_gdp_indices) < 5) {
      message("Skipping ", target_date, ": Not enough GDP data points.")
      next
    }
    
    # select GDP series
    first_obs_idx <- valid_gdp_indices[1]
    gdp_series    <- data_t$log_gdp[first_obs_idx:nrow(data_t)]
    
    # Run filter
    hp_res    <- mFilter::hpfilter(gdp_series, freq = 1600)
    gdp_cycle <- as.numeric(hp_res$cycle) * 100
    
    # Align cycle back to the time-series and create lags
    processed_data <- data_t
    processed_data$gap_l0 <- NA_real_
    processed_data$gap_l0[first_obs_idx:nrow(data_t)] <- gdp_cycle
    
    processed_data <- processed_data %>%
      mutate(
        gap_l1 = dplyr::lag(gap_l0, 1),
        gap_l2 = dplyr::lag(gap_l0, 2)
      ) %>%
      # Filter for Estimation Window
      filter(quarter >= val_T1) %>%
      filter(complete.cases(unemp_rate, gap_l0, gap_l1, gap_l2))
    
    if (nrow(processed_data) < 5) {
      message("Skipping ", target_date, ": No valid overlapping observations.")
      next
    }
    
    # Build Data Matrices
    Y_final <- as.matrix(processed_data[, c("unemp_rate", "spf_5y_unemp")])
    X_final <- as.matrix(processed_data[, c("gap_l0", "gap_l1", "gap_l2")])
    
    message(sprintf("Estimation range: %s to %s (%d obs)", 
                    min(processed_data$quarter), max(processed_data$quarter), nrow(processed_data)))
    
    # Initialize Blueprint
    my_ssm_model <- initialize_my_okun_ssm(Y_final,
                                           X_final,
                                           parameter_guesses = okun_parameter_guess)
    
    # Optimization: Multiple estimations with Warm Start
    # We use Nelder-Mead and BFGS that are very different for robustnes
    # each previous result becomes guess for the next
    opt_results <- ssm_optimizer_wrapper_okun(
      ssm       = my_ssm_model, 
      methods   = c("Nelder-Mead", "BFGS"), 
      iters     = 2, 
      start_par = current_theta
    )
    
    # Update current_theta for the next vantage point (Warm Start)
    current_theta <- opt_results$theta
    
    if(any(is.na(current_theta))) {
      message("Warning: Optimizer crashed. Resetting to manifest defaults.")
      current_theta <- NULL
    }
    
    
    # 8. Final Extraction of States
    final_states <- loglik_ssm_okun(current_theta, my_ssm_model, return_full_res = TRUE)
    
    # Store results
    comp[[as.character(target_date)]] <- list(
      target_date = target_date,
      params      = opt_results$params,
      states      = final_states
    )
    
    cat("Likelihood: ", -final_states$loglik, "Parameters", current_theta)
    cat(sprintf("\n [%d/%d] Estimated: %s\n", i, length(forecast_dates), as.character(target_date)))
  }
  
  return(comp)
}