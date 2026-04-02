# NEed MASS Packafe


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
#'
#' @return A list containing:
#' \item{r}{Filtered variables \eqn{\rho_{t|t}} (size \eqn{T \times nr}).}
#' \item{Sigma_tt}{Filtered state covariance \eqn{\Sigma_{t|t}} stored as flattened vectors.}
#' \item{loglik}{Scalar. Total log-likelihood of the model.}
#' \item{y_tp1_t}{Forecasted observables \eqn{y_{t|t-1}}.}
#' \item{S_tp1_t}{Predicted state covariance \eqn{\Sigma_{t|t-1}}.}
#' \item{r_tp1_t}{Predicted state vector \eqn{\rho_{t|t-1}}.}
#' \item{loglik.vector}{Vector of date-specific log-likelihoods.}
#' \item{Omega_tp1_t}{Innovation covariance \eqn{\Omega_{t|t-1}}.}
#' \item{M}{The measurement scaling matrix used.}
#' \item{fitted_obs}{Estimated observables given filtered states (\eqn{\hat{y}_t|t}).}
#' 
#' @note 
#' If \code{reconciliationf} is provided, \code{rho_tt} is modified to ensure consistency 
#' defined by the function (used in QKF for augmented state vectors).
#' 
#' @export
kalman_filter <- function(Y_t,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0,
                          indic_pos=0,
                          Rfunction=calc_covariance, Qfunction=calc_covariance,
                          reconciliationf = function(x,opt){x} # In case
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

    } else {
      rho <- rho_tt[t-1,]
      Sigma <- Sigma_tt[t-1,]
    }
      
    # First we project forward the initial guess of rho_0. here both H and nu_t[1,] will be estimated
    
    # I replaced the one below with the function visible next
    #rho_tp1_t[1,] = nu_t[1,] + t(H %*% rho_0) # rho_0 is initial guess, calculates first obs rho, nu_t 
    rho_tp1_t[t,] <- project_state_forward(rho = rho,
                                           H = H,
                                           nu_t = nu_step)
    
    
    # Calculate the Noise, these square M and N are the shocks hitting the system
    # Functions are defined below
    R = Rfunction(M, rho, t)
    Q = Qfunction(N, rho, t)
    
    
    
    # Here if any latent variable is imposed to be >=0 this selects the max of 0 and the variable
    if(sum(indic_pos==1)>0){ # Some latent variables imposed to be >= 0
      rho_tp1_t[t, indic_pos==1] = pmax(rho_tp1_t[t, indic_pos==1], 0)
    }
    
    aux_Sigma_tp1_t = Q + H %*% matrix(Sigma, nr, nr) %*% t(H) # predict sigma from t-1 var of state vector
    
    
    # Forecast the measurement using the previously forecasted state from t-1
    # the equation below is in function
    #y_tp1_t[t,] = mu_t[t,] + t(G %*% rho_tp1_t[t,]) # forecast the measurement from t-1
    
    y_tp1_t[t,] <- forecast_measurement_eq(mu_t = mu_t[t,], G = G, rho = rho)
      

    
    
    # Predict Sigma: If first step, use initial guess
    # Here Q is the uncertainty of the shocks, while Sigma is the state estimation error cov
    # We predict the uncertainty around the state here
    
    # The equation below is repaced with the function
    #aux_Sigma_tp1_t <- Q + H %*% Sigma %*% t(H)
    aux_Sigma_tp1_t <- project_covariance(Q, Sigma, H)
    
    
    # Save sigma here in the storage again by turning it into a single vector
    Sigma_tp1_t[t,] = matrix(aux_Sigma_tp1_t, 1, nr*nr)
    
    # Omega takes the predicted uncertainty around the state and turns it into the predicted measurement error
    # Omega is innovation covariance
    
    # The euqation below is replaced with the following function
    # omega = R + G %*% aux_Sigma_tp1_t %*% t(G) # sums up noise from measurements (R) and predicted state uncertainty
    omega <- project_covariance(R, aux_Sigma_tp1_t, G)
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
    
  }
  
  fitted_obs <- mu_t + rho_tt %*% t(G) # fitted observables -> estimate of y_hat_t given t
  
  # Create output list
  output = list(r=rho_tt,Sigma_tt=Sigma_tt,loglik=logl,y_tp1_t=y_tp1_t,
                S_tp1_t=Sigma_tp1_t,r_tp1_t=rho_tp1_t,
                loglik.vector = loglik.vector,Omega_tp1_t=Omega_tp1_t,M=M,
                fitted_obs=fitted_obs)
  return(output)
}



#===============================================================================
# Helper Function for the Kalman Filter
#===============================================================================


# Both equations below are the same but serve different purpouses
# need to find a way to unify
project_state_forward <- function(rho, H, nu_t) {
  # nu_t_step should be the specific row for time t: nu_t[t, ]
  # Ensure rho is a column vector
  rho_pred <- (H %*% matrix(rho)) + matrix(nu_t)
  
  return(rho_pred)
}

forecast_measurement_eq <- function(mu_t, G, rho) {
  
  y_tp1_t <- (G %*% matrix(rho)) + matrix(mu_t)
  
  return(t(y_tp1_t))
}


# Currently some parameter here unseless, beacause it may get extended later
calc_covariance <- function(SD_matrix, RHO = NULL, t = 0) {
  # Standard formula: Var = SD * SD'
  return(SD_matrix %*% t(SD_matrix))
}


# Replaces two equations
project_covariance <- function(noise, sigma_base, transform_matrix) {
  
  
  uncertainty <- noise + transform_matrix %*% sigma_base %*% t(transform_matrix)
  
  return(uncertainty)
}

calculate_kalman_gain <- function(sigma, G, R){
  
  K = sigma %*% t(G) %*% MASS::ginv(R + G %*% sigma %*% t(G))
  
  return(K)
}


#------------------------------------------------------------------------------- 
#
# Kalman Smoother
#
#-------------------------------------------------------------------------------
kalman_smoother <- function(Y_t,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0,indic_pos=0,
                            Rfunction=calc_covariance, Qfunction=calc_covariance){
  
  output_filter <- kalman_filter(Y_t,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0,indic_pos,
                                 Rfunction,Qfunction)
  rho_tt        <- output_filter$r
  Sigma_tt      <- output_filter$Sigma_tt
  Sigma_tp1_t   <- output_filter$S_tp1_t
  rho_tp1_t     <- output_filter$r_tp1_t
  Omega_tp1_t   <- output_filter$Omega_tp1_t
  
  # Number of observed variables:
  ny = NCOL(Y_t)
  # Number of unobs. variables:
  nr = NCOL(G)
  # Number of time periods:
  TT = NROW(Y_t)
  
  if(class(M)[1]=="list"){
    M.aux <- M$sigmas
  }else{
    M.aux <- M
  }
  
  #R = M%*%t(M)
  #Q = N%*%t(N)
  
  #Initialize output matrices:
  rho_tT       <- matrix(0,TT,nr)
  rho_tT[TT,]   <- rho_tt[TT,]
  Sigma_tT     <- matrix(0,TT,nr*nr)
  Omega_tT     <- matrix(0,TT,ny*ny) # This is the covariance matrix of Y_t if Y_t is missing
  Sigma_tT[TT,] <- Sigma_tt[TT,]
  Sigma_tT_aux <- matrix(Sigma_tt[TT,],nr,nr)
  R = Rfunction(M,rho_0) #Rfunction defined below (measurement eq.)
  Omega_tT[TT,] <- c(
    R + G %*% Sigma_tT_aux %*% t(G)
  )
  Var.model.implied.obs <- NaN * Y_t # this matrix will contain the variances of G*rho_t
  Var.model.implied.obs[TT,] <- diag(
    G %*% matrix(Sigma_tT[TT,],nr,nr) %*% t(G))
  
  for(t in seq(TT-1,1,by=-1)){
    F <- matrix(Sigma_tt[t,],nr,nr) %*% t(H) %*% solve(matrix(Sigma_tp1_t[t+1,],nr,nr))
    rho_tT[t,] <- rho_tt[t,] + t( F %*% (rho_tT[t+1,]-rho_tp1_t[t+1,]) )
    if(sum(indic_pos==1)>0){
      rho_tT[t,indic_pos==1] = pmax(rho_tT[t,indic_pos==1],0)
    }
    Sigma_tT_aux <- Sigma_tt[t,] + F %*%
      matrix(Sigma_tT[t+1,]-Sigma_tp1_t[t+1,],nr,nr) %*% t(F)
    Sigma_tT[t,] <- c(Sigma_tT_aux)
    R = Rfunction(M,rho_0) #Rfunction defined below (measurement eq.)
    Omega_tT[t,] <- c(
      R + G %*% Sigma_tT_aux %*% t(G)
    )
  }
  fitted_obs <- mu_t + rho_tT %*% t(G) # fitted observables
  output = list(r_smooth=rho_tT,S_smooth=Sigma_tT,
                loglik=output_filter$loglik,
                loglik.vector=output_filter$loglik.vector,
                Omega_tp1_t=Omega_tp1_t,
                M=M,
                Sigma_tt=Sigma_tt,
                fitted_obs=fitted_obs,G=G,mu_t=mu_t,
                Omega_tT = Omega_tT)
  return(output)
}









