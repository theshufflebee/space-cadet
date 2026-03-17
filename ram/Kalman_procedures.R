

Kalman_filter <- function(Y_t,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0,
                          indic_pos=0,
                          Rfunction=Rf, Qfunction=Qf,
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
  
  Sigma_tt    = matrix(0,T,nr*nr) # After new obs
  Sigma_tp1_t = matrix(0,T,nr*nr) # Before new obs -> forecast
  Omega_tt    = matrix(0,T,ny*ny) # After new Obs
  Omega_tp1_t = matrix(0,T,ny*ny) # Before new obs -> forecast
  
  #Initilize log-Likelihood:
  logl = ny*T/2*log(2*pi)
  
  #Kalman algorithm:
  
  for (t in 1:T){
    
    # print(t)
    
    # ==========================================================================
    # Forecasting step (between t-1 and t):
    # ==========================================================================
    # Recusrion only works for t>1 so for 0 need to start with the initial guess
    if(t==1){
      rho_tp1_t[1,] = nu_t[1,] + t(H %*% rho_0) # rho_0 is initial guess, calculates first obs rho
      R = Rfunction(M,rho_0) #Rfunction defined below (measurement eq.) Noise of measurement
      Q = Qfunction(N,rho_0) #Qfunction defined below (transition eq.) Noise of transition
    } else {
      # Here where recursion kicks in when there is past obs
      rho_tp1_t[t,] = nu_t[t,] + t(H %*% rho_tt[t-1,]) # takes state in t-1 and projects it forward to to t
      # rho _tt is the filtered state (after updating t-1 with t )
      R = Rfunction(M,rho_tt[t-1,],t) # noise for obs equation
      Q = Qfunction(N,rho_tt[t-1,],t) # Noise for state equation
    }
    if(sum(indic_pos==1)>0){ # Some latent variables imposed to be >= 0
      rho_tp1_t[t,indic_pos==1] = pmax(rho_tp1_t[t,indic_pos==1],0) # will take the max of either 0 or the value
    }
    y_tp1_t[t,] = mu_t[t,] + t(G %*% rho_tp1_t[t,]) # forecast the measurement from t-1
    
    if(t==1){
      aux_Sigma_tp1_t = Q + H %*% Sigma_0 %*% t(H) # predict sigma from initial guess var of state equation error
    } else {
      aux_Sigma_tp1_t = Q + H %*% matrix(Sigma_tt[t-1,],nr,nr) %*% t(H) # predict sigma from t-1 var of state vector
    }
    
    Sigma_tp1_t[t,] = matrix( aux_Sigma_tp1_t ,1,nr*nr) # flattens the matrix in a single vector
    omega           = R + G %*% aux_Sigma_tp1_t %*% t(G) # sums up noise from measurements (R) and predicted state uncertainty
    Omega_tp1_t[t,] = matrix(omega,1,ny*ny) # flattens the whole thing again, for storage
    
    
    # ==========================================================================
    # Updating step
    # ==========================================================================
    
    # Detect observed variables:
    # Adjust what variables you can truly update
    vec.obs.indices <- which(!is.na(Y_t[t,])) # indices of observed variables
    ny.aux <- length(c(vec.obs.indices)) # number of observed variables
    # Resize matrices accordingly:
    G.aux <- matrix(G[vec.obs.indices,],nrow=ny.aux) # select observed variables
    R.aux <- R[vec.obs.indices,] # select observed variables
    omega <- omega[vec.obs.indices,] # select observed variables
    if(class(R.aux)[1]=="numeric"){ # safety step so it stays a matrix here
      R.aux <- R.aux[vec.obs.indices]
      omega <- omega[vec.obs.indices]
    }else{
      R.aux <- R.aux[,vec.obs.indices]
      omega <- omega[,vec.obs.indices]
    }
    R.aux <- matrix(R.aux,ny.aux,ny.aux)
    
    #Compute gain K:
    if(dim(G.aux)[1]>0){ # only update if there is at least one new observation
      # Calculate Kalman gain
      K = aux_Sigma_tp1_t %*% t(G.aux) %*% MASS::ginv(R.aux + G.aux %*% aux_Sigma_tp1_t %*% t(G.aux))
      
      lambda_t   = Y_t[t,] - y_tp1_t[t,] # forecast error
      lambda_t <- matrix(lambda_t[vec.obs.indices],ncol=1)
      rho_tt[t,] = t( rho_tp1_t[t,] + K %*% lambda_t ) # new updated state with obs t available
      if(sum(indic_pos==1)>0){
        rho_tt[t,indic_pos==1] = pmax(rho_tt[t,indic_pos==1],0)
      }
      Id         = diag(1,nrow=nr,ncol=nr) # identity matrix
      Sigma_tt[t,] = matrix( (Id - K %*% G.aux) %*% aux_Sigma_tp1_t,1,nr*nr) # updated sigma / var matrix

      # Computation of log-likelihood (determinant):
      if(length(c(omega))==1){
        det.omega <- omega
      }else{
        det.omega <- det(omega)
      }
      
      # Potential reconciliation between components of rho_tt (QKF):
      # QUadratic stuff
      y2Bfitted  <- matrix(Y_t[t,],ncol=1)
      constant   <- matrix(mu_t[t,],ncol=1)
      Rho_tt_1   <- rho_tp1_t[t,]
      opt        <- list(y2Bfitted,constant,G,M,Rho_tt_1,Q)
      rho_tt[t,] <- reconciliationf(rho_tt[t,],opt)
      
      loglik.vector <- rbind(loglik.vector,
                             ny.aux/2*log(2*pi) - 1/2*(log(det.omega) +
                                                         t(lambda_t) %*% MASS::ginv(omega) %*% lambda_t)
      )
      logl <- logl + loglik.vector[t] # what needs to be optimmized
      
    }else{ # if no update just use preditions (increases error)
      rho_tt[t,] = t( rho_tp1_t[t,])
      Sigma_tt[t,] = matrix( aux_Sigma_tp1_t,1,nr*nr)
    }
    
  }
  
  fitted.obs <- mu_t + rho_tt %*% t(G) # fitted observables -> estimate of y_hat_t given t
  
  output = list(r=rho_tt,Sigma_tt=Sigma_tt,loglik=logl,y_tp1_t=y_tp1_t,
                S_tp1_t=Sigma_tp1_t,r_tp1_t=rho_tp1_t,
                loglik.vector = loglik.vector,Omega_tp1_t=Omega_tp1_t,M=M,
                fitted.obs=fitted.obs)
  return(output)
}

Rf <- function(M,RHO,t=0){
  return(M %*% t(M))
}
Qf <- function(N,RHO,t=0){
  return(N %*% t(N))
}

Kalman_smoother <- function(Y_t,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0,indic_pos=0,
                            Rfunction=Rf, Qfunction=Qf){
  
  output_filter <- Kalman_filter(Y_t,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0,indic_pos,
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
  
  #Initilize output matrices:
  rho_tT       <- matrix(0,TT,nr)
  rho_tT[TT,]   <- rho_tt[TT,]
  Sigma_tT     <- matrix(0,TT,nr*nr)
  Omega_tT     <- matrix(0,TT,ny*ny) # This is the covariance matrix of Y_t if Y_t is missing
  Sigma_tT[TT,] <- Sigma_tt[TT,]
  Sigma_tT.aux <- matrix(Sigma_tt[TT,],nr,nr)
  R = Rfunction(M,rho_0) #Rfunction defined below (measurement eq.)
  Omega_tT[TT,] <- c(
    R + G %*% Sigma_tT.aux %*% t(G)
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
    Sigma_tT.aux <- Sigma_tt[t,] + F %*%
      matrix(Sigma_tT[t+1,]-Sigma_tp1_t[t+1,],nr,nr) %*% t(F)
    Sigma_tT[t,] <- c(Sigma_tT.aux)
    R = Rfunction(M,rho_0) #Rfunction defined below (measurement eq.)
    Omega_tT[t,] <- c(
      R + G %*% Sigma_tT.aux %*% t(G)
    )
  }
  fitted.obs <- mu_t + rho_tT %*% t(G) # fitted observables
  output = list(r_smooth=rho_tT,S_smooth=Sigma_tT,
                loglik=output_filter$loglik,
                loglik.vector=output_filter$loglik.vector,
                Omega_tp1_t=Omega_tp1_t,
                M=M,
                Sigma_tt=Sigma_tt,
                fitted.obs=fitted.obs,G=G,mu_t=mu_t,
                Omega_tT = Omega_tT)
  return(output)
}




