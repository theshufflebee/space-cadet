################################################################################
#
# This Model forecasts the Swiss unemp_rate Rate
#
################################################################################

# Parameters used here
#name but dont define here

source(here("R", "kalman_procedures.R"))
library(optimx)


# --- Natural Rates: unemp_rate and output ---
# function: kalman_filter(unemp_rate)

# Trend Output
# function HP Filter(gdp)
# use the package for HP filter

# --- State equaltion and measurement equation build ---

# Have Kalman predict and update funtion
# Loop it
# How do I handle Betas of Okun? Time Varying parameters -> Rolling estimation?
# Reestimate every loop then?

# Use JPR Filter?
# -> Has ZLB in it


# ==============================================================================


# We can't have NaNs in the GDP Gap
# Further all estimation goes on from the spf start in 2015
Y <- Y_okun[Y_okun$quarter >= as.yearqtr("2015-01-01"), ]
Y <- as.matrix(Y[ , c("unemp_rate", "spf_5y_unemp")])

T <- nrow(Y)

X <- X_okun[X_okun$quarter >= as.yearqtr("2015-01-01"), ]

X <- as.matrix(X[ , c("gdp_gap", "gap_lag1", "gap_lag2")])


valid_rows <- complete.cases(X)

# Subset both matrices to keep only these rows
# This ensures Y and X remain perfectly synchronized by date
Y <- as.matrix(Y[valid_rows, c("unemp_rate", "spf_5y_unemp")])
X <- as.matrix(X[valid_rows, c("gdp_gap", "gap_lag1", "gap_lag2")])

# Check the new dimensions
nrow(Y)
nrow(X) # Both must be qual



# --- 1. Parameter Mapping Functions ---
# Theta: [log_sigma_u, log_sigma_spf, beta0, beta1, beta2]
param2model <- function(theta){
  list(
    sigma_u   = exp(theta[1]), # shock to the unemployment rate
    sigma_spf = exp(theta[2]), # Shock to the SPF forecast
    beta0     = theta[3],      
    beta1     = theta[4],      
    beta2     = theta[5]
  )
}

model2param <- function(model){
  c(log(model$sigma_u),
    log(model$sigma_spf),
    model$beta0,
    model$beta1,
    model$beta2)
}

# --- Likelihood Objective function---
loglik_okun <- function(theta, Y, X_data) {
  model <- param2model(theta)
  T_len <- nrow(Y)
  
  # Measurement Equation: [u, spf]' = [1, 1]' * Trend + [Cycle, 0]' + Noise
  G <- matrix(c(1, 1), 2, 1) # Matrix of the 
  M <- diag(c(model$sigma_u, model$sigma_spf)) # Error matrix
  
  # Cyclical component: Sum of Betas * GDP Gaps (only affects Unemployment)
  betas <- c(model$beta0, model$beta1, model$beta2)
  mu_t  <- matrix(0, T_len, 2)
  mu_t[, 1] <- X_data %*% matrix(betas, 3, 1)
  
  # Transition Equation: Random Walk
  H    <- matrix(1, 1, 1) # random walk so H =1 (<1 would be AR)
  nu_t <- matrix(0, T_len, 1) # no intercet, we have a random walk
  N <- matrix(0.0001, 1, 1)     # Fixed Natural Rate Shock (found per trial and error)
  
  # Initialization
  Sigma_0 <- matrix(10, 1, 1) # uninformative (?) prior, no much confidence i initial guess
  
  # Here initial guess idea: Y[1, 2] the first obs of spf or mean(Y[ ,2]))
  rho_0   <- 0.03          # Whatever initial guess can make it unsure if sigma_0 is large
  
  # Run Kalman Filter
  res <- kalman_filter(Y, nu_t, H, N, mu_t, G, M, Sigma_0, rho_0)
  
  # Return negative log-likelihood for minimization
  return(-sum(res$loglik.vector))
}

# --- 3. Initial Values & Optimization ---
# Educated guesses: 
# sigmas ~ 0.5, betas (Okun) ~ -0.4, -0.2, -0.1
model0 <- list(sigma_u = 0.5, sigma_spf = 0.5, 
               beta0 = -0.4, beta1 = -0.2, beta2 = -0.1)
theta_start <- model2param(model0)

# The Optimization Loop
theta_est <- theta_start
nb_loop   <- 3

for(i in 1:nb_loop){
  cat("\n--- Starting Loop", i, "---\n")
  for(method in c("Nelder-Mead", "nlminb")){
    res_opt <- optimx(par = theta_est, 
                      fn = loglik_okun, 
                      Y = as.matrix(Y), 
                      X_data = as.matrix(X), 
                      method = method,
                      control = list(maximize = FALSE, trace = 0))
    
    # Update estimate for next method/loop
    theta_est <- as.numeric(res_opt[1, 1:length(theta_start)])
    cat("Method:", method, "| NegLogLik:", round(res_opt$value, 4), "\n")
  }
}

# --- See Results ---
final_model <- param2model(theta_est)
print(final_model)


# ----------------------------------------
