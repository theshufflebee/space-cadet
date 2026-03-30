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








# --- Likelihood Objective function---
# its the function that the optimizer optimizes
loglik_okun <- function(theta, Y, X_data, model_spec) {
  
  # optimizer operates in "log" space where values from
  #negatie infitity to positive infinity are possible. But in the model it isn't for some
  # parameters such as standard deviations. so before calculating log likelyhood
  # we use the exp function to convert back to true parameters
  # model <- param2model(theta) 
  model <- param2model_gen(theta, model_spec)
  
  T_len <- nrow(Y)
  
  # Measurement Equation: [u, spf]' = [1, 1]' * Trend + [Cycle, 0]' + Noise
  # where Cycle is three obs of current and lag1, lag2 gdp cycle
  G <- matrix(c(1, 1), 2, 1) # Matrix of the state equation
  M <- diag(c(model$sigma_u, model$sigma_spf)) # Error matrix
  
  # Cyclical component: Sum of Betas * GDP Gaps (only affects Unemployment)
  betas <- c(model$beta0, model$beta1, model$beta2)
  mu_t  <- matrix(0, T_len, 2)
  mu_t[, 1] <- X_data %*% matrix(betas, 3, 1)
  
  # Transition Equation: Random Walk
  H    <- matrix(1, 1, 1) # random walk so H =1 (<1 would be AR)
  nu_t <- matrix(0, T_len, 1) # no intercet, we have a random walk
  N <- matrix(0.0001, 1, 1)     # Fixed Natural Rate Shock (found per trial and error)
  
  # Setting of KALMANN initial values
  Sigma_0 <- matrix(10, 1, 1) # uninformative (?) prior, no much confidence i initial guess
  
  # Here initial guess idea: Y[1, 2] the first obs of spf or mean(Y[ ,2]))
  rho_0   <- 0.03          # Whatever initial guess can make it "unsure" if sigma_0 is set large
  
  # Run Kalman Filter
  res <- kalman_filter(Y, nu_t, H, N, mu_t, G, M, Sigma_0, rho_0)
  
  # Return negative log-likelihood for minimization
  return(-sum(res$loglik.vector))
}


# --- Initial Values & Optimization ---
# Educated guesses: 
# sigmas ~ 0.5, betas (Okun) ~ -0.4, -0.2, -0.1
# model0 <- list(sigma_u = 0.5, sigma_spf = 0.5, 
#                beta0 = -0.4, beta1 = -0.2, beta2 = -0.1)
#theta_start <- model2param(model0)


okun_spec <- list(
  names   = c("sigma_u", "sigma_spf", "beta0", "beta1", "beta2"),
  pos_idx = c(1, 2) # First two are variances, rest are coefficients
)

# Start values as a readable list
start_list <- list(sigma_u = 0.005, sigma_spf = 0.003, beta0 = -0.4, beta1 = -0.2, beta2 = -0.1)

# Convert to raw vector for optimx
theta_start <- model2param_gen(start_list, okun_spec)




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
                      model_spec = okun_spec,
                      method = method,
                      control = list(maximize = FALSE, trace = 0))
    
    # Update estimate for next method/loop
    theta_est <- as.numeric(res_opt[1, 1:length(theta_start)])
    cat("Method:", method, "| NegLogLik:", round(res_opt$value, 4), "\n")
  }
}

# --- See Results ---
# final_model <- param2model(theta_est)
final_model <- param2model_gen(theta_est, okun_spec)
print(final_model)


# ----------------------------------------
