################################################################################
#
# This Model forecasts the Swiss unemp_rate Rate
#
################################################################################

# Parameters used here
#name but dont define here

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


# must turn this in a wrapper later

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

# Dataprep for Okun complete -> probably move this back into the previous script
# ==============================================================================

# --- Initialize all Factories ---
# This is how we get the functions for all matrices and the corresponding
# parameter mappings and rules for transformations should parameters be restricted

okun_factory  <- mu_t_matrix_factory(X, Y, intercept = FALSE)
trans_factory <- H_matrix_factory(random_walk = TRUE) # Set TRUE for Random Walk
g_link_factory <- G_matrix_factory(Y)
m_noise_factory <- M_matrix_factory(Y)

# --- Collect the Master Manifest ---
# We combine the rules and defaults from all active components into one for
#further processing
all_rules <- c(
  okun_factory$manifest$rules,
  trans_factory$manifest$rule,
  m_noise_factory$manifest$rules
)

all_defaults <- c(
  okun_factory$manifest$default,
  trans_factory$manifest$default,
  m_noise_factory$manifest$default
)

# -- Create the Master Spec ---
# this is to pass into the optimizer so parameter transformations are done correctly
okun_spec <- list(
  names = names(all_rules),
  rules = all_rules
)

# Convert the human / paramete guesses to optimizer starting values
theta_start <- model2param_gen(all_defaults, okun_spec)

# --- Run the Optimizer Wrapper ---
# pass into it the builder functions for all matrices
params_results <- ssm_optimizer_wrapper(
  nb_loop      = 3,
  theta_start  = theta_start,
  Y            = Y,
  X            = X,
  model_spec   = okun_spec,
  mu_t_builder = okun_factory$builder,
  G_builder    = g_link_factory$builder,
  H_builder    = trans_factory$builder,
  M_builder    = m_noise_factory$builder
)

