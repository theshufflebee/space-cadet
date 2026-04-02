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

# keep it in this format
start_date <- "2015-01-01"

# We can't have NaNs in the GDP Gap
# Further all estimation goes on from the spf start in 2015
Y_date_select <- Y_okun[Y_okun$quarter >= as.yearqtr(start_date), ]
Y <- as.matrix(Y_date_select[ , c("unemp_rate", "spf_5y_unemp")])

T <- nrow(Y)

X <- X_okun[X_okun$quarter >= as.yearqtr(start_date), ]

X <- as.matrix(X[ , c("gdp_gap", "gap_lag1", "gap_lag2")])

# Select non NA rows as we can't have missing values in the exogenous variables
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

mu_t_builder  <- mu_t_matrix_factory(X, Y, intercept = FALSE)
H_builder <- H_matrix_factory(random_walk = TRUE)
G_builder <- G_matrix_factory(Y)
M_builder <- M_matrix_factory(Y)

# sum all builders up fore forecasting later
all_builder_functions <- list(
  mu_t    = mu_t_builder$builder,
  H       = H_builder$builder,
  G       = G_builder$builder,
  M       = M_builder$builder
)

# --- Collect the Master Manifest ---
# We combine the rules and defaults from all active components into one for
#further processing
all_rules <- c(
  mu_t_builder$manifest$rules,
  H_builder$manifest$rule,
  M_builder$manifest$rules
)

all_defaults <- c(
  mu_t_builder$manifest$default,
  H_builder$manifest$default,
  M_builder$manifest$default
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
  mu_t_builder = all_builder_functions$mu_t,
  G_builder    = all_builder_functions$G,
  H_builder    = all_builder_functions$H,
  M_builder    = all_builder_functions$M
)

