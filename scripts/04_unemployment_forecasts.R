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


df_okun_merged <- X_okun %>%
  filter(quarter >= start_date) %>%
  left_join(Y_okun,
            by = "quarter")

non_na_range <- which(complete.cases(df_okun_merged[,colnames(X_okun)]))

first_valid <- min(non_na_range)
last_valid <- max(non_na_range)

message("save estimation range: ", first_valid, "to", last_valid)

df_okun_final <- df_okun_merged[first_valid:last_valid, ]

if(any(is.na(df_okun_final[, c("gdp_gap", "gap_lag1", "gap_lag2")]))) {
  warning("There are NA values in the exogenous regressors")
}


Y <- as.matrix(df_okun_final[ , c("unemp_rate", "spf_5y_unemp")])

T <- nrow(Y)

X <- as.matrix(df_okun_final[ , c("gdp_gap", "gap_lag1", "gap_lag2")])

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




parameter_output <- get_ssm_forecast_parameters(data = df_okun_final,
                                       y_cols = c("unemp_rate", "spf_5y_unemp"),
                                       x_cols = c("gdp_gap", "gap_lag1", "gap_lag2"),
                                       date_col = "quarter",
                                       all_builder_functions = all_builder_functions,
                                       spec = okun_spec ,
                                       all_defaults = all_defaults,
                                       forecast_start = "2021-04-01",
                                       forecast_end = NULL)



