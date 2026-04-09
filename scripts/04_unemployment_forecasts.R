################################################################################
#
# This script estimates the parameters for the Okun model and runs the forecast
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
# params_results <- ssm_optimizer_wrapper(
#   nb_loop      = 2,
#   theta_start  = theta_start,
#   Y            = Y,
#   X            = X,
#   model_spec   = okun_spec,
#   mu_t_builder = all_builder_functions$mu_t,
#   G_builder    = all_builder_functions$G,
#   H_builder    = all_builder_functions$H,
#   M_builder    = all_builder_functions$M
# )



parameter_output <- get_ssm_forecast_parameters(data = df_okun_final,
                                       y_cols = c("unemp_rate", "spf_5y_unemp"),
                                       x_cols = c("gdp_gap", "gap_lag1", "gap_lag2"),
                                       date_col = "quarter",
                                       all_builder_functions = all_builder_functions,
                                       spec = okun_spec ,
                                       all_defaults = all_defaults,
                                       forecast_start = "2022-07-01",
                                       forecast_end = NULL)



params_df <- parameter_output %>%
  map_dfr(function(x) {
    # Convert parameters to a data frame
    p_df <- as.data.frame(x$params)
    
    # Extract the last value of the filtered state r with tail()
    p_df$natural_rate <- as.numeric(tail(x$states$r, 1)) # so it's not another format
    
    return(p_df)
  }, .id = "quarter") %>%
  # Convert the character dates back to yearqtr
  mutate(quarter = as.yearqtr(quarter))

# Preview the result
print(head(params_df))



write_csv(params_df, "output/okun_forecast_parameters.csv")


predict_ssm_path_simple <- function(rho_start, betas, gdp_features) {
  # gdp_features is h rows by 3 columns
  # Matrix multiplication: (h x 3) %*% (3 x 1) = (h x 1)
  cyclical_impact <- as.matrix(gdp_features) %*% matrix(betas)
  
  # Forecast = Intercept (Natural Rate) + Cyclical part
  return(as.numeric(rho_start + cyclical_impact))
}



# Pseudo Out of sample Forecasts
################################################################################
# Load parameter df needed for forecast
okun_params_df <- read_csv("output/okun_forecast_parameters.csv") %>%
  mutate(quarter = as.yearqtr(quarter))

# Setup Dates that are to be forecast
# as the okun df contains dates that are the "today" for forecasts, for all available dates
# we forecast over its full range
dates <- okun_params_df$quarter
forecast_h <- 8

# we extend to the forecast dataframe so we can store the full forecast
# even for the last date
# dimensions are t columns and t+h rows

# get rows
extended_rows <- seq(min(dates), by = 0.25, length.out = length(dates) + forecast_h)

# Initialize Matrix to store forecast
eval_mat <- matrix(NA, nrow = length(extended_rows), ncol = length(dates))
rownames(eval_mat) <- as.character(extended_rows)
colnames(eval_mat) <- as.character(dates)

# Fill Diagonal with the actual values of unemployment at the given date
# select actual values
actual_unemp_df <- df_okun_final %>%
  filter(quarter %in% dates) %>%
  arrange(quarter)

# loop over it where row and culnmn are both date i to fill diagonal
for(i in seq_along(dates)) {
  eval_mat[as.character(dates[i]), i] <- actual_unemp_df$unemp_rate[i]
}


# Function defined to predict the random walk ssm with exogenous betas
# gdp features is a dataframe where each row is a time in h and each column is a lag
predict_ssm_path_simple <- function(rho_start, betas, gdp_features) {
  cyclical_impact <- as.matrix(gdp_features) %*% matrix(betas)
  return(as.numeric(rho_start + cyclical_impact))
}

# Fill the Forecasts
for (i in seq_along(dates)) {
  forecast_origin <- dates[i] # select start date
  
  # Select parameters / state for this vintage
  current_params <- okun_params_df[i, ]
  rho_T <- current_params$natural_rate
  betas <- c(current_params$beta1, current_params$beta2, current_params$beta3)
  
  # get 8 quarters after "today" the origin
  look_ahead_dates <- seq(forecast_origin + 0.25, by = 0.25, length.out = forecast_h)
  
  # Pivot the Long Data to Wide for the math
  gdp_features_wide <- master_okun_model_long %>%
    
    # Filter for the dates and the gdp variables
    filter(quarter %in% look_ahead_dates,
           variable %in% c("gdp_gap", "gap_lag1", "gap_lag2")) %>%
    
    # Pivot so each variable is a column and each row is a tim ein h for function
    pivot_wider(names_from = variable, values_from = value) %>%
    arrange(quarter) %>%
    # Ensure columns are in the order betas: t, t-1, t-2
    select(gdp_gap, gap_lag1, gap_lag2)
  
  # get forecast
  if (nrow(gdp_features_wide) > 0) {
    h_available <- nrow(gdp_features_wide)
    # currently here for end of horizon where we have less than 8 known dates
    # later we use the gdp forecasts and can run that easily
    
    # Use Random walk ssm prediction function
    path <- predict_ssm_path_simple(rho_T, betas, gdp_features_wide)
    
    # Add to matrix
    target_rows <- as.character(look_ahead_dates[1:h_available])
    eval_mat[target_rows, i] <- path
  }
}

eval_df <- as.data.frame(eval_mat)

# Final Result
eval_df <- as.data.frame(eval_mat)

write_csv(eval_df, "output/forecast_df.csv")









