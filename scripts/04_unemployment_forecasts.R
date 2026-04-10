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
start_date <- "2010-01-01"


df_okun_merged <- X_okun %>%
  filter(quarter >= start_date) %>%
  left_join(Y_okun,
            by = "quarter")

non_na_range <- which(complete.cases(df_okun_merged[,colnames(X_okun)]))

first_valid <- min(non_na_range)
last_valid <- max(non_na_range)

message("save estimation range: ", first_valid, "to", last_valid)

df_okun_final <- df_okun_merged[first_valid:last_valid, ]

if(any(is.na(df_okun_final[, c("log_gdp")]))) {
  warning("There are NA values in the exogenous regressors")
}


Y <- as.matrix(df_okun_final[ , c("unemp_rate", "spf_5y_unemp")])

T <- nrow(Y)

X <- as.matrix(df_okun_final[ , c("log_gdp")])

# Dataprep for Okun complete -> probably move this back into the previous script
# ==============================================================================

# --- Initialize all Factories ---
# This is how we get the functions for all matrices and the corresponding
# parameter mappings and rules for transformations should parameters be restricted
# This ensures p_names in the factory matches your 3-lag X matrix later.
beta_names <- c("beta1", "beta2", "beta3")

# Pass a dummy matrix with 3 columns so the factory initializes for 3 parameters
X_dummy <- matrix(0, nrow = nrow(Y), ncol = 3) 

# Initialize the factory using the dummy structure
mu_t_builder  <- mu_t_matrix_factory(X_dummy, Y, intercept = FALSE)
H_builder <- H_matrix_factory(random_walk = TRUE)
G_builder <- G_matrix_factory(Y)
M_builder <- M_matrix_factory(Y)

# sum all builders up for forecasting later
all_builder_functions <- list(
  mu_t    = mu_t_builder$builder,
  H       = H_builder$builder,
  G       = G_builder$builder,
  M       = M_builder$builder
)

# --- Collect the Master Manifest ---
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

# --- Force the correct Beta starting values ---
all_defaults$beta1 <- -0.1
all_defaults$beta2 <- -0.05
all_defaults$beta3 <- -0.01

# -- Create the Master Spec ---
okun_spec <- list(
  # Ensure mu_t params are explicitly listed as the 3 betas
  mu_t = list(params = beta_names),
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
                                       x_col = c("log_gdp"),
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


forecast_okun_ssm <- function(params_df,
                              date_col = "quarter",
                              forecast_h = 8,
                              ) {
  
  
}


okun_params_df <- read_csv("output/okun_forecast_parameters.csv") %>%
  mutate(quarter = as.yearqtr(quarter))

# Setup Dates that are to be forecast
# as the okun df contains dates that are the "today" for forecasts, for all available dates
# we forecast over its full range
dates <- okun_params_df$quarter # dates are all dates where we forecast
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



gdp_gap_forecasts <-df_okun_final[c("quarter", "log_gdp")]






# Fill the Forecasts
for (i in seq_along(dates)) {
  forecast_origin <- dates[i] # select start date
  
  # Select parameters / state for this vintage
  current_params <- okun_params_df[i, ]
  rho_T <- current_params$natural_rate
  betas <- c(current_params$beta1, current_params$beta2, current_params$beta3)
  
  # get 8 quarters after "today" the origin
  look_ahead_dates <- seq(forecast_origin + 0.25, by = 0.25, length.out = forecast_h)
  
  # get the historical true data known today
  history_gdp <- X_okun %>%
    filter(quarter <= forecast_origin) %>%
    select(quarter, log_gdp)
  
  # Get 8-quarter forecast starting after 'today'
  future_gdp <- gdp_gap_forecasts %>%
    filter(quarter > forecast_origin) %>%
    slice(1:forecast_h) %>%
    select(quarter, log_gdp)
  
  # Stack them to get future gdp gaps
  combined_gdp <- bind_rows(history_gdp, future_gdp) %>%
    arrange(quarter) %>%
    filter(!is.na(log_gdp))
  
  hp_res <- hpfilter(combined_gdp$log_gdp, freq = 1600)
  combined_gdp$y_gap <- as.numeric(hp_res$cycle)
  
  # Create the 3 Lag Columns
  gdp_features_wide <- combined_gdp %>%
    mutate(
      gdp_gap  = y_gap,
      gap_lag1 = dplyr::lag(y_gap, 1),
      gap_lag2 = dplyr::lag(y_gap, 2)
    ) %>%
    # Select only the horizon we want to forecast
    filter(quarter > forecast_origin) %>%
    arrange(quarter) %>%
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









