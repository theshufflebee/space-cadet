library(dplyr)
library(tidyr)
library(mFilter)

# Prepare GDP: Filter, ensure numeric, and remove any hidden NAs
gdp_prepared <- master_okun_model_long %>%
  filter(variable == "gdp", quarter >= as.yearqtr(hp_filter_burn_in)) %>%
  arrange(quarter) %>%
  # Crucial: Drop NAs here so the HP filter doesn't break
  filter(!is.na(value)) %>% 
  mutate(log_gdp = log(as.numeric(value)))


# Apply HP Filter
# If log_gdp has ANY NA or Inf, hpfilter returns a vector of NaNs
hp_results <- hpfilter(gdp_prepared$log_gdp, freq = 1600)

# econstruct the Exogenous features df
df_features <- data.frame(
  quarter = gdp_prepared$quarter,
  y_gap   = as.numeric(hp_results$cycle)
) %>%
  mutate(
    y_gap_l0 = y_gap,
    y_gap_l1 = dplyr::lag(y_gap, 1),
    y_gap_l2 = dplyr::lag(y_gap, 2)
  )


# ------------------------------------------------------------------------------

# Extract Unemployment and SPF from long df
target_data <- master_okun_model_long %>%
  filter(variable %in% c("unemp_rate", "spf_5y_unemp")) %>%
  pivot_wider(names_from = variable, values_from = value)

# join dfs together and filter out pre spf data by removing everything before 2015
df_okun_final <- df_features %>%
  left_join(target_data, by = "quarter") %>% 
  filter(quarter >= as.yearqtr("2015 Q1"))

# Create the Matrices for the Kalman Filter
Y_final <- as.matrix(df_okun_final[, c("unemp_rate", "spf_5y_unemp")])
X_final <- as.matrix(df_okun_final[, c("y_gap_l0", "y_gap_l1", "y_gap_l2")])


# ------------------------------------------------------------------------------

# Extract the latest parameters from the params df containing all param estimation
final_theta <- okun_params_df %>% filter(quarter == max(quarter))

final_params_list <- list(
  beta1 = final_theta$beta1,
  beta2 = final_theta$beta2,
  beta3 = final_theta$beta3,
  sigma_unemp_rate   = final_theta$sigma_unemp_rate,
  sigma_spf_5y_unemp = final_theta$sigma_spf_5y_unemp
)

# Initialize factories with the final data dimensions
final_okun_fact  <- mu_t_matrix_factory(X_final, Y_final, intercept = FALSE)
final_trans_fact <- H_matrix_factory(random_walk = TRUE)
final_link_fact  <- G_matrix_factory(Y_final)
final_noise_fact <- M_matrix_factory(Y_final)

# 3. Build the final matrices using the 'best_theta'
mu_t_res <- final_okun_fact$builder(final_theta)
H_res    <- final_trans_fact$builder(final_theta)
G_res    <- final_link_fact$builder(final_theta)
M_res    <- final_noise_fact$builder(final_theta)

# 4. Run the Kalman Filter one last time to get the 'rho' (Natural Rate)
# We use the full history Y_final
final_filter_results <- kalman_filter(
  Y_t     = Y_final, 
  nu_t    = matrix(0, nrow(Y_final), 1), 
  H       = H_res, 
  N       = matrix(0.0001, 1, 1), # Process noise for the state
  mu_t    = mu_t_res, 
  G       = G_res, 
  M       = M_res, 
  Sigma_0 = matrix(10, 1, 1), 
  rho_0   = Y_final[1, 2] # Initial guess usually based on SPF
)



# 4. Consolidate for Visualization
viz_df <- data.frame(
  date       = df_okun_final$quarter,
  actual     = as.numeric(Y_final[, 1]),
  spf        = as.numeric(Y_final[, 2]),
  u_star     = as.vector(final_filter_results$r),
  okun_cycle = as.numeric(X_final[, 1])
) %>%
  mutate(u_gap = actual - u_star)

#-------------------------------------------------------------------------------

library(ggplot2)
library(zoo) # for as.yearqtr if not already loaded

df <- viz_df

# Ensure your date column is in the correct format for plotting
df$date <- as.yearqtr(df$date)

ggplot(df, aes(x = date)) +
  # 1. Actual Unemployment Rate
  geom_line(aes(y = actual, color = "Obs Unemployment"), size = 1, alpha = 0.6) +
  
  # 2. SPF 5y Forecast
  geom_line(aes(y = spf, color = "SPF 5y Forecast"), linetype = "dashed", size = 0.8) +
  
  # 3. Estimated Natural Rate (rho)
  geom_line(aes(y = u_star, color = "Natural Rate"), size = 1.2) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c(
    "Obs Unemployment" = "gray50",
    "SPF 5y Forecast" = "firebrick3",
    "Natural Rate" = "dodgerblue4"
  )) +
  labs(
    title = "Swiss Unemployment: Observed vs. Structural",
    x = NULL,
    y = "Unemployment Rate",
    color = "Series"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
