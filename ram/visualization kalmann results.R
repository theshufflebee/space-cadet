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

okun_params_df <- okun_params_df_new

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



library(ggplot2)
library(patchwork)
library(scales)

# 1. Top Plot: Unemployment Breakdown
p_unemp <- ggplot(viz_df, aes(x = date)) +
  # Actual vs Natural Rate
  geom_line(aes(y = actual, color = "Obs Unemployment"), linewidth = 0.8, alpha = 0.5) +
  geom_line(aes(y = u_star, color = "Natural Rate (u*)"), linewidth = 1.2) +
  geom_line(aes(y = spf, color = "SPF 5y Forecast"), linetype = "dashed", size = 0.8) +
  # Formatting
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c(
    "Obs Unemployment" = "gray50",
    "SPF 5y Forecast" = "firebrick3",
    "Natural Rate (u*)" = "dodgerblue4"
  )) +
  labs(
    title = "Swiss Structural Unemployment and GDP Cycle",
    subtitle = "Top: Observed vs. Natural Rate | Bottom: Output Gap (HP Filter)",
    y = "Unemployment Rate", x = NULL, color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

# 2. Bottom Plot: GDP Gap (Exogenous driver)
p_gdp <- ggplot(viz_df, aes(x = date)) +
  # The GDP Gap as an area plot to show 'slack' vs 'boom'
  geom_area(aes(y = okun_cycle), fill = "firebrick3", alpha = 0.3) +
  geom_line(aes(y = okun_cycle), color = "firebrick3", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Formatting
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(y = "GDP Gap (%)", x = "Quarter") +
  theme_minimal()

# 3. Combine with Patchwork
# heights = c(2, 1) makes the unemployment plot larger than the gap plot
combined_okun_plot <- p_unemp / p_gdp + 
  plot_layout(heights = c(3, 1))

# Display
combined_okun_plot

# 4. Save
ggsave(
  filename = "output/okun_model_fit.pdf",
  plot = combined_okun_plot,
  width = 10, height = 8
)


# -------------------

library(ggplot2)
library(dplyr)
library(patchwork) # For combining plots

# 1. Calculate Predicted Values and Residuals
# The prediction in a State Space model is: y_hat = mu_t + G * rho_t
# Since G is 1 for the first variable:
viz_df <- viz_df %>%
  mutate(
    okun_impact = as.numeric(mu_t_res[, 1]),
    predicted   = u_star + okun_impact,
    error       = actual - predicted
  )

# 2. Plot A: Predicted vs. Actual
p1 <- ggplot(viz_df, aes(x = date)) +
  geom_line(aes(y = actual, color = "Actual"), size = 1, alpha = 0.5) +
  geom_line(aes(y = predicted, color = "Predicted (u* + Okun)"), size = 1, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c("Actual" = "black", "Predicted (u* + Okun)" = "firebrick")) +
  labs(
    title = "Model Fit: Predicted vs. Actual Unemployment",
    subtitle = "Prediction combines the Natural Rate and the cyclical Okun's Law component",
    y = "Rate", x = NULL, color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

# 3. Plot B: The Prediction Error (Residuals)
p2 <- ggplot(viz_df, aes(x = date)) +
  geom_area(aes(y = error), fill = "gray70", alpha = 0.5) +
  geom_line(aes(y = error), color = "gray30", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Prediction Error",
    subtitle = "Residuals (Actual - Predicted)",
    y = "Error (pp)", x = NULL
  ) +
  theme_minimal()

# Combine plots (Requires 'patchwork' package)
p1 / p2 + plot_layout(heights = c(2, 1))

ggsave(
  filename = "output/okun_model_error_plot.pdf", 
  width = 10, 
  height = 6
)


# =============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)

final_forecast_matrix <- forecast_df

# 1. Transform Matrix to Long Format
spaghetti_long <- final_forecast_matrix %>%
  mutate(target_date = as.yearqtr(rownames(.))) %>%
  pivot_longer(
    cols = -target_date, 
    names_to = "vantage_point", 
    values_to = "forecast_value"
  ) %>%
  mutate(vantage_point = as.yearqtr(vantage_point)) %>%
  filter(target_date >= vantage_point, !is.na(forecast_value))

# 2. Extract the Actuals (The Diagonal)
actuals_path <- spaghetti_long %>%
  filter(target_date == vantage_point)

# 3. The Plot
ggplot() +
  # Forecast Lines (Grouped by vantage point to avoid connecting them all)
  geom_line(data = spaghetti_long, 
            aes(x = target_date, y = forecast_value, group = vantage_point, color = "Forecast"), 
            linewidth = 0.5, alpha = 0.3) +
  
  # The Actual Realized Path
  geom_line(data = actuals_path, 
            aes(x = target_date, y = forecast_value, color = "Actuals"), 
            linewidth = 1.2) +
  
  # THE FIX: This links the strings above to specific colors
  scale_color_manual(values = c(
    "Forecast" = "steelblue", 
    "Actuals"  = "firebrick"
  )) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Swiss Okun Model: Pseudo Out-of-Sample Evaluation",
    subtitle = "Spaghetti lines show 8-quarter forecasts from each vantage point",
    x = NULL, y = "Unemployment Rate", color = "Series"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave("output/forecast_spaghetti_plot.pdf", width = 10, height = 6)


################################################################################

plot_model_parameters <- function(df) {

  # 1. Transform data to long format for plotting
  # We exclude 'quarter' from the gathering process
  plot_data <- df %>%
    mutate(quarter = as.yearqtr(quarter)) %>%
    pivot_longer(
      cols = -quarter, 
      names_to = "parameter", 
      values_to = "value"
    )

  # 2. Create the plot
  ggplot(plot_data, aes(x = quarter, y = value, color = parameter)) +
    geom_line(size = 1) +
    geom_point() +
    # facet_wrap creates the 'stacked' effect
    # scales = "free_y" is crucial as beta values and sigma values have different magnitudes
    facet_wrap(~parameter, ncol = 1, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Evolution of Model Parameters Over Recursive Windows",
      x = "Forecast Origin (Vantage Point)",
      y = "Estimated Value"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1, "lines")
    )
}

# Example Usage:
plot_model_parameters(philips_params_df)

