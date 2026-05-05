


#' Plots the philips results
plot_rolling_philips_results <- function(df) {
  
  # 1. Prepare Data: Ensure quarter is yearqtr and pivot for faceting
  plot_data <- df %>%
    mutate(quarter_date = as.yearqtr(quarter))
  
  # Pivot coefficients for the main panel
  coef_long <- plot_data %>%
    select(quarter_date, beta1, beta2, beta3) %>%
    pivot_longer(cols = starts_with("beta"), 
                 names_to = "parameter", 
                 values_to = "value") %>%
    mutate(parameter_label = case_when(
      parameter == "beta1" ~ "Persistence (Beta 1)",
      parameter == "beta2" ~ "GDP Gap Sensitivity (Beta 2)",
      parameter == "beta3" ~ "LOP Gap Sensitivity (Beta 3)"
    ))
  
  # 2. Plot A: Coefficients Panel
  p1 <- ggplot(coef_long, aes(x = quarter_date, y = value, color = parameter_label)) +
    geom_line(size = 1) +
    geom_point() +
    facet_wrap(~parameter_label, scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(title = "Evolution of Phillips Curve Parameters (Rolling Estimation)",
         subtitle = "Constrained Estimation: Betas >= 0",
         x = NULL, y = "Coefficient Value") +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold")) +
    scale_color_manual(values = c("#2c3e50", "#e74c3c", "#27ae60"))
  
  # 3. Plot B: Natural Rate (The Anchor)
  # Annualize if necessary (assuming current values are decimal yearly, e.g., 0.01)
  p2 <- ggplot(plot_data, aes(x = quarter_date, y = natural_rate * 100)) +
    geom_line(color = "#8e44ad", size = 1.2) +
    geom_point(color = "#8e44ad") +
    theme_minimal() +
    labs(title = "Estimated Natural Rate of Inflation (Trend)",
         subtitle = "Stable 'Anchor' would be arround 1.05%",
         x = "Vantage Quarter", y = "Percent (%)") +
    expand_limits(y = c(0.8, 1.3))
  
  # 4. Combine using patchwork
  combined_plot <- p1 / p2 + plot_layout(heights = c(2, 1))
  
  return(combined_plot)
}


Y_data <- master_philips %>%
  select(all_of(c("quarter", "log_inflation_diff"))) %>%
  filter(quarter >= as.yearqtr("2015 Q1"))

plot_ssm_results <- function(ssm_obj, Y_data) {
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  
  # 1. Extraction from nested lists
  state_mean <- as.numeric(ssm_obj$states$r)
  state_var  <- as.numeric(ssm_obj$states$Sigma_tt)
  
  # 2. Extract Observations
  # Pull the specific column and turn it into a numeric vector
  actual_infl <- as.numeric(Y_data$log_inflation_diff)
  print(actual_infl)
  message("actual inf above, fit inf below")
  # Based on your previous output, fitted_obs is at the top level
  fit_infl    <- as.numeric(ssm_obj$states$fitted_obs[, 1])
  
  print(fit_infl)
  
  # 3. Create an index
  idx <- 1:length(state_mean)
  
  # 4. Build Dataframe
  # We ensure all inputs are flat numeric vectors
  plot_df <- data.frame(
    index = idx,
    actual = actual_infl,
    fitted = fit_infl,
    trend  = state_mean,
    lower  = state_mean - 1.96 * sqrt(state_var),
    upper  = state_mean + 1.96 * sqrt(state_var)
  )
  
  # 5. Plot A: Inflation Comparison
  # Switched 'size' to 'linewidth' to address the warning
  p1 <- ggplot(plot_df, aes(x = index)) +
    geom_line(aes(y = actual, color = "Actual Inflation"), linewidth = 0.8) +
    geom_line(aes(y = fitted, color = "Fitted Phillips Curve"), linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = trend, color = "Natural Rate"), linewidth = 0.8) +
    theme_minimal() +
    labs(title = "Inflation: Actual vs. Fitted",
         y = "Rate", x = "Observations", color = NULL) +
    scale_color_manual(values = c("Actual Inflation" = "black",
                                  "Fitted Phillips Curve" = "red",
                                  "Natural Rate" = "blue")) +
    theme(legend.position = "top")
  
  # 6. Plot B: Natural Rate
  p2 <- ggplot(plot_df, aes(x = index, y = trend)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.15) +
    geom_line(color = "blue", linewidth = 1) +
    theme_minimal() +
    labs(title = "Estimated Natural Rate (State Vector)",
         y = "Trend Rate", x = "Observations")
  
  return(p1 / p2)
}

# Usage:
plot_ssm_results(params_philips$`2025 Q4`, Y_data = Y_data)

