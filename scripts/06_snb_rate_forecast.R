message("Starting SNB Policy Rate Forecast")


# Source Correct Kalmann Specs
source(here("R", "kalman_implementation_taylor.R"))

# ==============================================================================
# Estimate the model over the Full Horizon
# ==============================================================================

# --- Select the alreeady Transformed data for the model

# From The Full dataset select the Data needed for the Rolling estimation of the FUll timeline
Y_data_taylor <- master_taylor %>%
  select(all_of(c("saron_libor_splice", "forward_rate")))

# Set inf_gap to hp_inf_gap if you want to estimate with the HP inf gap
X_data_taylor <- master_taylor %>%
  select(all_of(c("gdp_gap", "inf_gap")))
  

# --- Initialize the SSM ---
ssm_taylor <- initialize_taylor_ssm(Y_data = Y_data_taylor,
                                   X_data = X_data_taylor,
                                   parameter_guesses =snb_rate_parameter_guess)


# --- Estimate the full Model ---
output_estim <- ssm_optimizer_wrapper_shadow(ssm = ssm_taylor)


# ==============================================================================
# Plot the Full Sample Fit
# ==============================================================================


# --- Run the filter with the final parameters ---
final_res <- kalman_filter_taylor(
  Y_t = ssm_taylor$data$Y,
  nu_t = matrix(0, nrow(ssm_taylor$data$Y), 3), # Adjust nr if needed
  H = ssm_taylor$builders$H(output_estim$params),
  N = ssm_taylor$builders$N(output_estim$params),
  mu_t = ssm_taylor$builders$mu_t(output_estim$params, ssm_taylor$data$X),
  G = ssm_taylor$builders$G(output_estim$params),
  M = ssm_taylor$builders$M(output_estim$params),
  ar_matrix = ssm_taylor$builders$ar_mat(output_estim$params, ssm_taylor$data$Y),
  Sigma_0 = diag(0.1, 3), # Or your specific Sigma_0
  rho_0 = matrix(ssm_taylor$rho_guess, ncol=1)
)

# --- Extract the values for the plot ---
shadow_rate <- final_res$fitted_obs[, 1]
natural_rate <- final_res$r[, 1] # State 1
fit_rate <- final_res$fitted_obs_t_t[, 1]
dates <- master_taylor$quarter # Assuming you have a date vector
snb_rate <-Y_data_taylor$saron_libor_splice
fw_rate <- Y_data_taylor$forward_rate
inflation_gap <- X_data_taylor$inf_gap
gdp_gap <- X_data_taylor$gdp_gap

# --- Plot the data ---
plot(dates, fit_rate, type="l", col="blue", lwd=3, 
     main="Obs vs Fitted Rate", ylab="Rate (%)")
mtext("Taylor Rule Specification: Bounded smoothing & Constant Inflation Gap", 
      side=3, line=0.5, cex=0.8)
lines(dates, natural_rate, col="red", lty=2, type="l")
lines(dates, snb_rate, col="green", lty=2)
lines(dates, fw_rate, col="orange", lty=2)
lines(dates, inflation_gap, col="violet", lty=2)
lines(dates, gdp_gap, col="darkgreen", lty=2)
abline(h=-0.00, col="black", lty=3) # The ZLB Floor
abline(h=-0.75, col="grey", lty=3) # The ZLB Floor

legend("bottomleft",
       legend=c("Fitted (Shadow) Rate",
                "Natural Rate (i*)",
                "Observed LIBOR / SARON",
                "Forward Rate", "Inflation Gap", "GDP Gap"), 
       col=c("blue", "red", "green", "orange", "violet", "darkgreen"), lty=c(1, 2, 3))



# ==============================================================================
# Run the Full Rolling Forecast
# ==============================================================================

# --- Run the Rolling Estimation
if(run_estimation){
  
  taylor_param_est <- rolling_est_taylor_ssm(data = master_taylor,
                                             forecast_start = as.yearqtr(forecast_starting_date))
  
  taylor_params <- extract_params_df(taylor_param_est, extract_fitted_obs = TRUE)
  
  write.csv(taylor_params, output_save_paths$params$rolling_param_est_taylor)
  
}else{
  
  taylor_params <- read.csv(output_save_paths$params$rolling_param_est_taylor)
}




# --- Run the Forecast ---
taylor_forecast_df <- forecast_taylor_ssm(params_df = taylor_params,
                                date_col = "quarter",
                                master_df = master_taylor,
                                forecast_h = 8,
                                exogenous_forecast_data = master_taylor,
                                hp_inf_gap = FALSE)


# Save the Forecast df (and reload)
write_csv(taylor_forecast_df, output_save_paths$forecasts$forecast_df_taylor)
taylor_forecast_df <- read_csv(output_save_paths$forecasts$forecast_df_taylor)

# --- Format df for the Evaluation --- 
# Make the Dataframe a square
last_origin <- ncol(taylor_forecast_df)

taylor_eval_square <- taylor_forecast_df[1:last_origin, 1:last_origin]

# Check: the number of rows should now equal the number of columns
dim(taylor_eval_square)

