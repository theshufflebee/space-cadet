# Extract Data from results (ssm_optimizer_wrapper)
final_params <- params_results$params 

# Build matrices with estimated parameters
mu_t_final <- mu_t_builder$builder(final_params)
G_final    <- G_builder$builder(final_params)
H_final    <- H_builder$builder(final_params)
M_final    <- M_builder$builder(final_params)

# State noise is surrently fixed
N_final  <- matrix(0.0001, 1, 1)

# no intercept
nu_final <- matrix(0, nrow(Y), 1)

# Run the filter once with given parameters
filter_results <- kalman_filter(
  Y_t     = as.matrix(Y), 
  nu_t    = nu_final, 
  H       = H_final, 
  N       = N_final, 
  mu_t    = mu_t_final, 
  G       = G_final, 
  M       = M_final, 
  Sigma_0 = matrix(10, 1, 1), 
  rho_0   = Y[1, 2] # use first SPF value as starting point
)

# Extract the Natural Rate
# r is the filtered state
u_star_filtered <- as.vector(filter_results$r)


# Set up the plot base
plot(Y[, 1], type = "l", col = "gray80", lwd = 2, # measured unemployment
     main = "Swiss Unemployment and natural Rate",
     ylab = "Rate", xlab = "Quarters",
     ylim = range(c(Y[, 1], u_star_filtered)))

# Add Filtered Natural Rate
lines(u_star_filtered, col = "darkblue", lwd = 2)

# Add SPF
lines(Y[, 2], col = "darkred", lty = 2, lwd = 1.5)

legend("topright", 
       legend = c("Actual Unemployment", "Natural Rate", "SPF 5y Forecast"),
       col = c("gray80", "darkblue", "darkred"), 
       lty = c(1, 1, 2), lwd = 2, bty = "n")



# Calculate the Unemp Gap / cyclical component
u_gap <- Y[, 1] - u_star_filtered

# Predicted Cyclical Part from Betas
# (mu_t_final[, 1] is X * Beta) -> does only have one column currently
# but good to keep like this
predicted_cycle <- mu_t_final[, 1]

# Plot the calculated gap
plot(u_gap, type = "h", col = "steelblue", lwd = 3,
     main = "Actual vs Predicted unemp Gap",
     ylab = "Rate", xlab = "Quarters")
abline(h = 0, lty = 2)

#Add GDP Gap prediction
lines(predicted_cycle, col = "orange", lwd = 2)

legend("topright", 
       legend = c("Actual Unemp Gap", "Predicted by GDP Gap (Okun)"),
       col = c("steelblue", "orange"), lty = 1, lwd = 2, bty = "n")