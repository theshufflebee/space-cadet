message("Starting SNB Policy Rate Forecast")

source(here("R", "kalman_implementation_shadow.R"))



data_prep_taylor <- master_taylor %>%
  mutate(
    # 1. Output Gap: HP filter on log GDP
    log_gdp = log_gdp * 100,
    gdp_gap = mFilter::hpfilter(log_gdp, freq = 1600)$cycle,
    
    log_cpi = log(cpi),
    
    # 2. Year-on-Year Inflation: log(CPI_t) - log(CPI_{t-4})
    yoy_inflation = log(cpi) - dplyr::lag(log(cpi), 4)) %>%
  
  filter(!is.na(yoy_inflation)) %>%
  mutate(
    
    # 3. Temporary Trend: HP filter 'trend' component of yoy_inflation
    # Note: Using $trend extracts the trend to subtract from the actual
    inf_hp_gap = mFilter::hpfilter(yoy_inflation, freq = 1600)$cycle,
    
    # 4. Lagged Policy Rate
    lag_rate = lag(saron_libor_splice, 1),
    
  ) %>%
  mutate(
    saron_libor_splice = case_when(
      # Use as.yearqtr to match the column type
      quarter >= zoo::as.yearqtr("2009 Q2") & quarter <= zoo::as.yearqtr("2022 Q2") ~ NA_real_,
      TRUE ~ saron_libor_splice
    )
  ) %>%
  select(all_of(c("quarter", "saron_libor_splice",
                  "forward_rate", "log_cpi", "lag_rate", "log_gdp", "gdp_gap",
                  "yoy_inflation", "12m_interest_forecast", "inf_hp_gap"))) %>%
  mutate(
    across(c("saron_libor_splice",
           "forward_rate", "log_cpi", "lag_rate",
           "yoy_inflation", "12m_interest_forecast", "inf_hp_gap"), ~ .x * 100) 
  ) %>%
  mutate(
    inf_gap = yoy_inflation -1
  )


true_rate <- master_taylor %>%
  select(quarter, saron_libor_splice) %>%
  rename(true_snb_rate = saron_libor_splice)%>%
  mutate(true_snb_rate = true_snb_rate * 100 )

data_prep_taylor <- data_prep_taylor %>%
  left_join(true_rate, by = "quarter" )



Y_data_taylor <- data_prep_taylor %>%
  select(all_of(c("saron_libor_splice", "forward_rate")))

X_data_taylor <- data_prep_taylor %>%
  select(all_of(c("gdp_gap", "inf_gap")))
  


################

ssm_taylor <- initialize_taylor_ssm(Y_data = Y_data_taylor,
                                   X_data = X_data_taylor,
                                   parameter_guesses =snb_rate_parameter_guess)


output_estim <- ssm_optimizer_wrapper_shadow(ssm = ssm_taylor)


if(run_estimation){
  
  taylor_param_est <- rolling_est_taylor_ssm(data = data_prep_taylor,
                                             forecast_start = as.yearqtr("2018 Q1"))
  
  saveRDS(taylor_param_est, file = "output/temp/rolling_results_taylor_2018_bound_smooth_hp_inf_gap.rds")
  
  taylor_params <- extract_params_df(taylor_param_est, extract_fitted_obs = TRUE)
  
  write.csv(taylor_params, "output/parameter_estimation/taylor_params_2018_bound_smooth_hp_inf_gap.csv")
  
}else{
  
  taylor_params <- read.csv("output/parameter_estimation/taylor_params_2018_const_smooth_hp_inf_gap.csv")
  
  taylor_params_est <- readRDS("output/temp/rolling_results_taylor_2018_const_smooth.rds")
  
  taylor_params <- extract_params_df(taylor_params_est, extract_fitted_obs = TRUE)
}







############################3


# Run the filter with the final parameters
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

# extract the data
shadow_rate <- final_res$fitted_obs[, 1]
natural_rate <- final_res$r[, 1] # State 1
fit_rate <- final_res$fitted_obs_t_t[, 1]
dates <- data_prep_taylor$quarter # Assuming you have a date vector
snb_rate <-Y_data_taylor$saron_libor_splice
fw_rate <- Y_data_taylor$forward_rate
inflation_gap <- X_data_taylor$inf_gap
gdp_gap <- X_data_taylor$gdp_gap

# 3. Plot
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





###############################################################3
source(here("R", "ssm_forecasting.R"))

taylor_forecast_df <- forecast_taylor_ssm(params_df = taylor_params,
                                date_col = "quarter",
                                master_df = data_prep_taylor,
                                forecast_h = 8,
                                exogenous_forecast_data = data_prep_taylor,
                                hp_inf_gap = TRUE)




last_origin <- ncol(taylor_forecast_df)

taylor_eval_square <- taylor_forecast_df[1:last_origin, 1:last_origin]

# CheckThe number of rows should now equal the number of columns
dim(taylor_eval_square)






