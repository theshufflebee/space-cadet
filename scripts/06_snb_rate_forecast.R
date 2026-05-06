message("Starting SNB Policy Rate Forecast")


Y_data_taylor <- master_taylor %>%
  select(all_of(c("quarter", "saron_libor_splice", "forward_rate")))

X_data_taylor <- master_taylor %>%
  mutate(
    # 1. Output Gap: HP filter on log GDP
    gdp_gap = mFilter::hpfilter(log_gdp, freq = 1600)$cycle,
    
    # 2. Year-on-Year Inflation: log(CPI_t) - log(CPI_{t-4})
    yoy_inflation = log(cpi) - dplyr::lag(log(cpi), 4)) %>%
  
  filter(!is.na(yoy_inflation)) %>%
  mutate(
    
    # 3. Temporary Trend: HP filter 'trend' component of yoy_inflation
    # Note: Using $trend extracts the trend to subtract from the actual
    inf_gap = mFilter::hpfilter(yoy_inflation, freq = 1600)$cycle,

    # 4. Lagged Policy Rate
    lag_rate = lag(saron_libor_splice, 1)
  ) %>%
  # Fix: Added comma between "lag_rate" and "gdp_gap"
  select(quarter, lag_rate, gdp_gap, inf_gap, yoy_inflation)


################

ssm_taylor <- initialize_taylor_ssm(Y_data = Y_data_taylor,
                                    X_data = X_data_taylor,
                                    parameter_guesses =snb_rate_parameter_guess)


output_estim <- ssm_optimizer_wrapper(ssm = ssm_taylor)


ssm_taylor$builders$mu_t()


print(head(Y_data_taylor
      ))




