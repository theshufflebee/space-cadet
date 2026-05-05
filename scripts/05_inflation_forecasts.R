################################################################################
# 
# RUN FORECASTS FOR MODEL 2 / INFLATIONS
#
################################################################################

source(here("R", "arma_forecasting.R"))
source(here("R", "philips_matrices.R"))

library(forecast)


# data_for_arma_forecast <- master_philips%>%
#   select(all_of(c("quarter", "REER_CREA", "ppi_eur_idx", "ppi_ch_idx", "ex_eoq")))



# LOP FORECAST
# ------------------------------------------------------------------------------

# Second Exogenous variable :

# vantage_q <- as.yearqtr("2023 Q2")

h <- 8


# log_gdp <- master_philips %>%
#   arrange(quarter) %>%
#   filter(quarter <= vantage_q) %>%
#   select(quarter, log_gdp) #

# log_gdp_forecast <- master_philips %>%
#   arrange(quarter) %>% #
#   filter(quarter > vantage_q) %>%
#   slice_head(n = h) %>%
#   select(quarter, log_gdp)

# Build X for the mu_t matrix by joining
# final_gap_value, cpi_t-1, just move up -> last quarters cpi is this quarters independent variable, and output gap
# selecting data from forecast start to vantage bpiz




full_philips_data_df <- as.data.frame(build_X_data_matrix_philips(T_0 = "2010 Q1"))






params_philips <- rolling_est_philips_ssm(data = master_philips,
                                       forecast_start = "2022 Q3"
)

params_philips_2 <- params_philips


philips_params_df <- extract_params_df(params_philips_2)



# To use it:
# my_results_plot <- plot_rolling_philips_results(philips_params_df)
# print(my_results_plot)


forecast_df <- forecast_phillips_ssm(philips_params_df,                  
                                  date_col = "quarter",
                                  master_df = master_philips,
                                  forecast_h = 8,
                                  gdp_forecast_data = master_philips)

write_csv(forecast_df, here("output/one_q_lag_inf.csv"))

