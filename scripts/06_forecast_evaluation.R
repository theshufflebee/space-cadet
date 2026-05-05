################################################################################
#
# Evaluation of Forecast results
#
################################################################################

library("creaFcstEval")

Y_okun_eval <- Y_okun %>%
  select(c("quarter", "unemp_rate")) %>%
  rename(date = quarter,
         value = unemp_rate) %>%
  mutate(date = format(as.yearqtr(date), "%YQ%q"))%>%
  drop_na()


okun_eval_square <- as.data.frame(okun_eval_square)
colnames(okun_eval_square) <- format(as.yearqtr(colnames(okun_eval_square)), "%YQ%q")

rownames(okun_eval_square) <- format(as.yearqtr(colnames(okun_eval_square)), "%YQ%q")

fcst_df_fixed <- okun_eval_square %>%
  as.data.frame() %>%
  rownames_to_column(var = "date")

# 2. Format the date column to the 'YYYYQX' style the function expects
# Based on your previous context, we remove the space
fcst_df_fixed$date <- format(as.yearqtr(fcst_df_fixed$date), "%YQ%q")



# Evaluate Okun Model

result <- creaFcstEval::run_evaluation(
  df              = Y_okun_eval,
  fcst_df         = fcst_df_fixed,
  model_name      = "Okun Model",
  benchmark_model = "RW",
  output_path     = "output/evaluation_okun.csv"
)

fcst_eval_results_okun <- result$table

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = fcst_df_fixed,
  output_path = "output/spaghetti_okun.png"
)

################################################################################


inf_fcst_df <- read_csv(here("output/one_q_lag_inf.csv"))


philips_eval_square <- as.data.frame(inf_fcst_df)

last_origin <- ncol(philips_eval_square)

philips_eval_square <- philips_eval_square[1:last_origin, 1:last_origin]

colnames(philips_eval_square) <- format(as.yearqtr(colnames(philips_eval_square)), "%YQ%q")

rownames(philips_eval_square) <- format(as.yearqtr(colnames(philips_eval_square)), "%YQ%q")

fcst_df_inf <- philips_eval_square %>%
  as.data.frame() %>%
  rownames_to_column(var = "date")

# 2. Format the date column to the 'YYYYQX' style the function expects
# Based on your previous context, we remove the space
fcst_df_inf$date <- format(as.yearqtr(fcst_df_inf$date), "%YQ%q")

Y_philips <- master_philips %>%
  select(c("quarter", "log_inflation_diff")) %>%
  rename(date = quarter,
         value = log_inflation_diff) %>%
  mutate(date = format(as.yearqtr(date), "%YQ%q"))
  




result_philips <- creaFcstEval::run_evaluation(
  df              = Y_philips,
  fcst_df         = fcst_df_inf,
  model_name      = "Philips Model",
  benchmark_model = "RW",
  output_path     = "output/evaluation_philips.csv"
)

fcst_eval_results_philips <- result_philips$table



creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = fcst_df_inf,
  output_path = "output/spaghetti_philips.png"
)




