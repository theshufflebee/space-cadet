################################################################################
#
# Evaluation of Forecast results
#
################################################################################

message("Starting Forecast Evaluation")


# ==============================================================================
# OKUN Model Evaluation
# ==============================================================================

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

# Format the date column to the 'YYYYQX' style the function expects
fcst_df_fixed$date <- format(as.yearqtr(fcst_df_fixed$date), "%YQ%q")



# Evaluate Okun Model

result <- creaFcstEval::run_evaluation(
  df              = Y_okun_eval,
  fcst_df         = fcst_df_fixed,
  model_name      = "Okun Model",
  benchmark_model = "RW",
  type = "growth"
  )

fcst_eval_okun_df <- result$table
export_eval_to_latex(fcst_eval_okun_df, output_save_paths$tables$eval_rw_okun)




# ==============================================================================
# Philips Model Evaluation
# ==============================================================================





Y_philips <- master_philips %>%
  select(c("quarter", "log_inflation_diff")) %>%
  filter(!is.na(log_inflation_diff)) %>%
  rename(date = quarter,
         value = log_inflation_diff) %>%
  mutate(date = format(as.yearqtr(date), "%YQ%q"))
  




result_philips <- creaFcstEval::run_evaluation(
  df              = Y_philips,
  fcst_df         = fcst_df_inf,
  model_name      = "Philips Model",
  benchmark_model = "RW"
  )

fcst_eval_philips_df <- result_philips$table
export_eval_to_latex(fcst_eval_philips_df, output_save_paths$tables$eval_rw_philips)



# ==============================================================================
# Taylor Model Evaluation
# ==============================================================================


colnames(taylor_eval_square) <- format(as.yearqtr(colnames(taylor_eval_square)), "%YQ%q")

rownames(taylor_eval_square) <- format(as.yearqtr(colnames(taylor_eval_square)), "%YQ%q")

taylor_eval_square <- as.data.frame(taylor_eval_square)

fcst_df_policy_rate <- taylor_eval_square %>%
  mutate(
    date = colnames(taylor_eval_square),
    date = format(as.yearqtr(date, format = "%Y Q%q"), format = "%YQ%q")
  ) %>%
  # date goes to front
  relocate(date, .before = everything()) 

Y_taylor <- master_taylor %>%
  select(quarter, true_snb_rate) %>%
  rename(date = quarter,
         value = true_snb_rate) %>%
  mutate(date = format(as.yearqtr(date), "%YQ%q"))



result_taylor <- creaFcstEval::run_evaluation(
  df              = Y_taylor,
  fcst_df         = fcst_df_policy_rate,
  model_name      = "Taylor Model",
  benchmark_model = "RW")

fcst_eval_taylor_df <- result_taylor$table
export_eval_to_latex(fcst_eval_taylor_df, output_save_paths$tables$eval_rw_taylor)

