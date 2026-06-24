################################################################################
#
# Evaluation of Forecast results
#
################################################################################

message("Starting Forecast Evaluation")


# ==============================================================================
# OKUN Model Evaluation
# ==============================================================================


# Data Preparation
# ------------------------------------------------------------------------------
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
# ------------------------------------------------------------------------------

# --- Random Walk ---
result_okun_rw <- creaFcstEval::run_evaluation(
  df              = Y_okun_eval,
  fcst_df         = fcst_df_fixed,
  model_name      = "Okun Model",
  benchmark_model = "RW",
  type = "level"
)

fcst_eval_okun_rw_df <- result_okun_rw$table
export_eval_to_latex(fcst_eval_okun_rw_df, output_save_paths$tables$eval_rw_okun)

# --- AR1 ---
result_okun_ar1 <- creaFcstEval::run_evaluation(
  df              = Y_okun_eval,
  fcst_df         = fcst_df_fixed,
  model_name      = "Okun Model",
  benchmark_model = "AR1"
  )

fcst_eval_okun_ar1_df <- result_okun_ar1$table
export_eval_to_latex(fcst_eval_okun_ar1_df, output_save_paths$tables$eval_ar1_okun)

# --- AUTO ARMA ---
result_okun_auto_arma <- creaFcstEval::run_evaluation(
  df              = Y_okun_eval,
  fcst_df         = fcst_df_fixed,
  model_name      = "Okun Model",
  benchmark_model = "AUTO_ARMA",
  max_p = 4,
  max_q = 4
  )

fcst_eval_okun_auto_arma_df <- result_okun_auto_arma$table
export_eval_to_latex(fcst_eval_okun_auto_arma_df, output_save_paths$tables$eval_auto_arma_okun)


# ==============================================================================
# Philips Model Evaluation
# ==============================================================================


# Data Preparation
# ------------------------------------------------------------------------------
Y_philips <- master_philips %>%
  select(c("quarter", "log_inflation_diff")) %>%
  filter(!is.na(log_inflation_diff)) %>%
  rename(date = quarter,
         value = log_inflation_diff) %>%
  mutate(date = format(as.yearqtr(date), "%YQ%q"))
  

# Run Evaluation
# ------------------------------------------------------------------------------

# --- Random Walk ---
result_philips_rw <- creaFcstEval::run_evaluation(
  df              = Y_philips,
  fcst_df         = fcst_df_inf,
  model_name      = "Philips Model",
  benchmark_model = "RW"
  )

fcst_eval_philips_rw_df <- result_philips_rw$table
export_eval_to_latex(fcst_eval_philips_rw_df, output_save_paths$tables$eval_rw_philips)


# --- AR1 ---
result_philips_ar1 <- creaFcstEval::run_evaluation(
  df              = Y_philips,
  fcst_df         = fcst_df_inf,
  model_name      = "Philips Model",
  benchmark_model = "AR1"
)

fcst_eval_philips_ar1_df <- result_philips_ar1$table
export_eval_to_latex(fcst_eval_philips_ar1_df, output_save_paths$tables$eval_ar1_philips)


# --- AUTO ARMA ---
result_philips_auto_arma <- creaFcstEval::run_evaluation(
  df              = Y_philips,
  fcst_df         = fcst_df_inf,
  model_name      = "Philips Model",
  benchmark_model = "AUTO_ARMA",
  max_p = 4,
  max_q = 4
)

fcst_eval_philips_auto_arma_df <- result_philips_auto_arma$table
export_eval_to_latex(fcst_eval_philips_auto_arma_df, output_save_paths$tables$eval_auto_arma_philips)



# ==============================================================================
# Taylor Model Evaluation
# ==============================================================================


# Data Preparation
#-------------------------------------------------------------------------------

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


# Benchmark Evaluation
#-------------------------------------------------------------------------------

# --- Random Walk ---
result_taylor_rw <- creaFcstEval::run_evaluation(
  df              = Y_taylor,
  fcst_df         = fcst_df_policy_rate,
  model_name      = "Taylor Model",
  benchmark_model = "RW"
)

fcst_eval_taylor_rw_df <- result_taylor_rw$table
export_eval_to_latex(fcst_eval_taylor_rw_df, output_save_paths$tables$eval_rw_taylor)


# --- AR1 ---
result_taylor_ar1 <- creaFcstEval::run_evaluation(
  df              = Y_taylor,
  fcst_df         = fcst_df_policy_rate,
  model_name      = "Taylor Model",
  benchmark_model = "AR1"
)

fcst_eval_taylor_ar1_df <- result_taylor_ar1$table
export_eval_to_latex(fcst_eval_taylor_ar1_df, output_save_paths$tables$eval_ar1_taylor)


# --- AUTO ARMA ---
result_taylor_auto_arma <- creaFcstEval::run_evaluation(
  df              = Y_taylor,
  fcst_df         = fcst_df_policy_rate,
  model_name      = "Taylor Model",
  benchmark_model = "AUTO_ARMA",
  max_p = 4,
  max_q = 4
  )

fcst_eval_taylor_auto_arma_df <- result_taylor_auto_arma$table
export_eval_to_latex(fcst_eval_taylor_auto_arma_df, output_save_paths$tables$eval_auto_arma_taylor)

message("[SUCCESS] Finished Forecast Evaluation")
