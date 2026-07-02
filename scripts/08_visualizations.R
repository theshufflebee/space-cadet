message("Starting Visualizations")

# NOTE TO SELF: Build upd the function to work on all models
# Then build a wrapper that gives a complete SSM report for inspection

# Temporarily here so updating functions in it and reruning is easier

# Parameters over Time Plot

okun_param_plot <- plot_model_parameters(df = params_okun_df,
                                         title = "Okun Rolling Parameter Estimation Plot",
                                         save_path = output_save_paths$plots$params_okun)

philips_param_plot <- plot_model_parameters(df = params_philips_df,
                                            title = "Philips Rolling Parameter Estimation Plot",
                                            save_path = output_save_paths$plots$params_philips)

taylor_param_plot <- plot_model_parameters(df = taylor_params,
                                            title = "Taylor Rolling Parameter Estimation Plot",
                                            save_path = output_save_paths$plots$params_taylor)


# ==============================================================================
#  Spaghetti Plots for Forecasts and Benchmarks
# ==============================================================================

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = fcst_df_fixed,
  output_path = output_save_paths$plots$spaghetti_okun
)


# --- RW Benchmark Spaghetti Plot ---
okun_rw_bench_wide <- creaFcstEval::bench_to_wide(result_okun_rw$bench_long)
okun_rw_bench_wide <- standardize_yq_seq(okun_rw_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = okun_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$okun_rw
)


# --- AR1 Benchmark Spaghetti Plot ---
okun_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_okun_ar1$bench_long)
okun_ar1_bench_wide <- standardize_yq_seq(okun_ar1_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = okun_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$okun_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
okun_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_okun_auto_arma$bench_long)
okun_auto_arma_bench_wide <- standardize_yq_seq(okun_auto_arma_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = okun_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$okun_auto_arma
)






# PHILLIPS CURVE MODEL SPAGHETTI PLOTS
# ------------------------------------------------------------------------------

# --- FOrecast SPaghetti Plot ---
creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = fcst_df_inf,
  output_path = output_save_paths$plots$spaghetti_philips
)

# --- RW Benchmark Spaghetti Plot ---
phillips_rw_bench_wide <- creaFcstEval::bench_to_wide(result_philips_rw$bench_long)
phillips_rw_bench_wide <- standardize_yq_seq(phillips_rw_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = phillips_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$philips_rw
)


# --- AR1 Benchmark Spaghetti Plot ---
phillips_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_philips_ar1$bench_long)
phillips_ar1_bench_wide <- standardize_yq_seq(phillips_ar1_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = phillips_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$philips_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
phillips_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_philips_auto_arma$bench_long)
phillips_auto_arma_bench_wide <- standardize_yq_seq(phillips_auto_arma_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = phillips_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$philips_auto_arma
)



# TAYLOR RULE MODEL SPAGHETTI PLOTS
# ------------------------------------------------------------------------------

# --- Forecast Spaghetti Plot ---
creaFcstEval::spaghetti_plot(
  df              = Y_taylor,
  fcst_df         = fcst_df_policy_rate,
  output_path     = output_save_paths$plots$spaghetti_taylor
)

# --- RW Benchmark Spaghetti Plot ---
taylor_rw_bench_wide <- creaFcstEval::bench_to_wide(result_taylor_rw$bench_long)
taylor_rw_bench_wide <- standardize_yq_seq(taylor_rw_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_taylor,
  fcst_df     = taylor_rw_bench_wide,
  output_path = output_save_paths$plots$benchmark$taylor_rw
)


# --- AR1 Benchmark Spaghetti Plot ---
taylor_ar1_bench_wide <- creaFcstEval::bench_to_wide(result_taylor_ar1$bench_long)
taylor_ar1_bench_wide <- standardize_yq_seq(taylor_ar1_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_taylor,
  fcst_df     = taylor_ar1_bench_wide,
  output_path = output_save_paths$plots$benchmark$taylor_ar1
)

# --- AUTO ARMA Benchmark Spaghetti Plot ---
taylor_auto_arma_bench_wide <- creaFcstEval::bench_to_wide(result_taylor_auto_arma$bench_long)
taylor_auto_arma_bench_wide <- standardize_yq_seq(taylor_auto_arma_bench_wide)

creaFcstEval::spaghetti_plot(
  df          = Y_taylor,
  fcst_df     = taylor_auto_arma_bench_wide,
  output_path = output_save_paths$plots$benchmark$taylor_auto_arma
)
