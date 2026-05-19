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
                                            title = "Taylor Rolling Parameter Estimation Plot (bounded phi, HP Inf Gap)",
                                            save_path = output_save_paths$plots$params_taylor)



# --- Spaghetti Plots ---

creaFcstEval::spaghetti_plot(
  df          = Y_okun_eval,
  fcst_df     = fcst_df_fixed,
  output_path = output_save_paths$plots$spaghetti_okun
)

creaFcstEval::spaghetti_plot(
  df          = Y_philips,
  fcst_df     = fcst_df_inf,
  output_path = output_save_paths$plots$spaghetti_philips
)

creaFcstEval::spaghetti_plot(
  df              = Y_taylor,
  fcst_df         = fcst_df_policy_rate,
  output_path     = output_save_paths$plots$params_taylor
)
