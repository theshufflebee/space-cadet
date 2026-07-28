
plot_current_forecasts(
  fcst_df   = forecast_okun_df,
  title     = "Okuns Law Forecast",
  save_path = output_save_paths$plots$current_forecasts$okun_current_forecasts,
  width     = 10, 
  height    = 6
)

plot_current_forecasts(
  fcst_df   = philips_forecast_df,
  title     = "Phillips Curve Forecast",
  save_path = output_save_paths$plots$current_forecasts$phillips_current_forecasts,
  width     = 10, 
  height    = 6
)

plot_current_forecasts(
  fcst_df   = taylor_forecast_df,
  title     = "Taylor Rule Forecast",
  save_path = output_save_paths$plots$current_forecasts$taylor_current_forecasts,
  width     = 10, 
  height    = 6
)

plot_current_forecasts(
  fcst_df   = taylor_forecast_df_rounded,
  title     = "Rounded Taylor Rule Forecast",
  save_path = output_save_paths$plots$current_forecasts$taylor_rounded_current_forecasts,
  width     = 10, 
  height    = 6
)
