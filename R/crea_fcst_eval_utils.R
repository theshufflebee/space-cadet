# This file contains extensions that I'd like to  add to the creaFcstEval package



#' Turn Fcst Df into Long
#' 
#' @details From creaFcstEval package
#' @export
make_fcst_long <- function(fcst_df, keep_na = FALSE) {
  colnames(fcst_df)[1] <- "target_date"
  fcst_long <- fcst_df %>%
    tidyr::pivot_longer(
      cols = -target_date,
      names_to = "origin_date",
      values_to = "forecast_value"
    )
  if (!keep_na) {
    fcst_long <- fcst_long %>%
      dplyr::filter(!is.na(forecast_value))
  }
  return(fcst_long)
}

#' Evaluate Forecast Errors (MAE and MSFE)
#'
#' @param long_fcst_df Long-format forecast data frame containing target_date, origin_date, and forecast_value.
#' @return A list containing `df_eval` (row-level errors) and `df_summary` (aggregated MAE/MSFE per target date).
#' @export
eval_forecasts <- function(long_fcst_df) {
  
  # 1. Join ground truth and compute forecast errors
  df_eval <- long_fcst_df %>%
    dplyr::left_join(
      long_fcst_df %>%
        dplyr::filter(.data$origin_date == .data$target_date) %>%
        dplyr::select("target_date", true_value = "forecast_value"),
      by = "target_date"
    ) %>%
    dplyr::filter(.data$origin_date != .data$target_date) %>%
    dplyr::mutate(forecast_error = .data$forecast_value - .data$true_value) %>%
    dplyr::filter(!is.na(.data$forecast_error))
  
  # 2. Summarize MAE and MSFE per target_date
  df_summary <- df_eval %>%
    dplyr::group_by(.data$target_date) %>%
    dplyr::summarise(
      forecast_count = dplyr::n(), 
      mae  = mean(abs(.data$forecast_error)),
      msfe = mean((.data$forecast_error)^2),
      .groups = "drop"
    ) %>%
    mutate(target_date = as.yearqtr(target_date))
  
  return(df_summary)
}


#' Plot Forecast Error Metrics Over Time
#'
#' @param df_summary Summary data frame output from `eval_forecasts()$df_summary`.
#' @param is_yearqtr Logical, if TRUE converts target_date using zoo::as.yearqtr. Default is TRUE.
#' @return A ggplot object.
#' @export
plot_forecast_errors <- function(df_summary, model_name) {
  
  plot_title <- paste0(model_name, " Forecast Errors Over Time")
  
  df_plot_data <- df_summary %>%
    tidyr::pivot_longer(
      cols = c("mae", "msfe"),
      names_to = "metric",
      values_to = "error_value"
    ) %>%
    dplyr::mutate(
      metric = dplyr::recode(
        .data$metric,
        "mae"  = "Mean Absolute Error (MAE)",
        "msfe" = "Mean Squared Forecast Error (MSFE)"
      )
    )
  
  p <- ggplot2::ggplot(
    df_plot_data, 
    ggplot2::aes(x = .data$target_date, y = .data$error_value, color = .data$metric, group = 1)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 1) +
    ggplot2::scale_color_manual(values = c(
      "Mean Absolute Error (MAE)" = "#2b5c8f", 
      "Mean Squared Forecast Error (MSFE)" = "#d95f02"
    )) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = "Errors are normalized by number of available forecasts per target date",
      x = "Target Date",
      y = ""
    )
  
  return(p)
}

error_plotting_wrapper <- function(fcst_df, model_name = "", save_path = NULL) {
  long_fcst_df <- make_fcst_long(fcst_df)
  
  eval_df <- eval_forecasts(long_fcst_df)
  
  p <- plot_forecast_errors(eval_df, model_name)
  print(p)
  
  if(!is.null(save_path)) {
    ggsave(filename = save_path, plot = p)
  }
}
