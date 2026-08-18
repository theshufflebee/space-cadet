################################################################################
#
# Functions to tie into the creaFcstEval Package
#
################################################################################

# These functions should be refined and then maybe introduced into the Crea Fcst Eval
# Package

# REMARK: All Documentation done with AI

#' Reshape Forecast Triangle Matrix to Long Format
#'
#' Converts a wide lower-triangular forecast data frame (with target dates as rows
#' and vintage origins as columns) into a tidy long-format data frame.
#'
#' @param fcst_df A data frame containing target dates in the first column and
#'   vintage forecast origins across remaining columns.
#' @param keep_na Logical. If \code{FALSE} (default), drops missing forecast combinations.
#'
#' @return A tibble with columns \code{target_date}, \code{origin_date}, and \code{forecast_value}.
#'
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter %>%
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



#' Evaluate Mean Absolute and Squared Forecast Errors Across Target Dates
#'
#' Computes realization benchmarks from origin-coincident diagonal observations
#' (\code{origin_date == target_date}), calculates forecast errors, and aggregates
#' Mean Absolute Error (MAE) and Mean Squared Forecast Error (MSFE) by target date.
#'
#' @param long_fcst_df A data frame in long format containing \code{target_date},
#'   \code{origin_date}, and \code{forecast_value}.
#'
#' @return A tibble aggregated by target date containing:
#' \item{target_date}{Target dates of class \code{yearqtr}.}
#' \item{forecast_count}{Number of multi-horizon forecast origins predicting this target date.}
#' \item{mae}{Mean Absolute Error for the given target date.}
#' \item{msfe}{Mean Squared Forecast Error for the given target date.}
#'
#' @seealso \code{\link{make_fcst_long}}
#'
#' @export
#' @importFrom dplyr left_join filter select mutate group_by summarise %>%
#' @importFrom rlang .data
#' @importFrom zoo as.yearqtr
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
#' Generates a faceted \code{ggplot2} time-series visualization comparing Mean Absolute Error (MAE)
#' and Mean Squared Forecast Error (MSFE) trajectories across evaluation target dates.
#'
#' @param df_summary A summary data frame produced by \code{\link{eval_forecasts}} containing
#'   \code{target_date}, \code{mae}, and \code{msfe}.
#' @param model_name Character string specifying the model identifier displayed in the plot title.
#'
#' @return A \code{ggplot} object with vertically faceted panels for MAE and MSFE.
#'
#' @seealso \code{\link{eval_forecasts}}
#'
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate recode %>%
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap scale_color_manual theme_minimal theme element_text element_blank labs
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



#' Wrapper Pipeline for Evaluating and Plotting Forecast Errors
#'
#' Executes the complete forecast error diagnostic workflow: reshapes a forecast triangle
#' into long format, calculates aggregated error metrics (MAE and MSFE), renders the summary plot,
#' and optionally writes the figure to disk.
#'
#' @param fcst_df A data frame formatted as a wide forecast matrix.
#' @param model_name Character. Descriptive model name used for plot titles. Defaults to \code{""}.
#' @param save_path Optional character string specifying the file path where the plot should be saved via \code{ggplot2::ggsave}.
#'
#' @return Invisibly returns \code{NULL} after printing and optionally saving the plot.
#'
#' @seealso \code{\link{make_fcst_long}}, \code{\link{eval_forecasts}}, \code{\link{plot_forecast_errors}}
#'
#' @export
#' @importFrom ggplot2 ggsave
error_plotting_wrapper <- function(fcst_df, model_name = "", save_path = NULL) {
  long_fcst_df <- make_fcst_long(fcst_df)
  
  eval_df <- eval_forecasts(long_fcst_df)
  
  p <- plot_forecast_errors(eval_df, model_name)
  print(p)
  
  if(!is.null(save_path)) {
    ggsave(filename = save_path, plot = p)
  }
}
