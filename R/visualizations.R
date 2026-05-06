################################################################################
#
# These functions are used to create visualizations of the result
#
################################################################################





#' Plot and Save Model Parameter Evolution
#'
#' @description
#' Visualizes the evolution of model parameters over the rolling estimation range.
#' Each parameter is displayed in its own facet with independent y-axes to accommodate
#' varying magnitudes (e.g., coefficients vs. variances).
#'
#' @param df A dataframe containing a \code{quarter} column and parameter estimate columns.
#' @param save_path Character. The file path (including filename and extension) 
#' where the plot should be saved. If \code{NULL}, the plot is not saved to disk.
#' @param title Character. The main title for the plot, typically indicating the 
#' model name (e.g., "Okun Model" or "Phillips Curve").
#'
#' @return A ggplot object.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom zoo as.yearqtr
#' @export
plot_model_parameters <- function(df, save_path = NULL, title) {
  
  # 1. Transform data to long format for plotting
  plot_data <- df %>%
    mutate(quarter = as.yearqtr(quarter)) %>%
    pivot_longer(
      cols = -quarter, 
      names_to = "parameter", 
      values_to = "value"
    )
  
  # 2. Create the plot
  p <- ggplot(plot_data, aes(x = quarter, y = value, color = parameter)) +
    geom_line(linewidth = 1) +
    geom_point() +
    # facet_wrap creates the 'stacked' effect
    # scales = "free_y" allows for different magnitudes across parameters
    facet_wrap(~parameter, ncol = 1, scales = "free_y") +
    theme_minimal() +
    labs(
      title = title,
      subtitle = "Evolution of Model Parameters Over Recursive Windows",
      x = "Forecast Origin (Vantage Point)",
      y = "Estimated Value"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1, "lines")
    )
  
  # 3. Save logic
  if (!is.null(save_path)) {
    # Adjust height dynamically based on the number of parameters to prevent squashing
    num_params <- length(unique(plot_data$parameter))
    ggsave(save_path, plot = p, width = 10, height = 2 * num_params, limitsize = FALSE)
    message(paste("Plot successfully saved to:", save_path))
  } else {
    message("No save path provided, proceeding without saving plot titeled: ", title)
  }
  
  return(p)
}





