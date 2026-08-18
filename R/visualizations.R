################################################################################
#
# These functions are used to create visualizations of the result
#
################################################################################

# Note to Self: Roxygen documentation has been generated with AI

#' Plot and Save Model Parameter Evolution
#'
#' Visualizes recursive parameter estimates across expanding or rolling sample windows.
#' Formats the input data into long form and arranges parameters in a faceted grid 
#' capped at three vertical rows with free y-axes, scaling export dimensions dynamically.
#'
#' @param df A data frame containing a \code{quarter} column (parseable via 
#'   \code{zoo::as.yearqtr} or as \code{Date}) and numeric columns corresponding to 
#'   individual parameter estimate series.
#' @param save_path Optional character string. Destination file path (including extension) 
#'   to save the plot via \code{ggplot2::ggsave}. Subdirectories are created automatically 
#'   if they do not exist. Defaults to \code{NULL}.
#' @param title Character. Main chart title, typically indicating the model name. 
#'   Defaults to \code{"Model Parameter Stability"}.
#'
#' @return A \code{ggplot} object containing the faceted parameter stability plots.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate select %>%
#' @importFrom tidyr pivot_longer
#' @importFrom zoo as.yearqtr as.Date
plot_model_parameters <- function(df, save_path = NULL, title = "Model Parameter Stability") {
  
  # Wide to Long Formatting
  plot_data <- df %>%
    mutate(
      # Ensure date class is correctly parsed for plotting
      date = if(inherits(quarter, "yearqtr")) zoo::as.Date(quarter) else zoo::as.Date(as.yearqtr(quarter))
    ) %>%
    select(-quarter) %>%
    pivot_longer(
      cols      = -date, 
      names_to  = "parameter", 
      values_to = "value"
    ) %>%
    # Clean up column handles into more elegant names
    mutate(parameter = gsub("_", " ", toupper(parameter)))
  
  # Build base plot
  p <- ggplot(plot_data, aes(x = date, y = value)) +
    
    # Intercept
    geom_hline(yintercept = 0.00, color = "grey75", linewidth = 0.5, linetype = "dotted") +
    # Line for tracking
    geom_line(color = "#34495e", linewidth = 1.0) +

    # Constrain to maximum 3 rows vertically, wrap columns to the right
    facet_wrap(~parameter, nrow = 3, scales = "free_y") +
    
    # Labeling payload
    labs(
      title    = title,
      subtitle = "Recursive Estimates of Rolling Coefficients",
      x        = "Last date of Sample",
      y        = "Coefficient Size"
    ) +
    
    # Native Time Mapping
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    
    # Styling
  theme_minimal(base_size = 11) +
    theme(
      # them specs for title and axis names
      plot.title      = element_text(face = "bold", size = 13, color = "#2c3e50"),
      plot.subtitle   = element_text(color = "grey40", size = 10, margin = margin(b = 12)),
      axis.title.x    = element_text(size = 10, color = "grey30", margin = margin(t = 10)),
      axis.title.y    = element_text(size = 10, color = "grey30", margin = margin(r = 10)),
      
      # Clean up design
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = 10, color = "#2c3e50", hjust = 0),
      
      # Format Grid
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95", linewidth = 0.5),
      panel.spacing    = unit(1.5, "lines") # Give panels breathing room
    )
  
  # Export and saving Logic
  if (!is.null(save_path)) {
    dir_name <- dirname(save_path)
    if (!dir.exists(dir_name) && dir_name != ".") {
      dir.create(dir_name, recursive = TRUE)
    }
    
    num_params <- length(unique(plot_data$parameter))
    
    # Dynamically calculate the dimensions and arrangement of the plots
    num_cols <- ceiling(num_params / 3)
    num_rows <- min(num_params, 3)
    
    # Scale width per column stack, and height per row panel
    export_width  <- max(8.5, 4.25 * num_cols)
    export_height <- max(4, 2.5 * num_rows)
    
    ggplot2::ggsave(
      filename = save_path, 
      plot     = p, 
      width    = export_width, 
      height   = export_height, 
      units    = "in",
      dpi      = 300
    )
    message(paste(title, "Plot successfully saved to disk:", save_path))
  } else {
    message("Proceeding without saving ", title, " Plot")
  }
  
  return(p)
}

#' Generate SSM Fit Plot Including Auxiliary Variables
#'
#' Constructs a two-panel stacked dashboard using \code{ggplot2} and \code{patchwork}
#' to visualize state-space model fits. The top panel displays primary measurement
#' variables and trend components (with optional zero lower bound shading), while
#' the bottom panel tracks cyclical deviations and auxiliary regressors.
#'
#' @param plot_df A data frame containing a \code{date} column (of class \code{Date}
#'   or \code{yearqtr}) alongside numeric metric series to plot.
#' @param title Character. Main chart title for the top panel.
#' @param subtitle Character. Chart subtitle for the top panel.
#' @param top_metrics A named character vector mapping column names in \code{plot_df}
#'   to their corresponding legend labels for the top panel, formatted as
#'   \code{c("column_name" = "Legend Label")}.
#' @param bottom_metrics A named character vector mapping column names in \code{plot_df}
#'   to their corresponding legend labels for the bottom panel, formatted as
#'   \code{c("column_name" = "Legend Label")}.
#' @param top_colors A named character vector of color hex codes or names matching
#'   the legend label values defined in \code{top_metrics}.
#' @param bottom_colors A named character vector of color hex codes or names matching
#'   the legend label values defined in \code{bottom_metrics}.
#' @param zlb_bounds Optional numeric vector of length 2: \code{c(ymin, ymax)}.
#'   Shades a horizontal band with threshold lines on the top plot (e.g., \code{c(-0.75, 0.00)}).
#'   Defaults to \code{NULL}.
#' @param y_label_top Character. Y-axis label for the top panel. Defaults to \code{"Rate (\%)"}.
#' @param y_label_bottom Character. Y-axis label for the bottom panel. Defaults to \code{"Deviation / Rate (\%)"}.
#' @param save_path Optional character string. File path where the plot should be exported via
#'   \code{ggplot2::ggsave}. Directories are created automatically if they do not exist. Defaults to \code{NULL}.
#'
#' @return A combined \code{patchwork} object containing the two stacked \code{ggplot} panels.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate %>%
#' @importFrom rlang sym !!
#' @importFrom patchwork plot_layout /
#' @importFrom zoo as.Date
plot_state_space_fit <- function(plot_df,
                                 title,
                                 subtitle,
                                 top_metrics,
                                 bottom_metrics,
                                 top_colors,
                                 bottom_colors,
                                 zlb_bounds = NULL,
                                 y_label_top = "Rate (%)",
                                 y_label_bottom = "Deviation / Rate (%)",
                                 save_path = NULL) {
  
  # Ensure date column is cleanly parsed as Date objects
  plot_df <- plot_df %>% 
    mutate(date = if(inherits(date, "yearqtr")) zoo::as.Date(date) else as.Date(date))
  
  # Build the upper part of the plot with the core measurement variables
  p1 <- ggplot(plot_df, aes(x = date))
  
  # Render ZLB
  if (!is.null(zlb_bounds)) {
    p1 <- p1 + 
      geom_rect(aes(xmin = min(date), xmax = max(date), 
                    ymin = zlb_bounds[1], ymax = zlb_bounds[2]),
                fill = "grey90", alpha = 0.05) +
      geom_hline(yintercept = zlb_bounds[2], linetype = "dotted", color = "grey40", linewidth = 0.6) +
      geom_hline(yintercept = zlb_bounds[1], linetype = "dashed", color = "grey50", linewidth = 0.5)
  }
  
  # local used for instant evaluation instead of sequention
  for (col_name in names(top_metrics)) {
    local({
      c_name <- col_name
      l_label <- top_metrics[col_name]
      p1 <<- p1 + geom_line(aes(y = !!sym(c_name), color = l_label), linewidth = 1.0)
    })
  }
  
  p1 <- p1 +
    scale_color_manual(values = top_colors) +
    labs(title = title, subtitle = subtitle, y = y_label_top, x = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey40", size = 10),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank()
    )
  
  # --- Bottom Plot ---
  p2 <- ggplot(plot_df, aes(x = date)) +
    geom_hline(yintercept = 0.00, color = "grey60", linewidth = 0.5)
  
  # Forces instant evaluation for the bottom tier metrics
  for (col_name in names(bottom_metrics)) {
    local({
      c_name <- col_name
      l_label <- bottom_metrics[col_name]
      p2 <<- p2 + geom_line(aes(y = !!sym(c_name), color = l_label), linewidth = 0.85)
    })
  }
  
  p2 <- p2 +
    scale_color_manual(values = bottom_colors) +
    labs(y = y_label_bottom, x = "Quarter") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal(base_size = 11) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  # Assemble and stack via patchwork syntax
  dashboard <- p1 / p2 + plot_layout(heights = c(1.2, 1.0))
  
  # Save Plot
  if (!is.null(save_path)) {
    # Extract directory name from path string to verify it exists
    dir_name <- dirname(save_path)
    if (!dir.exists(dir_name) && dir_name != ".") {
      dir.create(dir_name, recursive = TRUE)
    }
    
    # Save using standard publication landscape dimensions (7x5 or 8x6 inches works best)
    ggplot2::ggsave(
      filename = save_path,
      plot     = dashboard,
      width    = 8.5,
      height   = 6.5,
      units    = "in",
      dpi      = 300
    )
    message(paste("Successfully Exported Fit Plot for the", title, "to:", save_path))
  }
  return(dashboard)
}


#' Plot Current Forecasts
#'
#' Generates a ggplot visualising realized actual values along the diagonal of a
#' forecast matrix against the latest available forecast vintage path.
#'
#' @param fcst_df A matrix or data frame representing a lower-triangular forecast
#'   table. Must contain quarterly date strings parseable by \code{zoo::as.yearqtr}
#'   as row names (target dates) and column names (vintage dates).
#' @param title Optional string. Plot title. Defaults to \code{"Real-Time Out-of-Sample Forecast Evaluation"}.
#' @param save_path Optional string. File path where the plot should be exported.
#'   Directories are created automatically if they do not exist.
#' @param width Numeric. Width of exported plot in inches. Defaults to \code{8}.
#' @param height Numeric. Height of exported plot in inches. Defaults to \code{5}.
#'
#' @return A \code{ggplot} object showing realized actuals and the latest forecast trajectory.
#' @export
#' @import ggplot2
#' @importFrom dplyr filter arrange %>%
#' @importFrom zoo as.yearqtr
#' @importFrom stats setNames
#' @importFrom utils head tail
plot_current_forecasts <- function(fcst_df, title = NULL, save_path = NULL, width = 8, height = 5) {
  
  # Format & Type Conversion
  if (is.matrix(fcst_df)) {
    fcst_mat <- fcst_df
    fcst_df  <- as.data.frame(fcst_df)
  } else {
    fcst_mat <- as.matrix(fcst_df)
  }
  
  target_date_strs  <- rownames(fcst_df)
  vintage_date_strs <- colnames(fcst_df)
  
  if (is.null(target_date_strs) || is.null(vintage_date_strs)) {
    stop("The forecast matrix/data frame must have valid target dates as rownames and vintage dates as colnames.")
  }
  
  target_dates  <- as.Date(zoo::as.yearqtr(target_date_strs))
  vintage_dates <- as.Date(zoo::as.yearqtr(vintage_date_strs))
  
  # Filter for diagonal in df (realized actuals)
  common_quarters <- intersect(target_date_strs, vintage_date_strs)
  
  diag_actuals <- sapply(common_quarters, function(q) {
    fcst_mat[q, q]
  })
  
  actuals_df <- data.frame(
    date   = as.Date(zoo::as.yearqtr(common_quarters)),
    actual = as.numeric(diag_actuals)
  ) %>% 
    filter(!is.na(actual)) %>% 
    arrange(date)
  
  # Select latest date
  latest_vintage_str <- tail(vintage_date_strs, 1)
  
  latest_fcst_df <- data.frame(
    date     = target_dates,
    forecast = as.numeric(fcst_mat[, latest_vintage_str])
  ) %>% 
    filter(!is.na(forecast)) %>% 
    arrange(date)
  
  # Check for continuity between latest actual & first forecast
  last_actual_val <- tail(actuals_df$actual, 1)
  first_fcst_val  <- head(latest_fcst_df$forecast, 1)
  
  if (length(last_actual_val) > 0 && length(first_fcst_val) > 0) {
    if (!isTRUE(all.equal(last_actual_val, first_fcst_val, tolerance = 1e-4))) {
      warning(sprintf(
        "Discontinuity detected! Latest actual: %.4f vs. First forecast origin (%s): %.4f",
        last_actual_val, latest_vintage_str, first_fcst_val
      ))
    }
  }
  
  # Set default title if not supplied
  plot_title <- if (is.null(title)) "Real-Time Out-of-Sample Forecast Evaluation" else title
  
  # Construct Plot
  p <- ggplot() +
    # Realized Actuals Line
    geom_line(
      data = actuals_df, 
      aes(x = date, y = actual, color = "Realized Actuals (Diagonal)"), 
      linewidth = 1
    ) +
    geom_point(
      data = actuals_df, 
      aes(x = date, y = actual, color = "Realized Actuals (Diagonal)"), 
      size = 1.8
    ) +
    # Latest Vintage Forecast Path
    geom_line(
      data = latest_fcst_df, 
      aes(x = date, y = forecast, color = sprintf("Latest Vintage (%s)", latest_vintage_str)), 
      linewidth = 1, 
      linetype = "dashed"
    ) +
    geom_point(
      data = latest_fcst_df, 
      aes(x = date, y = forecast, color = sprintf("Latest Vintage (%s)", latest_vintage_str)), 
      size = 1.8
    ) +
    # Color Scale & Formatting
    scale_color_manual(values = setNames(
      c("#2c3e50", "#e74c3c"),
      c("Realized Actuals (Diagonal)", sprintf("Latest Vintage (%s)", latest_vintage_str))
    )) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(
      title = plot_title,
      subtitle = sprintf("Latest Forecast Vintage: %s", latest_vintage_str),
      x = "Quarter",
      y = "Value",
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Export / Save Plot
  if (!is.null(save_path)) {
    # Ensure directory exists if subfolders are specified
    dir_name <- dirname(save_path)
    if (dir_name != "." && !dir.exists(dir_name)) {
      dir.create(dir_name, recursive = TRUE)
    }
    
    ggsave(
      filename = save_path,
      plot     = p,
      width    = width,
      height   = height,
      dpi      = 300
    )
    message(sprintf("Plot successfully saved to: %s", save_path))
  }
  
  return(p)
}

