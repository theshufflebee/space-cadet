################################################################################
#
# These functions are used to create visualizations of the result
#
################################################################################





#' Plot and Save Model Parameter Evolution (Grid Optimization)
#'
#' @description
#' Visualizes the evolution of model parameters over the rolling estimation range.
#' Parameters are arranged in grids maximizing at 3 rows vertically, appending 
#' new column stacks to the right with independent y-axes.
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
plot_model_parameters <- function(df, save_path = NULL, title = "Model Parameter Stability") {
  
  # Wide to Long Formatting
  # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  p <- ggplot(plot_data, aes(x = date, y = value)) +
    
    # Intercept
    geom_hline(yintercept = 0.00, color = "grey75", linewidth = 0.5, linetype = "dotted") +
    # Line for tracking
    geom_line(color = "#34495e", linewidth = 1.0) +

    # FIXED HERE: Constrain to maximum 3 rows vertically, wrap columns to the right
    facet_wrap(~parameter, nrow = 3, scales = "free_y") +
    
    # Labeling payload
    labs(
      title    = title,
      subtitle = "Recursive Rolling Coefficients (Parameter Drift Time-Profile)",
      x        = "Estimation Vantage Origin (Forecast Date)",
      y        = "Coefficient Magnitude"
    ) +
    
    # Native Time Mapping
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    
    # Clean Academic Theme Styling
    # ----------------------------------------------------------------------------
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
  # ----------------------------------------------------------------------------
  if (!is.null(save_path)) {
    dir_name <- dirname(save_path)
    if (!dir.exists(dir_name) && dir_name != ".") {
      dir.create(dir_name, recursive = TRUE)
    }
    
    num_params <- length(unique(plot_data$parameter))
    
    # FIXED HERE: Dynamically calculate dimensions for horizontal grid wrapping
    # Max rows is fixed at 3. Calculate necessary columns:
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

# ==============================================================================
# Plotting function For the Model Fit
# ==============================================================================


#' Generate Two-Tier Diagnostics Plot for the SSM Model Fit
#' 
#' @param plot_df A data frame containing a `date` column (Date or yearqtr) and numeric tracks.
#' @param title Character. Main chart title.
#' @param subtitle Character. Chart subtitle.
#' @param top_metrics A named character vector mapping column names to Legend Titles for the top panel.
#'                    Format: c("column_name" = "Legend Label")
#' @param bottom_metrics A named character vector mapping column names to Legend Titles for the bottom panel.
#' @param top_colors A named vector of hex codes matching the *Legend Labels* specified in top_metrics.
#' @param bottom_colors A named vector of hex codes matching the *Legend Labels* specified in bottom_metrics.
#' @param zlb_bounds Optional numeric vector of length 2: c(ymin, ymax). Shades a horizontal band on the top plot (e.g., c(-0.75, 0.00)).
#' @param y_label_top Character. Y-axis label for top plot. Default "Rate (%)".
#' @param y_label_bottom Character. Y-axis label for bottom plot. Default "Deviation / Rate (%)".
#' 
#' @return A combined patchwork dashboard object.
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
  
  # ==============================================================================
  # PANEL 1: Core Target Space (Top Plot)
  # ==============================================================================
  p1 <- ggplot(plot_df, aes(x = date))
  
  # Render optional ZLB shaded horizontal region
  if (!is.null(zlb_bounds)) {
    p1 <- p1 + 
      geom_rect(aes(xmin = min(date), xmax = max(date), 
                    ymin = zlb_bounds[1], ymax = zlb_bounds[2]),
                fill = "grey90", alpha = 0.05) +
      geom_hline(yintercept = zlb_bounds[2], linetype = "dotted", color = "grey40", linewidth = 0.6) +
      geom_hline(yintercept = zlb_bounds[1], linetype = "dashed", color = "grey50", linewidth = 0.5)
  }
  
  # FIXED LOOP: Forces instant evaluation of BOTH column name and legend label string
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
  
  # ==============================================================================
  # PANEL 2: Covariates / Gaps (Bottom Plot)
  # ==============================================================================
  p2 <- ggplot(plot_df, aes(x = date)) +
    geom_hline(yintercept = 0.00, color = "grey60", linewidth = 0.5)
  
  # FIXED LOOP: Forces instant evaluation for the bottom tier metrics
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





