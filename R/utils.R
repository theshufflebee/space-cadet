standardize_time_series_df <- function(data,           # Dataframe
                                       date_col,       # The column with the date
                                       value_col,      # The column with the value to keep
                                       new_name,       # New name for the value
                                       date_format) {  # how the date is formated so it can properly be transformed
  
  df <- as.data.frame(data) # convert to make sure
  
  # This selects the date and value column (!!sym is for the system seeing it as column)
  df <- df %>%
    rename(
      date_raw = !!sym(date_col),
      value_raw = !!sym(value_col)
    )
}