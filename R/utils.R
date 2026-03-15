standardize_time_series_df <- function(data,           # Dataframe
                                       date_col,       # The column with the date
                                       value_col,      # The column with the value to keep
                                       new_name,       # New name for the value column
                                       date_format) {  # how the date is formated so it can properly be transformed
  
  df <- as.data.frame(data) # convert to make sure
  
  # This selects the date and value column (!!sym is for the system seeing it as column)
  df <- df %>%
    rename(
      date = !!sym(date_col),
     !!sym(new_name) := !!sym(value_col)
    ) %>%
    mutate(
      date = as.yearmon(date, format = date_format),
      !!sym(new_name) := as.numeric(!!sym(new_name))
      ) %>%
    select(date, !!sym(new_name)) %>%
    drop_na(!!sym(new_name)) %>%
    arrange(date)
  
  return(df)
}
