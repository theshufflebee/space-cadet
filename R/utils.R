format_time_series_df <- function(data,
                                  date_col,
                                  value_col,
                                  new_name,
                                  date_format) {
  
  # format as dataframe
  df <- as.data.frame(data)
  
  # rename the columns
  colnames(df)[colnames(df) == date_col] <- "date"
  colnames(df)[colnames(df) == value_col] <- new_name
  
  # format the date as yearmonth
  df$date <- zoo::as.yearmon(as.character(df$date), format = date_format)
  
  # format the value as numeric
  df[[new_name]] <- as.numeric(df[[new_name]])
  
  # 3. Clean up
  df <- df %>%
    select(date, !!sym(new_name)) %>%
    drop_na() %>%
    arrange(date)
  
  return(df)
}