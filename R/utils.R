
#--- Functions for Setup ---

# To install missing packages
# Input Vector of required packages
# Output none (Packages installed)

install_missing_packages <- function(required_packages){
  
  installed_packages <- rownames(installed.packages())
  
  missing_packages <- setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, repos="https://stat.ethz.ch/CRAN/")
  } else {
    message("All Required Packages are installed")
  }
}


# Load all required packages
# input: vector of packages to install
# output: none (Packages are loaded)

load_packages <- function(required_packages) {
  
  for (pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }
}



#--- Functions for Data Handling ---


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