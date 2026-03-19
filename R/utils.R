
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


# Generic download function
download_url_csv <- function(url, filepath) {
  
  tryCatch({
    data <- read_csv(url)
    
    write.csv(data, filepath)
    
    return(TRUE)
    
  }, error = function(e) {
    
    message("Download failed: ", e$message)
    
    return(FALSE)
  })
}

# Generic Download function Wrapper

download_url_csv_wrapper <- function(url, filepath, do_api_call) {
  
  if (!file.exists(filepath) | do_api_call) {
    success <- download_url_csv(url, filepath)
    
    if (success){
      message("Successfully downloaded file from", url)
    } else {
      message("Error in Downloading from", url)
    }
  } else {
    message("File '", filepath, "already exists. Skipping download.")
  }
}
  

### BFS WRAPPER

bfs_wrapper <- function(id, dest_file, do_api_call = FALSE, type = "data") {
  
  # Initialize success as FALSE, Update if data is downloaded
  # BFS FUnctions are marked wit BFS:: prefix
  success <- FALSE
  
  if (!file.exists(dest_file) | do_api_call) {
    message("Fetching BFS ", type, " for ID: ", id)
    
    if (type == "data") {
      
      # bfs_get_data returns a dataframe, we want to save it
      df <- BFS::bfs_get_data(number_bfs = id,
                              language = "fr")
      
      write_csv(df, dest_file)
      
      success <- TRUE
      
    } else if (type == "asset") {
      
      # bfs_download_asset downloads file directly to dest_file
      download_path <- BFS::bfs_download_asset(
        number_asset = id, 
        destfile = dest_file
      )
      
      success <- !is.null(download_path)
      
    } else {
      message("Error: type '", type, "' does not exist.")
      
      success <- FALSE
    }
    
  } else {
    message("Loading ", dest_file, " from Disk...")
    
    success <- TRUE
  }
  
  return(success)
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