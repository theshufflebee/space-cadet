
#--- Functions for Setup ---

# To install missing packages
# Input Vector of required packages
# Output none (Packages installed)


#' Function to install missing packages from a vector
#' 
#' This function takes a vector of required packages as input and then removes those already installed.
#' All non installed packages are then installed from the ethz CRAN repo
#' 
#' @param required_packages Vector of characters. A vector containing names of packages
#' 
#' @return None. Installs all non installed packages and indicates if there are no packages to be installed.
install_missing_packages <- function(required_packages){
  
  installed_packages <- rownames(installed.packages())
  
  missing_packages <- setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, repos="https://stat.ethz.ch/CRAN/")
  } else {
    message("All Required Packages are installed")
  }
}


#' Load all packages from a vector
#' 
#' Load all packages from a vector via library()
#' 
#' @param required_packages Vector of Strings. All packages that you want to be loaded
#' 
#' @return None. Loads all packages.
load_packages <- function(required_packages) {
  
  for (pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }
}


#' Download csv from an url
#' 
#' This is a very generic function that takes an url and downloads the csv data behind it.
#' 
#' @param url String. Url where the csv file is
#' @param filepath Where the file is stored
#' 
#' @return Bool. Messages indicate if download was succesfull and returns \code{TRUE} or \code{False} respectively
#' 
#' @seealso [download_url_csv_wrapper()]
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

#' Wrapper Function for the download_url_csv_function
#' 
#' Allows to call it only if wither file is missing or there is a forced api call
#' 
#' @param url String. Url where the csv file is
#' @param filepath Where the file is stored
#' @param do_api_call Bool. If \code{TRUE}, forces a download even if the file exists.
#' 
#' @return None. Messages indicating success.
#' @seealso [download_url_csv()]
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

# Still not really happy here, look at it again later  

#' A Wrapper for the BFS Packages download function
#' 
#' This wrapper allows to download functions from the BFS Package. Depending on the source
#' either data or asset we use different functions. This function handles that distinction.
#' It also only downloads if either teh data isn't at the given path or if there is a forced re-download
#' 
#' @param id String. The BFS identifier (PX-ID for 'data' or Asset-ID for 'asset').
#' @param dest_file String. The local file path where the data should be saved.
#' @param do_api_call Bool. If \code{TRUE}, forces a new download even if 
#' the file exists locally. Defaults to \code{FALSE}.
#' @param type String. The type of download: \code{"data"} for API table 
#' extraction or \code{"asset"} for direct file downloads (e.g., Excel/CSV).
#'
#' @return Bool Returns \code{TRUE} if the file exists or was successfully 
#' downloaded; \code{FALSE} otherwise.
#' 
#' @importFrom BFS bfs_get_data bfs_download_asset
#' @importFrom readr write_csv
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

#' Create Standardized Time Series Dataframe
#' 
#' Transforms a raw dataframe into a standardized two-column format (date and value). 
#' This function converts the date column to \code{zoo::yearmon} and ensures 
#' the value column is numeric, sorted, and free of missing data.
#'
#' @param data Dataframe. The input dataset to be formatted (e.g., raw BFS or SNB output).
#' @param date_col String. The name of the original column containing date information.
#' @param value_col String. The name of the original column containing numeric values.
#' @param new_name String. The final name for the value column in the output.
#' @param date_format String. The format string used to parse the date 
#' (e.g., \code{"\%Y-\%m"} for 2024-01, \code{"\%b \%Y"} for Jan 2001, or \code{"\%YQ\%q"} for 2024Q1).
#'
#' @return A tidy dataframe with two columns: \code{date} (class \code{yearmon}) 
#' and \code{[new_name]} (class \code{numeric}), sorted chronologically.
#' 
#' @details 
#' The function uses \code{rlang}'s injection operator (\code{!!}) to handle 
#' dynamic column naming, allowing you to pass the \code{new_name} as a simple string.
#' 
#' @importFrom zoo as.yearmon
#' @importFrom dplyr select arrange
#' @importFrom tidyr drop_na
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
    dplyr::select(date, !!sym(new_name)) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(date)
  
  return(df)
}




# parameter Mapper for the models
# Theta is the parameter vector from the optimizer
# exp_spec contains the names of the parameters and a list of the index of
# variables which can only be positive, that get an exp transformation
# Maps parameter to the model for optimizer

param2model_gen <- function(theta, spec) {
  
  output <- list()
  
  for (i in seq_along(spec$names)) {
    name <- spec$names[i]
    if (i %in% spec$pos_idx) {
      output[[name]] <- exp(theta[i])
    } else {
      output[[name]] <- theta[i]
    }
  }
  return(output)
}


# This does the inverse of above function it converts models back to the 

model2param_gen <- function(input_list, spec) {
  # Create a numeric vector of the correct length
  theta <- numeric(length(spec$names))
  
  for (i in seq_along(spec$names)) {
    name <- spec$names[i]
    val  <- input_list[[name]]
    
    # use the log function on values that are constrained
    if (i %in% spec$pos_idx) {
      theta[i] <- log(val)
    } else {
      theta[i] <- val
    }
  }
  return(theta)
}



