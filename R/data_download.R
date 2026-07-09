





## NEED TO CHANGE DATA PATH HANDLING HERE


#' Function to Download Data from SNB API
#' 
#' This function transforms the R script on the SNB's website into a reusable function to download Data.
#' An example dataset is available here: 
#' It does download the Data directly, it does not return the Data
#' 
#' @param cube String. The specific data set identifier from the SNB website (e.g., 'zimoma').
#' @param folder String. The storage directory. Must exist prior to calling the function.
#' @param file_name String. The base name for the saved files.
#' @param from_date String. Start date in 'YYYY-MM' format.
#' @param to_date String. End date in 'YYYY-MM' format.
#' @param D0 (Vector of) Strings. One or more series identifiers (e.g., 'SARON'), D0 in the cube.
#' @param D1 (Vector of) Strings. One or more series identifiers. Usually subcategories of D0 series. D1 in the cube
#' @param D2 (Vector of) Strings. One or more series identifiers. Usually subcategories of D0, D1, series. D2 in the cube.
#' @param file_type String. The file format for the data download. Default is 'csv'.
#' 
#' @seealso [get_snb_data_wrapper()]
#' @seealso [https://data.snb.ch/en/topics/ziredev/cube/zimoma]
#' @seealso [https://data.snb.ch/en]
#' 
#' @return Bool. An indicator: \code{TRUE} if download was successful \code{FALSE} otherwise.
#'
#' @importFrom glue glue
#' @importFrom utils download.file
download_snb_data <- function(cube = 'zimoma',
                          folder = "data",
                          file_name = "name",
                          from_date = "2010-01",
                          to_date = "2026-01", 
                          D0 = c('SARON', '3M0'),
                          D1 = NULL, 
                          D2 = NULL,
                          file_type = 'csv') {

  # Prepare the ids for the api call into one string
  collapsed_D0_ids <- paste(D0, collapse = ",")
  collapsed_D1_ids <- paste(D1, collapse = ",")
  collapsed_D2_ids <- paste(D2, collapse = ",")
  
  
  # Define save path and name of data
  local_file_name <- file.path(folder, paste0(file_name, ".", file_type))
  
  series <- glue("D0({collapsed_D0_ids})")
  
  # If D1 exists, append it
  if (!is.null(D1)) {
    series <- glue("{series},D1({collapsed_D1_ids})")
  }
  
  # If D2 exists, append it
  if (!is.null(D2)) {
    series <- glue("{series},D2({collapsed_D2_ids})")
  }
  

  # Define save path and name of metadata
  local_meta_file_name <- file.path(folder, paste0(file_name, "_metadata.json"))
  
  # Create the API Endpoint by inserting information into the url for data and metadata
  # Values in curly Brackets are replaced
  api_url <- glue("https://data.snb.ch/api/cube/{cube}/data/{file_type}/en?dimSel={series}&fromDate={from_date}&toDate={to_date}")
  metadata_url <- glue("https://data.snb.ch/api/cube/{cube}/dimensions/en")
  
  # Try catch to handle errors
  tryCatch({
    
    download.file(api_url, method = "curl", destfile = local_file_name, quiet = TRUE)
    
    download.file(metadata_url, method = "curl", destfile = local_meta_file_name, quiet = TRUE)
    
    return(TRUE)
    
  }, error = function(e) {
    
    message("Download failed: ", e$message)
    
    return(FALSE)
    
  })
}
  




#' Wrapper function for download SNB Data
#' 
#' Wrapper function for the download_snb_data function. It checks if either the data is missing,
#' or if there is forced download via \code{do_api_call} and downloads if either of these is the case.
#' 
#' @param file String. The full path to the local file to check.
#' @param do_api_call Bool. If \code{TRUE}, forces a download even if the file exists.
#' @param cube String. The SNB cube ID found on their website.
#' @param file_name String. Base name for the saved file.
#' @param ids (Vector of) Strings. The series ID(s) to fetch.
#' @param file_type String. Format of the file (default 'csv').
#'
#' @return Invisibly returns the result of the download attempt.
#' 
#' @seealso [download_snb_data()]
get_snb_data_wrapper <- function(file,
                                 do_api_call = FALSE,
                                 cube, 
                                 file_name,
                                 D0 = NULL,
                                 D1 = NULL,
                                 D2 = NULL,
                                 file_type = "csv") {
  
  if (!file.exists(file) | do_api_call){
    success <- download_snb_data(cube,
                                 folder = raw_data_path, # choose storage folder must exist already
                                 file_name = file_name, # file saved as name
                                 from_date = from_date, # choose start date
                                 to_date = to_date, # choose end date
                                 D0 = D0,
                                 D1 = D1,
                                 D2 = D2)
    
    if(success) {
      message("Successfully downloaded ", file_name, " files via SNB API")
    } else {
      warning("Error in downloading ", file_name)
    }
    
  } else {
    message("File '", file_name, "' already exists at ", file, ". Skipping download.")
  }
}


####

# Format to time Series
#' Format SNB Time Series
#' 
#' Function that prepares SNB Data to get transformed with the format_time_series_df function, then applies it.
#' 
#' @param dataset Dataframe. The raw data downloaded from the SNB API.
#' @param variable Sting. The ID of the specific variable to extract from the D0 column.
#' @param variable_name String. The new name to assign to the value column.
#' @param names_column String. The column containing variable IDs. Default is "D0".
#' @param values_column String. The column containing the numeric values. Default is "Value".
#'
#' @return A processed dataframe with a \code{date} column and the renamed value column.
#' 
#' @importFrom tidyr pivot_wider
snb_api_data_to_ts <- function(dataset,
                               variable, # name in the supplied dataset
                               variable_name, # choose name for return dataset
                               names_column = "D0", # Defaut, may change later / in other datasets
                               values_column = "Value") { # Default, may change later / in other datasets
  dataset_wide <- dataset %>%
    pivot_wider(
      names_from = names_column,
      values_from = values_column
    )
  ts <- format_time_series_df(dataset_wide, "Date", variable, variable_name, "%Y-%m")
  
  return(ts)
  
}

################################################################################
#
# KOF DATA DOWNLOAD
#
################################################################################



#' Download Data from the KOF Institute at ETHZ
#' 
#' This function calls on the KOF package to download a dataset to a certain
#' save location. It transforms the downloaded list of ts objects into a data
#' frame for easier saving and use.
#' 
#' @param file_path String. Where to save the dataset. Needs to include the actual file name such as "C:/Users/user1/Downloads/filename.csv
#' @param api_key String. If a KOF API Key is needed, add here. \code{NULL} by default
#' @param dataset String. Reference to what dataset should be downloaded. Found on KOF Webpage.
#' 
#' @return Bool. Returns \code{TRUE} if download was successful and \code{FALSE} if it wasn't.
#' 
#' @seealso [download_kof_data_wrapper()]
#' @seealso [https://kof.ethz.ch/]
#' 
#' @importFrom kofdata get_collection
#' @importFrom zoo as.zoo index coredata as.yearmon
#' @importFrom readr write_csv
download_kof_data <- function(file_path = kof_master, dataset = kof_data_key, api_key = NULL) {
  
  # Get KOF Data from KOF Package
  # As far as I can see no API Key needed, but added for easier handling if needed later
  tryCatch({
    kof_consensus_forecast <- get_collection(dataset, api_key = api_key, show_progress = FALSE)
    
    kof_merged <- do.call(merge, lapply(kof_consensus_forecast, as.zoo))
    
    # Transform this zoo/ts object into a dataframe
    master_kof_consensus_forecast <- data.frame(
      date = as.yearmon(index(kof_merged)), # Converts ts data to month (could just be quarter if needed)
      coredata(kof_merged) # selects all data from zoo object                 
    )
    
    write_csv(master_kof_consensus_forecast, file_path)
    return(TRUE)
  }, error = function(e) {
    
    message("Download failed: ", e$message)
    
    return(FALSE)
  })
}

#' Wrapper Function for download_kof_data
#' 
#' This function wraps the download_kof_data to handle the decision wether to call it.
#' If the file isn't saved at the given path or there is an explicit instruction to download the data
#' the wrapper skips the download. The wrapper also returns a message on what it does.
#' 
#' @param file String. Path to the file, including it's name: "C:/Users/user1/Downloads/filename.csv
#' @param kof_data_key String. Reference to what dataset should be downloaded. Found on KOF Webpage.
#' @param do_api_call Bool. Set to \code{TRUE} if you want to force an datadownload even if data has already been downloaded
#' @param kof_api_key String. KOF API Key if needed.
#' 
#' @return None. The function doesn't return anything: It prints messages and downloads directly
#' @seealso [download_kof_data()]
#' @examples
#' \dontrun{
#' download_kof_data_wrapper("data/kof_raw.csv", "kof_dataset_123", do_api_call = TRUE, kof_api_key = my_kof_api_key)
#' }
download_kof_data_wrapper <- function(file, kof_data_key = kof_data_key, do_api_call = FALSE, kof_api_key = NULL) {
  
  if (!file.exists(file) | do_api_call) {
    success <- download_kof_data(file_path = file, dataset = kof_data_key, api_key = kof_api_key)
    
    if (success){
      message("Successfully downloaded from KOF Package to: ", file)
    } else {
      warning("Error in downloading: ", file)
    }
  } else {
    message("KOF Data already exists at: ", file)
  }
}

###############################################################################
#
# DOWNLOAD FROM BFS PACKAGE
#
###############################################################################


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
#' @export
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
  
  return(message("Download successful == ", success))
}





################################################################################
#
# GENERIC DATA HANDLING
#
################################################################################



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
#' @export
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
#' @export
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

