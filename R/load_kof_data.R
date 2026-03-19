
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
#' @importFrom KOF get_collection
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
#' 
#' @examples
#' \dontrun{
#' download_kof_data_wrapper("data/kof_raw.csv", "kof_dataset_123", do_api_call = TRUE, kof_api_key = my_kof_api_key)
#' }
download_kof_data_wrapper <- function(file, kof_data_key = kof_data_key, do_api_call = FALSE, kof_api_key = NULL) {
  
  if (!file.exists(file) | do_api_call) {
    success <- download_kof_data(file_path = file, dataset = kof_data_key, api_key = kof_api_key)
    
    if (success){
      message("Successfully downloaded from KOF Package to ", file)
    } else {
      warning("Error in downloading ", file)
    }
  } else {
    message("KOF Data already exists at", file)
  }
}