

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


download_kof_data_wrapper <- function(file, kof_data_key = kof_data_key, do_api_call = FALSE) {
  
  if (!file.exists(file) | do_api_call) {
    success <- download_kof_data(file_path = file, dataset = kof_data_key, api_key = NULL)
    
    if (success){
      message("Successfully downloaded from KOF Package to ", file)
    } else {
      warning("Error in downloading ", file)
    }
  } else {
    message("KOF Data already exists at", file, "loadring from local disk...")
  }
}

  
