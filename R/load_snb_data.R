
download_snb_data <- function(cube = 'zimoma', # refers to the specific data set in the SNB website
                          folder = "data", # choose storage folder must exist already
                          file_name = "name", # file saved as name
                          from_date = "2010-01", # choose start date
                          to_date = "2016-01", # choose end date
                          ids = c('SARON', '3M0'), # Id's can be a string or a vector of strings
                          file_type = 'csv') { # best choice here
  
  # This is an adapted version from the SNB's API Calls and downloads data and metadata
  # of a given SNB Dataset and returns a list with the data and metadata
  # An example dataset is: https://data.snb.ch/en/topics/ziredev/cube/zimoma
  
  # Prepare the ids for the api call into one string
  collapsed_ids <- paste(ids, collapse = ",")
  
  # Define save path and name of data
  local_file_name <- file.path(folder, paste0(file_name, ".", file_type))
  
  # Define save path and name of metadata
  local_meta_file_name <- file.path(folder, paste0(file_name, "_metadata.json"))
  
  # Create the API Endpoint by inserting information into the url for data and metadata
  # Values in curly Brackets are replaced
  api_url <- glue("https://data.snb.ch/api/cube/{cube}/data/{file_type}/en?dimSel=D0({collapsed_ids})&fromDate={from_date}&toDate={to_date}")
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
  




# Wrapper function that calls function above and downloads the data based on
# If the file is 
get_snb_data_wrapper <- function(file,
                                 do_api_call = FALSE,
                                 cube, 
                                 file_name,
                                 ids,
                                 file_type = "csv") {
  
  if (!file.exists(file) | do_api_call){
    success <- download_snb_data(cube,
                                 folder = raw_path, # choose storage folder must exist already
                                 file_name = file_name, # file saved as name
                                 from_date = from_date, # choose start date
                                 to_date = to_date, # choose end date
                                 ids = ids)
    
    if(success) {
      message("Successfully downloaded ", file_name, " files via SNB API")
    } else {
      warning("Error in downloading ", file_name)
    }
    
  } else {
    message("File '", file_name, "' already exists at ", mm_csv, ". Skipping download.")
  }
}


####

# Format to time Series

snb_api_data_to_ts <- function(dataset,
                               variable, # name in the supplied dataset
                               variable_name, # choose name for return dataset
                               names_column = "D0", # Defaut, may change later / in other datasets
                               values_column = "Value") { # Default, may change later / in other datasets
  dataset_wide <- dataset %>%
    pivot_wider(
      names_from = "D0",
      values_from = "Value"
    )
  ts <- format_time_series_df(dataset_wide, "Date", variable, variable_name, "%Y-%m")
  
  return(ts)
  
}

