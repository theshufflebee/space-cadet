





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

