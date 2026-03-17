package_loader("RCurl") # API Call
package_loader("jsonlite") # Handling JSON files
package_loader("glue") # To bild API Endpoints


load_snb_data <- function(cube = 'zimoma', # refers to the specific data set in the SNB website
                          folder = "data", # choose storage folder must exist already
                          file_name = "name", # file saved as name
                          from_date = "2010-01", # choose start date
                          toDate = "2016-01", # choose end date
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
  local_string_file_name <- file.path(folder, paste0(file_name, "_metadata.json"))
  
  # Create the API Endpoint by inserting information into the url for data and metadata
  # Values in curly Brackets are replaced
  api_url <- glue("https://data.snb.ch/api/cube/{cube}/data/{file_type}/en?dimSel=D0({collapsed_ids})&fromDate={from_date}&toDate={toDate}")
  metadata_url <- glue("https://data.snb.ch/api/cube/{cube}/dimensions/en")
  
  # Download the Data directly into the folder
  download.file(api_url, method = "curl", destfile = local_file_name)
  
  # Download the Metadata directly into the folder
  download.file(metadata_url, method="curl", destfile=local_string_file_name)
  

}