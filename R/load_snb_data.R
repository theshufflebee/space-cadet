library(RCurl)
library(jsonlite)
library(glue)

load_snb_data <- function(cube = 'zimoma', # refers to the specific dataset in the SNB website
                          folder = "data", # choose storage folder
                          file_name = "name", # choose file name
                          fromDate = "2010-01", # choose start date
                          toDate = "2016-01", # choose end date
                          ids = c('SARON', '3M0'), # Id's can be a string or a vector of strings
                          file_type = 'csv') { # best choice here
  # This is an adapted version from the SNB's API Calls.
  # The following webpage is concerned: https://data.snb.ch/en/topics/ziredev/cube/zimoma
  
  if (!dir.exists(folder)) dir.create(folder)
  
  collapsed_ids <- paste(ids, collapse = ",")
  
  local_file_name <- file.path(folder, paste0(file_name, ".", file_type))
  local_string_file_name <- file.path(folder, paste0(file_name, "_metadata.json"))
  
  
  api_url <- glue("https://data.snb.ch/api/cube/{cube}/data/{file_type}/en?dimSel=D0({collapsed_ids})&fromDate={fromDate}&toDate={toDate}")
  metadata_url <- glue("https://data.snb.ch/api/cube/{cube}/dimensions/en")
  
  download.file(api_url, method = "curl", destfile = local_file_name)
  raw_data <- read.table(local_file_name, skip=3, header = TRUE, sep=";")
  
  # Download Meta Data
  download.file(metadata_url, method="curl", destfile=local_string_file_name)
  str_data <- fromJSON(paste(readLines(local_string_file_name, encoding = "UTF-8"), collapse=""))
  
  return(list(data = raw_data, metadata = str_data))
}