#!/usr/bin/env Rscript
# First line to make it an argparse file


# --------- ii. Load here package to enable finding script loading other packages 


# Create usable and readable folder
user_lib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", "4.4")
clean_path <- normalizePath(user_lib, mustWork = FALSE)
print(clean_path)
if (!dir.exists(clean_path)) dir.create(clean_path, recursive = TRUE)

# Only use this path
.libPaths(c(user_lib, .libPaths()))

if (!require("here", character.only = TRUE, quietly=TRUE)) {
  message(paste("Installing missing package:", "here"))
  install.packages("here",
                   dependencies = TRUE,
                   repos = "https://cloud.r-project.org"
                   )
  library("here", character.only = TRUE)
}


#------------------------------------------------------------------------------
# 0a. Run the Package Installation and Loading Script
#------------------------------------------------------------------------------
source(here("scripts", "00a_install_dependencies.R"))

# Find Python STILL BASED ON MY LAPTOP
options(python_cmd = "C:/Users/jonas/AppData/Local/Microsoft/WindowsApps/python.exe")

#------------------------------------------------------------------------------
# 0b. Run the Config script to get Parameters
#------------------------------------------------------------------------------
source(here("scripts", "00b_config.R"))


# ==============================================================================
# Define the CLI Parser Arguments
# ==============================================================================
parser <- ArgumentParser(
  description = "Project: Space-Cadet -- Forecasting the Swiss Economy with State Space models"
)

parser$add_argument(
  "-d", "--run-data", 
  action  = "store_true",        # Toggles to TRUE if typed, defaults to FALSE if absent
  default = FALSE,
  dest    = "run_data_pipeline", # Saved as args$run_data_pipeline
  help    = "Execute the 3-stage data ingestion, formatting, and transformation pipeline."
)

parser$add_argument(
  "-a", "--api-call", 
  action  = "store_true",        # Toggles to TRUE if typed, defaults to FALSE if absent
  default = FALSE,
  dest    = "do_api_call",       # argparse saves this directly as args$do_api_call
  help    = "Redownload Data from API instead of using local data files. If data doesn't exist, data will be downloaded even if it is set to false"
)

parser$add_argument(
  "-m", "--model", 
  type     = "character", 
  required = TRUE,
  choices  = c("okun", "phillips", "taylor", "all", "none"), # Added "none" if you ONLY want to run data updates
  help     = "Select the model to execute. If you run all select to eithe rrun on 50% of cores in parallel or sequentially"
)

parser$add_argument(
  "-r", "--run-estimation", 
  action  = "store_true", 
  default = FALSE,
  help    = "Run the recursive rolling maximum likelihood estimation loop."
)

parser$add_argument(
  "-s", "--start-date", 
  type    = "character", 
  default = "2000 Q1",
  help    = "Forecast starting origin vantage point (Format: 'YYYY Q[1-4]')."
)

parser$add_argument(
  "-p", "--persist", # Turns off temp storage
  action  = "store_false", 
  default = TRUE,
  dest    = "store_temp", # Keeps the variable name 'store_temp' inside your R script
  help    = "Disable temporary storage and save final outputs directly to the persistent production directory."
)

# Parse terminal inputs
args <- parser$parse_args()


# ==============================================================================
# Map CLI Arguments to Config Flags
# ==============================================================================
do_api_call            <- args$do_api_call
run_estimation         <- args$run_estimation
forecast_starting_date <- zoo::as.yearqtr(args$start_date)
store_temp <- args$store_temp


message("=========================================================")
message("[INFO] Live API Download Flag : ", do_api_call)
message("[INFO] Targeted Framework     : ", toupper(args$model))
message("[INFO] Run Optimization Loop  : ", run_estimation)
message("[INFO] Forecast Window Anchor : ", forecast_starting_date)
message("=========================================================")


# ==============================================================================
# Data Loading
# ==============================================================================

if (args$run_data_pipeline || do_api_call) {
  
  message("[START] Data Preparation Process...")
  message("=========================================================")
  
  message("  --> [STAGE 1/3] Sourcing: 01_load_data.R")
  source(here("scripts", "01_load_data.R"))
  
  message("  --> [STAGE 2/3] Sourcing: 02_format_data.R")
  source(here("scripts", "02_format_data.R"))
  
  message("  --> [STAGE 3/3] Sourcing: 03_transform_data.R")
  source(here("scripts", "03_transform_data.R"))
  
  message("[SUCCESS] Master data arrays compiled, saved to disk, and loaded.")
  message("---------------------------------------------------------")
  
} else {
  # If both flags are FALSE, skip the heavy lifting entirely
  message("[INFO] Skipping data preparation step. Estimation scripts will load cached historical datasets from disk.")
  message("---------------------------------------------------------")
}

# ==============================================================================
# 5. Execution Router (Sourcing your specific model scripts)
# ==============================================================================
switch(args$model,
       "okun" = {
         message("[RUNNING] Okun's Law Model Pipeline...")
         source(here("scripts", "04_unemployment_forecast.R"))
       },
       "phillips" = {
         message("[RUNNING] Phillips Curve Model Pipeline...")
         source(here("scripts", "05_inflation_forecast.R"))
       },
       "taylor" = {
         message("[RUNNING] Taylor Rule Model Pipeline...")
         source(here("scripts", "06_snb_rate_forecast.R"))
       },
       "all" = {
         message("[RUNNING] Full Package: Running all three macroeconomic blocks...")
         
         message("  -> Sourcing: Okun's Law Model...")
         source(here("scripts", "04_unemployment_forecast.R"))
         
         message("  -> Sourcing: Phillips Curve Model...")
         source(here("scripts", "05_inflation_forecast.R"))
         
         message("  -> Sourcing: Taylor Rule Model...")
         source(here("scripts", "06_snb_rate_forecast.R"))
       },
       "none" = {
         message("[INFO] Data processing completed. Skipping estimation loops per choice.")
       }
)

message("---------------------------------------------------------")
message("[SUCCESS] argparse.R script finished execution")