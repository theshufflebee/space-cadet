#!/usr/bin/env Rscript
#===============================================================================
# Project: Space-Cadet -- Parallel Single Vintage Estimation Engine
# Usage: Rscript R/estimate_single_vintage_para.R -m okun -d "2020 Q2" -f _test_v1
#===============================================================================

options(python_cmd = "C:/Users/jonas/AppData/Local/Microsoft/WindowsApps/python.exe")

# 1. Coordinate Isolated Windows User Library Space
# ------------------------------------------------------------------------------
user_lib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", "4.4")
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

# Ensure 'here' package is loaded and available
if (!require("here", character.only = TRUE, quietly = TRUE)) {
  install.packages("here", dependencies = TRUE, repos = "https://cloud.r-project.org")
  library("here", character.only = TRUE)
}

message("STARTING ESTIMATION")

# 2. Source System Infrastructure and Configuration Settings
# ------------------------------------------------------------------------------
source(here("scripts", "00a_install_dependencies.R"))
source(here("scripts", "00b_config.R"))
source(here("para_functions", "est_ssm_para.R"))


# ==============================================================================
# CORE CORE EXPORTABLE ESTIMATION MODULES (Library & Worker Safe)
# ==============================================================================



# ==============================================================================
# 3. Define the CLI Parser Elements
# ==============================================================================
parser <- ArgumentParser(description = "Space-Cadet Parallel Single Vintage Worker Node")

parser$add_argument("-m", "--model", 
                    type     = "character", 
                    required = TRUE,
                    choices  = c("okun", "phillips", "taylor"), 
                    help     = "Select the target model to optimize.")

parser$add_argument("-d", "--date", 
                    type     = "character", 
                    required = TRUE,
                    help     = "The vintage cutoff date (Format: 'YYYY Q[1-4]').")

parser$add_argument("-f", "--folder", 
                    type     = "character", 
                    default  = "default_run",
                    dest     = "sub_folder",
                    help     = "Custom subfolder name inside results directory structure.")

args <- parser$parse_args()

model_type      <- tolower(args$model)
target_date_str <- args$date
sub_folder      <- args$sub_folder

message("=========================================================")
message("[WORKER START] Processing Chunk Allocation Pipeline...")
message("[INFO] Selected Model Type    : ", toupper(model_type))
message("[INFO] Selected Target Vintage: ", target_date_str)
message("[INFO] Target Directory Token : ", paste0("output/para/", sub_folder))
message("=========================================================")

# 4. Global Dependencies Sourcing Block
# ------------------------------------------------------------------------------
source(here("R", sprintf("kalman_%s_specs.R", model_type)))

gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                show_col_types = FALSE) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)

# ==============================================================================
# 5. Route Functional Matrix Assembly & Execution Router
# ==============================================================================
if (model_type == "okun") {
  
  # Call the modular function
  run_est_para_okun(target_date_str = target_date_str, 
                    sub_folder      = sub_folder, 
                    gdp_forecasts_arima = gdp_forecasts_arima)
  
} else if (model_type == "phillips") {
  
  message("[INFO] Phillips curve framework route activated. Placeholder state remains unmapped.")
  
  
} else if (model_type == "taylor") {
  
  # Placeholder structure reserved for future Taylor rule framework alignment logic
  message("[INFO] Taylor rule framework route activated. Placeholder state remains unmapped.")
  
}