#!/usr/bin/env Rscript

#message("STARTING SCRIPT")

# ONly used for testing the full script on my laptop
#user_lib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", "4.4")
#clean_path <- normalizePath(user_lib, mustWork = FALSE)
#print(clean_path)
#if (!dir.exists(clean_path)) dir.create(clean_path, recursive = TRUE)

# Only use this path
#.libPaths(c(user_lib, .libPaths()))



#install.packages("here")
library(here)

# Load dependencies
source(here("scripts", "00a_install_dependencies.R"))
source(here("config.R"))
source(here("R", "01_matrix_ssm_construction", "build_okun_matrices.R"))
source(here("R", "01_matrix_ssm_construction", "build_phillips_matrices.R"))
source(here("R", "01_matrix_ssm_construction", "build_taylor_matrices.R"))
source(here("R", "kalman_filter_taylor.R"))
source(here("R", "02_parallel", "est_ssm_para.R"))

# Read LAI args
args <- commandArgs(trailingOnly = TRUE)
target_date_str <- args[which(args == "-d") + 1]
model_type      <- args[which(args == "-m") + 1]
sub_folder      <- args[which(args == "-f") + 1]

# Load global data
gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, show_col_types = FALSE) %>%
  mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
  rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)

# run estimation
if (model_type == "okun") {
  message("STARTING OKUN MODEL ESTIMATION")
  run_est_para_okun(target_date_str, sub_folder, gdp_forecasts_arima)
} else if (model_type == "phillips") {
  message("STARTING PHILLIPS MODEL ESTIMATION")
  run_est_para_phillips(target_date_str, sub_folder)
} else if (model_type == "taylor") {
  message("STARTING TAYLOR MODEL ESTIMATION")
  run_est_para_taylor(target_date_str, sub_folder, gdp_forecasts_arima)
}