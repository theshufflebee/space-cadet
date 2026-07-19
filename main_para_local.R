#===============================================================================
# Project: Space-Cadet -- Parallelized Estimation Script
# Parralelizes on each forecasting vintage. Only use on personal device
#===============================================================================

library(here)
library(foreach)
library(future.apply)


source(here("scripts", "00a_install_dependencies.R"))
source(here("config.R"))

source(here("R", "01_matrix_ssm_construction", "build_okun_matrices.R"))
source(here( "R", "01_matrix_ssm_construction", "build_phillips_matrices.R"))
source(here( "R", "01_matrix_ssm_construction", "build_taylor_matrices.R"))

source(here("R", "kalman_filter_taylor.R"))


source(here("R","02_parallel", "est_ssm_para.R"))

set.seed(42) # Set a seed so the shuffle is reproducible

DO_TRIAL_RUN <- FALSE

# What Models you Run
RUN_OKUN_MODEL <- FALSE
RUN_PHILLIPS_MODEL <- FALSE
RUN_TAYLOR_MODEL <- TRUE

# Save Folder
TARGET_FOLDER_OKUN <- "with_constraint_okun" # Output destination folder: output/para/...
TARGET_FOLDER_PHILLIPS <- "with_constraint_phillips" # Output destination folder: output/para/...
TARGET_FOLDER_TAYLOR <- "with_constraint_taylor" # Output destination folder: output/para/...


# --- Estimation Range ---
START_WINDOW_OKUN  <- zoo::as.yearqtr("2000 Q1")
END_WINDOW_OKUN    <- zoo::as.yearqtr("2025 Q4")
HIST_DATES_OKUN    <- format(seq(START_WINDOW_OKUN, END_WINDOW_OKUN, by = 0.25), format = "%Y Q%q")
HIST_DATES_OKUN <- sample(HIST_DATES_OKUN) # sampling (mixing up the date vector) no core has only models that are fast to estimate

START_WINDOW_PHILLIPS  <- zoo::as.yearqtr("2004 Q1")
END_WINDOW_PHILLIPS    <- zoo::as.yearqtr("2025 Q4")
HIST_DATES_PHILLIPS    <- format(seq(START_WINDOW_PHILLIPS, END_WINDOW_PHILLIPS, by = 0.25), format = "%Y Q%q")
HIST_DATES_PHILLIPS    <- sample(HIST_DATES_PHILLIPS)

START_WINDOW_TAYLOR  <- zoo::as.yearqtr("2000 Q1")
END_WINDOW_TAYLOR    <- zoo::as.yearqtr("2025 Q4")
HIST_DATES_TAYLOR    <- format(seq(START_WINDOW_TAYLOR, END_WINDOW_TAYLOR, by = 0.25), format = "%Y Q%q")
HIST_DATES_TAYLOR    <- sample(HIST_DATES_TAYLOR)


################################################################################
#
# OKUN PARALLEL ESTIMATION
#
################################################################################

if(RUN_OKUN_MODEL) {
  
  message("\n=== STARTING OKUN MODEL ESTIMATION ===")
  message("[CONFIG] Total Tasks Queued  : ", length(HIST_DATES_OKUN), " structural vintages.")
  
  # Load global ARIMA GDP gaps projections
  gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                  show_col_types = FALSE) %>%
    mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
    rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)
  
  if (DO_TRIAL_RUN) {
    message("\n=== STARTING OKUN MODEL TRIAL RUN ===")
    
    okun_trial_run_result <- run_est_para_okun(
      target_date_str     = "2000 Q1", 
      sub_folder          = TARGET_FOLDER_OKUN, 
      gdp_forecasts_arima = gdp_forecasts_arima
    )
      stop("STOPPING AFTER OKUN TRIAL RUN")
  }
  
  # Set up a parallel computing environment
  plan(multisession)
  
  # Run the function in parallel 
  message("\n=== STARTING OKUN MODEL PARALLEL ESTIMATION ===")
  
  results_okun <- future_lapply(HIST_DATES_OKUN, function(date_str) {
    run_est_para_okun(
      target_date_str     = date_str, 
      sub_folder          = TARGET_FOLDER_OKUN, 
      gdp_forecasts_arima = gdp_forecasts_arima
    )
  })
  message("\n=== FINISHED OKUN MODEL PARALLEL ESTIMATION ===")
  
  okun_parmas_list <- ingest_parallel_results(TARGET_FOLDER_OKUN)
  
  okun_parmas_df <- extract_params_df(okun_parmas_list)
  rownames(okun_parmas_df) <- NULL
}


################################################################################
#
# PHILLIPS PARALLEL ESTIMATION
#
################################################################################

if(RUN_PHILLIPS_MODEL) {
  
  message("\n=== STARTING PHILLIPS MODEL ESTIMATION ===")
  message("[CONFIG] Total Tasks Queued  : ", length(HIST_DATES_PHILLIPS), " structural vintages.")
  
  # Load global shared ARIMA GDP gaps projections
  gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                  show_col_types = FALSE) %>%
    mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
    rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)
  
  if (DO_TRIAL_RUN) {
    message("\n=== STARTING OKUN MODEL TRIAL RUN ===")
    
    phillips_trial_run_result <- run_est_para_phillips(
        target_date_str     = "2004 Q1", 
        sub_folder          = TARGET_FOLDER_PHILLIPS
      )
    
    stop("STOPPING AFTER PHILLIPS TRIAL RUN")
  }
  
  # Set up a parallel computing environment
  plan(multisession)
  
  message("\n=== STARTING PHILLIPS MODEL PARALLEL ESTIMATION ===")

  results_phillips <- future_lapply(HIST_DATES_PHILLIPS, function(date_str) {
    
    run_est_para_phillips(
      target_date_str     = date_str, 
      sub_folder          = TARGET_FOLDER_PHILLIPS
    )
  })
  message("\n=== FINISHED PHILLIPS MODEL PARALLEL ESTIMATION ===")
  
  phillips_parmas_list <- ingest_parallel_results(TARGET_FOLDER_PHILLIPS)
  
  phillips_parmas_df <- extract_params_df(phillips_parmas_list)
  rownames(phillips_parmas_df) <- NULL
}



################################################################################
#
# TAYLOR RULE PARALLEL ESTIMATION
#
################################################################################

if(RUN_TAYLOR_MODEL) {
  
  message("\n=== STARTING TAYLOR RULE MODEL ESTIMATION ===")
  message("[CONFIG] Total Tasks Queued  : ", length(HIST_DATES_TAYLOR), " structural vintages.")
  
  # Load global shared ARIMA GDP gaps projections
  gdp_forecasts_arima <- read_csv(output_save_paths$forecasts$forecast_df_gdp_arima, 
                                  show_col_types = FALSE) %>%
    mutate(date = format(zoo::as.yearqtr(date, format = "%Y Q%q"))) %>%
    rename_with(~ format(zoo::as.yearqtr(.x, format = "%Y Q%q")), .cols = -date)
  
  if (DO_TRIAL_RUN) {
    message("\n=== STARTING TAYLOR RULE MODEL TRIAL RUN ===")
    
    taylor_trial_run_result <- run_est_para_taylor(
      target_date_str     = "2015 Q1", 
      sub_folder          = TARGET_FOLDER_TAYLOR, 
      gdp_forecasts_arima = gdp_forecasts_arima
    )
    stop("STOPPING AFTER TAYLOR RULE TRIAL RUN")
  }
  
  # Set up a parallel computing environment
  plan(multisession)
  
  message("\n=== STARTING TAYLOR RULE MODEL PARALLEL ESTIMATION ===")
  
  results_taylor <- future_lapply(HIST_DATES_TAYLOR, function(date_str) {
    run_est_para_taylor(
      target_date_str     = date_str, 
      sub_folder          = TARGET_FOLDER_TAYLOR, 
      gdp_forecasts_arima = gdp_forecasts_arima
    )
  })
  message("\n=== FINISHED TAYLOR RULE MODEL PARALLEL ESTIMATION ===")
  
  taylor_params_list <- ingest_parallel_results(TARGET_FOLDER_TAYLOR)
  
  taylor_params_df <- extract_params_df(taylor_params_list)
  rownames(taylor_params_df) <- NULL
}


message("SCRIPT FINISHED")


