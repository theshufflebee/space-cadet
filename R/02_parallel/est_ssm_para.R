#' Run Parallel Single Vintage Okun Estimation
#' @param target_date_str Character. The vintage date (e.g., "2020 Q2").
#' @param sub_folder Character. Destination directory sub-token.
#' @param gdp_forecasts_arima Dataframe. Pre-loaded global ARIMA projections.
#' @export
run_est_para_okun <- function(target_date_str,
                              sub_folder = "default_okun",
                              gdp_forecasts_arima,
                              val_T1 = "1991-01-01") {
  
  # Can set val_T1 to "1981-01-01" to estimate longer
  
  target_date <- zoo::as.yearqtr(target_date_str)
  val_T1      <- zoo::as.yearqtr(val_T1)
  
  # Load dependencies matching configuration paths
  master_okun         <- read_csv(data_save_paths$processed$okun_master_csv, show_col_types = FALSE)
  master_okun$quarter <- zoo::as.yearqtr(master_okun$quarter)
  
  # Process and slice data slice
  data_t   <- master_okun[master_okun$quarter <= target_date, ]
  Y_matrix <- data_t[, c("quarter", "unemp_rate", "spf_5y_unemp")]
  
  valid_gdp_indices <- which(!is.na(data_t$log_gdp))
  if(length(valid_gdp_indices) == 0) stop("CRITICAL: No valid GDP observations found for current matrix build window!")
  
  processed_data <- get_hp_gap(data = data_t, 
                               gdp_forecast_data = gdp_forecasts_arima,
                               vantage_q         = target_date,
                               return_type       = "history")
  
  processed_data <- processed_data %>%
    dplyr::rename(gap_l0 = gap) %>% 
    dplyr::mutate(
      gap_l0 = gap_l0 * 100,
      gap_l1 = dplyr::lag(gap_l0, 1),
      gap_l2 = dplyr::lag(gap_l0, 2)
    ) %>%
    dplyr::filter(quarter >= val_T1) %>%
    dplyr::left_join(Y_matrix, by = "quarter") %>%
    dplyr::filter(complete.cases(unemp_rate, gap_l0, gap_l1, gap_l2))
  
  if (nrow(processed_data) < 5) stop("CRITICAL: Insufficient data rows remain after window filtering step!")
  
  Y_final <- as.matrix(processed_data[, c("unemp_rate", "spf_5y_unemp")])
  X_final <- as.matrix(processed_data[, c("gap_l0", "gap_l1", "gap_l2")])
  
  my_ssm_model <- initialize_my_okun_ssm(Y_final, X_final, parameter_guesses = okun_parameter_guess)
  
  # Optimization
  opt_results <- ssm_optimizer_wrapper_core(
    methods   = estimation_settings$okun$methods,
    iters     = estimation_settings$okun$iters,
    ssm       = my_ssm_model, 
    start_par = NULL 
  )
  
  # Extract States
  final_states <- loglik_ssm_core(opt_results$theta,
                                  my_ssm_model,
                                  return_full_res = TRUE,
                                  set_silent = TRUE)
  
  # Payload Construction
  payload <- list(
    target_date = target_date,
    params      = opt_results$params,
    states      = final_states
  )
  
  # File Management Layout
  output_dir <- here("output", "para", sub_folder)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  clean_date_str  <- gsub(" ", "_", target_date_str)
  output_filename <- file.path(output_dir, sprintf("param_est_okun_%s.rds", clean_date_str))
  
  saveRDS(payload, file = output_filename)
  message(sprintf("[SUCCESS] Completed Okun calculation block. Export written to: %s", output_filename))
  
  return(payload)
}






#' Run Parallel Single Vintage Phillips Estimation
#' @param target_date_str Character. The vintage date (e.g., "2020 Q2").
#' @param sub_folder Character. Destination directory sub-token.
#' @export
run_est_para_phillips <- function(target_date_str,
                                  sub_folder = "default_phillips",
                                  val_T1 = "1982-01-01") {
  
  # Parse dates using the standards established in the rolling framework
  target_date <- zoo::as.yearqtr(target_date_str)
  val_T1      <- zoo::as.yearqtr(val_T1) 
  
  # Load global master dataframe matching the configuration path keys
  # Assumes master_phillips is stored in data_save_paths$processed$phillips_master_csv
  master_path <- data_save_paths$processed$philips_master_csv
  master_phillips <- readr::read_csv(master_path, show_col_types = FALSE)
  master_phillips$quarter <- zoo::as.yearqtr(master_phillips$quarter)
  
  # Call the structural Phillips data builder function 
  data_t <- as.data.frame(
    build_data_matrix_philips(
      data            = master_phillips,
      vantage_quarter = target_date,
      T_0             = val_T1
    )
  )
  
  # Filter observations available up to "Today" inside the isolated window slice
  data_t <- data_t[data_t$quarter <= target_date, ]
  
  # Safety validation layer to prevent empty optimization loops
  if (nrow(data_t) < 5) {
    stop(sprintf("CRITICAL: Insufficient observations (%d) for Phillips vintage: %s", 
                 nrow(data_t), target_date_str))
  }
  
  # Build targeted model feature matrices
  Y_final <- as.matrix(data_t[, c("log_inflation_diff", "5y_cpi_forecast")])
  X_final <- as.matrix(data_t[, c("gdp_gap", "lop_gap")])
  
  # Initialize the blueprint state-space matrix infrastructure
  my_ssm_model <- initialize_my_philips_ssm(
    Y_final,
    X_final,
    parameter_guesses = philips_parameter_guess
  )
  
  # Optimization execution (Warm start breaks out to NULL to prevent worker cross-talk)
  opt_results <- ssm_optimizer_wrapper_core(
    methods   = estimation_settings$phillips$methods,
    iters     = estimation_settings$phillips$iters,
    ssm       = my_ssm_model, 
    start_par = NULL 
  )
  
  # Extract latent system states using optimized theta vector
  final_states <- loglik_ssm_core(
    opt_results$theta,
    my_ssm_model,
    return_full_res = TRUE,
    set_silent      = TRUE
  )
  
  # Assemble calculation workspace payload structure
  payload <- list(
    target_date = target_date,
    params      = opt_results$params,
    states      = final_states
  )
  
  # File Management Layout Setup
  output_dir <- here::here("output", "para", sub_folder)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  clean_date_str  <- gsub(" ", "_", target_date_str)
  output_filename <- file.path(output_dir, sprintf("param_est_phillips_%s.rds", clean_date_str))
  
  # Persist computational snapshot to drive
  saveRDS(payload, file = output_filename)
  message(sprintf("[SUCCESS] Completed Phillips calculation block. Export written to: %s", output_filename))
  
  return(payload)
}



#' Run Parallel Single Vintage Taylor Rule Estimation
#' @param target_date_str Character. The vintage date (e.g., "2020 Q2").
#' @param sub_folder Character. Destination directory sub-token.
#' @param gdp_forecasts_arima Dataframe. Pre-loaded global ARIMA projections.
#' @export
run_est_para_taylor <- function(target_date_str,
                                sub_folder = "default_taylor",
                                gdp_forecasts_arima,
                                val_T1 = "1989-01-01") {
  
  target_date <- zoo::as.yearqtr(target_date_str)
  val_T1      <- zoo::as.yearqtr(val_T1)
  
  # Load master dataset 
  master_path <- data_save_paths$processed$taylor_master_csv
  master_taylor <- readr::read_csv(master_path, show_col_types = FALSE)
  master_taylor$quarter <- zoo::as.yearqtr(master_taylor$quarter)
  
  # 2. Filter historical information slice up to "Today"
  data_t <- master_taylor[master_taylor$quarter <= target_date, ]
  
  # 3. Data Integrity Pre-checks
  valid_inf_indices <- which(!is.na(data_t$log_cpi))
  if (length(valid_inf_indices) < 5) {
    stop(sprintf("CRITICAL: Not enough valid CPI data points for vintage %s", target_date_str))
  }
  
  # 4. Construct Exogenous Gaps (HP Gap Real-Time Slice)
  gdp_gap_data <- get_hp_gap(
    data              = data_t,
    gdp_forecast_data = gdp_forecasts_arima, 
    vantage_q         = target_date
  )
  gdp_gap_data$gap <- gdp_gap_data$gap * 100
  
  inf_gap_data <- data_t %>%
    dplyr::select(dplyr::all_of(c("quarter", "inf_gap")))
  
  # 5. Compile Structural Matrix Space Alignment Window
  processed_data <- data_t %>%
    dplyr::select(quarter, saron_libor_splice, forward_rate, yoy_inf) %>%
    dplyr::left_join(gdp_gap_data %>% dplyr::select(quarter, gdp_gap = gap), by = "quarter") %>%
    dplyr::left_join(inf_gap_data %>% dplyr::select(quarter, inf_gap), by = "quarter") %>%
    dplyr::filter(quarter >= val_T1) %>%
    dplyr::arrange(quarter) %>%
    dplyr::filter(complete.cases(gdp_gap, inf_gap))
  
  # Safe structural boundary floor checkpoint
  if (nrow(processed_data) < 15) {
    stop(sprintf("CRITICAL: Insufficient data rows (%d) after filter window slice for vintage %s", 
                 nrow(processed_data), target_date_str))
  }
  
  # 6. Extract Clean Feature Matrices 
  Y_final <- as.matrix(processed_data[, c("saron_libor_splice", "forward_rate")])
  X_final <- as.matrix(processed_data[, c("gdp_gap", "inf_gap")])
  
  # 7. Map Blueprint Layout Instance
  my_ssm_model <- initialize_taylor_ssm(
    Y_data            = Y_final,
    X_data            = X_final,
    parameter_guesses = snb_rate_parameter_guess
  )
  
  # 8. Optimization (Warm start breaks out to NULL to ensure clean worker thread spaces)
  opt_results <- ssm_optimizer_wrapper_core(
    methods   = estimation_settings$taylor$methods,
    iters     = estimation_settings$taylor$iters,
    ssm       = my_ssm_model,
    start_par = NULL 
  )
  
  # 9. Extract System Latent Trend States Matrix Slices
  final_states <- loglik_ssm_core(
    opt_results$theta,
    my_ssm_model,
    return_full_res = TRUE,
    set_silent      = TRUE
  )
  
  # 10. Payload Assembly
  payload <- list(
    target_date = target_date,
    params      = opt_results$params,
    states      = final_states
  )
  
  # 11. File Persistence Output Layout Setup
  output_dir <- here::here("output", "para", sub_folder)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  clean_date_str  <- gsub(" ", "_", target_date_str)
  output_filename <- file.path(output_dir, sprintf("param_est_taylor_%s.rds", clean_date_str))
  
  saveRDS(payload, file = output_filename)
  message(sprintf("[SUCCESS] Completed Taylor calculation block. Export written to: %s", output_filename))
  
  return(payload)
}







# ==============================================================================
# LOAD THE ESTINATION DATA
# ==============================================================================

#' Ingest Parallel Estimation Results
#' 
#' @description Scans the target output directory, reads all saved single-vintage 
#' RDS snapshots, and compiles them into a list keyed by clean target date strings.
#'
#' @param target_folder Character. The directory token name (e.g., "baseline_v1_new_run")
#' @return A named list containing the model snapshots.
#' @export
ingest_parallel_results <- function(target_folder) {
  
  message("\n=== LOADING ALL PARALLEL ESTIMATION RESULTS ===")
  
  # get directory path
  target_dir  <- here::here("output", "para", target_folder)
  
  if (!dir.exists(target_dir)) {
    stop(sprintf("CRITICAL: The target directory does not exist: %s", target_dir))
  }
  
  saved_files <- list.files(path = target_dir, pattern = "\\.rds$", full.names = TRUE)
  
  message(sprintf("[INGEST] Found %d saved estimation files in: %s", length(saved_files), target_folder))
  
  if (length(saved_files) == 0) {
    warning("No files found to ingest. Returning an empty list.")
    return(list())
  }
  
  # load all files via lapply
  master_vintages_list <- lapply(saved_files, readRDS)
  
  # Universal Name Cleansing Block
  file_names <- tools::file_path_sans_ext(basename(saved_files))
  
  # Grabs the last 7 characters (e.g., "2020_Q4" or "2005_Q1")
  raw_dates  <- substr(file_names, nchar(file_names) - 6, nchar(file_names))
  
  # Convert snake case to space separated ("2020_Q4" -> "2020 Q4")
  clean_dates <- sub("_", " ", raw_dates)
  
  # Bind back onto memory elements
  names(master_vintages_list) <- clean_dates
  
  message("[SUCCESS] Ingestion complete. Clean list coordinates: ", 
          paste(head(names(master_vintages_list), 3), collapse = ", "), "...")
  
  return(master_vintages_list)
}



