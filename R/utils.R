
#--- Functions for Setup ---

# To install missing packages
# Input Vector of required packages
# Output none (Packages installed)


#' Helper function to try installing packages from binary or source if that fails
#' 
#' @param pkg Character(String?) the package to be installed
#' 
#' @return None if success, stops process if error
#' 
#' @seealso [install_missing_packages()]
#' @export
try_install <- function(pkg){
  for (type in c("both", "source")) {
    success <- tryCatch({
      install.packages(pkg, repos="https://stat.ethz.ch/CRAN/", type = type, quiet = TRUE, verbose = FALSE)
      # Top have an error, we need to try and load it first
      requireNamespace(pkg, quietly = TRUE)
    }, error = function(e) FALSE)
    if (success) {
      # change name to it clearer
      type <- if(type == "both") "binary"
      message("\n",pkg, ": installed successfully from ",type,"\n")
      return()
    }
  }
  stop("Failed to install '",pkg,"' from both binary and source, please install manually. Aborting")
}


#' Function to install missing packages from a vector
#' 
#' This function takes a vector of required packages as input, find those missing and installed them if necessary
#' 
#' @param required_packages Vector of characters. A vector containing names of packages
#' 
#' @return None. Installs all missing packages and indicates if there are no packages to be installed.
#' 
#' @seealso [try_install()]
#' @export
install_missing_packages <- function(required_packages){
  
  installed_packages <- rownames(installed.packages())
  
  missing_packages <- setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) == 0) {
    message("All Required Packages are installed")
    return()
  }
  # Vectorisation on each one, so we can do tryCatch
  lapply(missing_packages, try_install)
}
#' Load all packages from a vector
#' 
#' Load all packages from a vector via library()
#' 
#' @param required_packages Vector of Strings. All packages that you want to be loaded
#' 
#' @return None. Loads all packages.
#' @export
load_packages <- function(required_packages) {
  
  for (pkg in required_packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}


#' Download csv from an url
#' 
#' This is a very generic function that takes an url and downloads the csv data behind it.
#' 
#' @param url String. Url where the csv file is
#' @param filepath Where the file is stored
#' 
#' @return Bool. Messages indicate if download was succesfull and returns \code{TRUE} or \code{False} respectively
#' 
#' @seealso [download_url_csv_wrapper()]
#' @export
download_url_csv <- function(url, filepath) {
  
  tryCatch({
    data <- read_csv(url)
    
    write.csv(data, filepath)
    
    return(TRUE)
    
  }, error = function(e) {
    
    message("Download failed: ", e$message)
    
    return(FALSE)
  })
}

#' Wrapper Function for the download_url_csv_function
#' 
#' Allows to call it only if wither file is missing or there is a forced api call
#' 
#' @param url String. Url where the csv file is
#' @param filepath Where the file is stored
#' @param do_api_call Bool. If \code{TRUE}, forces a download even if the file exists.
#' 
#' @return None. Messages indicating success.
#' @seealso [download_url_csv()]
#' @export
download_url_csv_wrapper <- function(url, filepath, do_api_call) {
  
  if (!file.exists(filepath) | do_api_call) {
    success <- download_url_csv(url, filepath)
    
    if (success){
      message("Successfully downloaded file from", url)
    } else {
      message("Error in Downloading from", url)
    }
  } else {
    message("File '", filepath, "already exists. Skipping download.")
  }
}

# Still not really happy here, look at it again later  

#' A Wrapper for the BFS Packages download function
#' 
#' This wrapper allows to download functions from the BFS Package. Depending on the source
#' either data or asset we use different functions. This function handles that distinction.
#' It also only downloads if either teh data isn't at the given path or if there is a forced re-download
#' 
#' @param id String. The BFS identifier (PX-ID for 'data' or Asset-ID for 'asset').
#' @param dest_file String. The local file path where the data should be saved.
#' @param do_api_call Bool. If \code{TRUE}, forces a new download even if 
#' the file exists locally. Defaults to \code{FALSE}.
#' @param type String. The type of download: \code{"data"} for API table 
#' extraction or \code{"asset"} for direct file downloads (e.g., Excel/CSV).
#'
#' @return Bool Returns \code{TRUE} if the file exists or was successfully 
#' downloaded; \code{FALSE} otherwise.
#' 
#' @importFrom BFS bfs_get_data bfs_download_asset
#' @importFrom readr write_csv
#' @export
bfs_wrapper <- function(id, dest_file, do_api_call = FALSE, type = "data") {
  
  # Initialize success as FALSE, Update if data is downloaded
  # BFS FUnctions are marked wit BFS:: prefix
  success <- FALSE
  
  if (!file.exists(dest_file) | do_api_call) {
    message("Fetching BFS ", type, " for ID: ", id)
    
    if (type == "data") {
      
      # bfs_get_data returns a dataframe, we want to save it
      df <- BFS::bfs_get_data(number_bfs = id,
                              language = "fr")
      
      write_csv(df, dest_file)
      
      success <- TRUE
      
    } else if (type == "asset") {
      
      # bfs_download_asset downloads file directly to dest_file
      download_path <- BFS::bfs_download_asset(
        number_asset = id, 
        destfile = dest_file
      )
      
      success <- !is.null(download_path)
      
    } else {
      message("Error: type '", type, "' does not exist.")
      
      success <- FALSE
    }
    
  } else {
    message("Loading ", dest_file, " from Disk...")
    
    success <- TRUE
  }
  
  return(message("Download successful == ", success))
}




#--- Functions for Data Handling ---

#' Create Standardized Time Series Dataframe
#' 
#' Transforms a raw dataframe into a standardized two-column format (date and value). 
#' This function converts the date column to \code{zoo::yearmon} and ensures 
#' the value column is numeric, sorted, and free of missing data.
#'
#' @param data Dataframe. The input dataset to be formatted (e.g., raw BFS or SNB output).
#' @param date_col String. The name of the original column containing date information.
#' @param value_col String. The name of the original column containing numeric values.
#' @param new_name String. The final name for the value column in the output.
#' @param date_format String. The format string used to parse the date 
#' (e.g., \code{"\%Y-\%m"} for 2024-01, \code{"\%b \%Y"} for Jan 2001, or \code{"\%YQ\%q"} for 2024Q1).
#' @param seasonal_adj Set to true if you want data to be seasonally adjusted.
#' Detects either quarterly or monthly and adjusts accordingly
#'
#' @return A tidy dataframe with two columns: \code{date} (class \code{yearmon}) 
#' and \code{[new_name]} (class \code{numeric}), sorted chronologically.
#' 
#' @details 
#' The function uses \code{rlang}'s injection operator (\code{!!}) to handle 
#' dynamic column naming, allowing you to pass the \code{new_name} as a simple string.
#' 
#' @importFrom zoo as.yearmon
#' @importFrom dplyr select arrange
#' @importFrom tidyr drop_na
#' @export
format_time_series_df <- function(data,
                                  date_col,
                                  value_col,
                                  new_name,
                                  date_format,
                                  seasonal_adj = FALSE) {
  
  # format as dataframe
  df <- as.data.frame(data)
  
  # rename the columns
  colnames(df)[colnames(df) == date_col] <- "date"
  colnames(df)[colnames(df) == value_col] <- new_name
  
  # format the date as yearmonth
  df$date <- zoo::as.yearmon(as.character(df$date), format = date_format)
  
  # format the value as numeric
  df[[new_name]] <- as.numeric(df[[new_name]])
  
  # Clean up
  df <- df %>%
    select(all_of(c("date", new_name))) %>%
    drop_na() %>%
    filter(date >= zoo::as.yearmon(1990)) %>%
    arrange(date)
  
  if(seasonal_adj) {
    
    freq <- if(mean(diff(df$date)) < 0.3) 12 else 4
    start_year <- as.numeric(format(df$date[1], "%Y"))
    start_per <- as.numeric(format(df$date[1], "%m"))
    if(freq == 4) start_per <- as.numeric(as.yearqtr(df$date[1]))
    
    diff_mean <- mean(diff(df$date))
    message(paste("Detected Mean Diff:", round(diff_mean, 4)))
    message(paste("Assigned Frequency:", freq))
    
    ts_temp <- ts(df[[new_name]], start = c(start_year, start_per), frequency = freq)
    
    adj_model <- seasonal::seas(ts_temp)
# adj_model <- seasonal::seas(ts_temp,x11.mode = "add",x11.sigmalim=c(1.5,2.5),x11.appendfcst = "yes",transform.function="none",regression.aictest=NULL,    
    df[[new_name]] <- as.numeric(seasonal::final(adj_model))
    
    message(paste("Applied seasonal adjustment to", new_name))
  }
  
  return(df)
}





#' Extract Estimated Parameters and States from Rolling SSM Results
#'
#' @description
#' Iterates through the results of a rolling State-Space estimation (pseudo out-of-sample)
#' and collapses the estimated parameters and the final latent state into a single, 
#' tidy dataframe.
#'
#' @param results_list A nested list, typically the output from \code{new_forecasting_okun_ssm}. 
#'   Each element of the list must contain \code{target_date}, \code{params} (a named list of 
#'   economic scale parameters), and \code{states} (the Kalman Filter output containing the state vector \code{r}).
#'
#' @details 
#' For each vantage point (estimation window), the function:
#' \enumerate{
#'   \item Converts the list of mapped economic parameters (\eqn{\beta}, \eqn{\sigma}, etc.) into a row.
#'   \item Extracts the \bold{final} estimated value of the latent state (\eqn{\rho_T}), which 
#'   represents the Natural Rate of unemployment at that specific point in time.
#'   \item Appends the specific \code{quarter} to identify the estimation vintage.
#' }
#' 
#' This creates a "Vantage Point Dataframe" where each row represents the model's 
#' worldview at a specific date in history.
#'
#' @return A \code{tibble} or \code{data.frame} containing:
#' \item{quarter}{The date of the vantage point (estimation end date).}
#' \item{natural_rate}{The final filtered estimate of the structural trend (\eqn{\rho_T}).}
#' \item{...}{All estimated model parameters (e.g., beta1, beta2, sigma_unemp_rate).}
#'
#' @seealso \code{\link{loglik_ssm}} for the origin of the \code{params} and \code{states} objects.
#' @seealso [rolling_est_okun_ssm()]
#' 
#' @export
extract_params_df<- function(results_list, extract_fitted_obs = FALSE) {
  
  # Map over each rolling window vantage point in the list
  params_df <- purrr::map_dfr(results_list, function(item) {
    
    # Extract the vantage point date (e.g., "2018 Q3")
    vp_date <- item$target_date
    
    # Flatten the optimized structural coefficients into a 1-row data frame
    params <- as.data.frame(item$params)
    
    # Extract state(s)
    # item$states$r is a matrix of size [T x Num_States]
    state_matrix <- as.matrix(item$states$r)
    last_row_idx <- nrow(state_matrix)
    
    # State 1 is always the structural variable of interest here so we give it the natural rate name
    params$natural_rate <- state_matrix[last_row_idx, 1]
    
    # Dyif there are other states (here at most 3 we extract them as well)
    if (ncol(state_matrix) >= 2) {
      params$state_2_trend <- state_matrix[last_row_idx, 2]
    }
    if (ncol(state_matrix) >= 3) {
      params$state_3_trend <- state_matrix[last_row_idx, 3]
    }
    
    # do the same thing for fitted observations -> extract them all if needed
    if (extract_fitted_obs) {
      # item$states$fitted_obs_t_t is a matrix of size [T x Num_Observed_Y_Vars]
      fitted_matrix   <- as.matrix(item$states$fitted_obs_t_t)
      last_fitted_row <- nrow(fitted_matrix)
      
      # Column 1 is always the primary model fitting series (e.g., fit_rate, fit_inflation, fit_unemp)
      params$fitted_obs <- fitted_matrix[last_fitted_row, 1]
      
      # if there are other variables we extract them as well
      if (ncol(fitted_matrix) >= 2) {
        params$fitted_spf_survey <- fitted_matrix[last_fitted_row, 2]
      }
    }
    
    # Add the chronological tracking marker column
    params <- params %>%
      mutate(quarter = vp_date) %>%
      select(quarter, everything())
    
    return(params)
  })
  
  return(params_df)
}









#' Calculate 5y-5y Forward Rate
#' 
#' @description 
#' Implements the forward rate formula:
#' i_f = ((1 + i_t2) / (1 + i_t1))^(1 / (t2 - t1)) - 1
#'
#' @param yield_5y Numeric vector of 5-year government bond yields.
#' @param yield_10y Numeric vector of 10-year government bond yields.
#' @param input_as_percent Logical; if TRUE, divides yields by 100 before calculation.
#' 
#' @return A numeric vector containing the calculated 5y-5y forward rate.
calculate_forward_rate <- function(yield_5y, yield_10y, input_as_percent = TRUE) {
  
  # Ensure data is in decimal form if provided as percentages
  if(input_as_percent) {
    yield_5y <- yield_5y / 100
    yield_10y <- yield_10y / 100
  }
  
  # Calculate the 5y-5y Forward Rate
  # Based on t1 = 5 and t2 = 10, the maturity gap (t2 - t1) is 5
  forward_rate_5y5y <- ((1 + yield_10y)^10 / (1 + yield_5y)^5)^(1 / 5) - 1
  
  return(forward_rate_5y5y)
}





# ==============================================================================
# parameter mapping for keeping them positive if needed
# ==============================================================================

#' Map Optimizer Parameters to Model Parameters (Economic Scale)
#'
#' @description
#' Transforms unconstrained parameters from the optimizer to their 
#' constrained economic scale using the rules defined in the SSM manifest.
#' This is the inverse operation of \code{model2param_gen}.
#'
#' @param theta Numeric vector. The unconstrained parameter values (often 
#' produced by \code{optim} or \code{optimx}).
#' @param ssm A structured SSM object containing a \code{manifest} list. 
#' The manifest defines the transformation \code{rule} for each parameter.
#'
#' @details
#' Optimization algorithms often work best in an unconstrained space 
#' (\eqn{(-\infty, \infty)}). To ensure economic variables like variances 
#' are positive or persistence is bounded, we apply the following transformations:
#' \itemize{
#'   \item \bold{Rule 0 (Linear/Default):} \eqn{f(\theta) = \theta}. No transformation. 
#'   Used for parameters that can take any real value, such as Okun's Law \eqn{\beta} coefficients.
#'   \item \bold{Rule 1 (Exponential):} \eqn{f(\theta) = \exp(\theta)}. Constrains 
#'   the parameter to be strictly positive (\eqn{>0}). Used for measurement and process 
#'   standard deviations (\eqn{\sigma, \xi}).
#'   \item \bold{Rule 2 (Logistic):} \eqn{f(\theta) = \frac{1}{1 + \exp(-\theta)}}. 
#'   Constrains the parameter between 0 and 1. Used for persistence parameters 
#'   (\eqn{\phi}) to ensure stationarity in the State-Space model.
#' }
#'
#' @return A named list of parameters on their natural economic scale, 
#' ready to be used by the SSM matrix builders.
#'
#' @seealso \code{\link{model2param_gen}} for the forward transformation into 
#' optimizer space.
#' @export
param2model_gen <- function(theta, ssm) {
  
  # Extract the manifest from the ssm object
  spec <- ssm$manifest
  
  # get all available parameters
  p_names <- names(spec)
  
  # Initialize
  out <- list()
  
  for (i in seq_along(p_names)) {
    # Loop over each parameters and apply the given rule
    # This takes a guess by the optimizer such as -3 and applies the selected rule
    # variances have to be positive this is why we would apply exp()
    # they map one  to one so at the end we just take the last and best guess
    # from teh optimizer and apply the rule to get the true parameters
    name <- p_names[i]
    rule <- spec[[name]]$rule
    val  <- theta[i]
    
    # Switch has a default for all non 1 or 2 here -> 0 is exactly that, therefore
    # if there is no transformation specification it just takes the given value
    out[[name]] <- switch(as.character(rule),
                          "1" = exp(val) + 1e-7,                    # Exponential (Variances) I sthis okay to do
                          "2" = 1 / (1 + exp(-val)),         # Logistic (AR Phi)
                          "3" = {                           # Rule 3: Bounded
                            low  <- spec[[name]]$low
                            high <- spec[[name]]$high
                            low + (high - low) / (1 + exp(-val))
                          },
                          val                                # Default / Rule 0 (Betas)
    )
  }
  return(out)
}



#' Map Model Parameters to Optimizer Parameters (Unconstrained Scale)
#'
#' @description
#' Transforms parameters from their constrained economic scale back to the 
#' unconstrained scale used by the optimizer. This ensures that initial guesses 
#' are correctly represented in the search space.
#'
#' @param model_list A named list of parameters on their natural economic scale 
#' (e.g., standard deviations as positive numbers, persistence as values between 0 and 1).
#' @param ssm A structured SSM object containing a \code{manifest} list. 
#' The manifest defines the transformation \code{rule} for each parameter.
#'
#' @details
#' This function applies the inverse of the transformations found in \code{param2model_gen}:
#' \itemize{
#'   \item \bold{Rule 0 (Linear):} \eqn{g(y) = y}. Used for unconstrained parameters like betas.
#'   \item \bold{Rule 1 (Logarithmic):} \eqn{g(y) = \log(y)}. The inverse of \eqn{\exp(\theta)}. 
#'   Ensures that a positive standard deviation maps to a real number.
#'   \item \bold{Rule 2 (Logit):} \eqn{g(y) = \log\left(\frac{y}{1-y}\right)}. The inverse of the 
#'   logistic function. Maps a parameter bounded in \eqn{(0, 1)} to the real line \eqn{(-\infty, \infty)}.
#' }
#'
#' @return A numeric vector (\code{theta}) of unconstrained parameters suitable for 
#' use in \code{optim} or \code{optimx}.
#'
#' @seealso \code{\link{param2model_gen}} for the forward transformation used 
#' within the likelihood function.
#' @export
model2param_gen <- function(model_list, ssm) {
  spec <- ssm$manifest
  p_names <- names(spec)
  theta <- numeric(length(p_names))
  
  for (i in seq_along(p_names)) {
    name <- p_names[i]
    rule <- spec[[name]]$rule
    val  <- model_list[[name]]
    
    # Switch has a default for all non 1 or 2 here -> 0 is exactly that, therefore
    # if there is no transformation specification it just takes the given value
    theta[i] <- switch(as.character(rule),
                       "1" = log(val),
                       "2" = log(val / (1 - val)),        # Logit inverse
                       "3" = {
                         low  <- spec[[name]]$low
                         high <- spec[[name]]$high
                         # Inverse: Maps [low, high] back to (-inf, inf)
                         log((val - low) / (high - val))
                       },
                       val
    )
  }
  return(theta)
}


standardize_yq_seq <- function(df, start_date = "2000-01-01") {
  # 1. Convert the row date labels inside the first column safely
  df[[1]] <- format(zoo::as.yearqtr(df[[1]]), format = "%YQ%q")
  
  # 2. Build a continuous calendar sequence based on the number of vintage columns
  # (ncol(df) - 1) calculates exactly how many vintage columns need headers
  start_qtr <- zoo::as.yearqtr(as.Date(start_date))
  col_seq   <- start_qtr + (0:(ncol(df) - 2)) / 4
  
  # 3. Combine the blank first column name with your newly generated calendar sequence
  colnames(df) <- c(colnames(df)[1], format(col_seq, format = "%YQ%q"))
  
  return(df)
}


