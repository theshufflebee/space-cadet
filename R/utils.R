################################################################################
#
# Various Utility functions
#
################################################################################

# REMARK: Roxygen documentation has been generated with AI

#--- Functions for Setup ---

# To install missing packages
# Input Vector of required packages
# Output none (Packages installed)


#' Helper function to try installing packages from binary or source if that fails
#' 
#' @param pkg Character the package to be installed
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


#' Create Standardized Time Series Data Frame
#'
#' Standardizes raw data into a tidy two-column format (\code{date} and custom-named value column).
#' Parses date strings to \code{zoo::yearmon}, coerces values to numeric, sorts chronologically,
#' drops missing values, and optionally performs automated X-13ARIMA-SEATS seasonal adjustment.
#'
#' @param data Data frame containing raw time series data.
#' @param date_col Character. Name of the column containing date strings.
#' @param value_col Character. Name of the column containing numeric values.
#' @param new_name Character. Target name for the value column in the output data frame.
#' @param date_format Character. Date parsing format string passed to \code{zoo::as.yearmon}
#'   (e.g., \code{"\%Y-\%m"}, \code{"\%b \%Y"}, or \code{"\%YQ\%q"}).
#' @param seasonal_adj Logical. If \code{TRUE}, detects quarterly vs. monthly frequency
#'   and applies X-13ARIMA-SEATS seasonal adjustment via \code{seasonal::seas}. Defaults to \code{FALSE}.
#'
#' @return A tidy data frame containing:
#' \item{date}{Standardized dates of class \code{zoo::yearmon}.}
#' \item{[new_name]}{Numeric time series values (seasonally adjusted if requested).}
#'
#' @export
#' @importFrom zoo as.yearmon as.yearqtr
#' @importFrom dplyr select arrange all_of %>%
#' @importFrom tidyr drop_na
#' @importFrom stats ts
#' @importFrom seasonal seas final
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
    df[[new_name]] <- as.numeric(seasonal::final(adj_model))
    
    message(paste("Applied seasonal adjustment to", new_name))
  }
  
  return(df)
}



#' Extract Estimated Parameters and States from Recursive SSM Outputs
#'
#' Iterates across a number of recursive (expanding/rolling) state-space estimation results,
#' extracting optimized structural parameters, terminal filtered state estimates (\eqn{\boldsymbol{\rho}_T}),
#' and optional contemporaneous fitted observations into a tidy data frame.
#'
#' @param results_list A list of estimation vintage objects. Each element must contain
#'   \code{target_date} (vintage identifier), \code{params} (named list of structural parameters),
#'   and \code{states} (Kalman filter state outputs including the state trajectory matrix \code{r}).
#' @param extract_fitted_obs Logical. If \code{TRUE}, extracts the terminal filtered measurement
#'   values from \code{states$fitted_obs_t_t}. Defaults to \code{FALSE}.
#'
#' @return A \code{tibble}/\code{data.frame} where each row represents an estimation vintage, containing:
#' \item{quarter}{Vantage point date identifying the estimation sample endpoint.}
#' \item{natural_rate}{Terminal filtered estimate of the primary latent state (\eqn{\rho_{T,1}}).}
#' \item{state_2_trend, state_3_trend}{Terminal estimates of additional latent states, if present.}
#' \item{fitted_obs, fitted_spf_survey}{Terminal fitted values for observed measurement series (if \code{extract_fitted_obs = TRUE}).}
#' \item{...}{Estimated structural model parameters on their natural economic scale.}
#'
#' @export
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate select everything %>%
extract_params_df<- function(results_list, extract_fitted_obs = FALSE) {
  
  # Map over each rolling window vantage point in the list
  params_df <- purrr::map_dfr(results_list, function(item) {
    
    # Extract the vantage point date
    vp_date <- item$target_date
    
    # Flatten the optimized structural coefficients into a 1-row data frame
    params <- as.data.frame(item$params)
    
    # Extract state(s)
    # item$states$r is a matrix of size [T x Num_States]
    state_matrix <- as.matrix(item$states$r)
    last_row_idx <- nrow(state_matrix)
    
    # State 1 is always the structural variable of interest here so we give it the natural rate name
    params$natural_rate <- state_matrix[last_row_idx, 1]
    
    # If there are other states (here at most 3 we extract them as well)
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



#' Calculate 5y-5y Forward Interest Rate
#'
#' Computes the 5-year forward rate 5 years ahead (5y-5y forward) from 5-year
#' and 10-year spot bond yields using the compound interest formula:
#' \deqn{f_{5,10} = \left( \frac{(1 + y_{10})^{10}}{(1 + y_5)^5} \right)^{1/5} - 1}
#'
#' @param yield_5y Numeric vector of 5-year spot yields.
#' @param yield_10y Numeric vector of 10-year spot yields.
#' @param input_as_percent Logical. If \code{TRUE}, inputs are assumed to be in percent
#'   (e.g., 2.5) and are divided by 100 before calculation. Defaults to \code{TRUE}.
#'
#' @return A numeric vector of calculated 5y-5y forward rates in decimal format.
#' @export
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



#' Map Optimizer Parameters to Model Parameters (Structural Scale)
#'
#' Transforms unconstrained numeric parameter estimates (\eqn{\mathbb{R}}) generated
#' during numerical optimization back into their constrained structural parameter
#' space using transformations defined in the model manifest.
#'
#' @param theta Numeric vector of unconstrained parameter guesses generated by the optimizer.
#' @param ssm Structured state-space model object containing a \code{manifest} list with
#'   parameter transformation rules and bounds.
#'
#' @details
#' Optimization algorithms operate over an unconstrained domain (\eqn{-\infty, \infty}).
#' To satisfy economic and statistical bounds (e.g., positive variances, stationary persistence),
#' \code{theta} values are mapped via the following rules:
#' \itemize{
#'   \item \bold{Rule 0 (Linear/Identity):} \eqn{f(\theta) = \theta}. No transformation applied. Used for unconstrained parameters.
#'   \item \bold{Rule 1 (Exponential):} \eqn{f(\theta) = \exp(\theta) + 1e-7}. Enforces strict positivity (\eqn{y > 0}) with a minor regularizing lower bound to prevent variance collapse.
#'   \item \bold{Rule 2 (Logistic):} \eqn{f(\theta) = (1 + \exp(-\theta))^{-1}}. Maps strictly to \eqn{(0, 1)}. Used for autoregressive persistence parameters (\eqn{\phi}) to ensure stationarity.
#'   \item \bold{Rule 3 (Scaled Logistic):} \eqn{f(\theta) = \text{low} + (\text{high} - \text{low}) / (1 + \exp(-\theta))}. Maps strictly to custom bounds \eqn{(\text{low}, \text{high})}.
#' }
#'
#' @return A named list of structural model parameters mapped to their valid economic scales,
#'   formatted for ingestion by the state-space matrix builders.
#'
#' @seealso \code{\link{model2param_gen}}
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
#' Transforms parameters from their constrained structural scale to the unconstrained
#' search space (\eqn{\mathbb{R}}) required by numerical optimizers such as \code{optim} or \code{optimx}.
#'
#' @param model_list Named list of structural model parameters on their constrained domains
#'   (e.g., strictly positive variances or persistence parameters bounded within intervals).
#' @param ssm Structured state-space model object containing a \code{manifest} list with
#'   parameter transformation rules and bounds.
#'
#' @details
#' Applies inverse transformations according to the parameter's manifest \code{rule}:
#' \itemize{
#'   \item \bold{Rule 0 (Linear/Identity):} \eqn{g(y) = y}. Used for unconstrained parameters.
#'   \item \bold{Rule 1 (Logarithmic):} \eqn{g(y) = \log(y)}. Inverts \eqn{\exp(\theta)} for strictly positive parameters (\eqn{y > 0}).
#'   \item \bold{Rule 2 (Logit):} \eqn{g(y) = \log(y / (1 - y))}. Inverts standard logistic mapping for parameters bounded in \eqn{(0, 1)}.
#'   \item \bold{Rule 3 (Scaled Logit):} \eqn{g(y) = \log((y - \text{low}) / (\text{high} - y))}. Inverts generalized logistic mapping for parameters bounded in \eqn{(\text{low}, \text{high})}.
#' }
#'
#' @return A named numeric vector \code{theta} of unconstrained parameters.
#'
#' @seealso \code{\link{param2model_gen}}
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

#' Standardize Year-Quarter Labels and Column Sequences
#'
#' Standardizes row date labels in the first column to \code{"YYYYQq"} format and
#' generates an unbroken quarterly calendar sequence for all subsequent vintage column names,
#' starting from the quarter parsed in the second column header.
#'
#' @param df A data frame where the first column contains quarterly date identifiers
#'   and column headers from the second column onward represent quarterly forecast vintages.
#'
#' @return A data frame with normalized \code{"YYYYQq"} date strings in the first column
#'   and sequential \code{"YYYYQq"} headers across all vintage columns.
#' @export
#' @importFrom zoo as.yearqtr
standardize_yq_seq <- function(df) {
  # Convert the row date labels inside the first column safely
  df[[1]] <- format(zoo::as.yearqtr(df[[1]]), format = "%YQ%q")
  
  
  # Build a continuous calendar sequence based on the number of vintage columns
  # (ncol(df) - 1) calculates exactly how many vintage columns need headers
  start_qtr <- zoo::as.yearqtr(colnames(df)[2])
  col_seq   <- start_qtr + (0:(ncol(df) - 2)) / 4
  
  # Combine the blank first column name with newly generated calendar sequence
  colnames(df) <- c(colnames(df)[1], format(col_seq, format = "%YQ%q"))
  
  return(df)
}
# REMARK -> With a slight rework we'd make this redundant by standardizing dates
# To wor with the creaFcstEval

#' Round Numbers to the Nearest Multiple
#'
#' Rounds numeric values to the nearest specified step or increment (e.g., nearest 0.25).
#'
#' @param x Numeric vector to be rounded.
#' @param step Numeric scalar representing the rounding increment/multiple. Defaults to \code{0.25}.
#'
#' @return A numeric vector rounded to the nearest multiple of \code{step}.
#' @export
mround <- function(x, step = 0.25) 
{round(x / step) * step}




reduce_diagonal_matrix <- function(mat, max_h) {
  # mat: data frame or matrix where
  #      col 1 = forecast origin date
  #      cols 2:ncol = target dates
  #      diagonal (row i, col 1 + i) = observed actuals (h = 0)
  #      rows i > j = forecast horizon steps h = row - col + 1
  # max_h: maximum allowed forecast horizon (e.g., 4 or 8)
  
  mat_out <- mat
  n_rows <- nrow(mat_out)
  n_cols <- ncol(mat_out)
  
  # Forecast columns start at index 2
  for (col_idx in 2:n_cols) {
    origin_idx <- col_idx - 1  # Row index of the origin on the diagonal (h = 0)
    cutoff_row <- origin_idx + max_h + 1
    
    if (cutoff_row <= n_rows) {
      mat_out[cutoff_row:n_rows, col_idx] <- NA
    }
  }
  
  return(mat_out)
}

