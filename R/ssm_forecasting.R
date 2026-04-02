################################################################################
#
# These functions are used to create forecast
#
################################################################################

# Pseudocode

ssm_forecaster <- function(Y, X, forecast_start, all_builder_functions) {
  
  
  date vector <- select dates from Y
  
  drop the dates 
  
  
  forecast_dates_vector <- select dates from_date vector >= forecast_start
  
  comp <- list with length(forecast_dates_vector)
  
  
  for (i in len(forecast_dates_vector)) {
    select date i from forecast_dates_vector
    
    Y_select, X_select select Y, X where date <= forecast_dates_vector[i]
    
    output <- run ssm optimizer with Y_select, X_select select, all_builder functions$(the matrix here)
    
    
    comp[i] <- output # add tolist at correct forecast date
    
  }
}



