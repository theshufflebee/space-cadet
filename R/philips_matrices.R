################################################################################
# 
# Build the Matrices for the Phillips Model
#
################################################################################




build_X_data_matrix_philips <- function(T_0 = "2015-01-01",
                                        vantage_quarter = "2023 Q1",
                                        data = master_philips,
                                        h = 8) {
  
  # Lag CPI before burn cutoff
  raw_data <- data %>%
    arrange(quarter) %>%
    mutate(
      cpi_lagged = lag(cpi, n = 1)
    )
  
  # Add burn in cutoff
  raw_data <- raw_data %>%
    filter(quarter >= as.yearqtr(model_philips_burn_in)) %>%
    filter(quarter <= as.yearqtr(vantage_quarter)) %>%
    arrange(quarter)
  
  LOP_forecast <- splice_snb_series(vantage_quarter = vantage_quarter,
                                    snb_reer_delay = SNB_REER_DELAY,
                                    data = raw_data,
                                    burn_in = model_philips_burn_in)
  
  HP_gap_data <- get_hp_gap(raw_data,
                            raw_data,
                            vantage_q = vantage_quarter)
  
  X_matrix <- raw_data %>%
    select(quarter, cpi_lagged) %>%
    left_join(LOP_forecast %>% select(quarter, lop_gap), by = "quarter") %>%
    left_join(HP_gap_data %>% select(quarter, gdp_gap = gap), by = "quarter") %>%
    # Filter to return the specific estimation sample
    filter(quarter >= as.yearqtr(T_0)) %>%
    arrange(quarter) %>%
    as.matrix()
  
  return(X_matrix)
}




