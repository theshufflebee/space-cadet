################################################################################
# 
# Loading and installation of all required packages
#
################################################################################


# ------------------------------------------------------------------------------
# Required Package List
# ------------------------------------------------------------------------------

# All packages saved into a vector to load later
required_packages <- c(
  "MASS",        # For Matrix inversion in Kalman filter
  "tidyverse",  # Includes dplyr, readr, and others
  "here",       # Project-relative paths
  "RCurl",      # API network calls
  "jsonlite",   # Parsing JSON from KOF/BFS
  "glue",       # String interpolation for API URLs
  "readxl",     # Excel imports
  "kofdata",    # Swiss KOF Data Access
  "zoo",        # Time series / Date handling
  "BFS" ,       # Swiss Federal Statistical Office Data Access
  "roxygen2",    # For Documentation
  "readr",
  "optimx",      # For optimizing in the Kalman Filter
  "argparse",     # For the CLI
  "conflicted", # to handle conflict between packages
  "mFilter", # HP Filter
  "xts",  # Time series data handling
  "seasonal", # for seasonality adjustments
  "ggplot2", # For plotting
  "patchwork", # Also for Plotting and combining plots
  # "creaFcstEval", # Internal Package for forecast evaluation
  "forecast", # For running other forecasts such as ARIMA
  "eurostat",
  "reticulate",
  "remotes",
  "tsibble",
  "fable",
  "data.table"
)


# also sources install_missing_packages function
source(here("R", "utils.R"))

# ------------------------------------------------------------------------------
# Execute installation and loading
# ------------------------------------------------------------------------------

# installs only missing packages, does not re install installed packages
install_missing_packages(required_packages)

# Loads all packages
load_packages(required_packages)

if (!requireNamespace("data.table", quietly = TRUE) || 
    packageVersion("data.table") < "1.16.0") {
  message(">>> CRITICAL OUTDATED DEPENDENCY: Updating 'data.table' to restore 'sort_by'...")
  install.packages("data.table", repos = "https://cloud.r-project.org", type = "binary")
  
  # Unload and reload the updated namespace to prevent memory corruption
  if ("data.table" %in% loadedNamespaces()) unloadNamespace("data.table")
}

remotes::install_local("C:/Users/jonas/Desktop/repos/creaFcstEval", force = FALSE)

# Resolve conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag",    "stats") 
conflict_prefer("complete", "tidyr") 
conflict_prefer("last", "dplyr")
conflict_prefer("yearqtr", "zoo")


