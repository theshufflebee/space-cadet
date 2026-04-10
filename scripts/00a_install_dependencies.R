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
  "neverhpfilter", #for HP Filter
  "conflicted", # to handle conflict between packages
  "mFilter", # HP Filter
  "xts"  # Time series data handling
  
)


# ------------------------------------------------------------------------------
# Execute installation and loading
# ------------------------------------------------------------------------------

# installs only missing packages, does not re install installed packages
install_missing_packages(required_packages)

# Loads all packages
load_packages(required_packages)



# Resolve conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag",    "stats") 
conflict_prefer("complete", "tidyr") 
conflict_prefer("last", "dplyr")


