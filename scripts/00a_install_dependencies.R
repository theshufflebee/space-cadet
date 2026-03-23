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
  "tidyverse",  # Includes dplyr, readr, and others
  "here",       # Project-relative paths
  "RCurl",      # API network calls
  "jsonlite",   # Parsing JSON from KOF/BFS
  "glue",       # String interpolation for API URLs
  "readxl",     # Excel imports
  "kofdata",    # Swiss KOF Data Access
  "zoo",        # Time series / Date handling
  "BFS" ,       # Swiss Federal Statistical Office Data Access
  "roxygen2"    # For Documentation
)


# ------------------------------------------------------------------------------
# Execute installation and loading
# ------------------------------------------------------------------------------

# installs only missing packages, does not re install installed packages
install_missing_packages(required_packages)

# Loads all packages
load_packages(required_packages)
