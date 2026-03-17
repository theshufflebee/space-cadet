
#------------------------------------------------------------------------------
# 0. Setup
#------------------------------------------------------------------------------

# --------- i. Clear environment ---------

rm(list=ls())

# --------- ii. Load here package to enable finding script loading other packages 

# First, define a function that checks if a package is installed. 
# If not, it installs it. Then it loads it.
package_loader <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load here package to be able to locate subsequent files
package_loader("here")


# Load project functions
source(here("R", "load_snb_data.R"))
source(here("R", "utils.R"))

#------------------------------------------------------------------------------
# 1. Run the Data Loading Script
#------------------------------------------------------------------------------
source(here("scripts", "01_load_data.R"))
