################################################################################
# 
# Script for Running the Full Project
#
################################################################################

#------------------------------------------------------------------------------
# 0. Setup
#------------------------------------------------------------------------------

# --------- i. Clear environment ---------

rm(list=ls())

# --------- ii. Load here package to enable finding script loading other packages 

if (!require("here", character.only = TRUE, quietly=TRUE)) {
  message(paste("Installing missing package:", "here"))
  install.packages("here", dependencies = TRUE)
  library("here", character.only = TRUE)
}
# Load all internal functions

source(here("R", "load_snb_data.R"))
source(here("R", "utils.R"))

#------------------------------------------------------------------------------
# 0a. Run the Package Installation and Loading Script
#------------------------------------------------------------------------------
source(here("scripts", "00a_install_dependencies.R"))


#------------------------------------------------------------------------------
# 0b. Run the Config script to get Parameters
#------------------------------------------------------------------------------
source(here("scripts", "00b_config.R"))



#------------------------------------------------------------------------------
# 1. Run the Data Loading Script
#------------------------------------------------------------------------------
source(here("scripts", "01_load_data.R"))


#------------------------------------------------------------------------------
# 2. Format Data into one master df
#------------------------------------------------------------------------------
source(here("scripts", "02_format_data.R"))


#------------------------------------------------------------------------------
# 3. Prepare Data for analysis
#------------------------------------------------------------------------------
source(here("scripts", "03_transform_data.R"))


#------------------------------------------------------------------------------
# 4. Run Okun Model
#------------------------------------------------------------------------------
source(here("scripts", "04_unemployment_forecasts.R"))






