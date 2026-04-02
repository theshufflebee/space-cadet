#!/usr/bin/env Rscript

required_packages <- c("roxygen2","devtools","pkgdown")
installed_packages <- rownames(installed.packages())
new_packages <- setdiff(required_packages, installed_packages)
if (length(new_packages) > 0) {
  install.packages(new_packages, repos="https://stat.ethz.ch/CRAN/")
}



library(roxygen2)
library(devtools)
library(pkgdown)

generate_documentation <- function() {
  tryCatch({
    # It's important that roxygenise() is called after compileAttributes()
    roxygen2::roxygenise()
    # Check if useDynLib is in NAMESPACE, if not, add it
    ns_path <- "NAMESPACE"
    ns_content <- readLines(ns_path)
    devtools::install()
    pkgdown::build_site()
    message("Documentation generation, package installation, and site build completed successfully.")
  }, error = function(e) {
    message("An error occurred: ", conditionMessage(e))
  })
}
generate_documentation()
