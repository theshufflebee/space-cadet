# space-cadet

This repo contains all the code related to my Masters Thesis at CREA at HEC Lausanne. The Thesis' main goal is to forecast important macroeconomic variables of the Swiss Economy with State space models.

## How to run

This is currently a work in Progress. The working parts of the project can be run through the main.R script in the repo root. As of 20.03.2026 it runs only until script 02 and it isn't complete yet. Open the .rproj file with R Studio and run the main script via source("main.R")

## Libraries

```         
R version 4.3.3
```

### Data Acquisition

-   **`kofdata`**: Interface for downloading KOF Institute data.
-   **`BFS`**: Download data from the Swiss Federal Statistical Office.
-   **`RCurl`**: General purpose client for API calls.
-   **`jsonlite`**: Handling of JSON files

### File Handling

-   **`readxl`**: Importing data from Excel
-   **`readr`**: Data Handling for rectangular files

### Data Manipulation & Time Series

-   **`dplyr`**: Data manipulation
-   **`zoo`**: Time series Handling

## Project Directory Structure

Tree created via fs::dir_tree()

``` text
space-cadet/
├── data                                # Contains all data
│   └── raw                             # Contains all untransformed data
│       ├── cpi_series.xlsx
│       ├── employment_data.csv
│       ├── gov_bonds.csv
│       ├── gov_bonds_metadata.json
│       ├── kof_consensus_master.csv
│       ├── money_market.csv
│       ├── money_market_metadata.json
│       ├── snb_reer_manual_download.xlsx   # Need to find fix for Download
│       └── unemployment_canton.csv
├── main.R                              # script to run the project
├── workplace.Rmd                       # All work that is in progres is done here
├── output                              # All outputs are stored here
├── R                                   # This folder contains all functions
│   ├── load_snb_data.R                 # SNB API Calls
│   └── utils.R                         # Utility functions
├── ram                                 # This is a folder for experiments
│   ├── Code_Jonas_CSSA_GDP.R
│   ├── explorer.Rmd
│   ├── Kalman_examples.R
│   ├── Kalman_procedures.R
│   ├── state_space_playground.Rmd
│   └── swiss_real_gdp.csv
├── README.html
├── README.md
├── scripts                             # Scripts that make up project
│   └── 01_load_data.R                  # API Calls and Data Loading
└── spacce-cadet.Rproj
```

## Contributing

### Documentation Generation
The `generate_doc.R` script facilitates the generation of documentation. **It is mandatory to write documentation for every function you may write.**
Below is an explanation of each step in the script and how it contributes to generating the documentation:

#### Overview of the script
After loading the necessary libaries, the following commands are run:

```
roxygen2::roxygenise()
```
This function automatically generates the documentation files in the `.Rd` format. It extracts structured comments from the code and creates corresponding man/ directory files and NAMESPACE.

Here is an introduction on how to write such comments: https://roxygen2.r-lib.org/ 

```
devtools::install()
```
This function installs the package in your local R library, allowing you to use the package's functions in your R sessions as you would with any other installed package.

```
pkgdown::build_site()
```

Using the pkgdown library we build a website for the package, making the documentation accessible and navigable in a web format (HTML). The generated files are stored in the docs/ directory.

#### How to Use the Script
After writing comments in the roxygen2 format, execute the `generate_doc.R` script by running the following command in your terminal from the root of the repo:

```
Rscript generate_doc.R
```

It should also in RStudio using `source("generate_doc.R")`, but it is untested.
If it works, you should receive the message:

`Documentation generation, package installation, and site build completed successfully.` 

# Contact

Jonas Bruno\
jonas.bruno\@unil.ch

Last Update 20.03.2026
