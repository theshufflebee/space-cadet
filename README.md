# space-cadet

This repo contains all the code related to my Masters Thesis at CREA at HEC Lausanne. The Thesis' main goal is to forecast important macroeconomic variables of the Swiss Economy with State space models.

## How to run

This is currently a work in Progress. The working parts of the project can be run through the main.R script in the repo root. As of 17.03.2026 only script 01 runs and it isn't complete yet.

## Libraries

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

# Contact

Jonas Bruno\
jonas.bruno\@unil.ch

Last Update 17.03.2026
