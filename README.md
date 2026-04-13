# space-cadet

This repo contains all the code related to my Masters Thesis at CREA at HEC Lausanne. The Thesis' main goal is to forecast important macroeconomic variables of the Swiss Economy with State space models.

## How to run

This is currently a work in Progress. The working parts of the project can be run through the main.R script in the repo root. As of the Last Update (see at the bottom) it runs only until script 04 and it isn't complete yet. Open the .rproj file with R Studio and run the main script via source("main.R")

## Libraries

```         
R version 4.3.3
```

### Data Acquisition & API Connectivity

-   **`kofdata`**: Specialized interface for accessing KOF Swiss Economic Institute time series.
-   **`BFS`**: Interface for the Swiss Federal Statistical Office (BFS) open data API.
-   **`RCurl`**: General-purpose client for network-level API calls.
-   **`jsonlite`**: High-performance JSON parsing for API responses.
-   **`glue`**: Interpreted string interpolation, primarily used for dynamic API URL construction.

### Data Manipulation & File Handling

-   **`tidyverse`**: A core suite of tools (including **`dplyr`** and **`readr`**) designed for tidy data manipulation and visualization.
-   **`readxl`**: Importing data from Excel workbooks.
-   **`here`**: Manages project-relative file paths to ensure reproducibility across different machines.

### Time Series & Seasonal Adjustment

-   **`zoo`** & **`xts`**: Essential infrastructure for handling indexed time series and flexible date/time formats.
-   **`seasonal`**: Interface for X-13ARIMA-SEATS, used for professional-grade seasonal adjustments.
-   **`mFilter`**: Implementation of various time series filters, including the Hodrick-Prescott (HP) filter.
-   **`neverhpfilter`**: Provides an implementation of the Hamilton filter as a modern alternative to the HP filter.

### Modeling & Optimization

-   **`optimx`**: A unified interface for numerical optimization algorithms used to estimate the state-space model parameters.
-   **`MASS`**: Supports complex matrix operations, such as the inversions required within the Kalman Filter.

### Project Infrastructure & Workflow

-   **`argparse`**: Provides Command Line Interface (CLI) functionality for executing scripts with external arguments.
-   **`roxygen2`**: A system for documenting functions and generating standardized help files.
-   **`conflicted`**: Explicitly manages namespace conflicts between packages to prevent silent function masking. \## Project Directory Structure

Tree created via fs::dir_tree()

``` text
space-cadet/
├── data/                               # Contains all data
│   ├── master.csv                      # Formatted master dataset
│   └── raw/                            # Contains all untransformed data
│       ├── cpi_series.xlsx             # Swiss CPI Data
│       ├── employment_data.csv         # Swiss Employment Data
│       ├── gdp.csv                     # Swiss GDP Data
│       ├── gov_bonds.csv               # Swiss Gov Bond Data
│       ├── gov_bonds_metadata.json     # Swiss Gov Bond Metadata
│       ├── kof_consensus_master.csv    # KOF Consensus Forecasts Data
│       ├── money_market.csv            # SARON and LIBOR CHF Data
│       ├── money_market_metadata.json  # SARON and LIBOR CHF Metadata
│       ├── reer_ppi_eu.csv             # REER CHF/EU Data
│       ├── reer_ppi_eu_metadata.json   # REER CHF/EU Metadata
│       └── unemployment_canton.csv
├── data.md                             # Documentation for data sources
├── DESCRIPTION                         # Project metadata and dependencies
├── docs/                               # pkgdown-generated package documentation
├── generate_doc.r                      # Script to generate project documentation
├── main.R                              # Script to run the project
├── man/                                # R documentation files (.Rd)
│   ├── ...                             # R Documentations from Roxygen
│   └── figures/                        # Contains images used in documentation
├── NAMESPACE                           # R package namespace exports
├── output/                             # All outputs are stored here
│   ├── forecast_df.csv                 # Forecast matrix of Okun Model
│   └── okun_forecast_parameters.csv    # Rolling estimation parameters Okun Model
├── R/                                  # This folder contains all functions
│   ├── kalman_builder.R                # SSM matrix factory functions
│   ├── kalman_procedures.R             # Kalman Filter and Likelihood logic
│   ├── load_kof_data.R                 # KOF API implementation
│   ├── load_snb_data.R                 # SNB API Calls
│   ├── ssm_forecasting.R               # Prediction and rolling forecast logic
│   └── utils.R                         # Utility functions
├── ram/                                # Experimental Space: not relevant for proj
├── README.md
├── scripts/                            # Scripts that make up project
│   ├── 00a_install_dependencies.R      # Environment setup
│   ├── 00b_config.R                    # Project-wide configurations
│   ├── 01_load_data.R                  # API Calls and Data Loading
│   ├── 02_format_data.R                # Data cleaning and joining
│   ├── 03_transform_data.R             # Data Manipulation and prep for analysis
│   └── 04_unemployment_forecasts.R     # Forecasting first model
├── space-cadet.Rproj                   # RStudio project file
└── SSM_Documentation.Rmd               # Breakdown of the SSM model / Code
```

## Contributing

### Documentation Generation

The `generate_doc.R` script facilitates the generation of documentation. **It is mandatory to write documentation for every function you may write.** Below is an explanation of each step in the script and how it contributes to generating the documentation:

#### Overview of the script

After loading the necessary libaries, the following commands are run:

```         
roxygen2::roxygenise()
```

This function automatically generates the documentation files in the `.Rd` format. It extracts structured comments from the code and creates corresponding man/ directory files and NAMESPACE.

Here is an introduction on how to write such comments: <https://roxygen2.r-lib.org/>

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

It should also in RStudio using `source("generate_doc.R")`, but it is untested. If it works, you should receive the message:

`Documentation generation, package installation, and site build completed successfully.`

# Contact

Jonas Bruno\
jonas.bruno\@unil.ch

Last Update 13.04.2026
