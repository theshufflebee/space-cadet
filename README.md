# space-cadet

This repo contains all the code related to my Masters Thesis at CREA at HEC Lausanne. The Thesis' main goal is to forecast important macroeconomic variables of the Swiss Economy with State space models.

## How to run

This is currently a work in Progress. The working parts of the project can be run through the main.R script in the repo root. As of the Last Update (see at the bottom) it runs only until script 06 and it isn't complete yet. Open the .rproj file with R Studio and run the main script via source("main.R")

## Libraries

```         
R version 4.3.3
```

### Data Manipulation & File Handling

- **tidyverse**: A core suite of tools (including **dplyr**, **readr**, and **ggplot2**) designed for tidy data manipulation, file reading, and visualization.
- **readxl**: Facilitates the importation of data from Excel workbooks.
- **here**: Manages project-relative file paths to ensure reproducibility across different environments.

### API & Data Access

- **BFS**: Provides direct access to data from the Swiss Federal Statistical Office.
- **kofdata**: Specialized tool for accessing the Swiss KOF Economic Institute database.
- **RCurl**: Handles network connections and API calls.
- **jsonlite**: Parses JSON data structures returned by KOF and BFS APIs.
- **glue**: Provides string interpolation for constructing dynamic API URLs.

### Time Series & Seasonal Adjustment

- **zoo & xts**: Essential infrastructure for handling indexed time series and flexible quarterly date formats.
- **seasonal**: Interface for X-13ARIMA-SEATS, used for professional-grade seasonal adjustments of economic indicators.
- **mFilter**: Implementation of various time series filters, specifically the Hodrick-Prescott (HP) filter.
- **forecast**: Used for running benchmark forecasts, such as ARMA and ARIMA models.

### Modeling, Optimization & Evaluation

- **optimx**: A unified interface for numerical optimization algorithms used to estimate State-Space Model parameters.
- **MASS**: Supports complex matrix operations and inversions required within the Kalman Filter.
- **creaFcstEval**: An internal package specifically designed for forecast evaluation, including DM tests and error ratios.

### Visualization

- **ggplot2**: Part of the tidyverse, used for generating diagnostic and forecast plots.
- **patchwork**: A package for combining multiple ggplot objects into standardized, publication-quality layouts.

### Project Infrastructure & Workflow

- **argparse**: Provides Command Line Interface (CLI) functionality for executing scripts with external arguments.
- **roxygen2**: A system for documenting functions and generating standardized help files.
- **conflicted**: Explicitly manages namespace conflicts between packages (e.g., between `dplyr` and `stats`) to prevent silent function masking.

## Project Directory Structure

Tree created via fs::dir_tree()

``` text
space-cadet/
├── data/                                # Project datasets
│   ├── master.csv                       # Formatted master dataset
│   └── raw/                             # Untransformed source files
│       ├── cpi_series.xlsx              # Swiss CPI Data
│       ├── employment_data.csv          # Swiss Employment Data
│       ├── eu_ppi_raw.csv               # Euro Area PPI
│       ├── ex_eur_av.csv                # CHF/EUR Average Exchange Rates
│       ├── ex_eur_eom.csv               # CHF/EUR End-of-Month Exchange Rates
│       ├── gdp.csv                      # Swiss GDP Data
│       ├── kof_consensus_master.csv     # KOF Consensus Forecasts
│       ├── ppi_ch.csv                   # Swiss PPI Data
│       ├── reer_ppi_eu.csv              # REER CHF/EU Data
│       └── ...                          # Associated metadata (.json)
├── data.md                              # Documentation for data sources
├── DESCRIPTION                          # Project metadata and dependencies
├── docs/                                # pkgdown-generated html documentation
├── generate_doc.r                       # Script to generate project documentation
├── main.R                               # Primary execution script
├── man/                                 # R documentation files (.Rd) from Roxygen
├── NAMESPACE                            # R package namespace exports
├── output/                              # Model results and exports
│   ├── forecasts/                       # Forecasted series (Inflation/Unemployment)
│   └── parameter_estimation/            # Rolling estimation CSVs (Okun/Phillips)
├── R/                                   # Project function library
│   ├── build_model_matrices.R           # SSM Matrix factory functions (H, G, M, N)
│   ├── kalman_estimation_engine.R       # Optimizer wrappers and likelihood logic
│   ├── kalman_implementation.R          # Base Kalman Filter and Gain calculations
│   ├── load_kof_data.R                  # KOF API implementation
│   ├── load_snb_data.R                  # SNB API implementation
│   ├── model_input_preparation.R        # REER splicing and LOP/HP gap extraction
│   ├── ssm_forecasting.R                # Rolling forecast and prediction logic
│   ├── utils.R                          # General utility functions
│   └── visualizations.R                 # Functions for spaghetti and error plots
├── ram/                                 # Experimental space and scratchpad
├── README.md
├── scripts/                             # Ordered project pipeline
│   ├── 00a_install_dependencies.R       # Environment setup
│   ├── 00b_config.R                     # Global variables and burn-in settings
│   ├── 01_load_data.R                   # Data acquisition (API & Local)
│   ├── 02_format_data.R                 # Cleaning and joining
│   ├── 03_transform_data.R              # Manipulation and structural prep
│   ├── 04_unemployment_forecast.R       # Okun Model execution
│   ├── 05_inflation_forecast.R          # Phillips Model execution
│   ├── 06_snb_rate_forecast.R           # Interest rate projections
│   ├── 07_forecast_evaluation.R         # DM tests and MSFE/MAFE ratios
│   └── 08_visualizations.R              # Final plot generation
├── space-cadet.Rproj                    # RStudio project file
└── SSM_Documentation.Rmd                # Technical breakdown of the SSM logic
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

It can also be done in RStudio using `source("generate_doc.R")`.

# Contact

Jonas Bruno\
jonas.bruno\@unil.ch

Last Update 05.05.2026
