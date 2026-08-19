# space-cadet

This repo contains all the code related to my Masters Thesis at CREA at HEC Lausanne. The Thesis' main goal is to forecast important macroeconomic variables of the Swiss Economy with State Space Models.

## How to run

This is currently a work in Progress. The working parts of the project can be run through the main.R script in the repo root. As of the Last Update (see at the bottom) it runs the main.R file. Bugs may occure. Open the .rproj file with R Studio and run the main script via source("main.R")

Further there is the para_main.R in the para_functions folder which runs a parallel estimation of the parameters. It may require running a non estimation run of the main.R first. For that set the run_estimation = false in scripts/00b_config.R.

The argparse.R file in the working directory doesn't work anymore. Will be updated or deleted later.

#### Generate Plots and Tables

1.  **Configuration:**
    - In `config.R`, set the source folders to load parameter estimations from (e.g., `TARGET_FOLDER_OKUN`).
    - Specify the output destination: set `store_temp` to `TRUE` (temporary directory) or `FALSE` (persistent directory).
2.  **Execution:**
    - Run `source("main.R")`.

------------------------------------------------------------------------

#### Run Full Project in Parallel

1.  **Configuration & Trial Run:**
    - In `config.R`, verify the target destination directories (e.g., `TARGET_FOLDER_OKUN`, `TARGET_FOLDER_PHILLIPS`, `TARGET_FOLDER_TAYLOR`).
    - In `main_para_local.R`, enable the models you want to validate (`RUN_OKUN_MODEL`, `RUN_PHILLIPS_MODEL`, `RUN_TAYLOR_MODEL <- TRUE`).
    - Set `DO_TRIAL_RUN <- TRUE` to execute a single-vintage test run for validation.
2.  **Full Estimation:**
    - Set `DO_TRIAL_RUN <- FALSE`.
    - Enable models individually or simultaneously via the `RUN_*_MODEL` flags.
    - Run `source("main_para_local.R")`.

> **Approximate Runtime (Standard Laptop):** \* **Okun Model:** \~1 hour \* **Phillips Model:** \~2 hours \* **Taylor Model:** \~3 hours

3.  **Post-Processing:**
    - Once parallel estimation completes, run `source("main.R")` to generate the final plots and LaTeX tables.

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
- **future.apply**: Package to run parallel estimation in R

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
├── argparse.R # Depreciated
├── data
│   ├── master.csv
│   ├── okun_master.csv
│   ├── philips_master.csv
│   ├── raw
│   │   ├── cpi_series.xlsx
│   │   ├── employment_data.csv
│   │   ├── eu_ppi_raw.csv
│   │   ├── ex_eur_av.csv
│   │   ├── ex_eur_av_metadata.json
│   │   ├── ex_eur_eom.csv
│   │   ├── ex_eur_eom_metadata.json
│   │   ├── gdp.csv
│   │   ├── gov_bonds.csv
│   │   ├── gov_bonds_metadata.json
│   │   ├── kof_consensus_master.csv
│   │   ├── money_market.csv
│   │   ├── money_market_metadata.json
│   │   ├── ppi_ch.csv
│   │   ├── ppi_ch.xlsx
│   │   ├── reer_ppi_eu.csv
│   │   ├── reer_ppi_eu_metadata.json
│   │   ├── reer_snb.xlsx
│   │   ├── snb-chart-data-devwkieffirech-en-all-20260605_0900.xlsx
│   │   ├── snb_policy_rate.csv
│   │   ├── snb_policy_rate_metadata.json
│   │   └── unemployment_canton.csv
│   └── taylor_master.csv
├── data.md
├── DESCRIPTION
├── docs
│   └── ... # Package Documentation
├── generate_doc.r
├── main_cluster.R # To Run estimation on Cluster
├── main_para_local.R # Run Parallel Estimation on local machine
├── main.R # Run full Project non parallel on local Laptop
├── man
│   └── ... # .Rd files of documentation
├── NAMESPACE
├── output
│   ├── para
│   │   ├── example_folder_1 # Contains folders with different estimation runs
│   │   ├── example_folder_2 # Each folder contains multiple RDS files
│   │   └── example_folder_3 # One Rds File Per vintage
│   ├── persistent
│   │   ├── forecasts # Contains Forecasts dfs fr models and aux forecasts
│   │   │   ├── gdp_arima_forecasts.csv
│   │   │   ├── inflation_forecasts.csv
│   │   │   ├── inf_arima_forecasts.csv
│   │   │   ├── policy_rate_forecasts.csv
│   │   │   └── unemployment_forecasts.csv
│   │   ├── parameter_estimation
│   │   │   ├── okun_params.csv
│   │   │   ├── philips_params.csv
│   │   │   └── taylor_params.csv
│   │   ├── plots
│   │   │   ├── benchmark_spaghetti_okun_auto_arma.png
│   │   │   ├── benchmark_spaghetti_okun_rw.png
│   │   │   ├── benchmark_spaghetti_philips_ar1.png
│   │   │   ├── benchmark_spaghetti_philips_auto_arma.png
│   │   │   ├── benchmark_spaghetti_philips_rw.png
│   │   │   ├── benchmark_spaghetti_taylor_ar1.png
│   │   │   ├── benchmark_spaghetti_taylor_auto_arma.png
│   │   │   ├── benchmark_spaghetti_taylor_rw.png
│   │   │   ├── fit_okun_model.png
│   │   │   ├── fit_philips_model.png
│   │   │   ├── fit_taylor_model.png
│   │   │   ├── params_okun_model.png
│   │   │   ├── params_philips_model.png
│   │   │   ├── params_taylor_model.png
│   │   │   ├── spaghetti_okun.png
│   │   │   ├── spaghetti_philips.png
│   │   │   └── spaghetti_taylor.png
│   │   └── tables
│   │       ├── okun_eval_ar1_table.tex
│   │       ├── okun_eval_auto_arma_table.tex
│   │       ├── okun_eval_rw_table.tex
│   │       ├── philips_eval_ar1_table.tex
│   │       ├── philips_eval_auto_arma_table.tex
│   │       ├── philips_eval_rw_table.tex
│   │       ├── taylor_eval_ar1_table.tex
│   │       ├── taylor_eval_auto_arma_table.tex
│   │       └── taylor_eval_rw_table.tex
│   ├── temp
│   │   ├── forecasts
│   │   │   ├── gdp_arima_forecasts.csv
│   │   │   ├── inflation_forecasts.csv
│   │   │   ├── inf_arima_forecasts.csv
│   │   │   ├── policy_rate_forecasts.csv
│   │   │   ├── unemployment_forecasts.csv
│   │   │   └── unemployment_forecasts_copy_for_debugging.csv
│   │   ├── parameter_estimation
│   │   │   ├── okun_params.csv
│   │   │   ├── okun_params_long.csv
│   │   │   ├── okun_params_no_constraint_new_filter.csv
│   │   │   ├── philips_params.csv
│   │   │   └── taylor_params.csv
│   │   ├── plots
│   │   │   ├── benchmark_spaghetti_okun_ar1.png
│   │   │   ├── benchmark_spaghetti_okun_auto_arma.png
│   │   │   ├── benchmark_spaghetti_okun_rw.png
│   │   │   ├── benchmark_spaghetti_philips_ar1.png
│   │   │   ├── benchmark_spaghetti_philips_auto_arma.png
│   │   │   ├── benchmark_spaghetti_philips_rw.png
│   │   │   ├── benchmark_spaghetti_taylor_ar1.png
│   │   │   ├── benchmark_spaghetti_taylor_auto_arma.png
│   │   │   ├── benchmark_spaghetti_taylor_rw.png
│   │   │   ├── fit_okun_model.png
│   │   │   ├── fit_philips_model.png
│   │   │   ├── fit_taylor_model.png
│   │   │   ├── params_okun_model.png
│   │   │   ├── params_philips_model.png
│   │   │   ├── params_taylor_model.png
│   │   │   ├── spaghetti_okun.png
│   │   │   ├── spaghetti_philips.png
│   │   │   └── spaghetti_taylor.png
│   │   └── tables
│   │       ├── okun_eval_ar1_table.tex
│   │       ├── okun_eval_auto_arma_table.tex
│   │       ├── okun_eval_rw_table.tex
│   │       ├── okun_param_table.tex
│   │       ├── philips_eval_ar1_table.tex
│   │       ├── philips_eval_auto_arma_table.tex
│   │       ├── philips_eval_rw_table.tex
│   │       ├── philips_param_table.tex
│   │       ├── taylor_eval_ar1_table.tex
│   │       ├── taylor_eval_auto_arma_table.tex
│   │       ├── taylor_eval_rw_table.tex
│   │       └── taylor_param_table.tex
├── R
│   ├── 01_matrix_ssm_construction
│   │   ├── build_okun_matrices.R
│   │   ├── build_phillips_matrices.R
│   │   └── build_taylor_matrices.R
│   ├── 02_parallel
│   │   └── est_ssm_para.R  # functions for parallel estimation
│   ├── data_download.R
│   ├── forecasting_functions.R
│   ├── kalman_base.R
│   ├── kalman_filter_taylor.R
│   ├── latex_formatting.R
│   ├── model_input_preparation.R
│   ├── rolling_estimation.R
│   ├── utils.R
│   ├── visualizations.R
│   └── z99_archive
│       ├── 04_unemployment_forecast.R
│       ├── 05_inflation_forecast.R
│       ├── 06_snb_rate_forecast.R
│       ├── build_model_matrices.R
│       ├── kalman_estimation_engine.R
│       ├── kalman_implementation_base.R
│       ├── kalman_implementation_okun.R
│       ├── kalman_implementation_philips.R
│       ├── kalman_implementation_taylor.R
│       ├── kalman_okun_specs.R
│       ├── kalman_phillips_specs.R
│       ├── kalman_taylor_specs.R
│       └── security_copy_kalman_base.R
├── README.md
├── scripts
│   ├── 00a_install_dependencies.R
│   ├── 00b_config.R
│   ├── 01_load_data.R
│   ├── 02_format_data.R
│   ├── 03_transform_data.R
│   ├── 04_rolling_estimation.R
│   ├── 05_generate_forecasts.R
│   ├── 06_forecast_evaluation.R
│   └── 07_visualizations.R
├── space-cadet.Rproj
└── SSM_Documentation.Rmd
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

Last Update 09.07.2026
