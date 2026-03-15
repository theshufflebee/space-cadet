# space-cadet

This repo contains all the code related to my Masters Thesis at CREA at HEC Lausanne

# Project Directory Structure

``` text
space-cadet/
├── data
│   └── raw
│       ├── cpi_series.xlsx
│       ├── employment_data.csv
│       ├── gov_bonds.csv
│       ├── gov_bonds_metadata.json
│       ├── kof_consensus_master.csv
│       ├── money_market.csv
│       ├── money_market_metadata.json
│       ├── snb_reer_manual_download.xlsx
│       └── unemployment_canton.csv
├── main.Rmd
├── R
│   ├── load_snb_data.R
│   └── utils.R
├── ram
│   ├── Code_Jonas_CSSA_GDP.R
│   ├── explorer.Rmd
│   ├── Kalman_examples.R
│   ├── Kalman_procedures.R
│   ├── state_space_playground.Rmd
│   └── swiss_real_gdp.csv
├── README.html
├── README.md
├── scripts
│   └── 01_load_data.R
└── spacce-cadet.Rproj
```

run fs::dir_tree()
