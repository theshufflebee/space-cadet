# space-cadet
This repo contains all the code related to my Masters Thesis at CREA at HEC Lausanne

# Project Directory Structure


├── data
│   └── raw
│       ├── cpi_series.xlsx
│       ├── employment_data.csv
│       ├── gov_bonds.csv
│       ├── gov_bonds_metadata.json
│       ├── kof_consensus_master.csv        # Contains all KOF data
│       ├── money_market.csv                # Contains 
│       ├── money_market_metadata.json      
│       ├── snb_reer_manual_download.xlsx   # Need to download properly
│       └── unemployment_canton.csv
├── R
│   ├── load_snb_data.R
│   └── utils.R
├── ram                                     # folder for experimenting
├── README.md
├── scripts                   
│   └── 01_load_data.R                      # Downloads and loads Project Data
└── spacce-cadet.Rproj



run fs::dir_tree()