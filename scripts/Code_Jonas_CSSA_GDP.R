### ──────────────────────────────────────────────────────────────
### 4) TARGET REAL SECO VARIABLES (OFFICIAL SWISS NATIONAL DATA IN REAL TIME)
### ──────────────────────────────────────────────────────────────
url_csv <-
  "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"  # SECO GDP CSV URL
data <- read_csv(url_csv)                                                                   # load CSV from SECO website

data_seco_target <- data %>% 
  filter(structure %in% c("gdp","inv_constr","inv_fixed", "cons_priv","cons_gov",          # choose GDP components
                          "exp_good_ex_vm","exp_serv","imp_serv","imp_good_ex_v"),
         type == "real", seas_adj == "cssa")                                               # real, seasonally adjusted