




### GDP DATA VIKTOR
data_gdp <- data_gdp_raw %>% 
  dplyr::filter(
    structure %in% c("gdp","inv_constr","inv_fixed", "cons_priv","cons_gov",          # choose GDP components
                     "exp_good_ex_vm","exp_serv","imp_serv","imp_good_ex_v"),
    type == "real",
    seas_adj == "cssa",
    structure == "gdp"
    
  )  



####KOF DATA
# Second step of selecting data move away later
master_kof_consensus_forecast <- master_kof_consensus_forecast %>%
  select("ch.kof.consensus.q_qn_unemp_5y.mean",
         "ch.kof.consensus.q_qn_prices_5y.mean",
         "ch.kof.consensus.q_qn_3minterest_3m.mean",
         "ch.kof.consensus.q_qn_3minterest_12m.mean")

message("KOF Data ready for analysis.")