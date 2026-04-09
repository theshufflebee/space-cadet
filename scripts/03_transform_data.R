################################################################################
#
# This Script will transform the Data as model input
#
################################################################################



# --- General Transformations for Full dataset ---

# Transformation here are things such as HP filer.

library(xts)
library(neverhpfilter)


# I use hamilton filter on the full gdp Series to take care of NaN

master_df$unemp_rate <- master_df$unemployment / (master_df$unemployment + master_df$employment)
master_df$spf_5y_unemp <- master_df$`5y_unemp_forecast` / 100

gdp_df <- master_df %>%
  select(date, gdp) %>%
  arrange(date) %>%
  mutate(quarter = as.yearqtr(date)) %>%
  group_by(quarter) %>%
  summarise(gdp = mean(gdp, na.rm = TRUE))%>%
  mutate(log_gdp = log(gdp))


gdp_xts <- xts(
  gdp_df$log_gdp,
  order.by = as.yearqtr(gdp_df$quarter)
)




# --- Okuns Law Prep

master_quarterly <- master_df %>%
  arrange(date) %>%
  mutate(quarter = yearqtr(date)) %>%
  group_by(quarter) %>%
  summarise(
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# --- Specific Okuns Law model Transformations


master_okun_model_long <- master_quarterly %>%
  select(
    quarter,
    unemp_rate,
    employment,
    `spf_5y_unemp`,
    gdp
  ) %>%
  pivot_longer(
    cols = -quarter,
    names_to = "variable",
    values_to = "value"
  )




# =================================
# 1. DATA EXTRACTION FOR OKUN MODEL
# =================================

# Get Unemployment (Decimal)
Y_okun <- master_okun_model_long %>%
  filter(variable %in% c("unemp_rate", "spf_5y_unemp")) %>%
  pivot_wider(names_from = variable,
              values_from = value)%>%
  arrange(quarter)

X_okun <- master_okun_model_long %>%
  filter(variable %in% c("gdp")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(quarter) %>%
  mutate(log_gdp = log(gdp)) %>%
  select(quarter, log_gdp)


message("Okun Data Formatting Done")



