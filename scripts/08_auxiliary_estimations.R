################################################################################
#
# AUXILIARY ESTIMATIONS & HISTORICAL DECOMPOSITIONS
#
################################################################################
message("Starting Auxiliary Estimation")
library(tidyverse)
library(zoo)
library(mFilter)
library(dynlm)
library(stargazer)
library(patchwork)

# Set global HP smoothing parameter for quarterly macro data
lambda_val <- 1600


# ==============================================================================
# 1. OKUN MODEL AUXILIARY ESTIMATION & DECOMPOSITION
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Data Preparation & HP Decomposition
# ------------------------------------------------------------------------------
df_okun <- master_quarterly %>%
  arrange(quarter) %>%
  filter(quarter >= as.yearqtr("1990 Q1"))

# Run HP Filter
hp_unemp <- mFilter::hpfilter(df_okun$unemp_rate, freq = 10000)
hp_gdp   <- mFilter::hpfilter(df_okun$log_gdp,    freq = lambda_val)

df_okun <- df_okun %>%
  mutate(
    unemp_trend = as.numeric(hp_unemp$trend),
    u_cyclical  = as.numeric(hp_unemp$cycle),
    gdp_trend   = as.numeric(hp_gdp$trend),
    gdp_gap     = as.numeric(hp_gdp$cycle) * 100  # Percentage points
  )

var_unemp_trend <- var(df_okun$unemp_trend, na.rm = TRUE)
var_unemp_cycle <- var(df_okun$u_cyclical, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 1.2 Fit Dynamic Okun Regression Model (with Lagged Unemployment Gap)
# ------------------------------------------------------------------------------
start_year_okun <- as.numeric(format(min(df_okun$quarter), "%Y"))
start_qtr_okun  <- as.numeric(format(min(df_okun$quarter), "%q"))

ts_okun <- ts(
  data  = df_okun %>% select(u_cyclical, gdp_gap),
  start = c(start_year_okun, start_qtr_okun),
  freq  = 4
)

# Added L(u_cyclical, 1) to capture unemployment gap persistence
okun_reg <- dynlm(u_cyclical ~ L(u_cyclical, 1) + gdp_gap + L(gdp_gap, 1) + L(gdp_gap, 2), data = ts_okun)

# ------------------------------------------------------------------------------
# 1.3 Export LaTeX Table
# ------------------------------------------------------------------------------

okun_tex <- capture.output(
  stargazer(
    okun_reg,
    type = "latex",
    title = "Auxiliary Regression: Dynamic Okun's Law Model",
    covariate.labels = c(
      "Lagged Unemp. Gap ($\\tilde{u}_{t-1}$)", 
      "Output Gap ($\\tilde{y_t}$)", 
      "Output Gap ($\\tilde{y}_{t-1}$)", 
      "Output Gap ($\\tilde{y}_{t-2}$)"
    ),
    dep.var.labels = "Unemployment Gap ($\\tilde{u}_t$)",
    add.lines = list(
      c("HP Filter Smoothing Parameter ($\\lambda$)", "10,000"),
      c("Var($\\bar{u})", sprintf("%.4f", var_unemp_trend)),
      c("Var($\\tilde{u})", sprintf("%.4f", var_unemp_cycle))
    ),
    notes = "Standard errors in parentheses.",
    align = TRUE,
    no.space = TRUE
  )
)

writeLines(okun_tex, con = output_save_paths$aux_results$aux_reg_okun)


# ------------------------------------------------------------------------------
# 1.4 Plot & Save Okun HP Decomposition
# ------------------------------------------------------------------------------
plot_okun_df <- df_okun %>% mutate(date = as.Date(quarter))

p1_okun <- ggplot(plot_okun_df, aes(x = date)) +
  geom_line(aes(y = unemp_rate, color = "Actual Unemployment"), linewidth = 0.8) +
  geom_line(aes(y = unemp_trend, color = "HP Trend (\\lambda = 10,000)"), linewidth = 1.1, linetype = "dashed") +
  scale_color_manual(
    values = c("Actual Unemployment" = "#2c3e50", "HP Trend (\\lambda = 10,000)" = "#e74c3c")
  ) +
  labs(
    title = "Hodrick-Prescott Filter Decomposition: Unemployment Rate",
    y = "Percent (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

p2_okun <- ggplot(plot_okun_df, aes(x = date, y = u_cyclical)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
  geom_area(fill = "#3498db", alpha = 0.3) +
  geom_line(color = "#2980b9", linewidth = 0.8) +
  labs(
    subtitle = "Cyclical Component (u^c)",
    x = "Quarter",
    y = "Percentage Points"
  ) +
  theme_minimal(base_size = 12)

combined_plot_okun <- p1_okun / p2_okun + plot_layout(heights = c(2, 1))

ggsave(
  filename = output_save_paths$aux_results$aux_hp_plot_okun,
  plot = combined_plot_okun,
  width = 9,
  height = 6,
  units = "in"
)

# ------------------------------------------------------------------------------
# 1.5 Historical Contribution Decomposition Plot (Okun - SAFE VECTOR DECOUPLING)
# ------------------------------------------------------------------------------

# 1. Get exact observations used in fitted model
okun_mf <- model.frame(okun_reg)
n_obs   <- nrow(okun_mf)

# Match dates directly from df_okun by taking the last n_obs quarters 
reg_dates_okun <- tail(as.Date(df_okun$quarter), n_obs)

# 2. Extract values as pure numeric vectors
coefs_okun        <- as.vector(coef(okun_reg))
names(coefs_okun) <- names(coef(okun_reg))

u_cyclical_act <- c(okun_mf[, 1])
u_cyclical_lag1<- c(okun_mf[, 2])
y_gap_0        <- c(okun_mf[, 3])
y_gap_1        <- c(okun_mf[, 4])
y_gap_2        <- c(okun_mf[, 5])
resids_okun    <- c(residuals(okun_reg))

# 3. Construct clean data.frame (no ts metadata)
decomp_okun_df <- data.frame(
  date              = reg_dates_okun,
  actual_u_c        = u_cyclical_act,
  contrib_intercept = coefs_okun["(Intercept)"],
  contrib_u_lag1    = coefs_okun["L(u_cyclical, 1)"] * u_cyclical_lag1,
  contrib_gdp_gap0  = coefs_okun["gdp_gap"]           * y_gap_0,
  contrib_gdp_gap1  = coefs_okun["L(gdp_gap, 1)"]     * y_gap_1,
  contrib_gdp_gap2  = coefs_okun["L(gdp_gap, 2)"]     * y_gap_2,
  contrib_residual  = resids_okun,
  stringsAsFactors  = FALSE
)

# 4. Pivot into long format for ggplot
decomp_okun_long <- decomp_okun_df %>%
  select(date, actual_u_c, starts_with("contrib_")) %>%
  pivot_longer(
    cols = starts_with("contrib_"),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c(
        "contrib_intercept", 
        "contrib_u_lag1", 
        "contrib_gdp_gap0", 
        "contrib_gdp_gap1", 
        "contrib_gdp_gap2", 
        "contrib_residual"
      ),
      labels = c(
        "Intercept", 
        "Lagged Unemp. Gap (t-1)", 
        "Output Gap (t)", 
        "Output Gap (t-1)", 
        "Output Gap (t-2)", 
        "Unexplained Residual"
      )
    )
  )

okun_component_colors <- c(
  "Intercept"               = "#7f8c8d",
  "Lagged Unemp. Gap (t-1)" = "#8e44ad",
  "Output Gap (t)"          = "#2980b9",
  "Output Gap (t-1)"        = "#3498db",
  "Output Gap (t-2)"        = "#85c1e9",
  "Unexplained Residual"    = "#bdc3c7"
)

# 5. Build ggplot
p_decomp_okun <- ggplot() +
  geom_col(
    data = decomp_okun_long,
    aes(x = date, y = contribution, fill = component),
    width = 60,
    alpha = 0.85
  ) +
  geom_line(
    data = decomp_okun_df,
    aes(x = date, y = actual_u_c, color = "Actual Cyclical Unemployment (u^c)"),
    linewidth = 0.9
  ) +
  scale_fill_manual(values = okun_component_colors) +
  scale_color_manual(values = c("Actual Cyclical Unemployment (u^c)" = "#111111")) +
  scale_x_date(
    date_breaks = "5 years", 
    date_labels = "%Y",
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Historical Contribution Decomposition of Cyclical Unemployment",
    subtitle = "Additive breakdown based on Dynamic Okun's Law regression estimates",
    x = "Year",
    y = "Percentage Points",
    fill = "Model Component",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(order = 1)
  )

# 6. Save Plot
ggsave(
  filename = output_save_paths$aux_results$aux_plot_okun,
  plot = p_decomp_okun,
  width = 10,
  height = 6,
  units = "in"
)

message("Finished Auxiliary Okun Estimation")

# ==============================================================================
# 2. OPEN-ECONOMY PHILLIPS CURVE AUXILIARY ESTIMATION & DECOMPOSITION
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Data Preparation & HP Decomposition
# ------------------------------------------------------------------------------
df_inf <- master_quarterly %>%
  arrange(quarter) %>%
  filter(quarter >= as.yearqtr("1983 Q1")) %>%
  filter(!is.na(reer_eu_ppi)) %>%
  mutate(
    q_inf   = log_inflation_diff,
    inf_gap = q_inf - 1,
    log_gdp = log(gdp)
  )

hp_gdp_inf  <- mFilter::hpfilter(df_inf$log_gdp, freq = lambda_val)
hp_reer_inf <- mFilter::hpfilter(df_inf$reer_eu_ppi, freq = lambda_val)

df_inf <- df_inf %>%
  mutate(
    gdp_gap = as.numeric(hp_gdp_inf$cycle * 100),
    lop_gap = as.numeric(hp_reer_inf$cycle)
  )

# ------------------------------------------------------------------------------
# 2.2 Fit Dynamic Phillips Curve Regression
# ------------------------------------------------------------------------------
start_year_inf <- as.numeric(format(min(df_inf$quarter), "%Y"))
start_qtr_inf  <- as.numeric(format(min(df_inf$quarter), "%q"))

ts_state_space <- ts(
  data = df_inf %>% select(q_inf, inf_gap, gdp_gap, lop_gap),
  start = c(start_year_inf, start_qtr_inf),
  freq  = 4
)

ss_aux_reg <- dynlm(
  inf_gap ~ L(inf_gap, 1) + gdp_gap + lop_gap,
  data = ts_state_space
)

# ------------------------------------------------------------------------------
# 2.3 Export LaTeX Table
# ------------------------------------------------------------------------------

phillips_tex <- capture.output(
  stargazer(
    ss_aux_reg,
    type = "latex",
    title = sprintf("Phillips Curve Auxiliary Regression"),
    covariate.labels = c(
      "Inf. Gap ($ \\tilde{\\pi_{t-1}} $)",
      "Output Gap ($ \tilde{y_t} $)",
      "LOP Gap ($lop_t^{gap} $)"
    ),
    dep.var.labels = "Inflation Gap($\\tilde{\\pi}_t = \\pi_t -1 $)",
    add.lines = list(
      c("HP Filter ($\\lambda$) for independent Var:", as.character(lambda_val))
    ),
    notes = "Standard errors in parentheses.",
    align = TRUE,
    no.space = TRUE
  )
)

writeLines(phillips_tex, con = output_save_paths$aux_results$aux_reg_phillips)

# ------------------------------------------------------------------------------
# 2.4 Plot & Save Inflation HP Decomposition (Phillips Model)
# ------------------------------------------------------------------------------
lambda_structural <- 10000
hp_inf_plot <- mFilter::hpfilter(df_inf$q_inf, freq = lambda_structural)

plot_inf_df <- df_inf %>%
  mutate(
    date         = as.Date(quarter),
    inf_trend    = as.numeric(hp_inf_plot$trend),
    inf_cyclical = as.numeric(hp_inf_plot$cycle)
  )

p1_inf <- ggplot(plot_inf_df, aes(x = date)) +
  geom_line(aes(y = q_inf, color = "Actual Quarterly Inflation"), linewidth = 0.8) +
  geom_line(
    aes(y = inf_trend, color = sprintf("HP Trend (\\lambda = %d)", lambda_structural)), 
    linewidth = 1.1, 
    linetype = "dashed"
  ) +
  scale_color_manual(
    values = setNames(
      c("#2c3e50", "#e74c3c"), 
      c("Actual Quarterly Inflation", sprintf("HP Trend (\\lambda = %d)", lambda_structural))
    )
  ) +
  labs(
    title = "Hodrick-Prescott Filter Decomposition: Quarterly Inflation",
    y = "Percent (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

p2_inf <- ggplot(plot_inf_df, aes(x = date, y = inf_cyclical)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
  geom_area(fill = "#e67e22", alpha = 0.3) +
  geom_line(color = "#d35400", linewidth = 0.8) +
  labs(
    subtitle = "Cyclical Component (\\pi^c)",
    x = "Quarter",
    y = "Percentage Points"
  ) +
  theme_minimal(base_size = 12)

combined_plot_inf <- p1_inf / p2_inf + plot_layout(heights = c(2, 1))

ggsave(
  filename = output_save_paths$aux_results$aux_hp_plot_phillips,
  plot = combined_plot_inf,
  width = 9,
  height = 6,
  units = "in"
)

# ------------------------------------------------------------------------------
# 2.5 Inflation Contribution Decomposition Plot (Phillips Curve - SAFE VECTOR DECOUPLING)
# ------------------------------------------------------------------------------
pc_mf    <- model.frame(ss_aux_reg)
n_obs_pc <- nrow(pc_mf)

reg_dates_pc <- tail(as.Date(df_inf$quarter), n_obs_pc)

coefs_pc        <- as.vector(coef(ss_aux_reg))
names(coefs_pc) <- names(coef(ss_aux_reg))

q_inf_act   <- c(pc_mf[, 1])
inf_lag1    <- c(pc_mf[, 2])
gdp_gap_pc  <- c(pc_mf[, 3])
lop_gap_pc  <- c(pc_mf[, 4])
resids_pc   <- c(residuals(ss_aux_reg))

decomp_pc_df <- data.frame(
  date              = reg_dates_pc,
  actual_inf        = q_inf_act,
  contrib_intercept = coefs_pc["(Intercept)"],
  contrib_inf_lag   = coefs_pc["L(inf_gap, 1)"] * inf_lag1,
  contrib_gdp_gap   = coefs_pc["gdp_gap"]        * gdp_gap_pc,
  contrib_lop_gap   = coefs_pc["lop_gap"]        * lop_gap_pc,
  contrib_residual  = resids_pc,
  stringsAsFactors  = FALSE
)

decomp_pc_long <- decomp_pc_df %>%
  select(date, actual_inf, starts_with("contrib_")) %>%
  pivot_longer(
    cols = starts_with("contrib_"),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c("contrib_intercept", "contrib_inf_lag", "contrib_gdp_gap", "contrib_lop_gap", "contrib_residual"),
      labels = c("Intercept (Base)", "Inflation Persistence (t-1)", "Output Gap Contribution", "LOP Gap Contribution", "Unexplained Residual")
    )
  )

pc_component_colors <- c(
  "Intercept (Base)"            = "#7f8c8d",
  "Inflation Persistence (t-1)" = "#e67e22",
  "Output Gap Contribution"     = "#2ecc71",
  "LOP Gap Contribution"        = "#9b59b6",
  "Unexplained Residual"        = "#bdc3c7"
)

p_decomp_phillips <- ggplot() +
  geom_col(
    data = decomp_pc_long,
    aes(x = date, y = contribution, fill = component),
    width = 60,
    alpha = 0.85
  ) +
  geom_line(
    data = decomp_pc_df,
    aes(x = date, y = actual_inf, color = "Actual Quarterly Inflation"),
    linewidth = 0.9
  ) +
  scale_fill_manual(values = pc_component_colors) +
  scale_color_manual(values = c("Actual Quarterly Inflation" = "#111111")) +
  scale_x_date(
    date_breaks = "5 years", 
    date_labels = "%Y",
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Historical Contribution Decomposition of Quarterly Inflation",
    subtitle = "Additive breakdown based on open-economy Phillips curve estimates",
    x = "Year",
    y = "Quarterly Inflation Rate (%)",
    fill = "Model Component",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(order = 1)
  )

ggsave(
  filename = output_save_paths$aux_results$aux_plot_phillips,
  plot = p_decomp_phillips,
  width = 10,
  height = 6,
  units = "in"
)
message("Finished Auxiliary Phillips Estimation")


# ==============================================================================
# 3. PARTIAL-ADJUSTMENT TAYLOR RULE AUXILIARY MODEL
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Data Preparation & HP Decomposition
# ------------------------------------------------------------------------------
df_taylor <- master_quarterly %>%
  arrange(quarter) %>%
  filter(quarter >= as.yearqtr("1983 Q1")) %>%
  mutate(
    q_inf         = log_inflation_diff,
    inf_gap       = q_inf - 1,
    log_gdp       = log(gdp),
    interest_rate = true_snb_rate
  ) %>%
  filter(!is.na(interest_rate))

hp_gdp_tr <- mFilter::hpfilter(df_taylor$log_gdp,       freq = lambda_val)
hp_i_tr   <- mFilter::hpfilter(df_taylor$interest_rate, freq = lambda_val)

df_taylor <- df_taylor %>%
  mutate(
    gdp_gap = as.numeric(hp_gdp_tr$cycle * 100),
    i_trend = as.numeric(hp_i_tr$trend),
    i_gap   = as.numeric(hp_i_tr$cycle)  # HP Cyclical Interest Rate (i^c)
  )

var_i_trend <- var(df_taylor$i_trend, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 3.2 Fit Constrained Taylor Rule Model & Extract Parameters
# ------------------------------------------------------------------------------
start_year_tr <- as.numeric(format(min(df_taylor$quarter), "%Y"))
start_qtr_tr  <- as.numeric(format(min(df_taylor$quarter), "%q"))

ts_taylor <- ts(
  data = df_taylor %>% select(interest_rate, i_trend, i_gap, inf_gap, gdp_gap),
  start = c(start_year_tr, start_qtr_tr),
  freq  = 4
)

taylor_reg <- dynlm(
  i_gap ~ L(i_gap, 1) + gdp_gap + inf_gap - 1,
  data = ts_taylor
)

coefs_tr <- coef(taylor_reg)

# 1. Extract short-run coefficients
phi <- coefs_tr["L(i_gap, 1)"]
c_1 <- coefs_tr["gdp_gap"]
c_2 <- coefs_tr["inf_gap"]

# 2. Recover long-run structural feedback parameters: gamma_k = c_k / (1 - phi)
gamma_1 <- c_1 / (1 - phi)
gamma_2 <- c_2 / (1 - phi)

# ------------------------------------------------------------------------------
# 3.3 Export LaTeX Table
# ------------------------------------------------------------------------------

taylor_tex <- capture.output(
  stargazer(
    taylor_reg,
    type = "latex",
    title = sprintf("Taylor Rule Auxiliary Regression"),
    covariate.labels = c(
      "Lagged Cyclical Policy Rate ($ \\tilde{i}_{t-1} $)",
      "Output Gap ($ \\tilde{y}_t $)",
      "Inflation Gap ($ \\tilde{\\pi_t} $)"
    ),
    dep.var.labels = "Cyclical Policy Rate ($ \\tilde{i}_{t} = ($i_t - \\bar{i}_{t}$)",
    add.lines = list(
      c("HP Filter Smoothing Parameter ($\\lambda$)", as.character(lambda_val)),
      c("Var(Interest Rate Trend)", sprintf("%.6f", var_i_trend)),
      c("---", "---"),
      c("Implied Interest Rate Smoothing ($\\hat{\\phi}$)", sprintf("%.4f", phi)),
      c("Implied Trend Weight ($1 - \\hat{\\phi}$)", sprintf("%.4f", 1 - phi)),
      c("Implied Output Gap Reaction ($\\hat{\\gamma}_1$)", sprintf("%.4f", gamma_1)),
      c("Implied Inf Gap Reaction ($\\hat{\\gamma}_2$)", sprintf("%.4f", gamma_2))
    ),
    notes = "Standard errors in parentheses.",
    align = TRUE,
    no.space = TRUE
  )
)

# Direct export using writeLines
writeLines(taylor_tex, con = output_save_paths$aux_results$aux_reg_taylor)

# ------------------------------------------------------------------------------
# 3.4 Historical Contribution Decomposition Plot (Taylor Rule - CYCLICAL ONLY)
# ------------------------------------------------------------------------------
taylor_mf <- model.frame(taylor_reg)
n_obs_tr  <- nrow(taylor_mf)

# Align dates cleanly
reg_dates_tr <- tail(as.Date(df_taylor$quarter), n_obs_tr)

# Model matrix column extraction:
# Col 1: (interest_rate - L(i_trend, 1)) [Cyclical Policy Rate / Deviation]
# Col 2: L(i_gap, 1)                      [Lagged Cyclical Policy Rate]
# Col 3: gdp_gap                          [Output gap]
# Col 4: inf_gap                          [Inflation gap]
i_cycle_act <- c(taylor_mf[, 1])
i_gap_lag   <- c(taylor_mf[, 2])
y_gap       <- c(taylor_mf[, 3])
pi_gap      <- c(taylor_mf[, 4])
resids_tr   <- c(residuals(taylor_reg))

decomp_tr_df <- data.frame(
  date              = reg_dates_tr,
  actual_i_cycle    = i_cycle_act,
  i_target_cycle    = (gamma_1 * y_gap) + (gamma_2 * pi_gap),
  contrib_smoothing = phi * i_gap_lag,
  contrib_gdp_gap   = (1 - phi) * gamma_1 * y_gap,
  contrib_inf_gap   = (1 - phi) * gamma_2 * pi_gap,
  contrib_residual  = resids_tr,
  stringsAsFactors  = FALSE
)

decomp_tr_long <- decomp_tr_df %>%
  select(date, actual_i_cycle, starts_with("contrib_")) %>%
  pivot_longer(
    cols = starts_with("contrib_"),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c("contrib_smoothing", "contrib_inf_gap", "contrib_gdp_gap", "contrib_residual"),
      labels = c(
        "Cyclical Inertia: phi * i^c(t-1)", 
        "Inflation Gap Reaction: (1-phi)*gamma2 * pi_gap", 
        "Output Gap Reaction: (1-phi)*gamma1 * y_gap", 
        "Unexplained Shock / Residual"
      )
    )
  )

tr_colors <- c(
  "Cyclical Inertia: phi * i^c(t-1)"              = "#2980b9",
  "Inflation Gap Reaction: (1-phi)*gamma2 * pi_gap" = "#e67e22",
  "Output Gap Reaction: (1-phi)*gamma1 * y_gap"    = "#2ecc71",
  "Unexplained Shock / Residual"                  = "#bdc3c7"
)

p_tr_decomp <- ggplot() +
  geom_col(
    data = decomp_tr_long,
    aes(x = date, y = contribution, fill = component),
    width = 60,
    alpha = 0.85
  ) +
  geom_line(
    data = decomp_tr_df,
    aes(x = date, y = actual_i_cycle, color = "Actual Cyclical Rate (i_c)"),
    linewidth = 0.9
  ) +
  scale_fill_manual(values = tr_colors) +
  scale_color_manual(
    values = c("Actual Cyclical Rate (i_c)" = "#111111", "Implied Target Gap (i_c*)" = "#e74c3c")
  ) +
  scale_x_date(
    date_breaks = "5 years", 
    date_labels = "%Y",
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Cyclical Taylor Rule Historical Decomposition",
    x = "Year",
    y = "Percentage Points Deviation",
    fill = "Cyclical Component",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(order = 1)
  )

ggsave(
  filename = output_save_paths$aux_results$aux_plot_taylor,
  plot = p_tr_decomp,
  width = 10,
  height = 6,
  units = "in"
)

# ------------------------------------------------------------------------------
# 3.5 Plot & Save Policy Rate HP Decomposition (Taylor Rule Model)
# ------------------------------------------------------------------------------

# 1. Compute HP Filter for Policy Interest Rate
hp_taylor_plot <- mFilter::hpfilter(df_taylor$interest_rate, freq = lambda_val)

plot_taylor_df <- df_taylor %>%
  mutate(
    date        = as.Date(quarter),
    i_trend_plot = as.numeric(hp_taylor_plot$trend),
    i_cycle_plot = as.numeric(hp_taylor_plot$cycle)
  )

# 2. Panel 1: Actual Policy Rate vs. HP Trend
p1_taylor <- ggplot(plot_taylor_df, aes(x = date)) +
  geom_line(aes(y = interest_rate, color = "Actual Policy Rate"), linewidth = 0.8) +
  geom_line(
    aes(y = i_trend_plot, color = sprintf("HP Trend (\\lambda = %d)", lambda_val)), 
    linewidth = 1.1, 
    linetype = "dashed"
  ) +
  scale_color_manual(
    values = setNames(
      c("#2c3e50", "#e74c3c"), 
      c("Actual Policy Rate", sprintf("HP Trend (\\lambda = %d)", lambda_val))
    )
  ) +
  labs(
    title = "Hodrick-Prescott Filter Decomposition: Policy Interest Rate",
    y = "Percent (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

# 3. Panel 2: Cyclical Component (Interest Rate Gap)
p2_taylor <- ggplot(plot_taylor_df, aes(x = date, y = i_cycle_plot)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dotted") +
  geom_area(fill = "#16a085", alpha = 0.3) +
  geom_line(color = "#117a65", linewidth = 0.8) +
  labs(
    subtitle = "Cyclical Component (i^c)",
    x = "Quarter",
    y = "Percentage Points"
  ) +
  theme_minimal(base_size = 12)

# 4. Combine Panels & Save
combined_plot_taylor <- p1_taylor / p2_taylor + plot_layout(heights = c(2, 1))

# Export plot
ggsave(
  filename = output_save_paths$aux_results$aux_hp_plot_taylor,
  plot = combined_plot_taylor,
  width = 9,
  height = 6,
  units = "in"
)

message("Finished Auxiliary Taylor Estimation")
message("[SUCCESS] Finished all Auxiliary Estimations")
