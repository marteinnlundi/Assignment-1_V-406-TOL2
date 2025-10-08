# ----- 0) Libraries -----
need <- c("ggplot2","dplyr","tidyr","readxl","stringr","forcats","purrr","tibble",
          "janitor","skimr","broom","car","performance","lmtest","sandwich","ggrepel","rlang")
miss <- setdiff(need, rownames(installed.packages()))
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

library(ggplot2); library(dplyr); library(tidyr); library(readxl); library(stringr)
library(forcats); library(purrr); library(tibble); library(janitor); library(skimr)
library(broom); library(car); library(performance); library(lmtest); library(sandwich)
library(ggrepel); library(rlang)

# ----- 1) Import & clean -----
raw <- read_excel("data_group7.xlsx") |> clean_names()
skim(raw)

ff <- raw |>
  mutate(
    region1     = factor(region1),
    region2     = factor(region2, levels = c("Other","Capital Region")),
    urban       = factor(urban, levels = c("No","Yes")),
    branch_size = factor(branch_size, levels = c("Medium","Small","Large"))
  )

# ----- 2) Feature engineering -----
ff <- ff |>
  mutate(
    turnover_rate1 = staff_turnover1 / staff_fte,
    turnover_rate2 = staff_turnover2 / staff_fte,
    log_profits    = log(profits),
    log_area_pop   = log(area_pop),
    fte_per_staff  = staff_fte / staff_no
  )

print(summary(select(ff, profits, log_profits, area_pop, log_area_pop, starts_with("turnover_rate"))))

if (!dir.exists("figs")) dir.create("figs")

# ----- 3) Plots for the report -----
# 3a) Distributions
g1 <- ggplot(ff, aes(profits)) + geom_histogram(bins = 10) +
  labs(title = "Distribution of Profits (1,000 ISK)", x = "Profits (1,000 ISK)", y = "Count")
ggsave("figs/hist_profits.png", g1, width = 6, height = 4, dpi = 300)

g2 <- ggplot(ff, aes(log_profits)) + geom_histogram(bins = 10) +
  labs(title = "Distribution of log(Profits)", x = "log(Profits)", y = "Count")
ggsave("figs/hist_log_profits.png", g2, width = 6, height = 4, dpi = 300)

# 3b) Boxplots by categorical vars
g3 <- ggplot(ff, aes(branch_size, profits)) + geom_boxplot() +
  labs(title = "Profits by Branch Size", x = "Branch Size", y = "Profits (1,000 ISK)")
ggsave("figs/box_profits_branch_size.png", g3, width = 6, height = 4, dpi = 300)

g4 <- ggplot(ff, aes(urban, profits)) + geom_boxplot() +
  labs(title = "Profits by Urbanicity", x = "Urban", y = "Profits (1,000 ISK)")
ggsave("figs/box_profits_urban.png", g4, width = 6, height = 4, dpi = 300)

g5 <- ggplot(ff, aes(region2, profits)) + geom_boxplot() +
  labs(title = "Profits by Region 2", x = "Region 2", y = "Profits (1,000 ISK)")
ggsave("figs/box_profits_region2.png", g5, width = 6, height = 4, dpi = 300)

# 3c) Key scatterplots with loess smoother  (FIX: replace aes_string with tidy-eval)
scatter_var <- function(xvar, xlab = xvar) {
  ggplot(ff, aes(x = .data[[xvar]], y = profits)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(title = paste("Profits vs", xlab), x = xlab, y = "Profits (1,000 ISK)")
}
g6 <- scatter_var("staff_fte", "Staff FTE"); ggsave("figs/scatter_profits_staff_fte.png", g6, width=6, height=4, dpi=300)
g7 <- scatter_var("turnover_rate2", "Turnover Rate 2"); ggsave("figs/scatter_profits_turnover_rate2.png", g7, width=6, height=4, dpi=300)
g8 <- scatter_var("area_pop", "Area Population"); ggsave("figs/scatter_profits_area_pop.png", g8, width=6, height=4, dpi=300)

# 3d) Correlation heatmap (no extra packages)
num <- select(ff, where(is.numeric))
cm  <- round(cor(num), 2)
cm_df <- as.data.frame(cm) |> rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r")
g9 <- ggplot(cm_df, aes(var1, var2, fill = r)) +
  geom_tile() + geom_text(aes(label = r), size = 3) +
  scale_fill_gradient2(name = "r", limits = c(-1,1)) +
  labs(title = "Correlation Heatmap (numeric variables)", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figs/corr_heatmap.png", g9, width = 7, height = 6, dpi = 300)

# ----- 4) Candidate models (parsimonious) -----
m1 <- lm(log_profits ~ log_area_pop + staff_fte + turnover_rate2 +
           staff_mean_experience2 + manager_experience +
           branch_size + urban + region2, data = ff)

m2 <- lm(log_profits ~ log_area_pop + staff_fte + turnover_rate2 +
           staff_mean_experience2 + branch_size + urban, data = ff)

# Interaction on experience as a candidate (optional)
m3 <- lm(log_profits ~ log_area_pop + staff_fte + turnover_rate2 +
           staff_mean_experience2*manager_experience + branch_size + urban, data = ff)

AIC_values <- c(m1 = AIC(m1), m2 = AIC(m2), m3 = AIC(m3))
print(AIC_values)
best <- m2  # pick parsimonious by default

# ---- Model-selection trail (AIC, BIC, adj-R2) ----
sel_tbl <- tibble(
  model = c("m1_full","m2_parsimonious","m3_int_exp"),
  AIC   = c(AIC(m1), AIC(m2), AIC(m3)),
  BIC   = c(BIC(m1), BIC(m2), BIC(m3)),
  adjR2 = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared)
)
write.csv(sel_tbl, "model_selection_summary.csv", row.names = FALSE)


# ----- 5) Diagnostics -----
png("figs/diagnostic_plots.png", width = 1400, height = 1000, res = 150)
par(mfrow = c(2,2)); plot(best); par(mfrow = c(1,1))
dev.off()

# Save Q–Q only (nice to reference)
png("figs/qq_plot_studentized.png", width = 700, height = 500, res = 120)
qqPlot(best, simulate = TRUE, main = "Q–Q Plot (Studentized Residuals)")
dev.off()

# Combined checks (will print to console)
print(performance::check_model(best))

# Multicollinearity
vif_vals <- car::vif(best)
print(vif_vals)

if (is.matrix(vif_vals)) {
  vif_tbl <- as.data.frame(vif_vals) |>
    tibble::rownames_to_column("term")
  names(vif_tbl) <- c("term","GVIF","Df","GVIF_adj")
} else {
  vif_tbl <- data.frame(
    term = names(vif_vals),
    VIF  = as.numeric(vif_vals),
    row.names = NULL
  )
}
write.csv(vif_tbl, "vif_table.csv", row.names = FALSE)

# Heteroskedasticity
bp <- lmtest::bptest(best); print(bp)

# Robust (HC3) SEs + 95% CIs
rob        <- lmtest::coeftest(best, vcov = sandwich::vcovHC(best, type = "HC3"))
rob_df     <- data.frame(term = rownames(rob), estimate = rob[,1], robust_se = rob[,2],
                         z = rob[,3], p_value = rob[,4], row.names = NULL)
rob_df$ci_low  <- rob_df$estimate - 1.96*rob_df$robust_se
rob_df$ci_high <- rob_df$estimate + 1.96*rob_df$robust_se
rob_df$dummy_pct <- ifelse(grepl("^(branch_size|urban|region2)", rob_df$term),
                           100*(exp(rob_df$estimate)-1), NA_real_)
print(rob_df)
write.csv(rob_df, "coef_table_robust.csv", row.names = FALSE)

# Influence
infl <- influence.measures(best)
capture.output(summary(infl), file = "influence_summary.txt")

# ---- 6) Linearity checks (component+residual plots) ----
png("figs/partial_residual_plots.png", width = 1400, height = 700, res = 120)
car::crPlots(best)
dev.off()

# ---- 7) Plausible interaction test: Urban × log(AreaPop) (robust; no fragile names) ----
m_int <- update(best, . ~ . + urban:log_area_pop)

# Robust vcov (HC3)
vc_fun <- function(x) sandwich::vcovHC(x, type = "HC3")

# 1) Global robust Wald test: does adding the interaction improve fit?
wald_comp <- lmtest::waldtest(best, m_int, vcov = vc_fun, test = "Chisq")

# 2) Targeted robust test on the specific interaction coefficient using a contrast matrix
cn <- names(coef(m_int))
int_idx <- grep("urban.*:log_area_pop|log_area_pop:urban", cn)

capture.output({
  cat("=== Robust model comparison (HC3): best vs best+interaction ===\n")
  print(wald_comp)

  cat("\n=== Robust test for interaction term (contrast-matrix) ===\n")
  if (length(int_idx) >= 1) {
    cat("Detected interaction coefficient:", cn[int_idx[1]], "\n")
    H <- matrix(0, nrow = 1, ncol = length(cn))
    colnames(H) <- cn
    H[1, int_idx[1]] <- 1  # test coef == 0
    print(car::linearHypothesis(m_int, hypothesis.matrix = H,
                                vcov. = vc_fun(m_int), rhs = 0, singular.ok = TRUE))
  } else {
    cat("No matching interaction coefficient found. Available coefficients:\n")
    print(cn)
  }

  cat("\n=== Robust coefficient table for interaction model (for reference) ===\n")
  print(lmtest::coeftest(m_int, vcov = vc_fun(m_int)))
}, file = "interaction_test.txt")



# ---- 8) Leave-one-out robustness for influential cases ----
inf_ids <- which(apply(infl$is.inf, 1, any))
if (length(inf_ids) == 0) inf_ids <- integer(0)
loo_rows <- c("all", as.character(inf_ids))
stab <- lapply(c(NA, inf_ids), function(i) {
  mm <- if (is.na(i)) best else update(best, data = ff[-i,])
  out <- coef(mm)[c("log_area_pop","staff_fte")]
  tibble(id = ifelse(is.na(i), "all", as.character(i)),
         log_area_pop = out[[1]], staff_fte = out[[2]])
}) |> bind_rows()
write.csv(stab, "influence_refit_stability.csv", row.names = FALSE)

# ----- 9) What-if scenario (with UNCERTAINTY) -----
scenario <- ff |>
  mutate(turnover_rate2 = pmax(0, turnover_rate2 - 0.2),
         staff_mean_experience2 = staff_mean_experience2 + 6)

# Predict on log scale WITH prediction intervals, then back-transform
pred0 <- predict(best, newdata = ff, interval = "prediction")   
pred1 <- predict(best, newdata = scenario, interval = "prediction")

to_orig <- function(mat) {
  as.data.frame(mat) |>
    transmute(fit = exp(fit), lwr = exp(lwr), upr = exp(upr))
}
b <- to_orig(pred0)
w <- to_orig(pred1)

effects <- tibble(
  pred_base = b$fit,
  pred_base_lwr = b$lwr,
  pred_base_upr = b$upr,
  pred_scen = w$fit,
  pred_scen_lwr = w$lwr,
  pred_scen_upr = w$upr
) |>
  mutate(
    delta_isk = pred_scen - pred_base,
    delta_pct = 100*(pred_scen/pred_base - 1),
    delta_pct_lwr = 100*(pred_scen_lwr/pred_base_upr - 1),
    delta_pct_upr = 100*(pred_scen_upr/pred_base_lwr - 1),
    delta_isk_lwr = pred_scen_lwr - pred_base_upr,
    delta_isk_upr = pred_scen_upr - pred_base_lwr
  )

write.csv(effects, "what_if_effects_with_PI.csv", row.names = FALSE)

what_if_summary <- tibble(
  metric = c("delta_pct","delta_isk"),
  median = c(median(effects$delta_pct), median(effects$delta_isk)),
  p25    = c(quantile(effects$delta_pct, 0.25), quantile(effects$delta_isk, 0.25)),
  p75    = c(quantile(effects$delta_pct, 0.75), quantile(effects$delta_isk, 0.75)),
  lwr_med = c(median(effects$delta_pct_lwr), median(effects$delta_isk_lwr)),
  upr_med = c(median(effects$delta_pct_upr), median(effects$delta_isk_upr))
)
write.csv(what_if_summary, "what_if_summary_PI.csv", row.names = FALSE)

print(summary(effects$delta_pct))
print(summary(effects$delta_isk))
write.csv(select(effects, delta_pct, delta_pct_lwr, delta_pct_upr, delta_isk, delta_isk_lwr, delta_isk_upr),
          "what_if_effects.csv", row.names = FALSE)  

# ----- 10) Light-weight model table-----
sink("model_readout.txt")
cat("Model: log(Profits) ~ log(AreaPop) + Staff_FTE + Turnover_rate2 + Staff_mean_experience2 + Branch_size + Urban\n\n")
print(summary(best))
cat("\nBreusch-Pagan test:\n"); print(bp)
cat("\nVIF:\n"); print(vif_vals)
cat("\nRobust (HC3) coefficients:\n"); print(rob)
cat("\n\nModel selection summary (AIC/BIC/adjR2):\n"); print(sel_tbl)
cat("\n\nInfluence refit stability (key coefs):\n"); print(stab)
sink()

sink("sessionInfo.txt"); print(sessionInfo()); sink()

cat("\nDone. Files created:\n",
    " - figs/*.png\n",
    " - coef_table_robust.csv\n",
    " - vif_table.csv\n",
    " - what_if_effects.csv (now includes intervals)\n",
    " - what_if_effects_with_PI.csv\n",
    " - what_if_summary_PI.csv\n",
    " - model_selection_summary.csv\n",
    " - interaction_test.txt\n",
    " - influence_refit_stability.csv\n",
    " - model_readout.txt\n",
    " - influence_summary.txt\n",
    " - sessionInfo.txt\n")
