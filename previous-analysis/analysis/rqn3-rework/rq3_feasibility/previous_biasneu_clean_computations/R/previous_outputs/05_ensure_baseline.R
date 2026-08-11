
# 05_model_overall.R
# load packages
install.packages(c("readr", "dplyr", "ordinal", "xtable"))

# suppress package messages
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ordinal); library(xtable); library(forcats)
})
outdir <- "clean/feas_out/ensure_baseline"
feas <- readRDS(file.path(outdir, "feas_clean.rds"))

feas_llm <- feas %>% filter(source=="LLM")
message("Base-model levels IN MODEL (baseline is the first):")
print(levels(factor(feas_llm$base_model)))

feas_llm <- feas %>% filter(source == "LLM") %>%
  mutate(
    scenario   = fct_relevel(as.factor(scenario), "NEU"),  # set NEU as baseline
    base_model = as.factor(base_model)                     # (optional) set/inspect its baseline too
  )
levels(feas_llm$scenario)  # NEU should be first


# CLMM: rating_ord ~ scenario + base_model + (1|id_key)
fit <- clmm(rating_ord ~ scenario + base_model + (1|id_key),
            data = feas_llm, link="logit", Hess=TRUE)
capture.output(summary(fit), file = file.path(outdir, "feas_fit_llm_summary.txt"))

# ORs for scenario vs NEU
cf <- as.data.frame(coef(summary(fit))); cf$term <- rownames(cf)
sc <- cf[grepl("^scenario", cf$term), , drop = FALSE]
sc$scenario <- sub("^scenario", "", sc$term)
sc <- sc %>%
  transmute(
    scenario,
    OR      = exp(Estimate),
    CI_low  = exp(Estimate - 1.96*`Std. Error`),
    CI_high = exp(Estimate + 1.96*`Std. Error`),
    z       = `z value`,
    p       = 2*pnorm(-abs(`z value`))
  ) %>%
  mutate(contrast = paste0(scenario, " - NEU")) %>%
  select(contrast, scenario, OR, CI_low, CI_high, z, p)

write_csv(sc, file.path(outdir, "feas_or_by_scenario_llm.csv"))

#generate a latex table which stores the ORs

# Read your CSV file
sc <- read_csv(file.path(outdir, "feas_or_by_scenario_llm.csv"))

# Round values and format OR with CI
sc_table <- sc %>%
  mutate(
    OR_CI = sprintf("%.2f (%.2f, %.2f)", OR, CI_low, CI_high),
    z     = sprintf("%.2f", z),
    p     = sprintf("%.3f", p)
  ) %>%
  select(contrast, OR_CI, z, p)

# Rename columns for LaTeX table headers
colnames(sc_table) <- c("Contrast", "OR [95\\% CI]", "z", "p")

# Create LaTeX table
latex_table <- xtable(
  sc_table,
  caption = "Odds Ratios for each of the Biased Scenarios Compared with Neutral",
  label = "tab:or_scenarios",
  align = c("l", "l", "c", "c", "c")
)

# Define output path (you can change 'outdir' to any folder)
tex_file <- file.path(outdir, "feas_or_by_scenario_llm.tex")

# Save LaTeX code to .tex file
print(
  latex_table,
  include.rownames = FALSE,
  sanitize.text.function = identity,
  file = tex_file
)

message("✅ Wrote: ", file.path(outdir, "feas_or_by_scenario_llm.csv"))
message("✅ Wrote: ", file.path(outdir, "feas_fit_llm_summary.txt"))
message("✅ Wrote: ", tex_file)
