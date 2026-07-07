# 07_forest.R
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2); library(forcats)
})
outdir <- "clean/feas_out"
df <- read_csv(file.path(outdir, "feas_or_by_scenario_by_base_model.csv"), show_col_types = FALSE)

df <- df %>%
  mutate(
    scenario   = factor(scenario, levels = c("BIAS_WORD","BIAS_EXAMPLE","BIAS_NUM_LOW","BIAS_NUM_HIGH")),
    base_model = as.factor(base_model),
    lab = sprintf("%.2f", OR)
  )

p <- ggplot(df, aes(x = scenario, y = OR)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.15) +
  geom_point(size = 2) +
  geom_text(aes(label = lab), nudge_y = 0.06, size = 3, vjust = 0) +
  facet_wrap(~ base_model, nrow = 2) +
  labs(x = NULL, y = "Odds ratio (vs NEU)", title = "Framing Effects by Base Model") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(outdir, "feas_forest_by_base_model.png"), p, width = 9, height = 5, dpi = 300, bg = "white")
message("Wrote: ", file.path(outdir, "feas_forest_by_base_model.png"))
message("✅ Wrote: ", file.path(outdir, "feas_forest_by_base_model.png"))
