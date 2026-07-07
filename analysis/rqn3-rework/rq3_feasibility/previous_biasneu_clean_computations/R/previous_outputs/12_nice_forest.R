suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2); library(forcats)
})

outdir <- "clean/feas_out"
df <- readr::read_csv(file.path(outdir, "feas_or_by_scenario_by_base_model.csv"),
                      show_col_types = FALSE)

df <- df %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c("BIAS_WORD","BIAS_EXAMPLE","BIAS_NUM_LOW","BIAS_NUM_HIGH"),
      labels = c("Word Bias","Example Bias","Low-Numeric Bias","High-Numeric Bias")
    ),
    base_model = factor(base_model),
    sig = if ("p_fdr" %in% names(.)) p_fdr < 0.05 else if ("p" %in% names(.)) p < 0.05 else NA,
    lab = sprintf("%.2f", OR)
  ) %>%
  tidyr::drop_na(OR, CI_low, CI_high)

pA <- ggplot(df, aes(x = scenario, y = OR)) +
  # subtle “no effect” band
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.9, ymax = 1.1, alpha = 0.06) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0) +
  geom_point(aes(shape = sig), size = 2.3) +
  scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 1), na.translate = FALSE, guide = "none") +
  scale_y_log10(
    breaks = c(0.5, 0.75, 1, 1.5, 2, 3),
    minor_breaks = NULL
  ) +
  coord_flip() +
  facet_wrap(~ base_model, nrow = 1) +
  labs(
    x = NULL, y = "Odds ratio vs NEU (log scale)",
    title = "Framing effects by base model",
    subtitle = "Filled points: FDR < 0.05; shaded band ≈10% around null"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title.position = "panel" # title at the center
  )

ggsave(file.path(outdir, "feas_forest_by_base_model_horiz.png"),
       pA, width = 10, height = 4.2, dpi = 400, bg = "white")
