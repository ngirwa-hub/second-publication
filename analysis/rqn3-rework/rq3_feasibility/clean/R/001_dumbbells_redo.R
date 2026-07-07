# libs
library(tidyverse)
library(scales)
library(tidytext)   # reorder_within(), scale_y_reordered()

# ================= DATA =================
df <- tribble(
  ~base_model, ~scenario,         ~p_ge3,
  "gemma3-12b","CONTEXT",         0.973017762,
  "gemma3-12b","ANCHOR_WORD",     0.973017762,
  "gemma3-12b","ANCHOR_EXAMPLE",  0.973017762,
  "gemma3-12b","ANCHOR_NUM_LOW",  0.973017762,
  "gemma3-12b","ANCHOR_NUM_HIGH", 0.973017762,
  "llama-pro", "CONTEXT",         0.995545981,
  "llama-pro", "ANCHOR_WORD",     0.998801524,
  "llama-pro", "ANCHOR_EXAMPLE",  0.998429261,
  "llama-pro", "ANCHOR_NUM_LOW",  0.998094093,
  "llama-pro", "ANCHOR_NUM_HIGH", 0.998914664,
  "mistral",   "CONTEXT",         0.973017762,
  "mistral",   "ANCHOR_WORD",     0.980007798,
  "mistral",   "ANCHOR_EXAMPLE",  0.973017762,
  "mistral",   "ANCHOR_NUM_LOW",  0.973017762,
  "mistral",   "ANCHOR_NUM_HIGH", 0.973017762,
  "phi4",      "CONTEXT",         0.973017762,
  "phi4",      "ANCHOR_WORD",     0.973017762,
  "phi4",      "ANCHOR_EXAMPLE",  0.973017762,
  "phi4",      "ANCHOR_NUM_LOW",  0.973017762,
  "phi4",      "ANCHOR_NUM_HIGH", 0.973017762
)

# ================= PREP =================
anchor_levels <- c("ANCHOR_WORD","ANCHOR_EXAMPLE","ANCHOR_NUM_LOW","ANCHOR_NUM_HIGH")
anchor_label_map <- c(
  "ANCHOR_WORD"      = "Anchor: Word",
  "ANCHOR_EXAMPLE"   = "Anchor: Example",
  "ANCHOR_NUM_LOW"   = "Anchor: Low numeric",
  "ANCHOR_NUM_HIGH"  = "Anchor: High numeric"
)
label_map <- c(
  "phi4" = "Phi4",
  "gemma3-12b" = "Gemma3:12B",
  "llama-pro" = "LLaMA-Pro",
  "mistral" = "Mistral"
)

# ==== Dumbbell plot: CONTEXT vs ZEROSHOT (with fade & Δ labels, no base-model text) ====

# ---- Packages ----
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("tidytext",  quietly = TRUE)) install.packages("tidytext")
library(tidyverse)
library(tidytext)

# ---- INPUT ----
# CSV must have: condition, base_model, dc_solution, prob_geq
# (edit the path below if needed)
prob_csv <- file.path("feas_out", "clmm_outputs", "p_ge3_condition.csv")

# ---- Load & prep ----
df <- readr::read_csv(prob_csv, show_col_types = FALSE) %>%
  mutate(condition = toupper(condition)) %>%
  filter(condition %in% c("ZEROSHOT", "CONTEXT")) %>%
  mutate(base_model_label = recode(base_model, !!!label_map, .default = base_model))

# ---- Wide for Δ and ordering ----
wide <- df %>%
  select(dc_solution, base_model_label, condition, prob_geq) %>%
  mutate(condition = toupper(condition)) %>%
  pivot_wider(names_from = condition, values_from = prob_geq) %>%
  mutate(
    # ensure numeric & handle missing with 0 so lines draw
    ZEROSHOT = coalesce(as.numeric(ZEROSHOT), 0),
    CONTEXT  = coalesce(as.numeric(CONTEXT),  0),
    delta    = CONTEXT - ZEROSHOT,
    x_mid    = (CONTEXT + ZEROSHOT) / 2,
    delta_lab = sprintf("%+.3f", delta),
    # order lines within facet by Δ (largest Δ at top)
    base_model_f = tidytext::reorder_within(base_model_label, delta, dc_solution, fun = max),
    # fade rows where both are 0; soften Δ label if Δ = 0
    alpha_row   = ifelse(CONTEXT == 0 & ZEROSHOT == 0, 0.25, 1),
    alpha_label = ifelse(delta == 0, 0.6, 1)
  )

# ---- Long for points ----
pts <- wide %>%
  select(dc_solution, base_model_label, base_model_f, ZEROSHOT, CONTEXT, alpha_row) %>%
  pivot_longer(c(ZEROSHOT, CONTEXT), names_to = "condition", values_to = "prob_geq") %>%
  mutate(condition = factor(condition, levels = c("CONTEXT", "ZEROSHOT")))

# ---- Only the top-Δ row per dc_solution for annotation ----
top_delta <- wide %>%
  group_by(dc_solution) %>%
  slice_max(delta, n = 1, with_ties = FALSE) %>%
  ungroup()

# ---- Plot ----
p <- ggplot() +
  # segments: left (ZEROSHOT) -> right (CONTEXT)
  geom_segment(
    data = wide,
    aes(x = ZEROSHOT, xend = CONTEXT, y = base_model_f, yend = base_model_f, alpha = alpha_row),
    linewidth = 0.6, color = "grey70"
  ) +
  # points
  geom_point(
    data = pts,
    aes(x = prob_geq, y = base_model_f, color = condition, shape = condition, alpha = alpha_row),
    size = 2.2
  ) +
  # Δ label at midpoint (only top Δ per facet)
  geom_text(
    data = top_delta,
    aes(x = x_mid, y = base_model_f, label = delta_lab, alpha = alpha_label),
    vjust = -0.7, size = 3.2
  ) +
  scale_alpha_identity() +
  scale_shape_manual(
    values = c("CONTEXT" = 17, "ZEROSHOT" = 16),
    limits = c("CONTEXT", "ZEROSHOT"),
    breaks = c("CONTEXT", "ZEROSHOT"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("CONTEXT" = "#1b9e77", "ZEROSHOT" = "#d95f02"),
    limits = c("CONTEXT", "ZEROSHOT"),
    breaks = c("CONTEXT", "ZEROSHOT"),
    name   = NULL
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  tidytext::scale_y_reordered() +
  labs(
    x = "Probability of rating ≥ 3",
    y = NULL
  ) +
  facet_wrap(~ dc_solution, ncol = 3, scales = "free_y") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(8, "pt"),
    strip.text = element_text(face = "bold")
  )

# ---- Save ----
ggsave("faceted_dumbbells_context_vs_zeroshot.png", p, width = 12, height = 9, dpi = 300, bg = "white")
ggsave("faceted_dumbbells_context_vs_zeroshot.pdf", p, width = 12, height = 9)

message("✅ Saved: faceted_dumbbells_context_vs_zeroshot.png")
message("✅ Saved: faceted_dumbbells_context_vs_zeroshot.pdf")
