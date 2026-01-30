# ---- add these packages ----
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("cowplot",   quietly = TRUE)) install.packages("cowplot")
library(gridExtra)
library(cowplot)

# ---- Packages ----
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("tidytext",  quietly = TRUE)) install.packages("tidytext")
library(tidyverse)
library(tidytext)

# ---- Data ----
# df must have: condition, base_model, dc_solution, prob_geq  (long format)
prob_dir <-file.path("../clmm_outputs/probs/prob_ge3_condition.csv")
df <- readr::read_csv(prob_dir, show_col_types = FALSE)

# Keep only the two conditions we care about (upper-case as in your data)
df <- df %>% filter(condition %in% c("ZEROSHOT", "CONTEXT"))
label_map <- c(
  gemma3  = "Gemma3:12B",
  llama   = "LLaMa-Pro",
  mistral = "Mistral",
  phi4    = "Phi4"
)
# Keep only two conditions
df <- df %>% dplyr::filter(condition %in% c("ZEROSHOT", "CONTEXT"))

# ---- Rename base models ----
df <- df %>%
  dplyr::mutate(
    base_model_label = dplyr::recode(base_model, !!!label_map, .default = base_model)
  )

# ---- Wide for Δ and ordering ----
# ---- Wide for Δ and ordering ----
wide <- df %>%
  dplyr::select(dc_solution, base_model_label, condition, prob_geq) %>%
  dplyr::mutate(condition = toupper(condition)) %>%
  tidyr::pivot_wider(names_from = condition, values_from = prob_geq) %>%
  dplyr::mutate(
    ZEROSHOT  = coalesce(ZEROSHOT, 0),
    CONTEXT   = coalesce(CONTEXT,  0),
    delta     = CONTEXT - ZEROSHOT,
    x_mid     = (CONTEXT + ZEROSHOT) / 2,
    delta_lab = sprintf("%+.3f", delta),
    # facet-specific ordering: largest Δ at the top within each solution
    base_model_f = tidytext::reorder_within(base_model_label, delta, dc_solution, fun = max)
  )

# 1) Identify leaders (max Δ per dc_solution; ties allowed)
leaders <- wide %>%
  dplyr::group_by(dc_solution) %>%
  dplyr::filter(delta == max(delta, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(dc_solution, base_model_label, is_leader = TRUE)

# 2) Join back to mark leader rows, then summarise per base model
wide_with_flags <- wide %>%
  dplyr::left_join(leaders, by = c("dc_solution", "base_model_label")) %>%
  dplyr::mutate(is_leader = dplyr::coalesce(is_leader, FALSE))

legend_df <- wide_with_flags %>%
  dplyr::group_by(base_model_label) %>%
  dplyr::summarise(
    leads     = sum(is_leader, na.rm = TRUE),
    min_delta = min(delta, na.rm = TRUE),
    max_delta = max(delta, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(range = sprintf("[%.3f, %.3f]", min_delta, max_delta)) %>%
  dplyr::select(`Base model` = base_model_label,
                `Leads (count)` = leads,
                `Delta range` = range) %>%   # ← use ASCII here
  dplyr::arrange(dplyr::desc(`Leads (count)`), `Base model`)

# 3) Build a clean table grob
tbl <- gridExtra::tableGrob(
  legend_df,
  rows = NULL,
  theme = gridExtra::ttheme_minimal(
    core = list(fg_params = list(cex = 0.9)),
    colhead = list(fg_params = list(fontface = 2, cex = 0.95)),
    padding = unit(c(6, 6), "pt")
  )
)

# 4) Compose: plot on the left, table on the right
final <- cowplot::ggdraw() +
  cowplot::draw_plot(p,   x = 0.00, y = 0, width = 0.75, height = 1) +
  cowplot::draw_grob(tbl, x = 0.76, y = 0, width = 0.24, height = 1)

final

# Optional save
ggsave("faceted_dumbbells_with_side_legend.png", final, width = 13, height = 9, dpi = 300)
ggsave("faceted_dumbbells_with_side_legend.pdf", final, width = 13, height = 9)
