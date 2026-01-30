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

# points data
pts <- wide %>%
  dplyr::select(dc_solution, base_model_label, base_model_f, ZEROSHOT, CONTEXT) %>%
  tidyr::pivot_longer(c(ZEROSHOT, CONTEXT),
                      names_to = "condition", values_to = "prob_geq") %>%
  dplyr::mutate(condition = factor(condition, levels = c("CONTEXT", "ZEROSHOT")))

pts <- pts %>%
  dplyr::mutate(condition = dplyr::recode(condition,
                                         "CONTEXT"  = "Context",
                                         "ZEROSHOT" = "Zero-shot"))

# ---- Only the top-Δ row per dc_solution for annotation ----
top_delta <- wide %>%
  dplyr::group_by(dc_solution) %>%
  dplyr::slice_max(delta, n = 1, with_ties = FALSE) %>%  # if ties exist and you want all, set with_ties = TRUE
  dplyr::ungroup()

# ---- Plot ----
p <- ggplot() +
  # segments: left (ZEROSHOT) -> right (CONTEXT)
  geom_segment(
    data = wide,
    aes(x = ZEROSHOT, xend = CONTEXT, y = base_model_f, yend = base_model_f),
    linewidth = 0.6, color = "grey70"
  ) +
  # points
  geom_point(
    data = pts,
    aes(x = prob_geq, y = base_model_f, color = condition, shape = condition),
    size = 2.2
  ) +
  #shapes of point
  scale_shape_manual(
    values = c("Context"=17, "Zero-shot"=16),
    limits = c("Context", "Zero-shot"),
    breaks = c("Context", "Zero-shot"),
    name = NULL) +
  # annotate ONLY the highest Δ per facet (at the midpoint)
  geom_text(
    data = top_delta,
    aes(x = x_mid, y = base_model_f, label = delta_lab),
    vjust = -0.7, size = 3.2
  ) +
  scale_color_manual(
    values = c("Context" = "#1b9e77", "Zero-shot" = "#d95f02"),
    limits = c("Context", "Zero-shot"),
    breaks = c("Context", "Zero-shot"),
    name   = NULL
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  tidytext::scale_y_reordered() +
  labs(
    title = "Importance Question: Context vs Zero-shot (Probability ≥ 3)",
    x = "Probability",
    y = "Model"
  ) +
  facet_wrap(~ dc_solution, ncol = 3, scales = "free_y") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(8, "pt"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)   # <-- add this line
  )
# When building top_delta, keep using the updated 'wide'
top_delta <- wide %>%
  dplyr::group_by(dc_solution) %>%
  dplyr::slice_max(delta, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# Optional: push label a bit to the right of the CONTEXT point instead of midpoint:
# geom_text(data = top_delta, aes(x = CONTEXT + 0.02, y = base_model_f, label = delta_lab), hjust = 0)
# and then extend x-limits slightly: scale_x_continuous(limits = c(0, 1.05), breaks = seq(0,1,0.2))
# Optional saves:
ggsave("faceted_dumbbells_ordered_by_delta.png", p, width = 12, height = 9, dpi = 300)
#ggsave("faceted_dumbbells_ordered_by_delta.pdf", p, width = 12, height = 9)
