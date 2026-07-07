if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(tidyverse)
library(lubridate)

# ---- I/O ----
in_csv  <- "new_merged_importance_zero_context.csv"
out_dir <- "heatmaps"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pct_bm_cond_sol <- df %>%
  count(dc_solution, base_model, condition, rating, name = "n") %>%
  group_by(dc_solution, base_model, condition) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(
    rating = factor(rating, levels = rating_levels, ordered = TRUE),
    base_model = recode(base_model, !!!label_map, .default = base_model)
  )

# order base models within each solution by share of highest rating under CONTEXT (optional heuristic)
order_tbl <- pct_bm_cond_sol %>%
  filter(condition == "CONTEXT", as.numeric(rating) == max(as.numeric(rating_levels))) %>%
  group_by(dc_solution, base_model) %>%
  summarise(top_share = sum(pct, na.rm = TRUE), .groups = "drop")

pct_bm_cond_sol <- pct_bm_cond_sol %>%
  left_join(order_tbl, by = c("dc_solution", "base_model")) %>%
  group_by(dc_solution) %>%
  mutate(base_model_ord = fct_reorder(base_model, top_share, .desc = TRUE, .na_rm = TRUE)) %>%
  ungroup() %>%
  select(-top_share)

#save to csv
# ---------- For heatmap 2 (dc_solution x base_model x condition) ----------
# pct_bm_cond_sol already computed above
pct_bm_cond_sol_to_save <- pct_bm_cond_sol %>%
  mutate(pct = round(pct, 2)) %>%
  arrange(dc_solution, condition, base_model, rating)

readr::write_csv(pct_bm_cond_sol_to_save, file.path("heatmaps", "heatmap_bm_condition_solution_pct.csv"))
message("Saved: ", normalizePath(file.path("heatmaps", "heatmap_bm_condition_solution_pct.csv"),
                                 winslash="\\", mustWork = FALSE))


p2 <- ggplot(pct_bm_cond_sol,
             aes(x = rating, y = base_model_ord, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), size = 2.7) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  facet_grid(dc_solution ~ condition, scales = "free_y") +
  labs(
    x = "Rating",
    y = NULL,
    fill = "% of ratings",
    title = "Rating distribution (%) by base model × condition × DC solution"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )

p2
ggsave("heatmap_bm_condition_solution.png", p2, width = 11, height = 10, dpi = 300)
