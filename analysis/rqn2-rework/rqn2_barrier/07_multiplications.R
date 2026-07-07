# --- packages ---
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# ---------------------------
# 1) Load / shape the matrix
# ---------------------------
# Example: read from CSV (uncomment if you have a file)
mat <- readr::read_csv("heatmap_matrix_all_conditions.csv")

stopifnot(all(c("family","condition") %in% names(mat)))
barrier_cols <- setdiff(names(mat), c("family","condition"))

# keep only barrier id columns that look numeric
barrier_cols <- barrier_cols[grepl("^\\d+$", barrier_cols)]
stopifnot(length(barrier_cols) > 0)

# long format
long <- mat %>%
  pivot_longer(all_of(barrier_cols),
               names_to = "barrier_id", values_to = "pct") %>%
  mutate(
    barrier_id = as.integer(barrier_id),
    condition  = toupper(trimws(condition)),
    family     = tolower(trimws(family)),
    pct        = as.numeric(pct)
  )
#save
write_csv(long, "heatmap_matrix_long.csv")

# Ensure we only use the two conditions of interest
long <- long %>% filter(condition %in% c("ZEROSHOT","CONTEXT"))

# ---------------------------------------------------
# 2) Compute per-family ranks and Top-5 for each cond
# ---------------------------------------------------
# Rank within (family, condition) by descending pct
ranks <- long %>%
  group_by(family, condition) %>%
  arrange(family, condition, desc(pct), barrier_id) %>%
  mutate(rank = rank(-pct, ties.method = "first")) %>%
  ungroup()

# Top-5 sets
top5 <- ranks %>%
  filter(rank <= 5) %>%
  group_by(family, condition) %>%
  summarise(top5 = list(barrier_id), .groups = "drop")

# Helper to get union/intersection per family
top5_pairs <- top5 %>%
  pivot_wider(names_from = condition, values_from = top5) %>%
  mutate(
    intersection = map2(ZEROSHOT, CONTEXT, ~ intersect(.x, .y)),
    union_set    = map2(ZEROSHOT, CONTEXT, ~ union(.x, .y)),
    n_intersect  = map_int(intersection, length),
    n_union      = map_int(union_set, length),
    jaccard      = ifelse(n_union > 0, n_intersect / n_union, NA_real_),
    additions    = map2(CONTEXT, ZEROSHOT, ~ setdiff(.x, .y)),  # in CONTEXT not in ZEROSHOT
    drops        = map2(ZEROSHOT, CONTEXT, ~ setdiff(.x, .y))   # in ZEROSHOT not in CONTEXT
  )

# Family-level summary (CSV 1)
summary_tbl <- top5_pairs %>%
  transmute(
    family,
    jaccard = round(jaccard, 3),
    kept    = map_chr(intersection, ~ paste(sort(.x), collapse = ", ")),
    added   = map_chr(additions,    ~ paste(sort(.x), collapse = ", ")),
    dropped = map_chr(drops,        ~ paste(sort(.x), collapse = ", "))
  )

# ---------------------------------------------------------
# 3) Per-family per-barrier details for union of Top-5 sets
# ---------------------------------------------------------
# Extract the union rows (max 10) per family, then compute Δs and ranks
union_rows <- top5_pairs %>%
  select(family, union_set, jaccard) %>%
  unnest_longer(union_set, values_to = "barrier_id") %>%
  rename(jaccard_family = jaccard)

# Get pct and rank per condition for those barriers
details <- union_rows %>%
  left_join(
    ranks %>% filter(condition == "ZEROSHOT") %>%
      select(family, barrier_id, pct_zero = pct, rank_zero = rank),
    by = c("family","barrier_id")
  ) %>%
  left_join(
    ranks %>% filter(condition == "CONTEXT") %>%
      select(family, barrier_id, pct_ctx = pct, rank_ctx = rank),
    by = c("family","barrier_id")
  ) %>%
  mutate(
    delta_pct  = pct_ctx - pct_zero,
    delta_rank = rank_ctx - rank_zero
  )

# For paper table: keep tidy columns + include Jaccard as a column
paper_tbl <- details %>%
  arrange(family, rank_ctx) %>%  # order is your choice; here by context rank
  mutate(
    pct_zero  = round(pct_zero, 2),
    pct_ctx   = round(pct_ctx,  2),
    delta_pct = round(delta_pct, 2),
    jaccard   = round(jaccard_family, 3)
  ) %>%
  select(
    family, barrier_id,
    pct_zero, pct_ctx, delta_pct,
    rank_zero, rank_ctx, delta_rank,
    jaccard
  )

# Save both tables
dir.create("outputs", showWarnings = FALSE)
readr::write_csv(summary_tbl, file.path("outputs", "top5_overlap_summary_by_family.csv"))
readr::write_csv(paper_tbl,   file.path("outputs", "top5_union_per_barrier_with_jaccard.csv"))

message("Saved:\n- outputs/top5_overlap_summary_by_family.csv\n- outputs/top5_union_per_barrier_with_jaccard.csv")

# Rename families for display
label_map <- c(phi4 = "Phi4", llama = "LLaMa-Pro", mistral = "Mistral", gemma3 = "Gemma3:12B")

details  <- details  %>% mutate(family_lab = dplyr::recode(family, !!!label_map, .default = family))
#plot_df  <- plot_df  %>% mutate(family_lab = dplyr::recode(family, !!!label_map, .default = family))
#segs     <- segs     %>% mutate(   family_lab = dplyr::recode(family, !!!label_map, .default = family))

details <- details %>%
  mutate(family = dplyr::recode(family, !!!label_map, .default = family))

# Now build plot_df/segs from this relabeled `details`
plot_df <- details %>%
  select(family, barrier_id, rank_zero, rank_ctx) %>%
  pivot_longer(c(rank_zero, rank_ctx), names_to = "which", values_to = "rank") %>%
  mutate(condition = ifelse(which == "rank_zero", "ZEROSHOT", "CONTEXT"),
         x = factor(condition, levels = c("ZEROSHOT","CONTEXT")))

plot_df <- plot_df %>%
  mutate(condition = recode(condition,
                            "ZEROSHOT" = "Zero-shot",
                            "CONTEXT" = "Context"),
         x = factor(condition, levels = c("Zero-shot", "Context")))

segs <- details %>%
  mutate(x0 = "Zero-shot", y0 = rank_zero,
         x1 = "Context",  y1 = rank_ctx)

# Build and save
p <- ggplot() +
  geom_segment(data = segs,
               aes(x = x0, xend = x1, y = y0, yend = y1, group = interaction(family, barrier_id)),
               color = "grey60", linewidth = 0.6) +
  geom_point(data = plot_df, aes(x = x, y = rank, color = condition), size = 2.3) +
  geom_text(data = plot_df %>% filter(condition == "Zero-shot"),
            aes(x = x, y = rank, label = paste0("B", barrier_id)),
            hjust = 1.2, size = 3, color = "black") +
  geom_text(data = plot_df %>% filter(condition == "Context"),
            aes(x = x, y = rank, label = paste0("B", barrier_id)),
            hjust = -0.2, size = 3, color = "black") +
  scale_color_manual(values = c("Context"="#1b9e77","Zero-shot"="#d95f02"), name = NULL) +
  scale_x_discrete(expand = expansion(mult = c(0.12, 0.12))) +
  scale_y_reverse(breaks = scales::pretty_breaks()) +
  facet_wrap(~ family, ncol = 2, scales = "free_y") +
  labs(x = NULL, y = "Rank (1 = top)", title = "Top-5 selection changes by model") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))

ggsave(file.path("outputs","slopegraph_top5_union_by_family.png"), p, width = 10, height = 8, dpi = 300)
