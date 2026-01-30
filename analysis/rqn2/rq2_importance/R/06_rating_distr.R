# 01_heatmap_ratingsY_baseX_condcols.R
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# ---- I/O ----
in_csv  <- "../new_merged_importance_zero_context.csv"
out_dir <- "heatmaps"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Read ----
df <- readr::read_csv(in_csv, show_col_types = FALSE)

# 1) Normalize inputs
df <- df %>%
  mutate(
    condition = tolower(trimws(condition)),
    rating    = suppressWarnings(as.integer(rating))
  ) %>%
  filter(condition %in% c("context", "zeroshot")) %>%
  filter(!is.na(rating), rating >= 0)    # adapt if your min/max differ

# 2) Keep only the 4 target bases and create a short key
base_keep <- c("gemma3", "llama", "mistral", "phi4")
base_short_map <- c("gemma3"="gemma","llama"="llama","mistral"="mistral","phi4"="phi4")

df <- df %>%
  filter(base_model %in% base_keep) %>%
  mutate(
    base_key  = recode(base_model, !!!base_short_map),
    rating_f  = factor(rating, levels = sort(unique(rating)), ordered = TRUE),
    col_label = paste0(base_key, "-", condition)
  )

# 3) Build X levels from what’s present, ensuring pairs per base (context, zeroshot)
base_order <- c("phi4", "llama", "mistral", "gemma3") %>% intersect(unique(df$base_key))
col_levels <- as.vector(rbind(paste0(base_order, "-context"),
                              paste0(base_order, "-zeroshot")))
# keep only those columns (prevents stray labels)
df <- df %>% filter(col_label %in% col_levels)

# 4) Aggregate to % and include zero tiles
rating_levels <- levels(df$rating_f)
df_heat <- df %>%
  count(col_label, rating_f, name = "n") %>%
  tidyr::complete(
    col_label = factor(col_levels, levels = col_levels),
    rating_f  = factor(rating_levels, levels = rating_levels, ordered = TRUE),
    fill = list(n = 0)
  ) %>%
  group_by(col_label) %>%
  mutate(pct = if (sum(n) > 0) 100 * n / sum(n) else 0) %>%
  ungroup() %>%
  mutate(
    label  = sprintf("%.0f%%", pct),
    txt_col = ifelse(pct >= 40, "white", "black")
  )

# 5) Pretty display labels (edit on the right only)

# <<< PRETTY DISPLAY NAMES (edit here) >>>
pretty_col_labels <- c(
  "phi4-context"      = "Context-Phi4",
  "phi4-zeroshot"     = "Zeroshot-Phi4",
  "llama-context"    = "Context-LLaMA-Pro",
  "llama-zeroshot"   = "Zeroshot-LLaMA-Pro",
  "mistral-context"  = "Context-Mistral",
  "mistral-zeroshot" = "Zeroshot-Mistral",
  "gemma3-context"    = "Context-Gemma3:12B",
  "gemma3-zeroshot"   = "Zeroshot-Gemma3:12B"
)[ col_levels ]


# 6) Plot (no rows should be dropped now)
p <- ggplot(df_heat, aes(x = col_label, y = rating_f, fill = pct)) +
  geom_tile(color = "grey85", linewidth = 0.3) +
  geom_text(aes(label = label, color = txt_col), size = 3) +
  scale_color_identity() +
  scale_x_discrete(limits = col_levels, labels = pretty_col_labels, drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 100)) +
  labs(x = NULL, y = "Rating", fill = "% of ratings",
       title = "Rating distribution by base model and condition") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
    plot.title  = element_text(face = "bold", hjust = 0)
  )
# Save
out_png <- file.path(out_dir, "rq2_imp_ratings_distr.png")

ggsave(out_png, plot = p, width = 8, height = 4)
