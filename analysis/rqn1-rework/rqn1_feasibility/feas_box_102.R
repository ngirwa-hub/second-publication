
# ---- robust grouping + labeling (drop-in) ----
library(tidyverse)

csv_path       <- "../rqn1_feasibility/merged_feasibility_zeroshot.csv"  # <- set your file
plot_title     <- "Feasibility Ratings"
y_label        <- "Rating"
# source column: default everything to 'llm' if not present
df <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
  rename_with(tolower)

need <- c("source","base_model","variant_id","rating")
stopifnot(all(need %in% names(df)))

if (!is.null(keep_condition) && "condition" %in% names(df)) {
  df <- df %>% filter(condition %in% keep_condition)
}

df <- df %>%
  mutate(
    source      = tolower(source),
    base_family = case_when(
      str_starts(tolower(base_model %||% ""), "llama")   ~ "llama",
      str_starts(tolower(base_model %||% ""), "phi4")    ~ "phi4",
      str_starts(tolower(base_model %||% ""), "mistral") ~ "mistral",
      str_starts(tolower(base_model %||% ""), "gemma")   ~ "gemma3",
      TRUE                                              ~ tolower(base_model %||% "")
    ),
    rating = as.numeric(rating),
    group  = if_else(source == "human", "DC experts", base_family)
  ) %>%
  drop_na(rating)

# Order: DC experts first, then known bases, then any extras
group_levels <- c("DC experts",
                  fam_order[fam_order %in% unique(df_feas$base_family)],
                  setdiff(unique(df_feas$base_family), fam_order))
df_feas <- df_feas %>% mutate(group = factor(group, levels = unique(group_levels)))

# Pretty labels (safe lookup)
disp <- function(v) { out <- unname(base_label_map[as.character(v)]); ifelse(is.na(out), as.character(v), out) }
levels(df_feas$group) <- disp(levels(df_feas$group))

# Medians for annotation
med_df <- df_feas %>%
  group_by(group) %>%
  summarise(median = median(rating, na.rm = TRUE), .groups = "drop")

# =========================
# PLOT (matches your new aesthetic)
# =========================
p_feas <- ggplot(df_feas, aes(x = group, y = rating, fill = group)) +
  geom_boxplot(
    width = 0.65,
    outlier.shape = 16, outlier.alpha = 0.35
  ) +
  # whisker end caps only (no extra vertical line)
  stat_summary(
    fun = function(z) boxplot.stats(z)$stats[1],
    geom = "errorbar",
    aes(ymin = after_stat(y), ymax = after_stat(y)),
    width = cap_width, size = 0.5, colour = "black"
  ) +
  stat_summary(
    fun = function(z) boxplot.stats(z)$stats[5],
    geom = "errorbar",
    aes(ymin = after_stat(y), ymax = after_stat(y)),
    width = cap_width, size = 0.5, colour = "black"
  ) +
  # median label (no dot)
  geom_text(
    data = med_df,
    aes(x = group, y = median,
        label = sprintf(paste0("%.", median_digits, "f"), median)),
    inherit.aes = FALSE,
    vjust = -0.6, fontface = "bold", size = 3
  ) +
  labs(title = plot_title, x = NULL, y = y_label, fill = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    # subtle grids (optional)
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "grey93", linewidth = 0.25),
    panel.grid.minor   = element_blank(),

    panel.border    = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "grey96", colour = NA),
    plot.margin     = margin(8, 12, 8, 12),

    panel.grid.major.x = element_blank(),  # keep if you prefer no vertical grids
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

print(p_feas)
ggsave(outfile, p_feas, width = 8.5, height = 5.0, dpi = 300)
