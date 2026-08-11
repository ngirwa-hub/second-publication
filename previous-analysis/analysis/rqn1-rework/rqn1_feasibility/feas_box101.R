# ---- robust grouping + labeling (drop-in) ----
library(tidyverse)

csv_path       <- "../rqn1_feasibility/merged_feasibility_zeroshot.csv"  # <- set your file
plot_title     <- "Feasibility ratings boxplots"
y_label        <- "Rating"
x_label        <- "Source"
# source column: default everything to 'llm' if not present
df <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
  rename_with(tolower)
# Ensure required columns exist
need <- c("base_model","variant_id","rating")
stopifnot(all(need %in% names(df)))

# Make a 'source' if missing; treat anything not explicitly "human" as "llm"
if (!"source" %in% names(df)) {
  df <- df %>% mutate(source = "llm")
} else {
  df <- df %>% mutate(source = tolower(source))
}

# Optional condition filter
if ( exists("keep_condition", inherits=TRUE) &&
  !is.null(keep_condition) &&
  "condition" %in% names(df)) {
  df <- df %>% dplyr::filter(.data$condition %in% keep_condition)
}
# normalize base family names
df <- df %>%
  mutate(
    base_family = case_when(
      str_starts(tolower(base_model), "llama")   ~ "llama",
      str_starts(tolower(base_model), "phi4")    ~ "phi4",
      str_starts(tolower(base_model), "mistral") ~ "mistral",
      str_starts(tolower(base_model), "gemma")   ~ "gemma3",
      TRUE ~ tolower(base_model)
    ),
    rating = as.numeric(rating),
    # one box for humans; otherwise by base family (variants pooled)
    group = if_else(source == "human", "DC experts", base_family)
  ) %>%
  drop_na(rating)

# desired order: DC experts first, then known families, then any extras
fam_order <- c("phi4","mistral","llama","gemma3")
group_order <- c("DC experts",
                 fam_order[fam_order %in% unique(df$base_family)],
                 setdiff(unique(df$base_family), fam_order))
df <- df %>% mutate(group = factor(group, levels = unique(group_order)))

# pretty display names (safe lookup: '[' returns NA instead of error)
base_label_map <- c(
  phi4    = "Phi4",
  llama   = "LLaMa-Pro",
  mistral = "Mistral",
  gemma3  = "Gemma3:12B",
  Human   = "Human"
)
disp <- function(v) {
  out <- unname(base_label_map[as.character(v)])  # vectorized, safe
  ifelse(is.na(out), as.character(v), out)
}
levels(df$group) <- disp(levels(df$group))

# 1) compute per-group medians for labels
med_df <- df %>%
  group_by(group) %>%
  summarise(median = median(rating, na.rm = TRUE), .groups = "drop")

median_digits <- 0  # integers for ordinal scales; use 1/2 if you ever need decimals

# 2) plot: boxplot + whisker caps + median value as text (no dot)
p <- ggplot(df, aes(x = group, y = rating, fill = group)) +
  geom_boxplot(
    width = 0.65,
    outlier.shape = 16, outlier.alpha = 0.35
  ) +
  # whisker caps (Tukey whiskers with horizontal caps)
  # remove the old vertical errorbar layer

# lower whisker CAP only
stat_summary(
  fun = function(z) boxplot.stats(z)$stats[1],
  geom = "errorbar",
  aes(ymin = after_stat(y), ymax = after_stat(y)), # cap only
  width = 0.28, size = 0.5, colour = "black"
) +
# upper whisker CAP only
stat_summary(
  fun = function(z) boxplot.stats(z)$stats[5],
  geom = "errorbar",
  aes(ymin = after_stat(y), ymax = after_stat(y)), # cap only
  width = 0.28, size = 0.5, colour = "black"
) +
  # median value as bold text at the median line
  geom_text(
    data = med_df,
    aes(x = group, y = median, label = sprintf(paste0("%.", median_digits, "f"), median)),
    inherit.aes = FALSE,
    vjust = -0.6, fontface = "bold", size = 3
  ) +
  labs(title = plot_title, x = x_label, y = y_label, fill = NULL) +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(0,4, by = 1), minor_breaks = NULL, expand = expansion(mult = c(0.05, 0.12))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

print(p)
ggsave("rq1_feasibility_boxplot.png", p, width = 5, height = 3, dpi = 300)
