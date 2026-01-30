# install.packages("tidyverse")  # if needed
library(tidyverse)
library(grid)
options(dplyr.summarise.inform = FALSE)

# =========================
# CONFIG — tweak as needed
# =========================
csv_path       <- "../merged_importance_zeroshot.csv"  # <- set your file
keep_condition <- NULL   # e.g., "context" to keep only context; or NULL for all
fam_order      <- c("phi4","mistral","llama","gemma3")
base_label_map <- c(
  phi4    = "Phi4",
  llama   = "LLaMa-Pro",
  mistral = "Mistral",
  gemma3  = "Gemma3:12B",
  Human   = "DC experts"
)

facet_ncol     <- 3         # columns in the facet grid
plot_title     <- "Importance ratings boxplots per DC solution"
y_label        <- "Rating"
x_label        <- "Source"
outfile        <- "rq1_boxplot_importance.png"

# median label formatting
median_digits  <- 0         # ordinal → integers; set 1/2 if needed
cap_width      <- 0.28      # whisker cap width

# =========================
# LOAD & PREP
# =========================
df <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
  rename_with(tolower)

# required columns
need <- c("source","base_model","variant_id","dc_solution","rating")
stopifnot(all(need %in% names(df)))

# optional filter by condition
if (!is.null(keep_condition) && "condition" %in% names(df)) {
  df <- df %>% filter(condition %in% keep_condition)
}

# normalize & group
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
    group  = if_else(source == "human", "Human", base_family)
  ) %>%
  drop_na(rating, dc_solution)

# order groups: Human first, then known families, then any extras
group_levels <- c("Human",
                  fam_order[fam_order %in% unique(df$base_family)],
                  setdiff(unique(df$base_family), fam_order))
df <- df %>% mutate(group = factor(group, levels = unique(group_levels)))

# pretty labels (safe vectorized lookup; keeps originals if not mapped)
disp <- function(v) {
  out <- unname(base_label_map[as.character(v)])
  ifelse(is.na(out), as.character(v), out)
}
levels(df$group) <- disp(levels(df$group))

# per-facet (solution × group) medians for annotation
med_df <- df %>%
  group_by(dc_solution, group) %>%
  summarise(median = median(rating, na.rm = TRUE), .groups = "drop")

# =========================
# PLOT
# =========================

# ensure these exist from earlier:
# med_df, cap_width, median_digits, facet_ncol, outfile

p <- ggplot(df, aes(x = group, y = rating, fill = group)) +
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
  # median value as bold text (no dot)
  geom_text(
    data = med_df,
    aes(x = group, y = median,
        label = sprintf(paste0("%.", median_digits, "f"), median)),
    inherit.aes = FALSE,
    vjust = -0.6, fontface = "bold", size = 3
  ) +
  labs(title = plot_title, x = x_label, y = y_label, fill = NULL) +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(0,4, by = 1), minor_breaks = NULL, expand = expansion(mult = c(0.05, 0.12))) +
  coord_cartesian(clip = "off") +
  facet_wrap(~ dc_solution, ncol = facet_ncol) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold"),
    # --- your spacing/borders go here ---
    panel.spacing.x = grid::unit(0.8, "lines"),
    panel.spacing.y = grid::unit(1.1, "lines"),
    panel.border    = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "grey96", colour = NA),
    plot.margin     = margin(8, 12, 8, 12)
  )

print(p)
ggsave(outfile, p, width = 10, height = 8, dpi = 300)
