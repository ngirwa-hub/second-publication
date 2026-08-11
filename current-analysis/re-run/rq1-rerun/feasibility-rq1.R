# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq1-rerun" && Rscript feasibility-rq1.R

# ---- robust grouping + labeling (drop-in) ----
library(tidyverse)

csv_path <- "feas-zeroshot-working/feasibility-humanllm-responses.csv"
plot_title     <- "Feasibility Ratings by Source"
y_label        <- "Rating"
x_label        <- "Source"
df <- readr::read_csv(csv_path, show_col_types = FALSE)

# Ensure required columns exist
need <- c("base_model", "rating")
stopifnot(all(need %in% names(df)))
# Set the desired left-to-right order here
group_order <- c(
  "DC experts",
  "Phi-4",
  "Gemma3:12B",
  "Mistral",
  "LLaMa-Pro"
)
df <- df %>%
  mutate(
    group = recode(
      base_model,
      human   = "DC experts",
      phi4    = "Phi-4",
      gemma3  = "Gemma3:12B",
      mistral = "Mistral",
      llama   = "LLaMa-Pro"
    ),
    group = factor(
      group,
      levels = c(
        "DC experts",
        "Gemma3:12B",
        "LLaMa-Pro",
        "Mistral",
        "Phi-4"
      )
    )
  )

# 1) compute per-group medians for labels in the boxplots
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
  scale_x_discrete(limits = group_order, drop = FALSE) +
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
    vjust = 1.5, fontface = "bold", size = 3
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

output_dir <- "feas-zeroshot-working"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
ggsave(file.path(output_dir, "rerun-rq1_feas_bplot.png"), p, width = 5, height = 3, dpi = 300)
ggsave(file.path(output_dir, "rerun-rq1_feas_bplot.eps"), p, width = 5, height = 3, units = "in", dpi = 300, device = cairo_ps)

