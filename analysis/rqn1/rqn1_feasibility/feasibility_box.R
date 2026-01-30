# ---- Packages ----
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# ---- Paths ----
in_path  <- "merged_feasibility_zeroshot.csv"   # <-- change if needed
out_path <- "feasibility_boxplots.png"

# ---- Read ----
df_feas <- readr::read_csv(in_path, show_col_types = FALSE)

# ---- Clean & map labels ----
label_map <- c(
  "phi4"        = "Phi4",
  "gemma3-12b"  = "Gemma3:12B",
  "llama-pro"   = "LlaMa-Pro",
  "mistral"     = "Mistral",
  "human"       = "DC experts"
)


# assumes df_feas already built as in your last script
median_digits <- 0
cap_width <- 0.28
plot_title <- "Feasibility Ratings"
y_label <- "Rating"
outfile <- "feasibility_box_overall.png"

# ---- Pick colors here
pal <- c(
  "DC experts"      = "#4C78A8",
  "Gemma3:12B" = "#F58518",
  "LlaMa-Pro"  = "#54A24B",
  "Mistral"    = "#B279A2",
  "Phi4"       = "#E45756"
)

# medians for labels (overall)
med_df <- df_feas %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(median = median(rating, na.rm = TRUE), .groups = "drop")

p_feas <- ggplot(df_feas, aes(x = group, y = rating, fill = group)) +
  geom_boxplot(width = 0.65, outlier.shape = 16, outlier.alpha = 0.35) +
  stat_summary(fun = ~boxplot.stats(.x)$stats[1], geom = "errorbar",
               aes(ymin = after_stat(y), ymax = after_stat(y)),
               width = cap_width, size = 0.5, colour = "black") +
  stat_summary(fun = ~boxplot.stats(.x)$stats[5], geom = "errorbar",
               aes(ymin = after_stat(y), ymax = after_stat(y)),
               width = cap_width, size = 0.5, colour = "black") +
  geom_text(data = med_df, aes(x = group, y = median,
            label = sprintf(paste0("%.", median_digits, "f"), median)),
            inherit.aes = FALSE, vjust = -0.6, fontface = "bold", size = 3) +
  labs(title = plot_title, x = NULL, y = y_label, fill = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  # OPTIONAL: use your color palette
  scale_fill_manual(values = pal) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    # light Y-gridlines like the importance plots
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
    # keep vertical gridlines off
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),

    panel.border       = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
    strip.background   = element_rect(fill = "grey96", colour = NA),
    plot.margin        = margin(8, 12, 8, 12),
    axis.text.x        = element_text(angle = 20, hjust = 1),
    legend.position    = "none",
    plot.title         = element_text(hjust = 0.5)
  )

ggsave(outfile, p_feas, width = 6, height = 4, dpi = 300)
p
