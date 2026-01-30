# =============================
# Heatmap of Selection Rates by Base Model (percent) with annotations
# =============================

# ---- packages ----
library(readr); library(dplyr); library(tidyr)
library(stringr); library(ggplot2); library(scales)

# =============================
# USER CONFIG — EDIT THIS PART
# =============================

INPUT_CSV <- "unnumbered_list_selection_rates_by_combo_wide.csv"

# A) Rename base models (left = raw values in CSV; right = pretty label)
base_model_labels <- c(
  "gemma3"  = "Gemma3:12B",
  "llama"   = "LlaMa-Pro",
  "mistral" = "Mistral",
  "phi4"    = "Phi4"
)

# Optional row order (use the pretty names above). Set to NULL to keep data order.
BASE_MODEL_ORDER <- c("Gemma3:12B", "LlaMa-Pro", "Mistral", "Phi4")
# BASE_MODEL_ORDER <- NULL

# B) Barrier axis: use just codes "B1…B11" or "B# — custom text"
USE_BARRIER_TEXT <- FALSE  # set TRUE to show "B# — text"

barrier_text <- c(
  "Power losses, quality & safety",
  "Reduced reliability in DC devices",
  "Lack of DC-advantaged use cases",
  "Uncertain utility interaction",
  "Lack of pilot projects",
  "Public perception & champions",
  "Incompatibility of DC components",
  "Misconceptions & permitting delays",
  "Insufficient trained DC personnel",
  "Uncertain regulatory roadmap",
  "High costs of DC solutions"
)

WRAP_WIDTH <- 28
OUTPUT_PNG <- "unnumbered_list_selection_rates.png"

# Annotation settings
LABEL_DECIMALS <- 0                  # number of decimal places in labels
LABEL_SWITCH_THRESHOLD <- 40         # percent at/above which label text turns white

# =============================
# LOAD + RESHAPE
# =============================

rates_wide <- read_csv(INPUT_CSV, show_col_types = FALSE)

# Identify non-numeric "group" columns (includes total_count)
group_cols <- names(rates_wide)[!grepl("^[0-9]+$", names(rates_wide))]

rates_long <- rates_wide %>%
  pivot_longer(
    cols = -all_of(group_cols),
    names_to = "barrier_id",
    values_to = "rate"
  ) %>%
  # keep rates only; total_count stays separate
  filter(barrier_id != "total_count") %>%
  mutate(
    barrier_id = as.integer(barrier_id),
    rate = as.numeric(rate)
  )

# =============================
# AGGREGATE TO base_model (weighted percent)
# =============================

if (!"total_count" %in% names(rates_wide)) {
  stop("Expected 'total_count' column is missing from the wide table.")
}

# Join total_count back so we can weight by it
rates_long <- rates_long %>%
  left_join(rates_wide %>% select(all_of(group_cols), total_count), by = group_cols)

agg_by_base <- rates_long %>%
  group_by(base_model, barrier_id) %>%
  summarise(
    total_weight = sum(total_count, na.rm = TRUE),
    # Weighted average of rates * 100 to convert to percent
    rate_pct = ifelse(
      total_weight > 0,
      100 * sum(rate * total_count, na.rm = TRUE) / total_weight,
      100 * mean(rate, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Replace any NA with 0 to ensure labels always show (even for missing combos)
agg_by_base <- agg_by_base %>%
  mutate(rate_pct = ifelse(is.na(rate_pct) | !is.finite(rate_pct), 0, rate_pct))

# Rename base models
agg_by_base <- agg_by_base %>%
  mutate(
    base_model_label = recode(base_model, !!!base_model_labels, .default = base_model)
  )

# Set desired row order
if (!is.null(BASE_MODEL_ORDER)) {
  agg_by_base <- agg_by_base %>%
    mutate(base_model_label = factor(base_model_label, levels = BASE_MODEL_ORDER))
} else {
  agg_by_base <- agg_by_base %>%
    mutate(base_model_label = factor(base_model_label, levels = unique(base_model_label)))
}

# =============================
# BARRIER AXIS LABELS
# =============================

barrier_df <- tibble::tibble(
  barrier_id = 1:11,
  barrier_code = paste0("B", 1:11)
)

if (USE_BARRIER_TEXT) {
  btxt <- barrier_text
  length(btxt) <- 11
  btxt[is.na(btxt)] <- ""
  barrier_df <- barrier_df %>%
    mutate(barrier_label = str_wrap(paste0(barrier_code, " — ", btxt), WRAP_WIDTH))
} else {
  barrier_df <- barrier_df %>%
    mutate(barrier_label = barrier_code)
}

agg_for_plot <- agg_by_base %>%
  left_join(barrier_df, by = "barrier_id") %>%
  mutate(
    barrier_label = factor(barrier_label, levels = barrier_df$barrier_label) # keep B1..B11 order
  )

# Build annotation text and contrast-aware label color
fmt <- paste0("%.", LABEL_DECIMALS, "f%%")
agg_for_plot <- agg_for_plot %>%
  mutate(
    label_str = sprintf(fmt, rate_pct),
    text_col  = ifelse(rate_pct >= LABEL_SWITCH_THRESHOLD, "white", "black")
  )

# =============================
# PLOT
# =============================

p <- ggplot(agg_for_plot, aes(x = barrier_label, y = base_model_label, fill = rate_pct)) +
  geom_tile(color = "grey90", linewidth = 0.2) +
  geom_text(aes(label = label_str, color = text_col), size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_gradient(
    low = "white", high = "steelblue",
    labels = label_number(accuracy = 1, suffix = "%")
  ) +
  labs(
    title = "Unnumbered barrier-list selection rates",
    x = "Barrier code",
    y = "Base model",
    fill = "Selection rate (%)"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
  )

print(p)
ggsave(OUTPUT_PNG, p, width = 7, height = 4, dpi = 300)
