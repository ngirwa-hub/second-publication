# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq1-rerun" && Rscript barrier-select-rq1.R
library(tidyverse)
library(scales)

# Paths
in_path <- "barriers-zeroshot-working/barriers-humanllm-responses.csv"
rates_path <- "barriers-zeroshot-working/barrier_selection_rates.csv"
output_dir <- "barriers-zeroshot-working"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Read and validate 
df <- readr::read_csv(in_path, show_col_types = FALSE)

required_columns <- c("row_id", "variant_id", "base_model", "source", "barrier_id", "iteration")

missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Definition of independent response units
df_clean <- df |>
  mutate(
    source = str_to_lower(str_trim(source)),
    base_model = str_to_lower(str_trim(base_model)),
    barrier_id = as.integer(barrier_id),

    # One human response is identified by row_id.
    # One LLM response is identified by variant_id × iteration.
    response_unit = case_when(
      source == "human" ~ as.character(row_id),
      source == "llm" ~ paste(variant_id, iteration, sep = "__"),
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    source %in% c("human", "llm"),
    !is.na(response_unit),
    !is.na(barrier_id),
    barrier_id %in% 1:11
  ) |>
  # Prevent the same response from selecting the same barrier twice.
  distinct(source, base_model, response_unit, barrier_id)

# Audit of the number of barriers per response
selection_audit <- df_clean |>
  count(source, base_model, response_unit, name = "n_barriers")

print(
  selection_audit |>
    count(source, base_model, n_barriers, name = "n_responses") |>
    arrange(source, base_model, n_barriers)
)

if (any(selection_audit$n_barriers != 5)) {
  warning(
    "Some response units do not contain exactly five distinct barriers. ",
    "They remain in the analysis. Within-group shares use the actual ",
    "number of recorded selections as the denominator."
  )
}

# ---- Function to calculate within-group selection shares ----
calculate_shares <- function(data, unit_label) {
  data |>
    count(barrier_id, name = "selection_count") |>
    complete(
      barrier_id = 1:11,
      fill = list(selection_count = 0L)
    ) |>
    mutate(
      unit = unit_label,
      total_selections = sum(selection_count),
      selection_share = selection_count / total_selections
    )
}

# ---- Human selection shares ----
human_shares <- df_clean |>
  filter(source == "human") |>
  calculate_shares("DC experts")

# ---- Pooled LLM selection shares ----
pooled_llm_shares <- df_clean |>
  filter(source == "llm") |>
  calculate_shares("Pooled-LLM")

# ---- Selection shares for each base model independently ----
model_label_map <- c(
  "gemma3" = "Gemma3:12B",
  "mistral" = "Mistral",
  "llama" = "LLaMa-Pro",
  "phi4" = "Phi-4"
)

model_shares <- df_clean |>
  filter(source == "llm") |>
  group_split(base_model) |>
  map_dfr(function(model_data) {

    model_name <- first(model_data$base_model)

    display_name <- recode(
      model_name,
      !!!model_label_map,
      .default = model_name
    )

    calculate_shares(model_data, display_name)
  })

# ---- Combine results ----
shares_long <- bind_rows(
  human_shares,
  model_shares,
  pooled_llm_shares
)

# order of the units for plotting: inversely
unit_order <- c(
  "LLaMa-Pro",
  "Mistral",
  "Gemma3:12B",
  "Phi-4",
  "DC experts",
  "Pooled-LLM"
)

# Include any unexpected model names automatically.
unit_order <- c(
  unit_order[unit_order %in% shares_long$unit],
  setdiff(unique(shares_long$unit), unit_order)
)

plot_df <- shares_long |>
  mutate(
    barrier = factor(
      paste0("B", barrier_id),
      levels = paste0("B", 1:11)
    ),
    unit = factor(unit, levels = unit_order)
  )

# Verify that B1...B11 sum to 100% independently within every group.
share_check <- plot_df |>
  group_by(unit) |>
  summarise(share_sum = sum(selection_share), .groups = "drop")

print(share_check)

if (any(abs(share_check$share_sum - 1) > 1e-10)) {
  stop("Within-group barrier shares do not sum to 100%.")
}

# ---- Save transparent selection-share table ----
rates_wide <- plot_df |>
  select(unit, barrier, selection_share) |>
  pivot_wider(
    names_from = barrier,
    values_from = selection_share
  )

readr::write_csv(rates_wide, rates_path)

# ============================================================
# Expert-relative Top-5 membership and rank comparisons
# ============================================================

# Rank all 11 barriers independently within every source. Selection share
# determines rank; barrier ID resolves ties consistently.
ranked_barriers <- shares_long |>
  group_by(unit) |>
  arrange(
    desc(selection_share),
    barrier_id,
    .by_group = TRUE
  ) |>
  mutate(
    selection_rank = row_number(),
    barrier_code = paste0("B", barrier_id)
  ) |>
  ungroup()

rank_audit <- ranked_barriers |>
  group_by(unit) |>
  summarise(
    n_barriers = n(),
    n_ranks = n_distinct(selection_rank),
    minimum_rank = min(selection_rank),
    maximum_rank = max(selection_rank),
    .groups = "drop"
  )

print(rank_audit)

if (
  any(rank_audit$n_barriers != 11) ||
  any(rank_audit$n_ranks != 11) ||
  any(rank_audit$minimum_rank != 1) ||
  any(rank_audit$maximum_rank != 11)
) {
  stop("Barrier ranks are incomplete or duplicated in at least one source.")
}

rank_path <- file.path(
  output_dir,
  "rq1_barrier_selection_ranks.csv"
)

readr::write_csv(
  ranked_barriers |>
    arrange(unit, selection_rank) |>
    select(
      unit,
      barrier_id,
      barrier_code,
      selection_share,
      selection_rank
    ),
  rank_path
)

top5_ranked <- ranked_barriers |>
  filter(selection_rank <= 5)

top5_audit <- top5_ranked |>
  count(unit, name = "n_top5")

print(top5_audit)

if (any(top5_audit$n_top5 != 5)) {
  stop("At least one source does not contain exactly five Top-5 barriers.")
}

expert_top5 <- top5_ranked |>
  filter(unit == "DC experts") |>
  arrange(selection_rank) |>
  pull(barrier_id)

if (length(expert_top5) != 5) {
  stop("The DC-expert Top-5 reference set could not be identified.")
}

comparison_units <- c(
  "Gemma3:12B",
  "LLaMa-Pro",
  "Mistral",
  "Phi-4"
)

missing_comparison_units <- setdiff(
  comparison_units,
  unique(top5_ranked$unit)
)

if (length(missing_comparison_units) > 0) {
  stop(
    "Missing expected base-model groups: ",
    paste(missing_comparison_units, collapse = ", ")
  )
}

format_ranked_set <- function(ids) {
  if (length(ids) == 0) {
    return("\u2014")
  }

  paste0("B", ids, collapse = ", ")
}

top5_comparisons <- top5_ranked |>
  filter(unit %in% comparison_units) |>
  arrange(unit, selection_rank) |>
  group_by(unit) |>
  summarise(
    model_top5_ids = list(barrier_id),
    model_top5 = format_ranked_set(barrier_id),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(
    expert_top5_ids = list(.env$expert_top5),
    expert_top5_label = format_ranked_set(.env$expert_top5),
    shared_ids = list(intersect(.env$expert_top5, model_top5_ids)),
    model_only_ids = list(setdiff(model_top5_ids, .env$expert_top5)),
    expert_only_ids = list(setdiff(.env$expert_top5, model_top5_ids)),
    shared_barriers = format_ranked_set(shared_ids),
    model_only = format_ranked_set(model_only_ids),
    expert_only = format_ranked_set(expert_only_ids),
    n_shared = length(shared_ids),
    n_union = length(union(.env$expert_top5, model_top5_ids)),
    jaccard = n_shared / n_union
  ) |>
  ungroup() |>
  mutate(
    unit = factor(unit, levels = comparison_units)
  ) |>
  arrange(unit) |>
  mutate(unit = as.character(unit)) |>
  select(
    unit,
    expert_top5 = expert_top5_label,
    model_top5,
    shared_barriers,
    model_only,
    expert_only,
    n_shared,
    n_union,
    jaccard
  )

print(top5_comparisons)

comparison_path <- file.path(
  output_dir,
  "rq1_top5_expert_model_comparisons.csv"
)

readr::write_csv(top5_comparisons, comparison_path)

# ============================================================
# Two-panel mirrored Top-5 rank slopegraph
# ============================================================

# Each base model is compared independently with the DC experts in the
# centre. The left and right base models are not compared with each other.
panel_pairs <- tribble(
  ~panel, ~left_unit, ~right_unit,
  "Gemma3:12B & Phi-4", "Gemma3:12B", "Phi-4",
  "Mistral & LLaMa-Pro", "Mistral", "LLaMa-Pro"
)

make_panel_points <- function(panel, left_unit, right_unit) {
  bind_rows(
    top5_ranked |>
      filter(unit == left_unit) |>
      transmute(
        panel,
        display_unit = left_unit,
        x = 1,
        barrier_id,
        barrier_code,
        selection_rank
      ),
    top5_ranked |>
      filter(unit == "DC experts") |>
      transmute(
        panel,
        display_unit = "DC experts",
        x = 2,
        barrier_id,
        barrier_code,
        selection_rank
      ),
    top5_ranked |>
      filter(unit == right_unit) |>
      transmute(
        panel,
        display_unit = right_unit,
        x = 3,
        barrier_id,
        barrier_code,
        selection_rank
      )
  )
}

make_panel_segments <- function(panel, left_unit, right_unit) {
  expert_points <- top5_ranked |>
    filter(unit == "DC experts") |>
    select(
      barrier_id,
      expert_rank = selection_rank
    )

  left_segments <- top5_ranked |>
    filter(unit == left_unit) |>
    select(
      barrier_id,
      barrier_code,
      model_rank = selection_rank
    ) |>
    inner_join(expert_points, by = "barrier_id") |>
    transmute(
      panel,
      barrier_id,
      barrier_code,
      x = 1,
      xend = 2,
      y = model_rank,
      yend = expert_rank
    )

  right_segments <- top5_ranked |>
    filter(unit == right_unit) |>
    select(
      barrier_id,
      barrier_code,
      model_rank = selection_rank
    ) |>
    inner_join(expert_points, by = "barrier_id") |>
    transmute(
      panel,
      barrier_id,
      barrier_code,
      x = 2,
      xend = 3,
      y = expert_rank,
      yend = model_rank
    )

  bind_rows(left_segments, right_segments)
}

mirrored_points <- pmap_dfr(
  panel_pairs,
  make_panel_points
)

mirrored_segments <- pmap_dfr(
  panel_pairs,
  make_panel_segments
)

jaccard_lookup <- top5_comparisons |>
  select(unit, jaccard)

make_panel_labels <- function(panel, left_unit, right_unit) {
  left_jaccard <- jaccard_lookup |>
    filter(unit == left_unit) |>
    pull(jaccard)

  right_jaccard <- jaccard_lookup |>
    filter(unit == right_unit) |>
    pull(jaccard)

  if (length(left_jaccard) != 1 || length(right_jaccard) != 1) {
    stop("A panel JSI label could not be constructed.")
  }

  tibble(
    panel = panel,
    x = c(1, 2, 3),
    y = 5.55,
    label = c(
      sprintf("%s\nJSI = %.3f", left_unit, left_jaccard),
      "DC experts",
      sprintf("%s\nJSI = %.3f", right_unit, right_jaccard)
    )
  )
}

mirrored_labels <- pmap_dfr(
  panel_pairs,
  make_panel_labels
)

barrier_colours <- setNames(
  scales::hue_pal(l = 65, c = 100)(11),
  paste0("B", 1:11)
)

rank_slopegraph <- ggplot() +
  geom_segment(
    data = mirrored_segments,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend,
      colour = barrier_code,
      group = interaction(panel, barrier_code, x, xend)
    ),
    linewidth = 1.1,
    alpha = 0.9,
    lineend = "round"
  ) +
  geom_point(
    data = mirrored_points,
    aes(
      x = x,
      y = selection_rank,
      fill = barrier_code
    ),
    shape = 21,
    size = 5.4,
    stroke = 0.7,
    colour = "white"
  ) +
  geom_text(
    data = mirrored_points,
    aes(
      x = x,
      y = selection_rank,
      label = barrier_code
    ),
    size = 2.7,
    fontface = "bold",
    colour = "black"
  ) +
  geom_text(
    data = mirrored_labels,
    aes(x = x, y = y, label = label),
    size = 3.3,
    fontface = "bold",
    lineheight = 0.95
  ) +
  scale_colour_manual(
    values = barrier_colours,
    guide = "none"
  ) +
  scale_fill_manual(
    values = barrier_colours,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = NULL,
    limits = c(0.75, 3.25),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_reverse(
    breaks = 1:5,
    labels = paste("Rank", 1:5),
    limits = c(5.9, 0.7)
  ) +
  facet_wrap(
    ~ panel,
    ncol = 1
  ) +
  labs(
    title = "Top-5 Barrier Selection Changes—DC Experts vs. Models",
    x = NULL,
    y = "Aggregate selection-frequency rank\n(1 = highest)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.5
    ),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(
      fill = "grey96",
      colour = NA
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(8, 12, 14, 12)
  )

rank_png_path <- file.path(
  output_dir,
  "rq1-top5-expert-model-rank-slopegraph.png"
)

rank_eps_path <- file.path(
  output_dir,
  "rq1-top5-expert-model-rank-slopegraph.eps"
)

ggsave(
  rank_png_path,
  plot = rank_slopegraph,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  rank_eps_path,
  plot = rank_slopegraph,
  width = 10,
  height = 8,
  units = "in",
  dpi = 600,
  device = cairo_ps,
  bg = "white"
)

# ---- Generate heatmap ----
base_font_size <- 9
cell_font_size <- 2.7

p <- ggplot(
  plot_df,
  aes(x = barrier, y = unit, fill = selection_share)
) +
  geom_tile(
    color = "white",
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = percent(selection_share, accuracy = 1)),
    size = cell_font_size,
    color = "black"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "steelblue",
    limits = c(0, 1),
    labels = label_percent(accuracy = 1),
    name = "Share of Selections"
  ) +
  labs(
    title = "Barrier Selections by Source",
    x = "Barrier Code",
    y = "Source"
  ) +
  theme_minimal(base_size = base_font_size) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size = 10,
      hjust = 0.5
    ),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.position = "bottom"
  )

output_dir <- "barriers-zeroshot-working"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

png_path <- file.path(output_dir, "rerun-rq1_barriers.png")
eps_path <- file.path(output_dir, "rerun-rq1_barriers.eps")

ggsave(png_path, plot = p, width = 8, height = 4, dpi = 300, bg = "white")
ggsave(eps_path, plot = p, width = 5, height = 3, units = "in", device = cairo_ps)

message("Saved PNG plot: ", png_path)
message("Saved EPS plot: ", eps_path)
message("Saved selection shares: ", rates_path)
message("Saved full barrier ranks: ", rank_path)
message("Saved expert-model Top-5 comparisons: ", comparison_path)
message("Saved Top-5 rank slopegraph PNG: ", rank_png_path)
message("Saved Top-5 rank slopegraph EPS: ", rank_eps_path)
