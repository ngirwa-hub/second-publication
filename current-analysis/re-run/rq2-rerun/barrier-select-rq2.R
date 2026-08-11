# running command: cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq2-rerun" && Rscript "barrier-select-rq2.R"
library(tidyverse)

# 1. Read raw Zero-shot and Context barrier selections
input_path <- file.path("barriers-ctx-zeroshot-working", "barriers-ctx-zeroshot-responses.csv")

df <- readr::read_csv(input_path, show_col_types = FALSE)

required_columns <- c("base_model", "condition", "iteration", "barrier_id")

missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 2. Function to compute within-group selection shares
compute_selection_shares <- function(data, barrier_ids = 1:11) {

  clean_data <- data |>
    mutate(
      family = case_when(
        str_starts(str_to_lower(base_model), "llama") ~ "llama",
        str_starts(str_to_lower(base_model), "phi4") ~ "phi4",
        str_starts(str_to_lower(base_model), "mistral") ~ "mistral",
        str_starts(str_to_lower(base_model), "gemma3") ~ "gemma3",
        TRUE ~ str_to_lower(str_trim(base_model))
      ),
      condition = str_to_upper(str_trim(condition)),
      iteration = as.integer(iteration),
      barrier_id = as.integer(barrier_id)
    ) |>
    filter(
      condition %in% c("ZEROSHOT", "CONTEXT"),
      !is.na(family),
      !is.na(iteration),
      barrier_id %in% barrier_ids
    )

  # Number of times each barrier was selected within each: family × condition × iteration
  barrier_counts <- clean_data |>
    count(
      family,
      condition,
      iteration,
      barrier_id,
      name = "n_selected"
    )

  # Total selections made within each independent iteration
  iteration_totals <- clean_data |>
    count(
      family,
      condition,
      iteration,
      name = "total_selections"
    )

  # Add rows for barriers that were selected zero times
  complete_grid <- iteration_totals |>
    select(family, condition, iteration) |>
    tidyr::crossing(barrier_id = barrier_ids)

  iteration_rates <- complete_grid |>
    left_join(
      barrier_counts,
      by = c(
        "family",
        "condition",
        "iteration",
        "barrier_id"
      )
    ) |>
    left_join(
      iteration_totals,
      by = c(
        "family",
        "condition",
        "iteration"
      )
    ) |>
    mutate(
      n_selected = replace_na(n_selected, 0L),

      # Share of all selections in this independent iteration
      selection_share = n_selected / total_selections
    )

  # Average the iteration-level shares independently for every family and condition
  rates <- iteration_rates |>
    group_by(family, condition, barrier_id) |>
    summarise(
      pct = 100 * mean(selection_share),
      n_iterations = n_distinct(iteration),
      .groups = "drop"
    )
  return(rates)
}

# Compute the data used directly by the slopegraph
long <- compute_selection_shares(df)

# 3. Rank barriers within each family and condition
ranks <- long |>
  group_by(family, condition) |>
  arrange(family, condition, desc(pct), barrier_id) |>
  mutate(
    # barrier_id resolves ties consistently
    rank = rank(-pct, ties.method = "first")
  ) |>
  ungroup()

# Audit the complete rankings
print(
  ranks |>
    arrange(family, condition, rank) |>
    select(
      family,
      condition,
      barrier_id,
      pct,
      rank
    )
)

# 4. Identifying Top 5 barriers independently
top5 <- ranks |>
  filter(rank <= 5) |>
  group_by(family, condition) |>
  summarise(
    top5 = list(barrier_id),
    .groups = "drop"
  )

# Verifying that each family-condition group contains five barriers
top5_audit <- top5 |>
  mutate(n_top5 = map_int(top5, length))

print(top5_audit)

if (any(top5_audit$n_top5 != 5)) {
  stop("At least one family-condition group does not have five barriers.")
}

# 5. Comparing Zero-shot and Context Top 5 sets
top5_pairs <- top5 |>
  pivot_wider(
    names_from = condition,
    values_from = top5
  )

required_conditions <- c("ZEROSHOT", "CONTEXT")

missing_conditions <- setdiff(
  required_conditions,
  names(top5_pairs)
)

if (length(missing_conditions) > 0) {
  stop(
    "Missing required conditions: ",
    paste(missing_conditions, collapse = ", ")
  )
}

top5_pairs <- top5_pairs |>
  mutate(
    # Barriers present in both Top 5 sets
    intersection = map2(
      ZEROSHOT,
      CONTEXT,
      intersect
    ),

    # All barriers appearing in either Top 5 set
    union_set = map2(
      ZEROSHOT,
      CONTEXT,
      union
    ),

    n_intersection = map_int(
      intersection,
      length
    ),

    n_union = map_int(
      union_set,
      length
    ),

    jaccard = if_else(
      n_union > 0,
      n_intersection / n_union,
      NA_real_
    ),

    # Entered the Top 5 under Context
    added_context = map2(
      CONTEXT,
      ZEROSHOT,
      setdiff
    ),

    # Left the Top 5 under Context
    dropped_context = map2(
      ZEROSHOT,
      CONTEXT,
      setdiff
    )
  )

# 6. Create family-level Top 5 summary
format_barrier_set <- function(x) {
  if (length(x) == 0) {
    return("\u2014")
  }

  paste0("B", sort(x), collapse = ", ")
}

top5_summary <- top5_pairs |>
  transmute(
    family,

    zeroshot_top5 = map_chr(
      ZEROSHOT,
      format_barrier_set
    ),

    context_top5 = map_chr(
      CONTEXT,
      format_barrier_set
    ),

    retained = map_chr(
      intersection,
      format_barrier_set
    ),

    added_context = map_chr(
      added_context,
      format_barrier_set
    ),

    dropped_context = map_chr(
      dropped_context,
      format_barrier_set
    ),

    n_intersection,
    n_union,
    jaccard = round(jaccard, 3)
  )

print(top5_summary)

# 7. Get ranks and percentages for the union of Top 5 sets
union_rows <- top5_pairs |>
  select(
    family,
    union_set,
    jaccard
  ) |>
  unnest_longer(
    union_set,
    values_to = "barrier_id"
  ) |>
  mutate(barrier_id = as.integer(barrier_id)) |>
  rename(jaccard_family = jaccard)

details <- union_rows |>
  left_join(
    ranks |>
      filter(condition == "ZEROSHOT") |>
      select(
        family,
        barrier_id,
        pct_zeroshot = pct,
        rank_zeroshot = rank
      ),
    by = c("family", "barrier_id")
  ) |>
  left_join(
    ranks |>
      filter(condition == "CONTEXT") |>
      select(
        family,
        barrier_id,
        pct_context = pct,
        rank_context = rank
      ),
    by = c("family", "barrier_id")
  ) |>
  mutate(
    barrier_code = paste0("B", barrier_id),
    delta_pct = pct_context - pct_zeroshot,
    delta_rank = rank_context - rank_zeroshot
  )

# 8. Prepare and save result tables
output_dir <- "barriers-ctx-zeroshot-working"

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}

paper_table <- details |>
  arrange(family, rank_context) |>
  transmute(
    family,
    barrier_id,
    barrier_code,
    pct_zeroshot = round(pct_zeroshot, 2),
    pct_context = round(pct_context, 2),
    delta_pct = round(delta_pct, 2),
    rank_zeroshot,
    rank_context,
    delta_rank,
    jaccard = round(jaccard_family, 3)
  )

readr::write_csv(
  top5_summary,
  file.path(
    output_dir,
    "top5_overlap_summary_by_family.csv"
  )
)

readr::write_csv(
  paper_table,
  file.path(
    output_dir,
    "top5_union_per_barrier_with_jaccard.csv"
  )
)

# 9. Prepare display labels
family_label_map <- c(
  "gemma3" = "Gemma3:12B",
  "mistral" = "Mistral",
  "llama" = "LLaMa-Pro",
  "phi4" = "Phi-4"
)

family_order <- c(
  "Phi-4",
  "Gemma3:12B",
  "Mistral",
  "LLaMa-Pro"
)

details <- details |>
  mutate(
    family_label = recode(
      family,
      !!!family_label_map,
      .default = family
    ),
    family_label = factor(
      family_label,
      levels = c(
        family_order[family_order %in% family_label],
        setdiff(unique(family_label), family_order)
      )
    )
  )

# 10. Prepare Top-5 rank points and shared-barrier lines

# The slopegraph displays only the independently selected Top-5 barriers
# in each condition. Lines are drawn only for barriers shared by the two
# Top-5 sets; unconnected markers identify condition-specific membership.
top5_plot_df <- ranks |>
  filter(rank <= 5) |>
  mutate(
    barrier_code = paste0("B", barrier_id),
    family_label = recode(
      family,
      !!!family_label_map,
      .default = family
    ),
    family_label = factor(
      family_label,
      levels = family_order
    ),
    condition_label = recode(
      condition,
      "ZEROSHOT" = "Zero-shot",
      "CONTEXT" = "Context"
    ),
    x = if_else(condition == "ZEROSHOT", 1, 2)
  )

top5_plot_audit <- top5_plot_df |>
  count(family, condition, name = "n_top5")

print(top5_plot_audit)

if (any(top5_plot_audit$n_top5 != 5)) {
  stop("At least one family-condition group does not contain five plot points.")
}

shared_segments <- top5_plot_df |>
  filter(condition == "ZEROSHOT") |>
  select(
    family,
    family_label,
    barrier_id,
    barrier_code,
    rank_zeroshot = rank
  ) |>
  inner_join(
    top5_plot_df |>
      filter(condition == "CONTEXT") |>
      select(
        family,
        barrier_id,
        rank_context = rank
      ),
    by = c("family", "barrier_id")
  ) |>
  mutate(
    x = 1,
    xend = 2
  )

# Put each model's JSI directly in the corresponding plot panel.
panel_labels <- top5_pairs |>
  transmute(
    family,
    family_label = recode(
      family,
      !!!family_label_map,
      .default = family
    ),
    panel_label = sprintf(
      "%s\nJSI = %.3f",
      family_label,
      jaccard
    )
  ) |>
  select(family, panel_label)

top5_plot_df <- top5_plot_df |>
  left_join(panel_labels, by = "family") |>
  mutate(
    panel_label = factor(
      panel_label,
      levels = panel_labels |>
        mutate(
          family_order_value = match(
            recode(
              family,
              !!!family_label_map,
              .default = family
            ),
            family_order
          )
        ) |>
        arrange(family_order_value) |>
        pull(panel_label)
    )
  )

shared_segments <- shared_segments |>
  left_join(panel_labels, by = "family") |>
  mutate(
    panel_label = factor(
      panel_label,
      levels = levels(top5_plot_df$panel_label)
    )
  )

barrier_colours <- setNames(
  scales::hue_pal(l = 65, c = 100)(11),
  paste0("B", 1:11)
)

# 11. Generate the four-panel slopegraph
p <- ggplot() +
  geom_segment(
    data = shared_segments,
    aes(
      x = x,
      xend = xend,
      y = rank_zeroshot,
      yend = rank_context,
      colour = barrier_code,
      group = interaction(family, barrier_id)
    ),
    linewidth = 1.1,
    alpha = 0.9,
    lineend = "round"
  ) +
  geom_point(
    data = top5_plot_df,
    aes(
      x = x,
      y = rank,
      fill = barrier_code
    ),
    shape = 21,
    size = 6.5,
    stroke = 0.7,
    colour = "white"
  ) +
  geom_text(
    data = top5_plot_df,
    aes(
      x = x,
      y = rank,
      label = barrier_code
    ),
    size = 3.1,
    fontface = "bold",
    colour = "black"
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
    breaks = c(1, 2),
    labels = c("Zero-shot", "Context"),
    limits = c(0.8, 2.2),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_reverse(
    breaks = 1:5,
    labels = paste("Rank", 1:5),
    limits = c(5.35, 0.65)
  ) +
  facet_wrap(
    ~ panel_label,
    ncol = 2
  ) +
  labs(
    title = "Top-5 Barrier Selection—Context vs. Zero-shot",

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
    axis.ticks = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 11,
      lineheight = 0.95
    ),
    strip.background = element_rect(
      fill = "grey96",
      colour = NA
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(8, 12, 8, 12)
  )

print(p)

# 12. Save slopegraph
png_plot_path <- file.path(
  output_dir,
  "slopegraph_top5_union_by_family.png"
)

ggsave(
  filename = png_plot_path,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# save the eps
eps_plot_path <- file.path(
  output_dir,
  "slopegraph_top5_union_by_family.eps"
)

ggsave(
  filename = eps_plot_path,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white",
  device = cairo_ps
)

message("Saved slopegraph PNG: ", png_plot_path)
message("Saved slopegraph EPS: ", eps_plot_path)
message(
  "Saved summary: ",
  file.path(
    output_dir,
    "top5_overlap_summary_by_family.csv"
  )
)
message(
  "Saved barrier details: ",
  file.path(
    output_dir,
    "top5_union_per_barrier_with_jaccard.csv"
  )
)
