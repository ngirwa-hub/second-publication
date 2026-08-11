# Running command:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq3-rerun" &&
# Rscript "barrier-select-rq3.R"

suppressPackageStartupMessages(library(tidyverse))

# ============================================================
# Configuration
# ============================================================

input_path <- file.path(
  "barriers-ctx-anchor-working",
  "barriers-ctx-anchor-responses.csv"
)

output_dir <- "barriers-ctx-anchor-working"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

barrier_ids <- 1:11
top_k <- 5

family_order <- c(
  "phi4",
  "gemma3",
  "mistral",
  "llama"
)

arm_order <- c(
  "context",
  "word",
  "example",
  "low-numeric",
  "high-numeric"
)

family_label_map <- c(
  "gemma3" = "Gemma3:12B",
  "llama" = "LLaMa-Pro",
  "mistral" = "Mistral",
  "phi4" = "Phi4"
)

# ============================================================
# Helper functions
# ============================================================

format_barrier_set <- function(x) {
  if (length(x) == 0) {
    return("\u2014")
  }

  paste0("B", sort(as.integer(x)), collapse = ", ")
}

jaccard_sets <- function(a, b) {
  a <- unique(as.integer(a))
  b <- unique(as.integer(b))
  union_size <- length(union(a, b))

  if (union_size == 0) {
    return(NA_real_)
  }

  length(intersect(a, b)) / union_size
}

# Deterministic top-k:
# 1. descending selection rate;
# 2. ascending barrier ID when rates are tied.
extract_top_k <- function(data, k = top_k) {
  data |>
    arrange(desc(selection_rate), barrier_id) |>
    slice_head(n = k) |>
    pull(barrier_id) |>
    as.integer()
}

# ============================================================
# Load and validate the merged Context + Anchor data
# ============================================================

df_raw <- readr::read_csv(input_path, show_col_types = FALSE) |>
  rename_with(tolower)

required_columns <- c(
  "base_model",
  "model",
  "condition",
  "anchor_type",
  "iteration",
  "barrier_id"
)

missing_columns <- setdiff(required_columns, names(df_raw))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

df <- df_raw |>
  mutate(
    family = case_when(
      str_starts(str_to_lower(base_model), "llama") ~ "llama",
      str_starts(str_to_lower(base_model), "phi4") ~ "phi4",
      str_starts(str_to_lower(base_model), "mistral") ~ "mistral",
      str_starts(str_to_lower(base_model), "gemma3") ~ "gemma3",
      TRUE ~ str_to_lower(str_trim(base_model))
    ),
    model = str_to_lower(str_trim(model)),
    condition = str_to_lower(str_trim(condition)),
    anchor_type_clean = str_to_lower(str_trim(anchor_type)),
    iteration = as.integer(iteration),
    barrier_id = as.integer(barrier_id),
    arm = case_when(
      condition == "context" ~ "context",
      condition == "anchor" &
        anchor_type_clean == "anchor_word" ~ "word",
      condition == "anchor" &
        anchor_type_clean == "anchor_example" ~ "example",
      condition == "anchor" &
        anchor_type_clean == "anchor_num_low" ~ "low-numeric",
      condition == "anchor" &
        anchor_type_clean == "anchor_num_high" ~ "high-numeric",

      # Fallback mappings protect against minor naming changes.
      condition == "anchor" &
        str_detect(anchor_type_clean, "word") ~ "word",
      condition == "anchor" &
        str_detect(anchor_type_clean, "exam") ~ "example",
      condition == "anchor" &
        str_detect(anchor_type_clean, "low") ~ "low-numeric",
      condition == "anchor" &
        str_detect(anchor_type_clean, "high") ~ "high-numeric",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    condition %in% c("context", "anchor"),
    family %in% family_order,
    barrier_id %in% barrier_ids
  )

if (any(is.na(df$arm))) {
  unresolved <- df |>
    filter(is.na(arm)) |>
    distinct(condition, anchor_type_clean)

  stop(
    "Some rows could not be mapped to an RQ3 arm: ",
    paste(
      paste0(
        unresolved$condition,
        "/",
        unresolved$anchor_type_clean
      ),
      collapse = ", "
    )
  )
}

missing_families <- setdiff(family_order, unique(df$family))
missing_arms <- setdiff(arm_order, unique(df$arm))
missing_barriers <- setdiff(barrier_ids, unique(df$barrier_id))

if (length(missing_families) > 0) {
  stop(
    "Missing base-model families: ",
    paste(missing_families, collapse = ", ")
  )
}

if (length(missing_arms) > 0) {
  stop(
    "Missing RQ3 arms: ",
    paste(missing_arms, collapse = ", ")
  )
}

if (length(missing_barriers) > 0) {
  stop(
    "Missing barrier IDs: ",
    paste(missing_barriers, collapse = ", ")
  )
}

# One independent response is one family × model persona × arm × iteration.
response_keys <- c(
  "family",
  "model",
  "arm",
  "iteration"
)

duplicate_selections <- df |>
  count(
    across(all_of(c(response_keys, "barrier_id"))),
    name = "n_rows"
  ) |>
  filter(n_rows > 1)

if (nrow(duplicate_selections) > 0) {
  stop(
    "Duplicate selections were found for the same response and barrier. ",
    "Resolve them before calculating selection rates."
  )
}

response_audit <- df |>
  group_by(across(all_of(response_keys))) |>
  summarise(
    n_rows = n(),
    n_distinct_barriers = n_distinct(barrier_id),
    .groups = "drop"
  ) |>
  mutate(
    valid_top5_response =
      n_rows == top_k &
      n_distinct_barriers == top_k
  )

readr::write_csv(
  response_audit,
  file.path(
    output_dir,
    "rq3_barrier_response_audit.csv"
  )
)

if (any(!response_audit$valid_top5_response)) {
  invalid_summary <- response_audit |>
    filter(!valid_top5_response) |>
    count(
      n_rows,
      n_distinct_barriers,
      name = "n_responses"
    )

  print(invalid_summary)

  stop(
    "At least one response does not contain exactly five distinct barriers. ",
    "See rq3_barrier_response_audit.csv."
  )
}

response_counts <- response_audit |>
  count(
    family,
    arm,
    name = "n_responses"
  )

print(response_counts)

# ============================================================
# Response-level barrier selection rates
# ============================================================

# Each row below represents one distinct response selecting one barrier.
clean_selections <- df |>
  distinct(
    across(all_of(response_keys)),
    barrier_id
  )

barrier_counts <- clean_selections |>
  count(
    family,
    arm,
    barrier_id,
    name = "n_selected"
  )

rate_grid <- tidyr::crossing(
  family = family_order,
  arm = arm_order,
  barrier_id = barrier_ids
)

selection_rates <- rate_grid |>
  left_join(
    response_counts,
    by = c("family", "arm")
  ) |>
  left_join(
    barrier_counts,
    by = c("family", "arm", "barrier_id")
  ) |>
  mutate(
    n_selected = replace_na(n_selected, 0L),
    selection_rate = n_selected / n_responses,
    selection_percentage = 100 * selection_rate
  ) |>
  arrange(
    match(family, family_order),
    match(arm, arm_order),
    barrier_id
  )

if (
  any(is.na(selection_rates$n_responses)) ||
  any(selection_rates$n_responses <= 0)
) {
  stop("At least one family-arm group has no valid response units.")
}

# Every response selects five barriers, so rates must sum to five within
# each family-arm group.
selection_rate_audit <- selection_rates |>
  group_by(family, arm) |>
  summarise(
    n_responses = first(n_responses),
    rate_sum = sum(selection_rate),
    .groups = "drop"
  ) |>
  mutate(valid_rate_sum = abs(rate_sum - top_k) < 1e-10)

print(selection_rate_audit)

if (any(!selection_rate_audit$valid_rate_sum)) {
  stop(
    "Barrier selection rates do not sum to five in at least one ",
    "family-arm group."
  )
}

readr::write_csv(
  selection_rates,
  file.path(
    output_dir,
    "rq3_selection_rates_by_base_family_and_arm.csv"
  )
)

# ============================================================
# Top-five sets for Context and individual anchor arms
# ============================================================

top5_by_arm <- selection_rates |>
  group_by(family, arm) |>
  summarise(
    top5 = list(extract_top_k(pick(everything()))),
    .groups = "drop"
  )

top5_audit <- top5_by_arm |>
  mutate(n_top5 = map_int(top5, length))

print(top5_audit)

if (any(top5_audit$n_top5 != top_k)) {
  stop("At least one family-arm group does not contain five barriers.")
}

top5_wide <- top5_by_arm |>
  pivot_wider(
    names_from = arm,
    values_from = top5
  ) |>
  rename(
    low_numeric = `low-numeric`,
    high_numeric = `high-numeric`
  )

# ============================================================
# Pooled anchors
# ============================================================

# Maintain the previous RQ3 logic:
# give each of the four anchor arms equal weight, average each barrier's
# response-level selection rate across arms, and then select the pooled top 5.
pooled_anchor_rates <- selection_rates |>
  filter(arm != "context") |>
  group_by(family, barrier_id) |>
  summarise(
    n_anchor_arms = n_distinct(arm),
    pooled_selection_rate = mean(selection_rate),
    .groups = "drop"
  )

if (any(pooled_anchor_rates$n_anchor_arms != 4)) {
  stop(
    "Pooled-anchor rates could not be calculated from all four anchor arms."
  )
}

pooled_top5 <- pooled_anchor_rates |>
  group_by(family) |>
  arrange(
    family,
    desc(pooled_selection_rate),
    barrier_id
  ) |>
  slice_head(n = top_k) |>
  summarise(
    pooled = list(as.integer(barrier_id)),
    .groups = "drop"
  )

# ============================================================
# Jaccard similarity indices
# ============================================================

jaccard_wide <- top5_wide |>
  left_join(
    pooled_top5,
    by = "family"
  ) |>
  rowwise() |>
  mutate(
    jaccard_pooled = jaccard_sets(context, pooled),
    jaccard_word = jaccard_sets(context, word),
    jaccard_example = jaccard_sets(context, example),
    jaccard_low_numeric = jaccard_sets(
      context,
      low_numeric
    ),
    jaccard_high_numeric = jaccard_sets(
      context,
      high_numeric
    )
  ) |>
  ungroup() |>
  mutate(
    model_label = recode(
      family,
      !!!family_label_map,
      .default = family
    ),
    context_top5 = map_chr(
      context,
      format_barrier_set
    ),
    pooled_anchors_top5 = map_chr(
      pooled,
      format_barrier_set
    ),
    word_top5 = map_chr(
      word,
      format_barrier_set
    ),
    example_top5 = map_chr(
      example,
      format_barrier_set
    ),
    low_numeric_top5 = map_chr(
      low_numeric,
      format_barrier_set
    ),
    high_numeric_top5 = map_chr(
      high_numeric,
      format_barrier_set
    )
  ) |>
  arrange(match(family, family_order))

jaccard_audit <- jaccard_wide |>
  transmute(
    family,
    model_label,
    context_top5,
    pooled_anchors_top5,
    jaccard_pooled = round(jaccard_pooled, 3),
    word_top5,
    jaccard_word = round(jaccard_word, 3),
    example_top5,
    jaccard_example = round(jaccard_example, 3),
    low_numeric_top5,
    jaccard_low_numeric = round(
      jaccard_low_numeric,
      3
    ),
    high_numeric_top5,
    jaccard_high_numeric = round(
      jaccard_high_numeric,
      3
    )
  )

# Compact table used by the notebook to generate the paper's LaTeX table.
jaccard_table <- jaccard_wide |>
  transmute(
    family,
    model_label,
    jaccard_pooled = round(jaccard_pooled, 3),
    jaccard_word = round(jaccard_word, 3),
    jaccard_example = round(jaccard_example, 3),
    jaccard_low_numeric = round(
      jaccard_low_numeric,
      3
    ),
    jaccard_high_numeric = round(
      jaccard_high_numeric,
      3
    )
  )

jaccard_long <- jaccard_table |>
  pivot_longer(
    cols = starts_with("jaccard_"),
    names_to = "comparison",
    names_prefix = "jaccard_",
    values_to = "jaccard"
  ) |>
  mutate(
    comparison = recode(
      comparison,
      "pooled" = "pooled-anchors",
      "low_numeric" = "low-numeric",
      "high_numeric" = "high-numeric"
    )
  )

readr::write_csv(
  jaccard_audit,
  file.path(
    output_dir,
    "rq3_top5_jaccard_wide.csv"
  )
)

readr::write_csv(
  jaccard_long,
  file.path(
    output_dir,
    "rq3_top5_jaccard_long.csv"
  )
)

readr::write_csv(
  jaccard_table,
  file.path(
    output_dir,
    "rq3_top5_jaccard_table.csv"
  )
)

print(jaccard_table)

message(
  "Saved response audit: ",
  file.path(
    output_dir,
    "rq3_barrier_response_audit.csv"
  )
)

message(
  "Saved selection rates: ",
  file.path(
    output_dir,
    "rq3_selection_rates_by_base_family_and_arm.csv"
  )
)

message(
  "Saved Jaccard audit: ",
  file.path(
    output_dir,
    "rq3_top5_jaccard_wide.csv"
  )
)

message(
  "Saved LaTeX-ready Jaccard table data: ",
  file.path(
    output_dir,
    "rq3_top5_jaccard_table.csv"
  )
)
