# Run with:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq3-rerun" && Rscript "imp-rq3-analysis/ppc-importance-rq3.R"
#
# Posterior predictive checks for the RQ3 importance model with
# base-model/DC-solution-specific ordinal thresholds.
#
# This script does not refit the model or generate new language-model
# responses. It compares observed ratings with replicated ordinal
# ratings drawn from the fitted Bayesian model.

required_packages <- c(
  "brms",
  "dplyr",
  "readr",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(readr)
  library(ggplot2)
})

script_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)

if (length(script_argument) != 1) {
  stop("Run this analysis with Rscript.")
}

script_dir <- dirname(
  normalizePath(
    sub("^--file=", "", script_argument),
    mustWork = TRUE
  )
)

rq3_dir <- dirname(script_dir)

input_path <- file.path(
  rq3_dir,
  "imp-ctx-anchor-working",
  "imp-ctx-anchor-responses.csv"
)

analysis_dir <- file.path(
  rq3_dir,
  "imp-ctx-anchor-working",
  "bayesian-results",
  "base-solution-threshold"
)

fit_path <- file.path(
  analysis_dir,
  "rq3_importance_base_solution_threshold_fit.rds"
)

output_dir <- file.path(
  analysis_dir,
  "posterior-predictive-checks"
)

if (!file.exists(input_path)) {
  stop("Input CSV not found: ", input_path)
}

if (!file.exists(fit_path)) {
  stop(
    "Fitted model not found: ",
    fit_path,
    "\nRun importance-rq3.R successfully before running this PPC."
  )
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

anchor_levels <- c(
  "ANCHOR_EXAMPLE",
  "ANCHOR_WORD",
  "ANCHOR_NUM_HIGH",
  "ANCHOR_NUM_LOW"
)

base_model_levels <- c(
  "gemma3",
  "llama",
  "mistral",
  "phi4"
)

comparison_levels <- c(
  "CONTEXT",
  anchor_levels
)

# ------------------------------------------------------------------
# 1. Load and verify the original data and fitted model
# ------------------------------------------------------------------

raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE
)

required_columns <- c(
  "row_id",
  "base_model",
  "dc_solution",
  "rating",
  "condition",
  "anchor_type",
  "iteration"
)

missing_columns <- setdiff(required_columns, names(raw))

if (length(missing_columns) > 0) {
  stop(
    "Missing columns in the current CSV: ",
    paste(missing_columns, collapse = ", ")
  )
}

fit <- readRDS(fit_path)

cat("Rows in original CSV:", nrow(raw), "\n")
cat("Rows used by model:", nrow(fit$data), "\n")

if (nrow(raw) != nrow(fit$data)) {
  stop(
    "The saved model and current CSV contain different numbers of ",
    "rows. Refit importance-rq3.R before running this PPC."
  )
}

required_fit_columns <- c(
  "rating",
  "base_solution",
  "anchor_example",
  "anchor_word",
  "anchor_num_high",
  "anchor_num_low"
)

missing_fit_columns <- setdiff(
  required_fit_columns,
  names(fit$data)
)

if (length(missing_fit_columns) > 0) {
  stop(
    "The fitted model does not contain the variables used by the ",
    "RQ3 importance formula: ",
    paste(missing_fit_columns, collapse = ", "),
    "."
  )
}

analysis_data <- raw |>
  mutate(
    condition = toupper(
      trimws(as.character(condition))
    ),
    cleaned_anchor_type = toupper(
      trimws(as.character(anchor_type))
    ),
    comparison = case_when(
      condition == "CONTEXT" ~ "CONTEXT",
      condition == "ANCHOR" &
        cleaned_anchor_type %in% anchor_levels ~
        cleaned_anchor_type,
      TRUE ~ NA_character_
    )
  ) |>
  transmute(
    row_id = as.character(row_id),
    base_model = tolower(
      trimws(as.character(base_model))
    ),
    dc_solution = trimws(
      as.character(dc_solution)
    ),
    comparison = comparison,
    iteration = suppressWarnings(
      as.integer(iteration)
    ),
    rating_value = suppressWarnings(
      as.integer(rating)
    )
  )

if (anyNA(analysis_data$comparison)) {
  stop(
    "The current CSV contains invalid RQ3 comparison labels."
  )
}

solution_levels <- sort(
  unique(analysis_data$dc_solution)
)

analysis_data <- analysis_data |>
  mutate(
    comparison = factor(
      comparison,
      levels = comparison_levels
    ),
    base_model = factor(
      base_model,
      levels = base_model_levels
    ),
    dc_solution = factor(
      dc_solution,
      levels = solution_levels
    ),
    base_solution = interaction(
      base_model,
      dc_solution,
      drop = TRUE,
      sep = "__"
    ),
    anchor_example = as.integer(
      comparison == "ANCHOR_EXAMPLE"
    ),
    anchor_word = as.integer(
      comparison == "ANCHOR_WORD"
    ),
    anchor_num_high = as.integer(
      comparison == "ANCHOR_NUM_HIGH"
    ),
    anchor_num_low = as.integer(
      comparison == "ANCHOR_NUM_LOW"
    ),
    rating = ordered(
      rating_value,
      levels = sort(unique(rating_value))
    )
  )

# brms stores the variables used by the model formula in fit$data.
# Descriptive identifiers such as row_id and dc_solution need not be
# retained there, so verify the saved fit against the reconstructed
# formula data instead.
fit_model_data <- fit$data |>
  transmute(
    rating = suppressWarnings(
      as.integer(as.character(rating))
    ),
    base_solution = as.character(base_solution),
    anchor_example = as.integer(anchor_example),
    anchor_word = as.integer(anchor_word),
    anchor_num_high = as.integer(anchor_num_high),
    anchor_num_low = as.integer(anchor_num_low)
  )

current_model_data <- analysis_data |>
  transmute(
    rating = rating_value,
    base_solution = as.character(base_solution),
    anchor_example,
    anchor_word,
    anchor_num_high,
    anchor_num_low
  )

data_values_match <- isTRUE(
  all.equal(
    as.data.frame(current_model_data),
    as.data.frame(fit_model_data),
    check.attributes = FALSE
  )
)

if (!data_values_match) {
  stop(
    "The current CSV does not match the data stored in the fitted ",
    "RQ3 importance model. Refit importance-rq3.R before running ",
    "this PPC."
  )
}

observed <- analysis_data |>
  transmute(
    row_id = as.character(row_id),
    base_model = as.character(base_model),
    dc_solution = as.character(dc_solution),
    comparison = as.character(comparison),
    iteration = suppressWarnings(as.integer(iteration)),
    rating = rating_value
  )

if (anyNA(observed$rating) ||
    anyNA(observed$iteration)) {
  stop(
    "The fitted model data contain invalid rating or iteration values."
  )
}

rating_values <- sort(unique(observed$rating))

if (length(rating_values) != 2) {
  stop(
    "Expected two observed rating categories. Found: ",
    paste(rating_values, collapse = ", "),
    "."
  )
}

groups <- observed |>
  distinct(
    base_model,
    dc_solution,
    comparison
  ) |>
  mutate(
    comparison = factor(
      comparison,
      levels = comparison_levels
    )
  ) |>
  arrange(
    base_model,
    dc_solution,
    comparison
  )

if (nrow(groups) != 220) {
  stop(
    "Expected 220 PPC cells (4 base models x 11 solutions x ",
    "5 conditions). Found: ",
    nrow(groups),
    "."
  )
}

cat(
  "Observed rating categories:",
  paste(rating_values, collapse = ", "),
  "\n"
)
cat(
  "Base-model/DC-solution/condition cells:",
  nrow(groups),
  "\n"
)

# ------------------------------------------------------------------
# 2. Generate replicated ratings from the fitted posterior
# ------------------------------------------------------------------

set.seed(20260727)

yrep <- brms::posterior_predict(
  fit,
  ndraws = 1000
)

cat(
  "Posterior-predictive matrix:",
  nrow(yrep),
  "simulations x",
  ncol(yrep),
  "observations\n"
)

if (ncol(yrep) != nrow(observed)) {
  stop(
    "Posterior-predictive columns do not match observed rows."
  )
}

# Ordinal predictions may use the original rating labels or category
# indices. Convert indices back to the original labels when necessary.
predicted_values <- sort(unique(as.vector(yrep)))

if (all(predicted_values %in% rating_values)) {
  yrep_ratings <- yrep
} else if (
  all(predicted_values %in% seq_along(rating_values))
) {
  yrep_ratings <- matrix(
    rating_values[as.integer(yrep)],
    nrow = nrow(yrep),
    ncol = ncol(yrep)
  )
} else {
  stop(
    "Posterior-predictive ratings contain unexpected categories: ",
    paste(predicted_values, collapse = ", "),
    "."
  )
}

# ------------------------------------------------------------------
# 3. Compare complete rating distributions in all 220 cells
# ------------------------------------------------------------------

ppc_distribution <- bind_rows(
  lapply(seq_len(nrow(groups)), function(group_index) {
    current_base_model <- groups$base_model[group_index]
    current_solution <- groups$dc_solution[group_index]
    current_comparison <- as.character(
      groups$comparison[group_index]
    )

    row_indices <- which(
      observed$base_model == current_base_model &
        observed$dc_solution == current_solution &
        observed$comparison == current_comparison
    )

    if (length(row_indices) == 0) {
      stop("An expected PPC cell contains no observations.")
    }

    bind_rows(
      lapply(rating_values, function(rating_category) {
        observed_proportion <- mean(
          observed$rating[row_indices] == rating_category
        )

        simulated_proportions <- rowMeans(
          yrep_ratings[, row_indices, drop = FALSE] ==
            rating_category
        )

        predictive_interval <- unname(
          quantile(
            simulated_proportions,
            c(0.025, 0.975)
          )
        )

        tibble(
          base_model = current_base_model,
          dc_solution = current_solution,
          comparison = current_comparison,
          rating = rating_category,
          n_observed = length(row_indices),
          observed_proportion = observed_proportion,
          predicted_mean = mean(simulated_proportions),
          predicted_median = median(simulated_proportions),
          predicted_l95 = predictive_interval[1],
          predicted_u95 = predictive_interval[2],
          observed_within_predictive_95 =
            observed_proportion >= predictive_interval[1] &
            observed_proportion <= predictive_interval[2],
          absolute_difference = abs(
            observed_proportion -
              median(simulated_proportions)
          )
        )
      })
    )
  })
)

if (nrow(ppc_distribution) != 440) {
  stop(
    "Expected 440 category-level PPC checks ",
    "(220 cells x 2 ratings). Produced: ",
    nrow(ppc_distribution),
    "."
  )
}

readr::write_csv(
  ppc_distribution,
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_ppc_rating_proportions.csv"
  )
)

ppc_summary <- ppc_distribution |>
  group_by(base_model, comparison) |>
  summarise(
    category_checks = n(),
    checks_within_predictive_95 = sum(
      observed_within_predictive_95
    ),
    proportion_within_predictive_95 = mean(
      observed_within_predictive_95
    ),
    median_absolute_difference = median(
      absolute_difference
    ),
    maximum_absolute_difference = max(
      absolute_difference
    ),
    .groups = "drop"
  )

overall_summary <- ppc_distribution |>
  summarise(
    base_model = "all",
    comparison = "all",
    category_checks = n(),
    checks_within_predictive_95 = sum(
      observed_within_predictive_95
    ),
    proportion_within_predictive_95 = mean(
      observed_within_predictive_95
    ),
    median_absolute_difference = median(
      absolute_difference
    ),
    maximum_absolute_difference = max(
      absolute_difference
    )
  )

ppc_summary <- bind_rows(
  ppc_summary,
  overall_summary
)

readr::write_csv(
  ppc_summary,
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_ppc_summary.csv"
  )
)

# ------------------------------------------------------------------
# 4. Compare median ratings as a supplementary ordinal check
# ------------------------------------------------------------------

ppc_medians <- bind_rows(
  lapply(seq_len(nrow(groups)), function(group_index) {
    current_base_model <- groups$base_model[group_index]
    current_solution <- groups$dc_solution[group_index]
    current_comparison <- as.character(
      groups$comparison[group_index]
    )

    row_indices <- which(
      observed$base_model == current_base_model &
        observed$dc_solution == current_solution &
        observed$comparison == current_comparison
    )

    observed_median <- median(
      observed$rating[row_indices]
    )

    simulated_medians <- apply(
      yrep_ratings[, row_indices, drop = FALSE],
      1,
      median
    )

    predictive_interval <- unname(
      quantile(
        simulated_medians,
        c(0.025, 0.975)
      )
    )

    tibble(
      base_model = current_base_model,
      dc_solution = current_solution,
      comparison = current_comparison,
      n_observed = length(row_indices),
      observed_median = observed_median,
      predicted_median = median(simulated_medians),
      predicted_l95 = predictive_interval[1],
      predicted_u95 = predictive_interval[2],
      observed_within_predictive_95 =
        observed_median >= predictive_interval[1] &
        observed_median <= predictive_interval[2]
    )
  })
)

readr::write_csv(
  ppc_medians,
  file.path(
    output_dir,
    "rq3_importance_base_solution_threshold_ppc_median_ratings.csv"
  )
)

# ------------------------------------------------------------------
# 5. Plot distributions separately for each base model
# ------------------------------------------------------------------

base_models <- sort(unique(ppc_distribution$base_model))

for (current_base_model in base_models) {
  model_plot_data <- ppc_distribution |>
    filter(base_model == current_base_model) |>
    mutate(
      comparison = factor(
        comparison,
        levels = comparison_levels
      )
    )

  distribution_plot <- ggplot(
    model_plot_data,
    aes(
      x = factor(rating),
      y = predicted_median
    )
  ) +
    geom_errorbar(
      aes(
        ymin = predicted_l95,
        ymax = predicted_u95
      ),
      width = 0.15,
      colour = "#0072B2",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    geom_point(
      aes(
        colour = "Posterior prediction",
        shape = "Posterior prediction"
      ),
      size = 2.2
    ) +
    geom_point(
      aes(
        y = observed_proportion,
        colour = "Observed proportion",
        shape = "Observed proportion"
      ),
      stroke = 1.2,
      size = 2.7
    ) +
    scale_colour_manual(
      name = NULL,
      breaks = c(
        "Posterior prediction",
        "Observed proportion"
      ),
      values = c(
        "Posterior prediction" = "#0072B2",
        "Observed proportion" = "#D55E00"
      )
    ) +
    scale_shape_manual(
      name = NULL,
      breaks = c(
        "Posterior prediction",
        "Observed proportion"
      ),
      values = c(
        "Posterior prediction" = 16,
        "Observed proportion" = 4
      )
    ) +
    facet_grid(
      rows = vars(dc_solution),
      cols = vars(comparison),
      labeller = labeller(
        dc_solution = label_wrap_gen(width = 32),
        comparison = label_wrap_gen(width = 15)
      )
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25)
    ) +
    labs(
      title = paste(
        "Posterior predictive check:",
        "RQ3 importance —",
        current_base_model
      ),
      subtitle = paste(
        "Base-model/DC-solution-specific thresholds"
      ),
      x = "Rating category",
      y = "Proportion"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = 7
      ),
      strip.text.y = element_text(angle = 0),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )

  safe_base_model <- gsub(
    "[^[:alnum:]_-]+",
    "_",
    current_base_model
  )

  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "rq3_importance_ppc_distributions_",
        safe_base_model,
        ".png"
      )
    ),
    plot = distribution_plot,
    width = 16,
    height = 20,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "rq3_importance_ppc_distributions_",
        safe_base_model,
        ".eps"
      )
    ),
    plot = distribution_plot,
    width = 16,
    height = 20,
    dpi = 300,
    bg = "white",
    device = grDevices::cairo_ps,
    onefile = FALSE,
    fallback_resolution = 600
  )
}

# ------------------------------------------------------------------
# 6. Print the numerical PPC assessment
# ------------------------------------------------------------------

failed_distribution_checks <- ppc_distribution |>
  filter(!observed_within_predictive_95)

cat(
  "\nCategory checks within the 95% predictive interval:",
  sum(ppc_distribution$observed_within_predictive_95),
  "of",
  nrow(ppc_distribution),
  "\n"
)

cat("\nPPC summary by base model and condition:\n")
print(ppc_summary, n = nrow(ppc_summary))

cat("\nCategory checks outside the 95% predictive interval:\n")

if (nrow(failed_distribution_checks) == 0) {
  cat("None\n")
} else {
  print(
    failed_distribution_checks |>
      select(
        base_model,
        dc_solution,
        comparison,
        rating,
        observed_proportion,
        predicted_median,
        predicted_l95,
        predicted_u95
      ),
    n = nrow(failed_distribution_checks)
  )
}

cat("\nMedian-rating posterior predictive checks:\n")
print(ppc_medians, n = nrow(ppc_medians))

cat(
  "\nRQ3 importance PPC outputs saved in:\n",
  output_dir,
  "\n"
)
