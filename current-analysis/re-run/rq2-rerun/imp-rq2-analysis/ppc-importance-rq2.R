# Run with:
# cd "/Users/HP/Documents/second-publication/current-analysis/re-run/rq2-rerun" && Rscript "imp-rq2-analysis/ppc-importance-rq2.R"
#
# Posterior predictive checks for the RQ2 importance model with
# base-model/DC-solution-specific ordinal thresholds.
#
# This script does not refit the model. It:
# - loads the fitted model created by importance-rq2.R;
# - verifies that the fitted data match the current combined CSV;
# - simulates replicated ordinal ratings from the fitted posterior;
# - compares observed and predicted rating-category proportions within
#   every base-model/DC-solution/condition cell;
# - performs a supplementary median-rating check;
# - saves numerical results and separate PNG/EPS plots for each base model.

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

# Resolve paths from this script's location rather than the terminal's
# current working directory.
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

rq2_dir <- dirname(script_dir)

input_path <- file.path(
  rq2_dir,
  "imp-ctx-zeroshot-working",
  "imp-ctx-zeroshot-responses.csv"
)

analysis_dir <- file.path(
  rq2_dir,
  "imp-ctx-zeroshot-working",
  "bayesian-results",
  "base-solution-threshold"
)

fit_path <- file.path(
  analysis_dir,
  "rq2_importance_base_solution_threshold_fit.rds"
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
    "\nRun importance-rq2.R successfully before running this PPC."
  )
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
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
    "The saved model and current CSV contain different numbers of rows. ",
    "Refit importance-rq2.R before running this PPC."
  )
}

required_fit_columns <- c(
  "rating",
  "base_solution",
  "context_indicator"
)

missing_fit_columns <- setdiff(
  required_fit_columns,
  names(fit$data)
)

if (length(missing_fit_columns) > 0) {
  stop(
    "The fitted model does not contain the expected RQ2 importance ",
    "model variables: ",
    paste(missing_fit_columns, collapse = ", "),
    "."
  )
}

# brms stores only variables used by the fitted formula. Reconstruct
# those model variables from the CSV for an exact row-wise comparison,
# while retaining readable metadata from the CSV for the PPC groups.
observed <- raw |>
  transmute(
    row_id = as.character(row_id),
    base_model = tolower(
      trimws(as.character(base_model))
    ),
    dc_solution = trimws(
      as.character(dc_solution)
    ),
    condition = toupper(
      trimws(as.character(condition))
    ),
    iteration = suppressWarnings(as.integer(iteration)),
    rating = suppressWarnings(
      as.integer(rating)
    )
  )

if (anyNA(observed$base_model) ||
    anyNA(observed$dc_solution) ||
    anyNA(observed$condition) ||
    any(observed$dc_solution == "")) {
  stop(
    "The current CSV contains missing or empty grouping values."
  )
}

base_model_levels <- c(
  "gemma3",
  "llama",
  "mistral",
  "phi4"
)

if (!setequal(
  unique(observed$base_model),
  base_model_levels
)) {
  stop(
    "The current CSV does not contain the expected four base models."
  )
}

if (any(!observed$condition %in% c(
  "ZEROSHOT",
  "CONTEXT"
))) {
  stop(
    "Condition must contain only ZEROSHOT and CONTEXT."
  )
}

if (anyNA(observed$rating) ||
    anyNA(observed$iteration)) {
  stop(
    "The current CSV contains invalid rating or iteration values."
  )
}

solution_levels <- sort(unique(observed$dc_solution))
rating_values <- sort(unique(observed$rating))

if (length(solution_levels) != 11) {
  stop(
    "Expected 11 DC solutions. Found: ",
    length(solution_levels),
    "."
  )
}

if (length(rating_values) != 3) {
  stop(
    "Expected three observed rating categories. Found: ",
    paste(rating_values, collapse = ", "),
    "."
  )
}

csv_model_data <- observed |>
  mutate(
    base_model_factor = factor(
      base_model,
      levels = base_model_levels
    ),
    dc_solution_factor = factor(
      dc_solution,
      levels = solution_levels
    ),
    base_solution = interaction(
      base_model_factor,
      dc_solution_factor,
      drop = TRUE,
      sep = "__"
    ),
    context_indicator = as.integer(
      condition == "CONTEXT"
    )
  ) |>
  transmute(
    rating = rating,
    base_solution = as.character(base_solution),
    context_indicator = context_indicator
  )

fit_model_data <- fit$data |>
  transmute(
    rating = suppressWarnings(
      as.integer(as.character(rating))
    ),
    base_solution = as.character(base_solution),
    context_indicator = suppressWarnings(
      as.integer(context_indicator)
    )
  )

data_values_match <- isTRUE(
  all.equal(
    as.data.frame(csv_model_data),
    as.data.frame(fit_model_data),
    check.attributes = FALSE
  )
)

if (!data_values_match) {
  stop(
    "The current CSV does not match the data stored in the fitted ",
    "RQ2 importance model. Refit importance-rq2.R before running ",
      "this PPC."
  )
}

if (anyNA(fit_model_data$rating) ||
    anyNA(fit_model_data$context_indicator)) {
  stop(
    "The fitted model contains invalid stored model values."
  )
}

groups <- observed |>
  distinct(
    base_model,
    dc_solution,
    condition
  ) |>
  arrange(
    base_model,
    dc_solution,
    condition
  )

if (nrow(groups) != 88) {
  stop(
    "Expected 88 PPC cells (4 base models x 11 solutions x ",
    "2 conditions). Found: ",
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

# Depending on package internals, ordinal predictions may be returned
# either with the original labels (for example 2, 3, 4) or as category
# indices (1, 2, 3). Convert indices back to the original labels when
# necessary.
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
# 3. Compare complete rating distributions in all 88 cells
# ------------------------------------------------------------------

ppc_distribution <- bind_rows(
  lapply(seq_len(nrow(groups)), function(group_index) {
    current_base_model <- groups$base_model[group_index]
    current_solution <- groups$dc_solution[group_index]
    current_condition <- groups$condition[group_index]

    row_indices <- which(
      observed$base_model == current_base_model &
        observed$dc_solution == current_solution &
        observed$condition == current_condition
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
          condition = current_condition,
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

if (nrow(ppc_distribution) != 264) {
  stop(
    "Expected 264 category-level PPC checks ",
    "(88 cells x 3 ratings). Produced: ",
    nrow(ppc_distribution),
    "."
  )
}

readr::write_csv(
  ppc_distribution,
  file.path(
    output_dir,
    "rq2_importance_base_solution_threshold_ppc_rating_proportions.csv"
  )
)

# Summarise fit without replacing the category-level evidence.
ppc_summary <- ppc_distribution |>
  group_by(base_model) |>
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
    "rq2_importance_base_solution_threshold_ppc_summary.csv"
  )
)

# ------------------------------------------------------------------
# 4. Compare median ratings as a supplementary ordinal check
# ------------------------------------------------------------------

ppc_medians <- bind_rows(
  lapply(seq_len(nrow(groups)), function(group_index) {
    current_base_model <- groups$base_model[group_index]
    current_solution <- groups$dc_solution[group_index]
    current_condition <- groups$condition[group_index]

    row_indices <- which(
      observed$base_model == current_base_model &
        observed$dc_solution == current_solution &
        observed$condition == current_condition
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
      condition = current_condition,
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
    "rq2_importance_base_solution_threshold_ppc_median_ratings.csv"
  )
)

# ------------------------------------------------------------------
# 5. Plot distributions separately for each base model
# ------------------------------------------------------------------

base_models <- sort(unique(ppc_distribution$base_model))

for (current_base_model in base_models) {
  model_plot_data <- ppc_distribution |>
    filter(base_model == current_base_model)

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
      size = 2.3
    ) +
    geom_point(
      aes(
        y = observed_proportion,
        colour = "Observed proportion",
        shape = "Observed proportion"
      ),
      stroke = 1.2,
      size = 2.8
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
      cols = vars(condition),
      labeller = labeller(
        dc_solution = label_wrap_gen(width = 32)
      )
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25)
    ) +
    labs(
      title = paste(
        "Posterior predictive check:",
        "RQ2 importance —",
        current_base_model
      ),
      subtitle = paste(
        "Base-model/DC-solution-specific thresholds"
      ),
      x = "Rating category",
      y = "Proportion"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = 8
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
        "rq2_importance_ppc_distributions_",
        safe_base_model,
        ".png"
      )
    ),
    plot = distribution_plot,
    width = 11,
    height = 20,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "rq2_importance_ppc_distributions_",
        safe_base_model,
        ".eps"
      )
    ),
    plot = distribution_plot,
    width = 11,
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

cat("\nPPC summary by base model:\n")
print(ppc_summary)

cat("\nCategory checks outside the 95% predictive interval:\n")

if (nrow(failed_distribution_checks) == 0) {
  cat("None\n")
} else {
  print(
    failed_distribution_checks |>
      select(
        base_model,
        dc_solution,
        condition,
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
  "\nRQ2 importance PPC outputs saved in:\n",
  output_dir,
  "\n"
)
